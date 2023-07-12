library(Rblpapi)
library(tidyverse)
library(lubridate)
library(purrr)
library(arrow) # .parquet

blpConnect()

# Dates
start_date <- as.Date('2008-09-30')
end_date <- as.Date('2023-03-31')
n_months <- interval(start_date, end_date) %/% months(1)

dates <- start_date %m+% months(0:n_months)
dates <- ceiling_date(dates, unit = 'months') - days(1)

# Fetch Index Composition
indexes <- c('SMLLBV Index', 'IBX Index')

indexes_composition <- list()
for (index in indexes){
  
  indexes_composition[[index]] <- map_dfr(
    str_replace_all(dates, '-', ''), # Date to Rblpapi format  
    \(x) {
      bds(index, 'INDX_MWEIGHT', overrides = c("END_DT" = x)) %>% 
        mutate(
          date = x, .before = 1
        )
    }
  ) %>% 
    rename(
      ticker = `Member Ticker and Exchange Code`,
      weight = `Percentage Weight`
    ) %>% 
    mutate(
      index = index, .before = 1
    )
  
}

indexes_composition <- bind_rows(indexes_composition) %>% 
  mutate(
    date = ymd(date),
    ticker = paste(ticker, 'Equity')
  )

# Get Investable Universe
fundamental_ticker <- bdp(unique(indexes_composition$ticker), 'EQY_FUND_TICKER') %>% 
  rownames_to_column('ticker') %>% 
  rename(fundamental_ticker = EQY_FUND_TICKER) %>% 
  mutate(
    fundamental_ticker = paste(fundamental_ticker, 'Equity')
  ) 

indexes_composition <- left_join(indexes_composition, fundamental_ticker, by = 'ticker')

write_parquet(indexes_composition, paste0('clean_data/', 'ibx_smll_composition.parquet'))

# Fetch, from Bloomberg, the metrics that we will consider
investable_universe <- read_parquet(paste0('clean_data/', 'ibx_smll_composition.parquet')) %>% 
  distinct(
    date, fundamental_ticker, .keep_all = TRUE
  ) %>% 
  dplyr::select(
    c(index, date, fundamental_ticker, weight)
  ) %>% 
  rename(
    ticker = fundamental_ticker
  ) 

fields <- c(
  'PX_TO_BOOK_RATIO', 'PE_RATIO', # VALUE
  'CUR_MKT_CAP' # SIZE
)

opt <- structure(
  c("MONTHLY", "ACTUAL", "PREVIOUS_VALUE", "ACTIVE_DAYS_ONLY"),
  names = c("periodicitySelection", "periodicityAdjustment", "nonTradingDayFillMethod", "nonTradingDayFillOption")
)
metrics <- bdh(
  unique(investable_universe$ticker), 
  fields = fields, options = opt,
  start.date = '20061231', end.date = '20230331'
)

clean_metrics <- metrics %>% 
  bind_rows(.id = 'ticker') %>% 
  arrange(date, ticker) %>% 
  pivot_longer(
    !c(date, ticker), names_to = 'metric', values_to = 'value'
  ) %>% 
  drop_na(value)

write_parquet(clean_metrics, paste0('clean_data/', 'metrics.parquet'))

# Indexes
indexes_prices <- bdh(
  c(indexes, 'BZACCETP Index'), 
  fields = 'PX_LAST', start.date = '20061231', 
  end.date = '20230331'
) %>% 
  bind_rows(.id = 'ticker')

write_parquet(indexes_prices, paste0('clean_data/', 'indexes_prices.parquet'))


