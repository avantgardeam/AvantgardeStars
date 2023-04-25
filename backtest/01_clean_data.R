library(Rblpapi)
library(tidyverse)
library(lubridate)
library(purrr)
library(arrow) # .parquet

source('99_functions.R')

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

write_parquet(indexes_composition, paste0('clean_data/', 'ibx_smll_composition.parquet'))

indexes_composition <- read_parquet(paste0('clean_data/', 'ibx_smll_composition.parquet'))

# Get Investable Universe
fundamental_ticker <- bdp(unique(indexes_composition$ticker), 'EQY_FUND_TICKER') %>% 
  rownames_to_column('ticker') %>% 
  rename(fundamental_ticker = EQY_FUND_TICKER) %>% 
  mutate(
    fundamental_ticker = paste(fundamental_ticker, 'Equity')
  ) 

# Fetch, from Bloomberg, the metrics that we will consider
fields <- c(
  'TRAIL_12M_EPS_BEF_XO_ITEM', 'BOOK_VAL_PER_SH', # VALUE
  'CUR_MKT_CAP', # SIZE
  'CURRENT_TRR_1MO', 'CURRENT_TRR_1YR', # MOMENTUM
  'EQY_BETA', 'VOLATILITY_260D' # LOW VOL
)

investable_universe <- indexes_composition %>% 
  left_join(
    fundamental_ticker, by = 'ticker'
  ) %>% 
  distinct(date, fundamental_ticker) %>% 
  rename(ticker = fundamental_ticker) 

opt <- c("periodicitySelection"="MONTHLY")
metrics <- bdh(
  unique(investable_universe$ticker), 
  fields = fields, options = opt,
  start.date = '20061231', end.date = '20230331'
) %>% 
  bind_rows(.id = 'ticker') %>% 
  pivot_longer(
    !c(date, ticker), names_to = 'metric', values_to = 'value'
  ) %>% 
  drop_na(value) %>% 
  arrange(date, ticker) %>% 
  pivot_wider(
    names_from = 'metric', values_from = 'value'
  )

write_parquet(metrics, paste0('clean_data/', 'metrics.parquet'))

