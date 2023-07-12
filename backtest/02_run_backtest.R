library(tidyverse)
library(lubridate)
library(purrr)
library(arrow) # .parquet
library(httr) # API
library(jsonlite) # API
library(readxl)
library(xts)
library(PerformanceAnalytics)
library(ggpubr)

source('99_functions.R')

`%ni%` = Negate(`%in%`)

# Import data ----

clean_data_folder <- 'clean_data/'

index_composition <- read_parquet(
  paste0(clean_data_folder, 'ibx_smll_composition.parquet')
) %>% 
  rename(sao_paulo_ticker = ticker, ticker = fundamental_ticker)

metrics <- read_parquet(
  paste0(clean_data_folder, 'metrics.parquet')
) %>% 
  pivot_wider(
    names_from = 'metric', values_from = 'value'
  )

nefin <- fetch_nefin_data()

index_returns <- read_parquet(
  paste0(clean_data_folder, 'indexes_prices.parquet')
) %>% 
  right_join(
    dplyr::select(nefin, date), by = 'date'
  ) %>% 
  drop_na(ticker) %>% 
  arrange(date) %>% 
  group_by(ticker) %>% 
  mutate(
    return = append(NA, diff(PX_LAST)/PX_LAST[-length(PX_LAST)]),
    return = replace_na(return, 0)
  ) %>% 
  dplyr::select(!PX_LAST) 
  

prices <- read_parquet(
  paste0(clean_data_folder, 'new_prices.parquet')
) %>% 
  arrange(date, ticker)

dates <- get_dates(
  as.Date('2008-12-31'), as.Date('2023-03-31'), 1
)

rm(clean_data_folder)

# Eligible Securities ----

respect_missing_data_restriction <- map_dfr(
  dates,
  \(x) select_eligible_securities(x, 12, prices),
  .progress = TRUE
)

composition <- index_composition %>%
  inner_join(
    respect_missing_data_restriction, by = c('date', 'ticker')
  ) 

index_coverage <- composition %>% 
  group_by(index, date) %>%  
  summarise(sum(weight))
  
composition <- composition %>% 
  distinct(date, ticker, .keep_all = TRUE) %>% 
  dplyr::select(
    !c(index, sao_paulo_ticker, weight)
  )

rm(respect_missing_data_restriction, index_coverage)

# Calculate Return Metrics ----

price_metrics_list <- list(
  list(function_name = .calculate_beta                           , n_month_lookback = 12, nefin_df = nefin, factors_names = c('Rm_minus_Rf', 'Risk_free')),
  list(function_name = .calculate_cumulative_return_minus_1_month, n_month_lookback = 12, nefin_df = NULL , factors_names = NULL),
  list(function_name = .calculate_volatility                     , n_month_lookback = 12, nefin_df = NULL , factors_names = NULL)
)

metric_list <- vector('list', length = length(price_metrics_list))
for (i in seq_along(price_metrics_list)) {
  print(i)
  
  metric_name <- price_metrics_list[[i]][['name']]
  metric_function <- price_metrics_list[[i]][['function_name']]
  estimation_period <- price_metrics_list[[i]][['n_month_lookback']]
  nefin_df <- price_metrics_list[[i]][['nefin_df']]
  nefin_factors <- price_metrics_list[[i]][['factors_names']]
  
  metric_list[[i]] <- map_dfr(
    dates,
    \(x) calculate_price_metrics(
      metric_function, dplyr::filter(composition, date == x)$ticker, x,
      estimation_period, prices, nefin_df, nefin_factors
    ),
    .progress = TRUE
  )
  
}

names(metric_list) <- c('BETA', 'MOMENTUM', 'VOLATILITY')

return_metrics <- metric_list %>% 
  bind_rows(.id = 'metric_name') %>% 
  pivot_wider(
    names_from = 'metric_name', values_from = 'metric'
  )

rm(price_metrics_list, metric_list, i, metric_name, metric_function, estimation_period, nefin_df, nefin_factors)

# All Metrics ----

all_metrics <- composition %>% 
  left_join(
    metrics, by = c('date', 'ticker')
  ) %>% 
  left_join(
    return_metrics, by = c('date', 'ticker')
  ) %>% 
  arrange(date, ticker) %>% 
  mutate(
    EARNINGS_TO_PRICE = 1 / PE_RATIO,
    EARNINGS_TO_PRICE = replace_na(EARNINGS_TO_PRICE, 0),
    BOOK_TO_MARKET = 1 / PX_TO_BOOK_RATIO,
    BOOK_TO_MARKET = replace_na(BOOK_TO_MARKET, 0)
  ) %>% 
  dplyr::select(
    !c(PE_RATIO, PX_TO_BOOK_RATIO)
  ) %>% 
  group_by(ticker) %>% 
  mutate(
    CUR_MKT_CAP = na.locf(CUR_MKT_CAP)
  )

clean_metrics <- all_metrics %>% 
  mutate_at(
    vars(VOLATILITY, BETA, CUR_MKT_CAP),
    ~ .x * -1 # Buy low, sell high to buy high, sell low
  ) %>%
  group_by(date) %>% 
  # Normalize data
  mutate(across(
    !ticker, ~(.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE)
  )) %>%
  ungroup() %>% 
  rowwise() %>% 
  mutate(
    # Summarize multiple factor metrics into one
    LOW_VOLATILITY = mean(c(VOLATILITY, BETA)),
    VALUE = mean(c(EARNINGS_TO_PRICE, BOOK_TO_MARKET))
  ) %>% 
  rename(SIZE = CUR_MKT_CAP) %>% 
  dplyr::select(
    'date', 'ticker', 'VALUE', 'SIZE', 'MOMENTUM', 'LOW_VOLATILITY'
  )

rm(metrics, return_metrics)

# Stars ----

stars <- clean_metrics %>% 
  group_by(date) %>% 
  # Stars for each asset and factor
  mutate(across(
    !ticker, ~as.numeric(get_number_of_stars(.x))
  )) %>% 
  ungroup() %>% 
  # Overall star rating
  rowwise() %>% 
  mutate(
    ALL_FACTORS = mean(c_across(!c(ticker, date)))
  ) %>% 
  ungroup() %>% 
  group_by(date) %>%
  mutate(
    ALL_FACTORS = as.numeric(get_number_of_stars(ALL_FACTORS))
  )

rm(clean_metrics)

# Weights ----

stars_weights <- stars %>% 
  left_join(
    dplyr::select(all_metrics, ticker, date, CUR_MKT_CAP), by = c('ticker', 'date')
  ) %>% 
  pivot_longer(
    !c(date, ticker, CUR_MKT_CAP), names_to = 'metric_name', values_to = 'stars'
  ) %>% 
  group_by(date, metric_name, stars) %>% 
  mutate(
    equal_weighted = 1 / n(),
    market_cap_weighted = CUR_MKT_CAP / sum(CUR_MKT_CAP)
  ) %>% 
  dplyr::select(
    metric_name, date, ticker, stars, equal_weighted, market_cap_weighted
  ) 

rm(stars)

# Portfolio Returns ----

stars_returns <- stars_weights %>% 
  dplyr::filter(
    date < '2023-03-31'
  ) %>% 
  pivot_longer(
    !c(metric_name, date, ticker, stars), names_to = 'weight_type', values_to = 'weight'
  ) %>% 
  group_by(metric_name, date, stars, weight_type) %>% 
  group_map(~data.frame(
    metric_name = .y$metric_name,
    stars = .y$stars,
    weight_type = .y$weight_type,
    calculate_backtest_returns(
      prices, .x$ticker, .y$date, .x$weight, 1
    )
  )) %>% 
  bind_rows() %>% 
  arrange(date, weight_type, metric_name, stars)

# Performance Analysis ----

## Table
nefin_evaluation_period <- nefin %>% 
  dplyr::filter(
    date >= min(stars_returns$date) & date <= max(stars_returns$date)
  )

indexes_performance_summary <- index_returns %>%
  dplyr::filter(
    date >= min(stars_returns$date) & date <= max(stars_returns$date)
  ) %>% 
  group_by(ticker) %>% 
  group_map(~data.frame(
    metric_name = .y$ticker,
    performance_summary(.x$return, nefin_evaluation_period)
  )) %>% 
  bind_rows() %>% 
  mutate(
    values = ifelse(metric_name == 'BZACCETP Index' & names != 'Annual. Return', NA, values),
    values = ifelse(metric_name %in% c('IBX Index', 'SMLLBV Index') & names %in% c('Alpha', 't(alpha)', 'Beta', 'Info. Ratio', 'Track Error'), NA, values)
  ) %>% 
  pivot_wider(names_from = 'names', values_from = 'values')

portfolios_performance_summary <- stars_returns %>% 
  group_by(metric_name, stars, weight_type) %>% 
  group_map(~data.frame(
    metric_name = .y$metric_name,
    stars = .y$stars,
    weight_type = .y$weight_type,
    performance_summary(.x$portfolio.returns, nefin_evaluation_period)
  )) %>% 
  bind_rows() %>% 
  pivot_wider(names_from = 'names', values_from = 'values') %>% 
  bind_rows(indexes_performance_summary) %>% 
  arrange(weight_type)

## Plots

cumprod_stars_returns <- stars_returns %>% 
  group_by(metric_name, stars, weight_type) %>% 
  mutate(
    portfolio.returns = cumprod(1 + portfolio.returns),
    metric_name = ifelse(metric_name == 'LOW_VOLATILITY', 'Baixa Volatilidade', metric_name),
    metric_name = ifelse(metric_name == 'MOMENTUM', 'Momento', metric_name),
    metric_name = ifelse(metric_name == 'SIZE', 'Tamanho', metric_name),
    metric_name = ifelse(metric_name == 'VALUE', 'Valor', metric_name),
    metric_name = ifelse(metric_name == 'ALL_FACTORS', 'Todos Fatores', metric_name),
    stars = ifelse(stars == 1, '1 Estrela', paste(stars, 'Estrelas')),
    stars = factor(stars, levels = c(paste(5:2, 'Estrelas'), '1 Estrela'))
  )

ew_four_factors <- cumprod_stars_returns %>% 
  dplyr::filter(
    weight_type == 'equal_weighted' & metric_name != 'Todos Fatores'
  )  %>% 
  ggplot(aes(x=date, y=portfolio.returns, group=stars, color=stars)) +
  geom_line() +
  viridis::scale_color_viridis(discrete = TRUE) +
  xlab(element_blank()) + ylab(element_blank()) +
  theme_classic() +
  theme(
    legend.title=element_blank()
  ) +
  facet_wrap(vars(metric_name), ncol = 2)
ggsave('ew_four_factors.png', ew_four_factors, width = 2000, height = 1000, units = 'px', dpi = 'retina')

ew_one_factor <- cumprod_stars_returns %>% 
  dplyr::filter(
    weight_type == 'equal_weighted' & metric_name == 'Todos Fatores'
  )  %>% 
  ggplot(aes(x=date, y=portfolio.returns, group=stars, color=stars)) +
  geom_line() +
  viridis::scale_color_viridis(discrete = TRUE) +
  xlab(element_blank()) + ylab(element_blank()) +
  theme_classic() +
  theme(
    legend.title=element_blank()
  )
ggsave('ew_one_factor.png', ew_one_factor, width = 2000, height = 1000, units = 'px', dpi = 'retina')

mcw_four_factors <- cumprod_stars_returns %>% 
  dplyr::filter(
    weight_type == 'market_cap_weighted' & metric_name != 'Todos Fatores'
  )  %>% 
  ggplot(aes(x=date, y=portfolio.returns, group=stars, color=stars)) +
  geom_line() +
  viridis::scale_color_viridis(discrete = TRUE) +
  xlab(element_blank()) + ylab(element_blank()) +
  theme_classic() +
  theme(
    legend.title=element_blank()
  ) +
  facet_wrap(vars(metric_name), ncol = 2)
ggsave('mcw_four_factors.png', mcw_four_factors, width = 2000, height = 1000, units = 'px', dpi = 'retina')

mcw_one_factor <- cumprod_stars_returns %>% 
  dplyr::filter(
    weight_type == 'market_cap_weighted' & metric_name == 'Todos Fatores'
  )  %>% 
  ggplot(aes(x=date, y=portfolio.returns, group=stars, color=stars)) +
  geom_line() +
  viridis::scale_color_viridis(discrete = TRUE) +
  xlab(element_blank()) + ylab(element_blank()) +
  theme_classic() +
  theme(
    legend.title=element_blank()
  )
ggsave('mcw_one_factor.png', mcw_one_factor, width = 2000, height = 1000, units = 'px', dpi = 'retina')

