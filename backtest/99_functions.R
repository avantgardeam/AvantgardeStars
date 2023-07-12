
#################################################################
##                       Number of Stars                       ##
#################################################################

get_number_of_stars <- function(factor_metric){
  #' Assign Stars.
  #' @description This function assigns a star rating (ranging from 1 to 5) 
  #' to each asset based on how well it ranks in comparison to 
  #' other assets with respect to a given factor. 
  #' 
  #' The higher the 
  #' asset's rank, the higher the star rating it will receive. 
  #' Specifically, the top 10% of assets will receive a 5-star 
  #' rating, the next 20% (i.e., assets ranked between 10% and 30%) 
  #' will receive a 4-star rating, the next 40% 
  #' (i.e., assets ranked between 30% and 70%) will receive a 3-star 
  #' rating, the next 20% (i.e., assets ranked between 70% and 90%) 
  #' will receive a 2-star rating, and the bottom 10% of assets will 
  #' receive a 1-star rating. 
  #' @param factor_metric numeric vector. Vector containing the values for a factor metric (PE RATIO, e.g.).
  #' @returns Categorical vector indicating the star rating of each asset.
  
  ranked_factor_metric <- rank(factor_metric)
  
  len <- length(ranked_factor_metric)
  
  # Defines breaks for each star level based on the length of the ranked vector
  breaks <- round(c(
    1, len * 0.1, len * 0.3, len * 0.7, len * 0.9, len
  ))
  
  # Break points are used to determine which star level each asset will receive 
  number_of_stars <- cut(
    ranked_factor_metric, breaks = breaks, include.lowest = TRUE, labels = 1:5
  )
  
  return(number_of_stars)
}

##################################################################
##                       Fetch Nefin Data                       ##
##################################################################

fetch_nefin_data <- function(){
  
  factors_names <- c('Market', 'SMB', 'HML', 'WML', 'Risk_Free')
  
  nefin_dados <- list()
  for (fator in factors_names) {
    if(fator == 'Risk_Free'){
      url <- paste0('http://nefin.com.br/resources/risk_factors/', fator, '.xls')
    } else {
      url <- paste0('http://nefin.com.br/resources/risk_factors/', fator, '_Factor.xls')
    }
    
    GET(url, write_disk(tf <- tempfile(fileext = ".xls")))
    
    aux_nefin <- read_excel(tf)
    
    unlink(tf)
    
    datas <- make_date(aux_nefin$year, aux_nefin$month, aux_nefin$day)
    
    nefin_dados[[fator]] <- aux_nefin[, 4, drop = FALSE]
  }
  
  nefin_dados <- data.frame(date = datas, do.call('cbind', nefin_dados))
  
  return(nefin_dados)
}

#################################################################
##                          Get Dates                          ##
#################################################################

get_dates <- function(start_date, end_date, hold_period) {
  
  n_months <- interval(start_date, end_date) %/% months(1)
  
  dates <- ceiling_date( start_date %m+% months(seq(0, n_months, by = hold_period)), unit = 'months' ) - days(1)
  
  return(dates)
  
}

##################################################################
##                       Complete Returns                       ##
##################################################################

.complete_returns <- function(start_date, end_date, securities, price_df){
  returns <- price_df %>% 
    dplyr::filter(
      ticker %in% securities & date > start_date & date <= end_date
    ) %>% 
    complete(date, ticker) %>% 
    mutate_at(
      vars(return), ~replace_na(.x, 0)
    ) %>% 
    dplyr::select(!price)
  
  return(returns)
}

############################################################################
############################################################################
###                                                                      ###
###                        SELECT ELEGIBLE ASSETS                        ###
###                                                                      ###
############################################################################
############################################################################

select_eligible_securities <- function(base_date, n_month_lookback, price_df){
  start_date <- base_date %m-% months(n_month_lookback)
  
  # Filter data by dates
  prices_period <- price_df %>% 
    dplyr::filter(
      date > start_date & date <= base_date
    )
  
  # Restrictions
  first_restriction <- .missing_value_restriction(
    prices_period, 0.05
  )
  
  elegible_securities <- data.frame(
    date = base_date,
    ticker = first_restriction
  )
  
  return(elegible_securities)
  
}

.missing_value_restriction <- function(price_df, max_pct_na){
  missing_value_restriction <- price_df %>% 
    complete(date, ticker) %>% 
    group_by(ticker) %>% 
    summarise(
      pct_na = sum(is.na(return)) / n()
    ) %>% 
    ungroup() %>% 
    dplyr::filter(
      pct_na < max_pct_na
    ) %>% 
    pull(ticker)
  
  return(missing_value_restriction)
}

###########################################################################
###########################################################################
###                                                                     ###
###                          CALCULATE METRICS                          ###
###                                                                     ###
###########################################################################
###########################################################################

calculate_price_metrics <- function(metric_function, securities, base_date, n_month_lookback, price_df, nefin_df = NULL, factors_names = NULL){
  start_date <- base_date %m-% months(n_month_lookback)
  
  returns_period <- .complete_returns(
    start_date, base_date, securities, price_df
  )
  
  parms <- list(return_df = map(
    securities, \(x) filter(returns_period, ticker == x)
  ))
  
  parms[['return_df']] <- Filter(Negate(is.null), parms[['return_df']])
  
  securities <- unlist(map(parms[['return_df']], \(x) unique(x$ticker)))
  
  # If its a regression metric, we need to add the independent variables (NEFIN)
  if(!is.null(factors_names)){
    nefin_period <- nefin_df %>% 
      dplyr::filter(
        date > start_date & date <= base_date
      ) 
    
    parms <- append(
      parms, list(nefin_df = list(nefin_period), factors_names = list(factors_names))
    )
  }
  
  metric <- pmap(parms, metric_function)
  
  metric <- data.frame(
    date = base_date,
    ticker = securities,
    metric = unlist(metric)
  )
  
  return(metric)
  
}

.calculate_beta <- function(return_df, nefin_df, factors_names){
  regression <- .run_factor_model(return_df, nefin_df, factors_names)
  
  beta <- coef(regression)['Rm_minus_Rf']
  
  return(as.numeric(beta))
}

.calculate_cumulative_return <- function(return_df){
  cum_return <- prod(1 + return_df$return) - 1
  
  return(cum_return)
}

.calculate_volatility <- function(return_df){
  vol <- sd(return_df$return)
  
  return(vol)
}

.calculate_cumulative_return_minus_1_month <- function(return_df){
  last_month <- max(return_df$date) %m-% months(1)
  
  assets_returns_minus1 <- return_df %>% 
    dplyr::filter(
      date <= last_month
    )
  
  cum_return_minus1 <- .calculate_cumulative_return(
    assets_returns_minus1
  )
  
  return(cum_return_minus1)
}

.run_factor_model <- function(return_df, nefin_df, factors_names){
  return_df <- return_df %>% 
    inner_join(nefin_df, by = 'date') %>% 
    mutate(
      return = return - Risk_free
    ) %>% 
    dplyr::select(
      c('return', all_of(factors_names))
    )
  
  regression <- lm(return ~ ., data = return_df)
  
  return(regression)
  
}

############################################################################
############################################################################
###                                                                      ###
###                               BACKTEST                               ###
###                                                                      ###
############################################################################
############################################################################

calculate_backtest_returns <- function(price_df, securities, base_date, weights, n_months_holding_period, risk_free_df = NULL){
  end_date <- base_date %m+% months(n_months_holding_period)
  end_date <- ceiling_date(end_date, unit = 'months') - days(1)
  
  selected_securities_returns <- price_df %>%
    dplyr::select(!price) %>% 
    complete(date, ticker) %>% 
    # If we use only one filter, a security that does not exist in the evaluation period will disappear
    dplyr::filter(
      ticker %in% securities
    ) %>% 
    dplyr::filter(
      date > base_date & date <= end_date
    ) %>% 
    mutate_at(
      vars(return), ~replace_na(.x, 0)
    ) %>% 
    pivot_wider(
      names_from = 'ticker', values_from = 'return'
    )
  
  if(!is.null(risk_free_df)){
    selected_securities_returns <- left_join(selected_securities_returns, risk_free_df, by = 'date')
    
    weights <- c(weights, 1 - sum(weights)) # Residual weight
  }
  
  selected_securities_returns <- xts(selected_securities_returns[,-1], selected_securities_returns$date)
  
  portfolio_returns <- Return.portfolio(
    selected_securities_returns, weights = weights
  )
  
  portfolio_returns <- data.frame(
    date = index(portfolio_returns),
    portfolio_returns,
    row.names = NULL
  )
  
  return(portfolio_returns)
  
}

##################################################################
##                     Performance Analysis                     ##
##################################################################

performance_summary <- function(portfolio_returns, nefin) {
  
  market_return <- nefin$Rm_minus_Rf + nefin$Risk_free
  
  # CAPM
  regres <- lm(I(portfolio_returns - Risk_free) ~ Rm_minus_Rf + SMB + HML + WML, data = nefin)
  # Get regression coefficients
  regres_coef <- summary(regres)$coefficients
  
  alpha <- (regres_coef[1, 1] + 1) ^ 252 - 1
  t_alpha <- regres_coef[1, 3]
  
  beta <- regres_coef[2, 1]
  
  cumulative_ret <- prod(portfolio_returns + 1)^(252 / length(portfolio_returns)) - 1
  # Portfolio volatility
  vol <- sd(portfolio_returns) * sqrt(252)
  
  # Israelsen Information Ratio (ttps://doi.org/10.1057/palgrave.jam.2240158)
  ER <- (1 + mean(portfolio_returns - market_return)) ^ 252 - 1
  SD <- sd(portfolio_returns - market_return) * sqrt(252)
  mir <- ER / (SD^(ER / abs(ER))) # Modified IR
  
  # Israelsen Sharpe Ratio (https://doi.org/10.1057/palgrave.jam.2240158)
  ER <- (1 + mean(portfolio_returns - nefin$Risk_free)) ^ 252 - 1
  SD <- sd(portfolio_returns - nefin$Risk_free) * sqrt(252)
  msr <- ER / (SD^(ER / abs(ER))) # Modified SR
  
  # Tracking Error
  track_error <- sd(portfolio_returns - market_return) * sqrt(252)
  
  # CVaR
  cvar <- as.numeric(CVaR(portfolio_returns))
  
  # Maximum Drawdown
  ## WARNING: Warning message because we are working with a vector instead of xts
  suppressWarnings(max_draw <- maxDrawdown(portfolio_returns))
  
  metrics_values <- c(
    cumulative_ret, vol, 
    alpha, t_alpha, beta, 
    mir, msr, track_error,
    cvar, max_draw
  )
  
  metrics_names <- c(
    "Annual. Return",
    "Std. Deviation", 
    "Alpha", "t(alpha)",
    "Beta", "Info. Ratio", 
    "Sharpe Ratio", "Track Error",
    "CVaR", "Max. Drawdown"
  )
  
  results <- data.frame(metrics_names, metrics_values) %>% 
    set_names('names', 'values')
  
  results[c(1, 2, 3, 8, 9, 10), 2] <- round(results[c(1, 2, 3, 8, 9, 10), 2]*100, 2)
  results[c(4, 5, 6, 7), 2] <- round(results[c(4, 5, 6, 7), 2], 2)
  
  return(results)
}
