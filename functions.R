
##################################################################
##                     get_index_comp_dates                     ##
##################################################################

get_index_comp_dates <- function(start_date, end_date, month_freq){
  start_date <- ceiling_date(as.Date(start_date), "month") - days(1)
  end_date <- ceiling_date(as.Date(end_date), "month") - days(1)
  
  n_months <- interval(start_date, end_date) %/% months(1)
  
  index_comp_dates <- start_date %m+% months(seq(0, n_months, month_freq))
  
  return(index_comp_dates)
}

##################################################################
##                        get_index_comp                        ##
##################################################################

get_index_comp <- function(index_name, start_date, end_date, month_freq){
  
  index_dates <- get_index_comp_dates(start_date, end_date, month_freq)
  
  ibx_comp <- vector('list', length = length(index_dates))
  for (i in seq_along(index_dates)) {
    
    date <- str_replace_all(index_dates[i], '-', '')
    
    overrd <- c("END_DT" = date)
    ibx_comp_month <- bds(index_name, 'INDX_MWEIGHT', overrides = overrd)
    
    colnames(ibx_comp_month) <- c('Ticker', paste0('M', date))
    
    ibx_comp[[i]] <- ibx_comp_month
  }
  
  ibx_comp <- Reduce(function(x, y) merge(x, y, all=TRUE), ibx_comp)
  
  ibx_comp[is.na(ibx_comp)] <- 0
  
  return(ibx_comp)
  
}

##################################################################
##                       merge_comp_indxs                       ##
##################################################################

merge_comp_indxs <- function(comp_indxs, weight_indxs){
  # Get all unique dates
  indx_months <- lapply(comp_indxs, function(x) colnames(x)[-1])
  indx_months <- unique(unlist(indx_months))
  # Order dates
  indx_months <- ymd(sapply(indx_months, function(x) (str_replace(x, 'M', ''))))
  indx_months <- sort(indx_months)
  indx_months <- sapply(indx_months, function(x) paste0('M', str_replace_all(x, '-', '')))
  
  # For every unique date...
  comp_indx_final <- vector('list', length = length(indx_months))
  for (i in seq_along(indx_months)) {
    # ...Find all the indexes that existed at this date and merge them based on
    # a weight vector
    month_comp_indx <- list()
    for (j in seq_along(comp_indxs)) {
      
      comp_indx <- comp_indxs[[j]]
      
      ## Test if a given index existed on the date i...
      if(indx_months[i] %in% colnames(comp_indx)){
        ## ... if it does, multiply the assets weights by the index weight 
        comp_indx <- comp_indx[c('Ticker', indx_months[i])]
        colnames(comp_indx)[2] <- paste0('comp', j)
        
        comp_indx[,2] <- comp_indx[,2] * weight_indxs[j]
        
        month_comp_indx[[paste0('comp', j)]] <- comp_indx
      }
    }
    
    month_comp_indx <- Reduce(function(x, y) merge(x, y, all=TRUE), month_comp_indx)
    
    month_comp_indx[is.na(month_comp_indx)] <- 0
    
    # Divide every assets weight by the total weight
    month_comp_indx[,-1] <- lapply(month_comp_indx[,-1,drop=FALSE], function(x) x / sum(month_comp_indx[,-1] / 100))
    
    # Sum every weight to get the portfolio final weight vector
    month_comp_indx <- data.frame(month_comp_indx[['Ticker']], apply(month_comp_indx[,-1,drop=FALSE], 1, sum))
    
    colnames(month_comp_indx) <- c('Ticker', indx_months[i])
    
    comp_indx_final[[i]] <- month_comp_indx
    
  }
  
  comp_indx_final <- Reduce(function(x, y) merge(x, y, all=TRUE), comp_indx_final)
  
  comp_indx_final[is.na(comp_indx_final)] <- 0
  
  # Make sure that the columns oreder is right
  comp_indx_final <- comp_indx_final %>%
    dplyr::select(c('Ticker', all_of(indx_months)))
  
  return(comp_indx_final)
}


#################################################################
##                         Change Sign                         ##
#################################################################

change_sign <- function(metrics_df, metrics_name){
  metrics2change <- intersect(colnames(metrics_df), metrics_name)
  
  for (column in metrics2change) {
    metrics_df[, column] <- metrics_df[, column] * -1
  }
  
  metrics_df <- as.data.frame(metrics_df)
  
  return(metrics_df)
}


##################################################################
##                       get_stars_rating                       ##
##################################################################

get_stars_rating <- function(factor_metric_df){
  one_five <- round(nrow(factor_metric_df) * 0.1)
  two_four <- round(nrow(factor_metric_df) * 0.2)
  three <- nrow(factor_metric_df) - 2*(one_five + two_four)
  
  stars <- rep(5:1, c(one_five, two_four, three, two_four, one_five))
  
  return(stars)
}
