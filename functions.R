
#################################################################
##                     Get Number of Stars                     ##
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
