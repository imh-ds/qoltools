#' Frequency Table
#' 
#' @description
#' A quick function to get a frequency table of a variable's values.
#' 
#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param variable A required single string denoting the name of the variable.
#' 
#' @returns A frequency table of the variable's unique values.
#' 
#' @examples
#' freq(data, "education")
#' 
#' @export
freq <- function(data = .,
                 variable){
  
  frequency <- data %>% 
    group_by(!!sym(variable)) %>% 
    count()
  
  return(frequency)
  
}