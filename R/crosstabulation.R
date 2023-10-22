#' Crosstabulation
#' 
#' @description
#' A quick function to get a crosstabulation of two variables in a dataframe.
#' 
#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param x Name of the first variable. Needs to be entered as string
#' character.
#' @param y Name of the second variable. Needs to be entered as string
#' character.
#' 
#' @returns A crosstab of two variables.
#' 
#' @examples
#' crosstab(data, "gender", "gpa")
#' 
#' @export
crosstab = function(data = .,
                    x,
                    y){
  
  ct = data %>% 
    group_by(!!sym(x), !!sym(y)) %>% 
    tally() %>% 
    spread(!!sym(x), n)
  
  return(ct)
  
}