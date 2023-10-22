#' CORRELATION MATRIX
#' 
#' @description
#' Create a correlation matrix with variable mean and standard deviation.
#' The 'engine' of the correlation matrix is derived from Stefan Engineering's
#' code (Stefan Engineering, 2018).
#' 
#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param variables A required vector of column names representing their 
#' manifest variables. For latent and composite variables that are comprised of
#' multiple indicators, use the desc_wrapper() function and specify a named
#' list of variables in the \code{varlist} argument. 
#' @param digits Number of decimal places for the correlation matrix. Default
#' is 3 decimal points. If not specified, the function will use the default
#' value.
#' 
#' @return A correlation matrix dataframe with variable mean and standard
#' deviation. The dataframe can be exported as an excel workbook to be copy
#' and pasted into a word document or LaTeX.
#' 
#' @examples
#' variables <- c("age", "education", "income")
#' cor_matrix(data = data, variables = variables, digits = 3)
#' 
#' @references Stefan Engineering (2018). Create an APA style correlation
#' table with R. \url{https://stefaneng.github.io/apa_correlation_table/}.
#' 
#' @export
cor_matrix = function(data = .,
                      variables,
                      digits = 3){
  
  # Main 'engine' of the correlation matrix table is derived from 
  # Stefan Engineering
  # See reference: https://stefaneng.github.io/apa_correlation_table/
  
  apaCorr <- function(mat, corrtype = "pearson") {
    
    apply_if <- function(mat, p, f) {
      # Fill NA with FALSE
      p[is.na(p)] <- FALSE
      mat[p] <- f(mat[p])
      mat
    }
    
    matCorr <- mat
    if (class(matCorr)[1] != "rcorr") {
      matCorr <- Hmisc::rcorr(mat, type = corrtype)
    }
    
    # Round the matrix
    mat_rounded <- round(matCorr$r, digits)
    
    # Ensure matrix is being reported to designated decimal points
    matCorr_formatted <- format(mat_rounded, nsmall = digits)
    
    # Add one star for each p < 0.05, 0.01, 0.001
    stars <- apply_if(matCorr_formatted, matCorr$P < 0.05, function(x) paste0(x, "*"))
    stars <- apply_if(stars, matCorr$P < 0.01, function(x) paste0(x, "*"))
    stars <- apply_if(stars, matCorr$P < 0.001, function(x) paste0(x, "*"))
    
    # Put - on diagonal and blank on upper diagonal
    stars[upper.tri(stars, diag = T)] <- "-"
    stars[upper.tri(stars, diag = F)] <- ""
    n <- length(stars[1,])
    colnames(stars) <- 1:n
    
    # Remove _ and convert to title case
    row.names(stars) <- tools::toTitleCase(sapply(row.names(stars), gsub, pattern="_", replacement = " "))
    
    # Add index number to row names
    row.names(stars) <- paste(paste0(1:n,"."), row.names(stars))
    stars
  }
  
  # Get means of variables
  mean <- data %>%
    as.data.frame() %>% 
    summarise(across(.cols = all_of(variables),
                     .fns = function(x) mean(x, na.rm = T))) %>% 
    t() %>% 
    magrittr::set_colnames(., "M")
  
  # Get standard deviations of variables
  stdev <- data %>%
    as.data.frame() %>% 
    summarise(across(.cols = all_of(variables),
                     .fns = function(x) sd(x, na.rm = T))) %>% 
    t() %>% 
    magrittr::set_colnames(., "SD")
  
  # Build correlation matrix table
  cortable <- apaCorr(as.matrix(data %>%
                                  dplyr::select(all_of(variables)),
                                corrtype = "pearson")) %>%
    as.data.frame()
  
  cormatrix = cortable %>%
    rownames_to_column(var = "Variable") %>%
    cbind(mean) %>%
    cbind(stdev) %>%
    select(Variable, M, SD, everything())
  
  # Return the correlation matrix
  return(cormatrix)
  
}