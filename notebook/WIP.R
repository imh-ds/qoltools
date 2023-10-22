

# ----------------------- #
# WRAPPER FUNCTIONS INDEX #
# ----------------------- #


# source("C:\\Users\\imhoh\\OneDrive\\Academia\\Projects\\RScript_Wrap_v1.1.R")

#' @author Hohjin Im, PhD
#' Version 0.1.0
#' Beta version.



# DESCRIPTIVE WRAPPER -----------------------------------------------------

#' DESCRIPTIVE STATISTICS WRAPPER
#' 
#' @description
#' A simple function for automating the calculation of general descriptive
#' statistics, like means, skew, missingness, reliability, and correlation
#' matrices. The 'engine' of the correlation matrix is derived from Stefan
#' Engineering's code (Stefan Engineering, 2018).
#' 
#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param varlist An optional named list of vectors. Each name in the list
#' represents a composite or latent variable, and the corresponding vector
#' contains the column names that are associated with the indicators comprising
#' said composite or latent variable. Default is NULL.
#' @param digits Number of decimal places for the correlation matrix. Default
#' is 3 decimal points. If not specified, the function will use the default
#' value.
#' 
#' @return A list of dataframes containing the general descriptives of all
#' variables in the dataset from `psych::describe()` and variable missingness
#' information (Revelle, 2023). If the varlist was specified of latent or
#' composite variables and their indicators, then three additional dataframes
#' are returned. These include a new dataset with the calculated latent or
#' composite variables using the specified indicators, a correlation matrix of
#' the latent or composite variables, and a measurement reliability table of the
#' latent or composite variables. The latent or composite variables are 
#' calculated as the unweighted means of all indicators. The measurement
#' reliability table returns the Cronbach's alpha and McDonald's omega as
#' calculated by the jamovi package's `jmv::reliability()` function (Selker et
#' al., 2022).
#' 
#' @examples
#' varlist <- list(anxiety = c("anx_1", "anx_2", "anx_3"),
#'                 sleep = c("slp_1", "slp_2"),
#'                 life_sat = c("lsat_1", "lsat_2", "lsat_3"))
#' desc_wrapper(data, varlist, digits = 3)
#' 
#' @references William Revelle (2023). _psych: Procedures for Psychological,
#' Psychometric, and Personality Research_. Northwestern University, Evanston,
#' Illinois. R package version 2.3.3,
#' <https://CRAN.R-project.org/package=psych>.
#' Selker R, Love J, Dropmann D, Moreno V (2022). _jmv: The 'jamovi' Analyses_.
#' R package version 2.3.4, <https://CRAN.R-project.org/package=jmv>.
#' Stefan Engineering (2018). _Create an APA style correlation
#' table with R_. <https://stefaneng.github.io/apa_correlation_table/>.
#' 
#' @export
desc_wrapper = function(data = .,
                        varlist = NULL,
                        digits = 3){
  
  # If varlist is given, then get reliability metrics for each composite
  
  # Reliability
  reliability_list = list()
  if(!is.null(varlist)){
    
    for(x in seq_along(names(varlist))){
      
      reliability_list[[x]] = jmv::reliability(data = data,
                                               vars = varlist[[x]],
                                               omegaScale = T,
                                               meanScale = T,
                                               sdScale = T)$scale$asDF
      
    }
    
    reli_table = reliability_list %>%
      purrr::reduce(rbind)
    
    reli_table = reli_table %>% 
      rownames_to_column(var = "Variable") %>% 
      mutate(Variable = names(varlist))
    
  }
  
  # If varlist is given, then get correlation matrix
  if(!is.null(varlist)){
    
    for(x in seq_along(names(varlist))){
      
      varname <- names(varlist)[x]
      
      data <- data %>% 
        mutate(!!varname := rowMeans(x = select(., varlist[[x]]), na.rm = T))
      
    }
    
    cormat <- cor_matrix(data = data,
                         variables = names(varlist),
                         digits = digits)
    
  }
  
  
  # Descriptive for data
  datadesc = data %>%
    psych::describe() %>% as.data.frame() %>%
    rownames_to_column(var = "variable")
  
  # Data missingness
  datamissing = data %>%
    summarise_all(list(~sum(is.na(.))/nrow(data))) %>%
    gather(key = "variable",
           value = "missingness") %>% 
    mutate(n_total = nrow(data),
           n_missing = n_total*missingness)
  
  
  # Report results
  if(!is.null(varlist)){
    
    sheets = list("data" = data,
                  "desc" = datadesc,
                  "miss" = datamissing,
                  "reli" = reli_table,
                  "corr" = cormat)
    
  } else {
    
    sheets = list("desc" = datadesc,
                  "miss" = datamissing)
    
  }
  
  return(sheets)
  
}



# DEMOGRAPHICS TABLE WRAPPER ----------------------------------------------

#' DEMOGRAPHICS FREQUENCY TABLE
#' 
#' @description
#' Create a demographic frequency table that provides the count and
#' proportion of each variable value. The current version does not support
#' automatically extracting variable labels from SPSS files through `haven`
#' (Wickham et al., 2023).
#' The variable values will be returned as is, meaning that if the variables
#' are numeric, then the table will display numeric values. Users are
#' recommended to recode the demographic variables of interest with string
#' values as necessary.
#' 
#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param variables A required vector of column names representing their 
#' manifest demographic variables.
#' 
#' @return A dataframe of the frequencies of each category
#' 
#' @examples
#' variables <- c("age", "education", "gender")
#' demographic_table(data, variables)
#' 
#' @references Wickham H, Miller E, Smith D (2023). _haven: Import and Export
#' 'SPSS', 'Stata' and 'SAS' Files_. R package version 2.5.2,
#' <https://CRAN.R-project.org/package=haven>.
#' 
#' @export
demographic_table <- function(data = .,
                              variables){
  
  # Create empty list
  dem_list <- list()
  
  # Run loop for every variable
  for(var in variables){
    
    # Run frequency table
    frequency <- data %>% 
      freq(., var)
    
    # Get total n value
    total <- frequency %>% 
      ungroup() %>% 
      summarize(total = sum(n, na.rm = T)) %>% 
      .$total
    
    # Mutate proportion of total and reorganize frequency table
    frequency <- frequency %>% 
      mutate(percent = paste0(sprintf('%.2f',
                                      100*(n / total)),
                              "%"),
             Variable = var) %>% 
      select(Variable, everything()) %>% 
      magrittr::set_colnames(., c("Variable",
                                  "Value",
                                  "n",
                                  "Proportion"))
    
    # Save to demographic list
    dem_list[[var]] <- frequency
    
  }
  
  # Reduce to working table
  dem_table <- purrr::reduce(dem_list,
                             rbind)
  
  # Return working table
  return(dem_table)
  
}



# CORRELATION WRAPPER -----------------------------------------------------

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
#' list of variables in the `varlist` argument. 
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
#' @references Stefan Engineering (2018). _Create an APA style correlation
#' table with R_. <https://stefaneng.github.io/apa_correlation_table/>.
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



# EFA (EXPLORATORY FACTOR ANALYSIS) WRAPPER -------------------------------

#' EXPLORATORY FACTOR ANALYSIS WRAPPER
#' 
#' @description
#' Runs exploratory factor analysis (EFA) through the `psych` package's `fa()`
#' function. A simple wrapper that automates a general EFA for most needs.
#' For complex or custom settings, users are recommended to simply use the
#' `psych::fa` (Revelle, 2023). 
#' 
#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param variables A required vector of column names representing their 
#' manifest variables.
#' @param dictionary variable dictionary
#' @param factors_n A single numeric value for the number of factors to
#' identify. Default is NULL where it automatically determines the optimal
#' number of factors.
#' @param rotation Defaults to "oblimin" for an oblique rotation. See
#' help(psych::fa) for other rotation options.
#' @param missing Defaults to NULL, and does not impute missing values. If
#' set to TRUE, specify impute option.
#' @param impute default to "none". Specify impute option "mean" or "median" to replace missing values.
#' @param plot default to FALSE. If set to TRUE, returns elbow plot.
#' 
#' @return The exploratory factor analysis output, a dataframe of the items
#' sorted from highest to lowest factor loading on their respective factors,
#' and a list of additional outputs.
#' 
#' @examples
#' variables = c("item1", "item2", "item3", "item4")
#' efa_wrapper(data, variables, plot = TRUE)
#' 
#' @references William Revelle (2023). _psych: Procedures for Psychological,
#' Psychometric, and Personality Research_. Northwestern University, Evanston,
#' Illinois. R package version 2.3.3,
#' <https://CRAN.R-project.org/package=psych>.
#' 
#' @export
efa_wrapper = function(data = .,
                       variables,
                       dictionary = NULL,
                       factors_n = NULL,
                       rotation = "oblimin",
                       missing = FALSE,
                       impute = "none",
                       plot = FALSE){
  
  data <- data %>% 
    select(all_of(variables))
  
  if(is.null(factors_n)){
    
    efa <- psych::fa(data,
                     fm = "minres",
                     nfactors = psych::fa.parallel(data,
                                                   fm = "minres",
                                                   plot = plot)$nfact,
                     rotate = rotation,
                     missing = missing,
                     impute = impute)
    
  } else {
    
    efa <- psych::fa(data,
                     fm = "minres",
                     nfactors = factors_n,
                     rotate = rotation,
                     missing = missing,
                     impute = impute)
    
  }
  
  efa_sort <- as.data.frame(psych::fa.sort(efa)$loadings[]) %>%
    rownames_to_column(var = "Item") %>%
    inner_join(as.data.frame(efa$uniqueness) %>%
                 rownames_to_column(var = "Item")) %>% 
    rename(Uniqueness = "efa$uniqueness")
  
  if(!is.null(dictionary)){
    
    efa_sort <- efa_sort %>%
      inner_join(as.data.frame(dictionary)) %>% 
      select(Item, Label, everything())
    
  }
  
  fa_scores <- list("SCORES" = efa$scores,
                    "WEIGHTS" = efa$weights,
                    "R_SCORES" = efa$r.scores,
                    "R2" = efa$R2)
  
  sheets <- list("EFA" = efa,
                 "EFA_sorted" = efa_sort,
                 "EFA_scores" = fa_scores)
  
  return(sheets)
  
}



# CFA (CONFIRMATORY FACTOR ANALYSIS) WRAPPER ------------------------------

#' CONFIRMATORY FACTOR ANALYSIS WRAPPER
#' 
#' @description
#' Runs confirmatory factor analysis (CFA) through the `lavaan` package
#' (Rosseel, 2012) and automates the exporting of commonly used results. 
#' 
#' 
#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param model `Lavaan` defined confirmatory factor model. Refer to `lavaan`
#' documentation for a description of the user-specified model.
#' @param mi_groups A required vector of column names representing the 
#' categorical group variables to test measurement invariance. Default is NULL
#' to not run measurement invariance. If specified, then the function will
#' run measurement invariance on the groups and return fit metrics.
#' @param estimator Estimator to be used in the confirmatory factor analysis.
#' Default is maximum likelihood (ML). Alternative esimators are limited to
#' those offered by `lavaan`, e.g., "GLS", "WLS", "DWLS", "ULS", "DLS", and
#' "PML". Refer to `lavaan` documentation for additional details.
#' @param se Parameter to determine how to compute standard errors. Default
#' is set to NULL. Set to "robust" to use either "robust.sem" or
#' "robust.huber.white" depending on the estimator used. Refer to lavaan
#' documentation for additional details.
#' @param missing Parameter to determine how to handle missingness. Default
#' is set to "listwise" where all observations with missing values are deleted
#' prior to analysis. As detailed in `lavaan`'s documentation, this is only
#' recommended when data are missing completely at random (MCAR). Another
#' options is "ml" for full information maximum likelihood approach (fiml).
#' Refer to lavaan documentation for additional details.
#' 
#' @return A list of dataframes to be easily exported as an Excel workbook.
#' Basic output includes the goodness of fit metrics and model estimations.
#' If measurement invariance parameters were specified, then fit indices and
#' goodness of fit comparison tables are also included in the returned output.
#' 
#' @examples
#' model <- 'anxiety =~ x1 + x2 + x3
#'           sleep   =~ z1 + z2'
#' cfa_wrapper(data, model, mi_groups = c("gender", "ethnicity"))
#' 
#' 
#' @references Yves Rosseel (2012). lavaan: An R Package for Structural
#' Equation Modeling. _Journal of Statistical Software_, 48(2), 1-36.
#' <https://doi.org/10.18637/jss.v048.i02>.
#' 
#' @export
cfa_wrapper = function(data = .,
                       model,
                       mi_groups = NULL,
                       estimator = "ML",
                       se = NULL,
                       missing = "listwise"){
  
  # Run CFA on the model
  cfa_model = lavaan::cfa(model,
                          data = df,
                          estimator = estimator,
                          se = se,
                          missing = missing)
  
  # Extract goodness of fit names
  names = rownames(lavaan::fitmeasures(cfa_model) %>% as.data.frame())
  
  # Get goodness of fit measures
  fittab = lavaan::fitmeasures(cfa_model) %>% 
    as.data.frame() %>% 
    rename("Value" = ".") %>% 
    rownames_to_column(var = "Metric")
  
  # Create goodness of fit table
  fittext = lavaan::fitmeasures(cfa_model) %>% 
    as.data.frame() %>% 
    data.table::transpose() %>% 
    magrittr::set_colnames(names) %>% 
    mutate(Value = paste0("(chi-sq(", df,
                          ") = ", sprintf('%.3f', chisq),
                          ", CFI = ", sprintf('%.3f', cfi),
                          ", TLI = ", sprintf('%.3f', tli),
                          ", SRMR = ", sprintf('%.3f', srmr),
                          ", RSMEA = ", sprintf('%.3f', rmsea),
                          ", 90% CI [", sprintf('%.3f', rmsea.ci.lower),
                          ", ", sprintf('%.3f', rmsea.ci.upper),
                          "])")) %>% 
    dplyr::select(Value) %>% 
    rownames_to_column(var = "Metric") %>% 
    mutate(Metric = "Text")
  
  fittab = fittab %>% 
    rbind(fittext)
  
  if(!is.null(mi_groups)){
    
    nested_sums = list()
    fit_total_sums = list()
    fit_table_sums = list()
    fitdif_sums = list()
    fitdif_table_sums = list()
    
    for(group in mi_groups){
      
      # Configural Invariance
      Configural = cfa(model,
                       data = df,
                       group = group,
                       missing = missing,
                       se = se,
                       estimator = estimator)
      
      # Metric Invariance
      Metric = cfa(model,
                   data = df,
                   group = group,
                   missing = missing,
                   se = se,
                   estimator = estimator,
                   group.equal = c("loadings"))
      
      # Scalar Invariance
      Scalar = cfa(model,
                   data = df,
                   group = group,
                   missing = missing,
                   se = se,
                   estimator = estimator,
                   group.equal = c("loadings", "intercepts"))
      
      # Strict Invariance
      Strict = cfa(model,
                   data = df,
                   group = group,
                   missing = missing,
                   se = se,
                   estimator = estimator,
                   group.equal = c("loadings", "intercepts", "residuals"))
      
      # Compare across invariances
      compare = semTools::compareFit(Configural,
                                     Metric,
                                     Scalar,
                                     Strict)
      
      # Create specific summary function
      summary.FitDiff = function(object){
        
        export = list(nested = object@nested,
                      fit = object@fit,
                      fit.diff = object@fit.diff)
        
        return(export)
        
      }
      
      # Create summary object
      comparesum = summary(compare)
      
      # Chi-sq differences across groups
      nested_sums[[group]] = comparesum$nested %>% 
        as.data.frame() %>% 
        rownames_to_column(var = "INVARIANCE") %>% 
        add_row(., INVARIANCE = group,
                .before = 1)
      
      # Get all fit metrics across invariances
      fit_total_sums[[group]] = comparesum$fit %>% 
        as.data.frame() %>% 
        rownames_to_column(var = "INVARIANCE") %>% 
        add_row(., INVARIANCE = group,
                .before = 1)
      
      # Get most relevant fit metrics and create reportable table
      fit_table_sums[[group]] = comparesum$fit %>% 
        select(chisq, df, pvalue, rmsea, rmsea.ci.lower, rmsea.ci.upper, rmsea.pvalue, cfi, tli, srmr, aic, bic) %>% 
        mutate(chisq = sprintf('%.3f', chisq),
               pvalue = ifelse(pvalue < 0.001, "<.001", sprintf('%.3f', pvalue)),
               rmsea = sprintf('%.3f', rmsea),
               rmsea.ci.lower = sprintf('%.3f', rmsea.ci.lower),
               rmsea.ci.upper = sprintf('%.3f', rmsea.ci.upper),
               rmsea.pvalue = ifelse(rmsea.pvalue < 0.001, "<.001", sprintf('%.3f', rmsea.pvalue)),
               cfi = sprintf('%.3f', cfi),
               tli = sprintf('%.3f', tli),
               srmr = sprintf('%.3f', srmr),
               aic = sprintf('%.3f', aic),
               bic = sprintf('%.3f', bic)) %>% 
        rownames_to_column(var = "INVARIANCE") %>% 
        add_row(., INVARIANCE = group,
                .before = 1) %>%  
        rename("χ2" = chisq,
               p = pvalue,
               RMSEA = rmsea,
               LOWER = rmsea.ci.lower,
               UPPER = rmsea.ci.upper,
               rmsea_p = rmsea.pvalue,
               CFI = cfi,
               TLI = tli,
               SRMR = srmr,
               AIC = aic,
               BIC = bic)
      
      # Get all fit difference metrics across invariances
      fitdif_sums[[group]] = comparesum$fit.diff %>% 
        as.data.frame() %>% 
        rownames_to_column(var = "INVARIANCE") %>% 
        add_row(., INVARIANCE = group,
                .before = 1)
      
      # Get relevant fit metrics into reportable table
      fitdif_table_sums[[group]] = comparesum$fit.diff %>% 
        select(rmsea, cfi, tli, srmr, aic, bic) %>% 
        mutate(rmsea = sprintf('%.3f', rmsea),
               cfi = sprintf('%.3f', cfi),
               tli = sprintf('%.3f', tli),
               srmr = sprintf('%.3f', srmr),
               aic = sprintf('%.3f', aic),
               bic = sprintf('%.3f', bic)) %>% 
        rownames_to_column(var = "COMPARISON") %>% 
        magrittr::set_colnames(toupper(names(.))) %>% 
        add_row(., COMPARISON = group,
                .before = 1)
      
    }
    
    # Reduce tables into dataframes
    nested_table = purrr::reduce(nested_sums,
                                 rbind)
    fit_total_table = purrr::reduce(fit_total_sums,
                                    rbind)
    fit_table = purrr::reduce(fit_table_sums,
                              rbind)
    fitdif_total_table = purrr::reduce(fitdif_sums,
                                       rbind)
    fitdif_table = purrr::reduce(fitdif_table_sums,
                                 rbind)
    
  }
  
  if(is.null(mi_groups)){
    
    sheets = list("GOF" = fittab,
                  "EST" = as.data.frame(standardizedsolution(cfa_model)) %>%
                    rename(Item = rhs))
    
  } else {
    
    sheets = list("GOF" = fittab,
                  "EST" = as.data.frame(standardizedsolution(cfa_model)) %>%
                    rename(Item = rhs),
                  "CHISQ" = nested_table,
                  "FIT" = fit_table,
                  "FITDIF" = fitdif_table,
                  "FIT_ALL" = fit_total_table,
                  "FITDIF_ALL" = fitdif_total_table)
    
  }
  
  return(sheets)
  
}


# SEM (STRUCTURAL EQUATION MODELING) WRAPPER ------------------------------

#' STRUCTURAL EQUATION MODELING WRAPPER
#' 
#' @description
#' A simple wrapper that automates the running and extraction of common
#' SEM fit metrics, coefficients, and fit metrics.
#' 
#' 
#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param model `Lavaan` defined SEM object. Refer to `lavaan`
#' documentation for a description of the user-specified model.
#' @param cluster If working with clustered/multilevel/hierarchical data, define
#' the cluster or group variable to account for shared SE. Cluster should be
#' a categorical factor variable.
#' @param predictors An optional vector of character strings of the predictors
#' of interest. Doing so will filter the coefficient table to just the
#' predictors of interest for easier viewing.
#' @param estimator Estimator to be used in the confirmatory factor analysis.
#' Default is maximum likelihood (ML). Alternative esimators are limited to
#' those offered by `lavaan`, e.g., "GLS", "WLS", "DWLS", "ULS", "DLS", and
#' "PML". Refer to `lavaan` documentation for additional details.
#' @param se Parameter to determine how to compute standard errors. Default
#' is set to NULL. Set to "robust" to use either "robust.sem" or
#' "robust.huber.white" depending on the estimator used. Refer to lavaan
#' documentation for additional details.
#' @param missing Parameter to determine how to handle missingness. Default
#' is set to "listwise" where all observations with missing values are deleted
#' prior to analysis. As detailed in `lavaan`'s documentation, this is only
#' recommended when data are missing completely at random (MCAR). Another
#' options is "ml" for full information maximum likelihood approach (fiml).
#' Refer to lavaan documentation for additional details.
#' 
#' @examples
#' # Note: Model example courtesy of `lavaan`
#' 
#' model <- '
#'   # measurement model
#'     ind60 =~ x1 + x2 + x3
#'     dem60 =~ y1 + y2 + y3 + y4
#'     dem65 =~ y5 + y6 + y7 + y8
#'   # regressions
#'     dem60 ~ ind60
#'     dem65 ~ ind60 + dem60
#'   # residual correlations
#'     y1 ~~ y5
#'     y2 ~~ y4 + y6
#'     y3 ~~ y7
#'     y4 ~~ y8
#'     y6 ~~ y8
#'  '
#' 
#' sem_wrapper(data = data,
#'             model = model)
#' 
#' @returns A list of two additional lists. The first list consists of the
#' standard `lavaan` SEM object. The second list is a series of dataframes
#' containing common outputs of an SEM object compiled into an exportable
#' format to an excel document. 
#' 
#' @references Yves Rosseel (2012). lavaan: An R Package for Structural Equation
#' Modeling. Journal of Statistical Software, 48(2), 1-36.
#' https://doi.org/10.18637/jss.v048.i02
#' 
#' @export
sem_wrapper = function(data = .,
                       model,
                       cluster = NULL,
                       predictors = NULL,
                       estimator = "ML",
                       se = NULL,
                       missing = "listwise"){
  
  # SEM Modeling
  sem_mod = lavaan::sem(model,
                        data = data,
                        cluster = cluster,
                        estimator = estimator,
                        se = se,
                        missing = missing,
                        fixed.x = FALSE)
  
  # Get Summary of SEM
  sem_sum <- summary(sem_mod)
  sem_nobs <- sem_sum$data$nobs
  if(is.null(sem_sum$data$norig)){
    
    sem_norig <- "-"
    
  } else{
    
    sem_norig <- sem_sum$data$norig
    
  }
  sem_groups <- sem_sum$data$ngroups
  
  # Extract goodness of fit names
  names = rownames(lavaan::fitmeasures(sem_mod) %>% as.data.frame())
  
  # Get goodness of fit measures
  fittab = lavaan::fitmeasures(sem_mod) %>% 
    as.data.frame() %>% 
    rename("Value" = ".") %>% 
    rownames_to_column(var = "Metric")
  
  # Create goodness of fit table
  fittext = lavaan::fitmeasures(sem_mod) %>% 
    as.data.frame() %>% 
    data.table::transpose() %>% 
    magrittr::set_colnames(names) %>% 
    mutate(Value = paste0("(chi-sq(", df,
                          ") = ", sprintf('%.3f', chisq),
                          ", CFI = ", sprintf('%.3f', cfi),
                          ", TLI = ", sprintf('%.3f', tli),
                          ", SRMR = ", sprintf('%.3f', srmr),
                          ", RSMEA = ", sprintf('%.3f', rmsea),
                          ", 90% CI [", sprintf('%.3f', rmsea.ci.lower),
                          ", ", sprintf('%.3f', rmsea.ci.upper),
                          "])")) %>% 
    dplyr::select(Value) %>% 
    rownames_to_column(var = "Metric") %>% 
    mutate(Metric = "Text")
  
  # Combine fit measures and fit text
  fittab = fittab %>% 
    rbind(fittext)
  
  # Get beta coefficient estimates
  esttab = as.data.frame(standardizedsolution(sem_mod)) %>%
    rename(Item = rhs) %>% 
    mutate(text = paste0("(beta = ", sprintf('%.3f', est.std),
                         ", se = ", sprintf('%.3f', se),
                         ", z = ", sprintf('%.3f', z),
                         ", 95% CI [", sprintf('%.3f', ci.lower),
                         ", ", sprintf('%.3f', ci.upper),
                         "], p ", ifelse(pvalue < .001, "< .001",
                                         paste0("= ", sprintf('%.3f',pvalue))),
                         ")"),
           fig.text = paste0(sprintf('%.3f', est.std),
                             ifelse(pvalue < .001, "***",
                                    ifelse(pvalue > .001 & pvalue < .01, "**",
                                           ifelse(pvalue > .01 & pvalue < .05, "*", ""))),
                             " [", sprintf('%.3f', ci.lower),
                             ", ", sprintf('%.3f', ci.upper),
                             "]"),
           path = paste0(Item, " -> ", lhs)) %>%
    select(path, lhs, op, Item, label, est.std, se, ci.lower, ci.upper, everything())
  
  # Get measurement model loadings
  loadtab <- esttab %>% 
    filter(op == "=~")
  
  # Get regression coefficients
  regtab <- esttab %>% 
    filter(op == "~")
  
  # Get R^2 of outcome paths
  r2tab <- parameterestimates(sem_mod,
                              rsquare = T) %>% 
    filter(op == "r2")
  
  # Get residual correlations
  restab <- esttab %>% 
    filter(op == "~~")
  
  # Get indirect effects
  indtab <- esttab %>% 
    filter(op == ":=")
  
  
  # -------------------------------------- #
  # -- REGRESSION TABLES IN LONG FORMAT -- #
  # -------------------------------------- #
  
  reg_long <- list()
  outcomes <- unique(regtab$lhs)
  
  # Loop for every endogenous variables (e.g., mediators,)
  for (var in seq_along(outcomes)){
    
    # Get name of current outcome
    name <- outcomes[[var]]
    
    # Filter to just predictors relevant to current outcome
    pt_outcome <- regtab %>% 
      filter(lhs == name)
    
    # Simplify coefficient output to 3 digits
    pt_est = pt_outcome %>% 
      mutate(est = paste(sprintf('%.3f', est.std),
                         ifelse(pvalue > .05, "",
                                ifelse(pvalue < .05 & pvalue > .01, "*",
                                       ifelse(pvalue < .01 & pvalue > .001, "**", "***"))), sep = " ")) %>% 
      select(Item, est)
    
    # Simplify confidence interval output
    pt_ci <- pt_outcome %>% 
      mutate(ci = paste0("(",
                         sprintf('%.3f', ci.lower),
                         ", ",
                         sprintf('%.3f', ci.upper),
                         ")"),
             Item = paste0(Item, "_ci")) %>% 
      select(Item, ci)
    
    df1 <- c(rbind(pt_est$est, pt_ci$ci)) %>% 
      as.data.frame() %>% 
      rename(!!sym(paste0(name, " beta (95% CI)")) := ".")
    
    df2 <- c(rbind(pt_est$Item, pt_ci$Item)) %>% 
      as.data.frame() %>% 
      rename("Variable" = ".")
    
    df3 <- r2tab %>% 
      filter(lhs == name) %>% 
      select(op, est) %>% 
      mutate(est = sprintf("%.3f", est)) %>% 
      as.data.frame() %>% 
      rename("Variable" = op,
             !!sym(paste0(name, " beta (95% CI)")) := "est") %>% 
      mutate(Variable = "R^2")
    
    df4 <- data.frame("v1" = c("n used",
                               "n total",
                               "Groups"),
                      "v2" = c(sem_nobs,
                               sem_norig,
                               sem_groups)) %>% 
      rename("Variable" = v1,
             !!sym(paste0(name, " beta (95% CI)")) := v2)
    
    pt_table <- cbind(df2, df1) %>% 
      rbind(df3) %>% 
      rbind(df4)
    
    reg_long[[var]] <- pt_table
    
  }
  
  # Reduce into workable model
  reg_long_table <- purrr::reduce(reg_long,
                                  full_join) %>% 
    mutate(Variable = ifelse(grepl("_ci", Variable), "", Variable))
  
  # Collapse the path table list into one dataframe and send R^2 to the bottom
  pt_ltab_eff <- reg_long_table %>%
    as.data.frame() %>% 
    filter(!grepl("R\\^2|^n used$|^n total$|^Groups$",
                  Variable))
  
  pt_ltab_desc <- reg_long_table %>%
    as.data.frame() %>% 
    filter(grepl("R\\^2|^n used$|^n total$|^Groups$",
                 Variable))
  
  # Get Model Numbers
  long_table <- rbind(pt_ltab_eff,
                      pt_ltab_desc) %>% 
    magrittr::set_colnames(.,
                           c("Variable",
                             rep("Beta (95% CI)", length(outcomes)))) %>% 
    rbind(names(.),
          .) %>%
    magrittr::set_colnames(.,
                           c("",
                             paste0(sprintf("Model %01d ", seq(length(outcomes))),
                                    outcomes)))
  
  
  # ------------------------------------- #
  # -- DIRECT & INDIRECT EFFECTS TABLE -- #
  # ------------------------------------- #
  
  if(!is.null(predictors)){
    
    # Get Direct Effects
    direff <- regtab %>% 
      filter(Item %in% predictors)
    
    # Combine with Indirect Effects
    if(nrow(indtab) > 0){
      
      eff_table <- direff %>% 
        rbind(indtab)
      
    } else {
      
      eff_table <- direff  
      
    }
    
  }
  
  
  # Get exportable tabs
  if(!is.null(predictors)){
    
    if(nrow(indtab) > 0){
      
      sheets = list("mod" = sem_mod,
                    "export" = list("fit" = fittab,
                                    "coef" = regtab,
                                    "resid" = restab,
                                    "indirect" = indtab,
                                    "efftable" = eff_table,
                                    "table" = long_table))
      
    } else {
      
      sheets = list("mod" = sem_mod,
                    "export" = list("fit" = fittab,
                                    "coef" = regtab,
                                    "resid" = restab,
                                    "efftable" = eff_table,
                                    "table" = long_table))
      
    }
    
  } else {
    
    if(nrow(indtab) > 0){
      
      sheets = list("mod" = sem_mod,
                    "export" = list("fit" = fittab,
                                    "coef" = regtab,
                                    "resid" = restab,
                                    "indirect" = indtab,
                                    "table" = long_table))
      
    } else {
      
      sheets = list("mod" = sem_mod,
                    "export" = list("fit" = fittab,
                                    "coef" = regtab,
                                    "resid" = restab,
                                    "table" = long_table))
      
    }
    
  }
  
  return(sheets)
  
}


# MULTIPLE SEM WRAPPER ----------------------------------------------------

#' MULTIPLE STRUCTURAL EQUATION MODEL WRAPPER
#' 
#' @description
#' A simple wrapper to automate the running of multiple SEMs to compare fits.
#' 
#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param model A list of named `Lavaan` defined SEM objects. Refer to `lavaan`
#' documentation for a description of the user-specified model.
#' @param cluster If working with clustered/multilevel/hierarchical data, define
#' the cluster or group variable to account for shared SE. Cluster should be
#' a categorical factor variable.
#' @param estimator Estimator to be used in the confirmatory factor analysis.
#' Default is maximum likelihood (ML). Alternative esimators are limited to
#' those offered by `lavaan`, e.g., "GLS", "WLS", "DWLS", "ULS", "DLS", and
#' "PML". Refer to `lavaan` documentation for additional details.
#' @param se Parameter to determine how to compute standard errors. Default
#' is set to NULL. Set to "robust" to use either "robust.sem" or
#' "robust.huber.white" depending on the estimator used. Refer to lavaan
#' documentation for additional details.
#' @param missing Parameter to determine how to handle missingness. Default
#' is set to "listwise" where all observations with missing values are deleted
#' prior to analysis. As detailed in `lavaan`'s documentation, this is only
#' recommended when data are missing completely at random (MCAR). Another
#' options is "ml" for full information maximum likelihood approach (fiml).
#' Refer to lavaan documentation for additional details.
sem_table = function(data = .,
                     models,
                     cluster = NULL,
                     missing = "listwise",
                     se = NULL,
                     estimator = "ML"){
  
  library(lavaan)
  library(semTools)
  
  sem_est_list = list()
  sem_fit_list = list()
  sem_fit_table = list()
  mod_name_list = names(models)
  mod_reference = models
  
  for(mod in models){
    sem_model = sem(mod,
                    data = data,
                    missing = missing,
                    se = se,
                    cluster = cluster,
                    estimator = estimator,
                    fixed.x = FALSE)
    
    mod_order = which(mod_reference == mod) %>% as.numeric()
    mod_name = mod_name_list[mod_order]
    
    names = rownames(fitmeasures(sem_model) %>% as.data.frame())
    fittab = fitmeasures(sem_model) %>% as.data.frame() %>% data.table::transpose()
    colnames(fittab) = names
    fit_table = fittab %>% 
      mutate(Text = paste0("(chi-sq(", df,
                           ") = ", sprintf('%.3f',chisq),
                           ", CFI = ", sprintf('%.3f',cfi),
                           ", TLI = ", sprintf('%.3f',tli),
                           ", SRMR = ", sprintf('%.3f',srmr),
                           ", RSMEA = ", sprintf('%.3f',rmsea),
                           ", 90% CI [", sprintf('%.3f',rmsea.ci.lower),
                           ", ", sprintf('%.3f',rmsea.ci.upper),
                           "])")) %>% 
      dplyr::select(Text, everything())
    
    sem_fit_table[[mod_name]] = matrix(NA, nrow = 1, ncol = 10)
    colnames(sem_fit_table[[mod_name]]) = c("MODEL", "X2", "df", "CFI", "TLI",
                                            "RMSEA", "LOWER", "UPPER", "SRMR", "ECVI")
    sem_fit_table[[mod_name]][1, ] = c(paste(mod_name), 
                                       sprintf('%.3f',
                                               fitmeasures(sem_model,
                                                           c("chisq", "df", "cfi", "tli", "rmsea",
                                                             "rmsea.ci.lower", "rmsea.ci.upper",
                                                             "srmr", "ecvi"))))
    
    sem_fit_list[[paste0(mod_name,"_fit")]] = fit_table %>% as.data.frame()
    sem_est_list[[paste0(mod_name,"_est")]] = as.data.frame(standardizedsolution(sem_model)) %>% rename(Item = rhs)
    
  }
  
  fit_tables = sem_fit_table %>% purrr::reduce(rbind) %>% as.data.frame()
  fit_lists = sem_fit_list %>% purrr::reduce(rbind) %>% as.data.frame() %>% 
    rownames_to_column(var = "Model") %>% 
    mutate(Model = names(models))
  
  table_full = c("Fit_Table" = list(fit_tables),
                 "Fit_List" = list(fit_lists),
                 sem_est_list)
  
  return(table_full)
}

#' MULTIPLE CONFIRMATORY FACTOR ANALYSIS WRAPPER
#' 
#' @description
#' A simple wrapper to automate the running of multiple SEMs to compare fits.
#' 
#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param model A list of named `Lavaan` defined SEM objects. Refer to `lavaan`
#' documentation for a description of the user-specified model.
#' @param cluster If working with clustered/multilevel/hierarchical data, define
#' the cluster or group variable to account for shared SE. Cluster should be
#' a categorical factor variable.
#' @param estimator Estimator to be used in the confirmatory factor analysis.
#' Default is maximum likelihood (ML). Alternative esimators are limited to
#' those offered by `lavaan`, e.g., "GLS", "WLS", "DWLS", "ULS", "DLS", and
#' "PML". Refer to `lavaan` documentation for additional details.
#' @param se Parameter to determine how to compute standard errors. Default
#' is set to NULL. Set to "robust" to use either "robust.sem" or
#' "robust.huber.white" depending on the estimator used. Refer to lavaan
#' documentation for additional details.
#' @param missing Parameter to determine how to handle missingness. Default
#' is set to "listwise" where all observations with missing values are deleted
#' prior to analysis. As detailed in `lavaan`'s documentation, this is only
#' recommended when data are missing completely at random (MCAR). Another
#' options is "ml" for full information maximum likelihood approach (fiml).
#' Refer to lavaan documentation for additional details.
cfa_table = function(data = .,
                     models,
                     cluster = NULL,
                     missing = "listwise",
                     se = NULL,
                     estimator = "ML"){
  
  # Require packages
  library(lavaan)
  library(semTools)
  
  # Create empty lists
  cfa_est_list = list()
  cfa_fit_list = list()
  cfa_fit_table = list()
  mod_name_list = names(models)
  mod_reference = models
  
  for(mod in models){
    cfa_model = cfa(mod,
                    data = data,
                    missing = missing,
                    cluster = cluster,
                    se = se,
                    estimator = estimator)
    
    mod_order = which(mod_reference == mod) %>% as.numeric()
    mod_name = mod_name_list[mod_order]
    
    names = rownames(fitmeasures(cfa_model) %>% as.data.frame())
    fittab = fitmeasures(cfa_model) %>% as.data.frame() %>% data.table::transpose()
    colnames(fittab) = names
    fit_table = fittab %>% 
      mutate(Text = paste0("(chi-sq(", df,
                           ") = ", sprintf('%.3f',chisq),
                           ", CFI = ", sprintf('%.3f',cfi),
                           ", TLI = ", sprintf('%.3f',tli),
                           ", SRMR = ", sprintf('%.3f',srmr),
                           ", RSMEA = ", sprintf('%.3f',rmsea),
                           ", 90% CI [", sprintf('%.3f',rmsea.ci.lower),
                           ", ", sprintf('%.3f',rmsea.ci.upper),
                           "])")) %>% 
      dplyr::select(Text, everything())
    
    cfa_fit_table[[mod_name]] = matrix(NA, nrow = 1, ncol = 10)
    colnames(cfa_fit_table[[mod_name]]) = c("MODEL", "X2", "df", "CFI", "TLI",
                                            "RMSEA", "LOWER", "UPPER", "SRMR", "ECVI")
    cfa_fit_table[[mod_name]][1, ] = c(paste(mod_name), 
                                       sprintf('%.3f',
                                               fitmeasures(cfa_model,
                                                           c("chisq", "df", "cfi", "tli", "rmsea",
                                                             "rmsea.ci.lower", "rmsea.ci.upper",
                                                             "srmr", "ecvi"))))
    
    cfa_fit_list[[paste0(mod_name,"_fit")]] = fit_table %>% as.data.frame()
    cfa_est_list[[paste0(mod_name,"_est")]] = as.data.frame(standardizedsolution(cfa_model)) %>% rename(Item = rhs)
    
  }
  
  fit_tables = cfa_fit_table %>% purrr::reduce(rbind) %>% as.data.frame()
  fit_lists = cfa_fit_list %>% purrr::reduce(rbind) %>% as.data.frame() %>% 
    rownames_to_column(var = "Model") %>% 
    mutate(Model = names(models))
  
  table_full = c("Fit_Table" = list(fit_tables),
                 "Fit_List" = list(fit_lists),
                 cfa_est_list)
  
  return(table_full)
}


# PLS-SEM WRAPPER ---------------------------------------------------------

#' Partial Least Squares Structural Equation Modeling Wrapper
#' 
#' @description
#' A wrapper function to automate PLS-SEM and extracting its results. The
#' current function is limited to only 2 serial mediators. Due to the undue
#' level of complexity added to the model estimations when there are 3 or
#' more serial mediators, the current function is not designed to handle this
#' type of model. This automation function uses the `seminr` package (Ray et
#' al., 2022).
#' 
#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param measurements Measurement model object from `seminr` package. Refer
#' to the `seminr` help documentation for more details.
#' @param structure Structural model object from `seminr` package.
#' @param file Location to save output file as excel workbook if specified.
#' @param bootn Number of bootstrap replications to calculate p-values at the
#' structural pathways. Default to 1000.
#' 
#' @examples
#' measurements = constructs(
#'   composite("comp1", multi_items("com", 1:3)),
#'   composite("comp2", multi_items("com", 4:6)),
#'   higher_composite("high_comp", c("comp1", "comp2")),
#'   composite("mani1", c("v1")),
#'   composite("mani2", c("v2")),
#'   composite("outcome", multi_items("out", 1:5)),
#'   interaction_term(iv = "high_comp", moderator = "mani1", method = two_stage)
#' )
#' 
#' structure = relationships(
#'   paths(from = c("high_comp"),
#'         to = c("mani1", "mani2")),
#'   paths(from = c("high_comp", "mani1", "mani2", "high_comp*mani1"),
#'         to = c("outcome"))
#' )
#' 
#' pls_summary(data, measurements, structure)
#' 
#' 
#' @references Ray S, Danks N, Calero Valdez A (2022). _seminr: Building and
#' Estimating Structural Equation Models_. R package version 2.3.2,
#' <https://CRAN.R-project.org/package=seminr>.
#' 
#' @export
pls_summary = function(data = .,
                       measurements,
                       structure,
                       file = NULL,
                       bootn = 1000){
  
  # ---------------------------- #
  # -- EXTRACT STRUCTURE VARS -- #
  # ---------------------------- #
  
  # Create separate dataframe for structure to extract variables
  structure_df = structure %>% 
    as.data.frame()
  
  # Get unique values of all composites and latent vars in structure model
  vars = unique(c(unique(structure_df$source),
                  unique(structure_df$target)))
  
  # Identify endogenous mediators, serial mediators, and outcome vars
  paths = unique(structure_df$target)
  
  # Create empty lists for relevant model variables
  predictors_list = list()
  mediators_list = list()
  serial_mediators_list = list()
  outcomes_list = list()
  
  # Extract predictors & outcomes
  for(var in vars){
    
    # If var only shows up in "source" and not "target", assign as predictor
    if(var %in% structure_df$source && !var %in% structure_df$target){
      
      predictors_list[[var]] = paste(var)
      
    }
    
    # If var only shows up in "target" and not "source", assign as outcome
    if(!var %in% structure_df$source && var %in% structure_df$target){
      
      outcomes_list[[var]] = paste(var)
      
    }
    
  }
  
  # Reduce into single combined string
  predictors_bind = purrr::reduce(predictors_list, cbind)
  outcomes_bind = purrr::reduce(outcomes_list, cbind)
  
  predictors = c(predictors_bind)
  outcomes = c(outcomes_bind)
  
  # Exclude outcomes and get unique targets for mediator variables
  structure_df2 <- structure_df %>% 
    filter(!target %in% outcomes)
  
  med_vars <- unique(structure_df2$target)
  
  # Identify any pathways indicating serial mediators based on whether mediators are predicting mediators
  structure_df2 <- structure_df2 %>% 
    mutate(serial_flag = source %in% med_vars & target %in% med_vars)
  
  # Flag if there are any serial mediators; 1 if flagged, 0 if not.
  serial_flag <- as.integer(any(structure_df2$serial_flag))
  
  
  # Run if presence of serial mediators is identified
  if(serial_flag == 1){
    
    # Get second structural dataframe but exclude all predictors and outcomes
    structure_df2 <- structure_df %>% 
      filter(!source %in% predictors) %>% 
      filter(!target %in% outcomes)
    
    # Get vector of unique mediating variables
    med_vars = unique(c(unique(structure_df2$source),
                        unique(structure_df2$target)))
    
    # Extract mediating and serial mediating variables
    for(var in med_vars){
      
      # If variable is only in source and not target, assign as mediator
      if(var %in% structure_df2$source && !var %in% structure_df2$target){
        
        mediators_list[[var]] = paste(var)
        
      }
      
      # If variable is only in target and not in source, assign as serial mediator
      if(!var %in% structure_df2$source && var %in% structure_df2$target){
        
        serial_mediators_list[[var]] = paste(var)
        
      }
      
    }
    
    # If there are mediators, get vector, otherwise assign as NULL
    if(length(mediators_list) > 0){
      
      mediators_bind = purrr::reduce(mediators_list, cbind)
      mediators = c(mediators_bind)
      
    } else {
      
      mediators = NULL
      
    }
    
    # If there are serial mediators, get vector, otherwise assign as NULL
    if(length(serial_mediators_list) > 0){
      
      serial_mediators_bind = purrr::reduce(serial_mediators_list, cbind)
      serial_mediators = c(serial_mediators_bind)
      
    } else {
      
      serial_mediators = NULL
      
    }
    
  } else {
    
    mediators = med_vars
    serial_mediators = NULL
    
  }
  
  # If mediators is length 0, then assign NULL
  if(length(mediators) == 0){
    
    mediators = NULL
    
  }
  
  
  # ---------------------------- #
  # -- PLS-SEM BASIC ANALYSIS -- #
  # ---------------------------- #
  
  # Run PLS-SEM analysis
  pls_model = estimate_pls(data = data,
                           measurement_model = measurements,
                           structural_model = structure)
  
  # Get PLS-SEM summary of results
  model_summary = summary(pls_model)
  
  # Get general model path summary
  modelsum = model_summary$paths %>%
    as.data.frame() %>%
    rownames_to_column(var = "Variable")
  
  # Get model effect sizes
  modeleffsize = model_summary$fSquare %>%
    as.data.frame() %>%
    rownames_to_column(var = "Variable")
  
  # Get model reliability statistics
  modelreliability = model_summary$reliability %>%
    as.data.frame() %>%
    rownames_to_column(var = "Variable")
  
  # Create empty list to assign VIF statistics
  viftables = list()
  
  # Loop and assign VIF statistics 
  for (pathvar in paths){
    viftables[[pathvar]] = model_summary[["vif_antecedents"]][[pathvar]] %>%
      as.data.frame() %>%
      rownames_to_column(var = "Variable") %>%
      rename(!!paste0(pathvar) := ".")
  }
  
  # Collapse VIF stats together into table
  modelvif = viftables %>%
    purrr::reduce(full_join)
  
  
  
  
  
  # --------------------------- #
  # -- PLS-SEM BOOTSTRAPPING -- #
  # --------------------------- #
  
  # Boostrap PLS-SEM to get confidence intervals
  boot_mod1 = bootstrap_model(seminr_model = pls_model,
                              nboot = bootn)
  
  # Get summary statistics
  summarystat = summary(boot_mod1) #Create object
  
  # Create degrees of freedom numbers based on outcome variable
  n = nrow(pls_model$construct_scores)
  
  # Degrees of freedom for outcome path
  df_out = n - (1 + length(predictors) + length(serial_mediators) + length(mediators))
  # Degrees of freedom for serial mediator path
  df_smd = n - (1 + length(predictors) + length(mediators))
  # Degrees of freedom for mediator path
  df_med = n - (1 + length(predictors))
  
  # Get direct effect paths from t-statistic
  paths_t = as.data.frame(summarystat$bootstrapped_paths) %>% 
    rownames_to_column(var = "Path") %>%
    rename(`Bootstrap SE` = `Bootstrap SD`) %>%
    mutate(p.value = 2*pt(q = abs(`T Stat.`),
                          df = ifelse(str_extract(Path, "(?<=  ->  )[^->]+$") %in% outcomes,
                                      df_out,
                                      ifelse(str_extract(Path, "(?<=  ->  )[^->]+$") %in% serial_mediators,
                                             df_smd,
                                             df_med)),
                          lower.tail = F),
           text = paste("(beta = ",sprintf('%.3f',`Bootstrap Mean`),
                        ", t(",sprintf('%.f',ifelse(str_extract(Path, "(?<=  ->  )[^->]+$") %in% outcomes,
                                                    df_out,
                                                    ifelse(str_extract(Path, "(?<=  ->  )[^->]+$") %in% serial_mediators,
                                                           df_smd,
                                                           df_med))),
                        ") = ",sprintf('%.3f',`T Stat.`),
                        ", 95% CI [",sprintf('%.3f',`2.5% CI`),", ",sprintf('%.3f',`97.5% CI`),
                        "], p ", ifelse(p.value < 0.001, "< 0.001", paste("=",sprintf('%.3f',p.value))),")",
                        sep = ""),
           fig.text = paste(sprintf('%.3f',`Bootstrap Mean`),
                            ifelse(p.value > .05, "",
                                   ifelse(p.value < .05 & p.value > .01, "*",
                                          ifelse(p.value < .01 & p.value > .001, "**", "***"))),
                            " [", sprintf('%.3f',`2.5% CI`),
                            ", ", sprintf('%.3f',`97.5% CI`),
                            "]",
                            sep = ""))
  
  # Get direct effect paths from z-statistic
  # z-statistic generally preferred due to bootstrap nature of PLS-SEM's p-value calculation
  paths_z = as.data.frame(summarystat$bootstrapped_paths) %>% 
    rownames_to_column(var = "Path") %>%
    rename(`Bootstrap SE` = `Bootstrap SD`) %>% # Renamed as `seminr` labels standard error as standard deviation
    mutate(z = `Bootstrap Mean` / `Bootstrap SE`, # z-statistic calculated with (coef estimate) / (SE of coef)
           p.value = 2*pnorm(-abs(z)),
           text = paste("(beta = ",sprintf('%.3f',`Bootstrap Mean`),
                        ", z(",sprintf('%.f',ifelse(str_extract(Path, "(?<=  ->  )[^->]+$") %in% outcomes,
                                                    df_out,
                                                    ifelse(str_extract(Path, "(?<=  ->  )[^->]+$") %in% serial_mediators,
                                                           df_smd,
                                                           df_med))),
                        ") = ",sprintf('%.3f',z),
                        ", 95% CI [",sprintf('%.3f',`2.5% CI`),", ",sprintf('%.3f',`97.5% CI`),
                        "], p ", ifelse(p.value < 0.001, "< 0.001", paste("=",sprintf('%.3f',p.value))),")",
                        sep = ""),
           fig.text = paste(sprintf('%.3f',`Bootstrap Mean`),
                            ifelse(p.value > .05, "",
                                   ifelse(p.value < .05 & p.value > .01, "*",
                                          ifelse(p.value < .01 & p.value > .001, "**", "***"))),
                            " [", sprintf('%.3f',`2.5% CI`),
                            ", ", sprintf('%.3f',`97.5% CI`),
                            "]",
                            sep = ""))
  
  # Create empty lists for regression tables
  path_table_v1 = list()
  path_table_v2 = list()
  
  
  # -------------------------------------- #
  # -- REGRESSION TABLES IN LONG FORMAT -- #
  # -------------------------------------- #
  
  
  # Loop for every endogenous variables (e.g., mediators,)
  for (var in paths){
    
    pt_outcome <- paths_z %>%
      as.data.frame() %>% 
      filter(grepl(paste0(var,"$"),Path))
    
    pt_est = pt_outcome %>% 
      mutate(std_est = paste(sprintf('%.3f', `Bootstrap Mean`),
                             ifelse(p.value > .05, "",
                                    ifelse(p.value < .05 & p.value > .01, "*",
                                           ifelse(p.value < .01 & p.value > .001, "**", "***"))), sep = " ")) %>% 
      select(Path, std_est)
    
    pt_ci <- pt_outcome %>% 
      mutate(ci = paste0("(",
                         sprintf('%.3f',`2.5% CI`),
                         ", ",
                         sprintf('%.3f',`97.5% CI`),
                         ")")) %>% 
      select(Path, ci)
    
    # Get degrees of freedom for number of predictors (numerator)
    degfree1 <- length(pt_est$Path)
    # Get degrees of freedom for number of observations minus predictors - 1 (denominator)
    degfree2 <- nrow(pls_model$construct_scores) - (1 + degfree1)
    
    pt_df1 <- data.frame("EST" = c(rbind(pt_est$std_est, pt_ci$ci)))
    pt_df2 <- data.frame("Variable" = c(rbind(pt_est$Path,
                                              paste0(sub("(.*)  ->  .*", "\\1", pt_est$Path),"_CI")))) %>% 
      mutate(Variable = gsub("  ->.*", "",Variable))
    pt_df3 <- data.frame("Variable" = c("Observations",
                                        "R^2",
                                        "R^2 Adj"),
                         "EST" = c(nrow(pls_model$construct_scores),
                                   sprintf('%.3f',as.data.frame(pls_model$rSquared)[[var]])))
    
    # Get R^2 value for F-statistic calculation
    rsqr <- as.data.frame(pls_model$rSquared)[[var]][1]
    
    # Calculate F-statistic
    pt_df4 <- data.frame("Variable" = c("F"),
                         "EST" = sprintf('%.3f', (rsqr / (1 - rsqr)) * (degfree2 / degfree1)))
    
    pt_table = pt_df2 %>%
      cbind(pt_df1) %>% 
      rbind(pt_df3) %>% 
      rbind(pt_df4) %>% 
      rename(!!sym(paste0(var,
                          " Beta (95% CI)")) := EST)
    
    path_table_v1[[paste0(var,"_V1")]] = pt_table
    
  }
  
  # Collapse the path table list into one dataframe and send R^2 to the bottom
  pt_tab_eff <- path_table_v1 %>%
    purrr::reduce(full_join) %>%
    as.data.frame() %>% 
    filter(!grepl("R\\^2|^Observations$|^F$",
                  Variable))
  
  pt_tab_desc <- path_table_v1 %>%
    purrr::reduce(full_join) %>%
    as.data.frame() %>% 
    filter(grepl("R\\^2|^Observations$|^F$",
                 Variable))
  
  # Combine into final product
  pathtab1 <- rbind(pt_tab_eff,
                    pt_tab_desc) %>% 
    mutate(Variable = ifelse(grepl("_CI", Variable), "", Variable)) %>% 
    magrittr::set_colnames(.,
                           c("Variable",
                             rep("Beta (95% CI)", length(paths))))
  
  # Get Model Numbers
  pathtab1 <- rbind(names(pathtab1),
                    pathtab1) %>% 
    magrittr::set_colnames(.,
                           c("",
                             paste0(sprintf("Model %01d ", seq(length(paths))),
                                    paths)))
  
  
  
  # -------------------------------------- #
  # -- REGRESSION TABLES IN WIDE FORMAT -- #
  # -------------------------------------- #
  
  for (var in paths){
    
    # Get direct effects relevant to exogenous outcome
    pt_outcome = paths_z %>%
      as.data.frame() %>% 
      filter(grepl(paste0(var,"$"),Path))
    
    # Select only relevant metrics (beta, standard error, p-value)
    pt_est = pt_outcome %>% 
      mutate(std_est = paste(sprintf('%.3f', `Bootstrap Mean`),
                             ifelse(p.value > .05, "",
                                    ifelse(p.value < .05 & p.value > .01, "*",
                                           ifelse(p.value < .01 & p.value > .001, "**", "***"))), sep = " "),
             std_se = sprintf("%.3f",`Bootstrap SE`),
             p_val = ifelse(p.value < .001, "<.001", sprintf("%.3f", p.value))) %>% 
      select(Path, std_est, std_se, p_val)
    
    # Grab 95% confidence intervals
    pt_ci = pt_outcome %>% 
      mutate(ci = paste0("(",
                         sprintf('%.3f',`2.5% CI`),
                         ", ",
                         sprintf('%.3f',`97.5% CI`),
                         ")")) %>% 
      select(Path, ci)
    
    # Clean up labels of variables
    pt_df1 = pt_est %>% 
      inner_join(pt_ci) %>% 
      select(Path, std_est, std_se, ci, p_val) %>% 
      mutate(Path = gsub("  ->.*", "",Path))
    
    # Get basic model descriptives and metrics
    pt_df2 = data.frame("Path" = c("Observations",
                                   "R^2",
                                   "R^2 Adj"),
                        "std_est" = c(nrow(pls_model$construct_scores),
                                      sprintf('%.3f',as.data.frame(pls_model$rSquared)[[var]])),
                        "std_se" = rep("",3),
                        "ci" = rep("",3),
                        "p_val" = rep("",3))
    
    # Get degrees of freedom for number of predictors (numerator)
    degfree1 <- length(pt_est$Path)
    # Get degrees of freedom for number of observations minus predictors - 1 (denominator)
    degfree2 <- nrow(pls_model$construct_scores) - (1 + degfree1)
    
    # Get R^2 value for F-statistic calculation
    rsqr <- as.data.frame(pls_model$rSquared)[[var]][1]
    
    # Calculate F-statistic
    pt_df4 <- data.frame("Variable" = c("F"),
                         "EST" = sprintf('%.3f', (rsqr / (1 - rsqr)) * (degfree2 / degfree1)))
    
    pt_df3 <- data.frame("Path" = c("F"),
                         "std_est" = sprintf('%.3f', (rsqr / (1 - rsqr)) * (degfree2 / degfree1)),
                         "std_se" = "",
                         "ci" = "",
                         "p_val" = "")
    
    pt_table = pt_df1 %>%
      rbind(pt_df2,
            pt_df3) %>% 
      rename("Variable" = Path,
             !!sym(paste0(var," std_est")) := std_est,
             !!sym(paste0(var," std_se")) := std_se,
             !!sym(paste0(var," ci")) := ci,
             !!sym(paste0(var," p_val")) := p_val)
    
    path_table_v2[[paste0(var,"_V2")]] = pt_table
    
  }
  
  # Collapse the path table list into one dataframe and send R^2 to the bottom
  pt_wtab_eff <- path_table_v2 %>%
    purrr::reduce(full_join) %>%
    as.data.frame() %>% 
    filter(!grepl("R\\^2|^Observations$|^F$",
                  Variable))
  
  pt_wtab_desc <- path_table_v2 %>%
    purrr::reduce(full_join) %>%
    as.data.frame() %>% 
    filter(grepl("R\\^2|^Observations$|^F$",
                 Variable))
  
  # Combine into final product
  pathtab2 <- rbind(pt_wtab_eff,
                    pt_wtab_desc) %>% 
    magrittr::set_colnames(.,
                           c("Variable",
                             rep(c("Beta",
                                   "SE",
                                   "95% CI (Lower, Upper)",
                                   "p"),
                                 length(paths))))
  
  # Get model numbers and exogenous variable
  pt2_names <- c("",
                 c(rbind(paste0(sprintf("Model %01d",
                                        seq(length(paths))),
                                " ",
                                paths), "", "", "")))
  
  # Rename to combine with model numbers and exogenous variable
  pathtab2 <- rbind(names(pathtab2),
                    pathtab2) %>% 
    magrittr::set_colnames(pt2_names)
  
  
  
  # -------------------------------- #
  # -- ADDITIONAL PLS-SEM METRICS -- #
  # -------------------------------- #
  
  #Create loadings dataframe
  loadings = as.data.frame(summarystat$bootstrapped_loadings) %>%
    rownames_to_column(var = "Path") %>%
    rename(`Bootstrap SE` = `Bootstrap SD`) %>%
    mutate(z = `Bootstrap Mean` / `Bootstrap SE`,
           p.value = 2*pnorm(-abs(z)))
  
  #Create weights dataframe
  weights = as.data.frame(summarystat$bootstrapped_weights) %>%
    rownames_to_column(var = "Path")
  
  #Create total paths dataframe
  totpaths = as.data.frame(summarystat$bootstrapped_total_paths) %>%
    rownames_to_column(var = "Path")
  
  #Create HTMT dataframe
  htmt = as.data.frame(summarystat$bootstrapped_HTMT) %>%
    rownames_to_column(var = "Path")
  
  # Running all possible combinations of predictors -> mediators -> outcomes
  if(is.null(serial_mediators) == TRUE){
    
    indtables = list()
    
    for(pvar in predictors){
      for(mvar in mediators){
        for(ovar in outcomes){
          indtables[[paste(pvar,mvar,ovar,sep = " -> ")]] = specific_effect_significance(boot_seminr_model = boot_mod1,
                                                                                         from = pvar,
                                                                                         through = mvar,
                                                                                         to = ovar,
                                                                                         alpha = 0.05)
        }
      }
    }
  } else {
    
    # Flagged if serial_mediators indicates that there are second serial mediators
    # Running all possible combinations of predictors -> mediators -> second mediators -> outcomes
    
    indtables1 = list()
    
    for(pvar in predictors){
      for(mvar in mediators){
        for(ovar in outcomes){
          indtables1[[paste(pvar,mvar,ovar, sep = " -> ")]] = specific_effect_significance(boot_seminr_model = boot_mod1,
                                                                                           from = pvar,
                                                                                           through = mvar,
                                                                                           to = ovar,
                                                                                           alpha = 0.05)
        }
      }
    }
    
    indtables2 = list()
    
    for(pvar in predictors){
      for(mvar in mediators){
        for(svar in serial_mediators){
          for(ovar in outcomes){
            indtables2[[paste(pvar,mvar,svar,ovar, sep = " -> ")]] = specific_effect_significance(boot_seminr_model = boot_mod1,
                                                                                                  from = pvar,
                                                                                                  through = c(mvar,svar),
                                                                                                  to = ovar,
                                                                                                  alpha = 0.05)
          } 
        }
      }
    }
    
    indtables3 = list()
    
    for(pvar in predictors){
      for(mvar in mediators){
        for(svar in serial_mediators){
          indtables3[[paste(pvar,mvar,svar, sep = " -> ")]] = specific_effect_significance(boot_seminr_model = boot_mod1,
                                                                                           from = pvar,
                                                                                           through = mvar,
                                                                                           to = svar,
                                                                                           alpha = 0.05)
        }
      }
    }
    
    indtables4 = list()
    
    for(pvar in predictors){
      for(svar in serial_mediators){
        for(ovar in outcomes){
          indtables4[[paste(pvar,svar,ovar, sep = " -> ")]] = specific_effect_significance(boot_seminr_model = boot_mod1,
                                                                                           from = pvar,
                                                                                           through = svar,
                                                                                           to = ovar,
                                                                                           alpha = 0.05)
        }
      }
    }
    
    indtables5 = list()
    
    for(mvar in mediators){
      for(svar in serial_mediators){
        for(ovar in outcomes){
          indtables5[[paste(mvar,svar,ovar, sep = " -> ")]] = specific_effect_significance(boot_seminr_model = boot_mod1,
                                                                                           from = mvar,
                                                                                           through = svar,
                                                                                           to = ovar,
                                                                                           alpha = 0.05)
        }
      }
    }
    
    indtables_full = list(indtables1, indtables2, indtables3, indtables4, indtables5)
    indtables = lapply(rapply(indtables_full, enquote, how="unlist"), eval)
    
  }
  
  # Create a table of indirect effects
  if(!is.null(mediators)){
    
    indirect_effects = indtables %>%
      do.call(rbind, .) %>% 
      as.data.frame() %>% 
      rownames_to_column(var = "Path") %>%
      mutate(Path = names(indtables)) %>% 
      rename(`Bootstrap SE` = `Bootstrap SD`) %>%
      mutate(z = `Bootstrap Mean` / `Bootstrap SE`,
             p.value = 2*pnorm(-abs(z)),
             text = paste0("(beta = ", sprintf('%.3f', `Bootstrap Mean`),
                           ", SE = ", sprintf('%.3f', `Bootstrap SE`),
                           ", z = ", sprintf('%.3f', z),
                           ", 95% CI [", sprintf('%.3f', `2.5% CI`),
                           ", ", sprintf('%.3f', `97.5% CI`),
                           "], p ", ifelse(p.value < 0.001, "< 0.001",
                                           paste("=", sprintf('%.3f', p.value))),")"))
    
  }
  
  if(is.null(mediators) & is.null(serial_mediators)){
    
    sheets = list("PLS_Model" = pls_model,
                  "PLS_Summary" = model_summary,
                  "Boot_Model" = boot_mod1,
                  "Boot_Summary" = summarystat,
                  "Export" = list("ModelR" = modelsum,
                                  "ModelES" = modeleffsize,
                                  "Reliability" = modelreliability,
                                  "VIF" = modelvif,
                                  "Paths_z" = paths_z,
                                  "Paths_t" = paths_t,
                                  "Tables_Long" = pathtab1,
                                  "Tables_Wide" = pathtab2,
                                  "Loadings" = loadings,
                                  "Weights" = weights,
                                  "TotPaths" = totpaths,
                                  "HTMT" = htmt))
    
  } else {
    
    sheets = list("PLS_Model" = pls_model,
                  "PLS_Summary" = model_summary,
                  "Boot_Model" = boot_mod1,
                  "Boot_Summary" = summarystat,
                  "Export" = list("ModelR" = modelsum,
                                  "ModelES" = modeleffsize,
                                  "Reliability" = modelreliability,
                                  "VIF" = modelvif,
                                  "Paths_z" = paths_z,
                                  "Paths_t" = paths_t,
                                  "Tables_Long" = pathtab1,
                                  "Tables_Wide" = pathtab2,
                                  "Loadings" = loadings,
                                  "Weights" = weights,
                                  "TotPaths" = totpaths,
                                  "HTMT" = htmt,
                                  "Indirect" = indirect_effects))
    
  }
  
  
  if(is.null(file) == FALSE){
    
    writexl::write_xlsx(sheets$Export, path = file)
    
  }
  
  return(sheets)
  
}


# REPLACE WITH NA ---------------------------------------------------------

#' REPLACE VALUES WITH NA
#' 
#' @description
#' A simple function to replace values of specific variables with NA. This
#' function is meant to mimic the \code{naniar} package's \code{replace_with_na()}
#' function expedite the processing speed. For more complex functionality
#' around dealing with missing data, see Tierney and Cook's (2023) \code{naniar}
#' package.
#' 
#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param variables A required vector of column names representing their 
#' manifest variables.
#' @param values A required vector of numeric or string values to be replaced
#' with NA.
#' 
#' @return A dataframe of the dataset with the values of the variables replaced
#' with NA (missing).
#' 
#' @examples
#' replace_with_na(data,
#'                 variables = c("var1", "var2"),
#'                 values = c(99, 98))
#' replace_with_na(data,
#'                 variables = c("educ"),
#'                 values = c("Unknown", "Prefer not to say"))
#' 
#' @references
#' Tierney N, Cook D (2023). “Expanding Tidy Data Principles to Facilitate
#' Missing Data Exploration, Visualization and Assessment of Imputations.”
#' \emph{Journal of Statistical Software}, \emph{105}(7), 1-31.
#' doi:10.18637/jss.v105.i07.
#' 
#' @export
replace_with_na <- function(data = .,
                            variables,
                            values){
  
  # Change values in variables to NA
  data <- data %>% 
    mutate(across(.cols = all_of(variables),
                  ~replace(., . %in% values, NA)))
  
  # Return data
  return(data)
  
}

# REGRESSION COEFFICIENT WRAPPER ------------------------------------------

std_coef = function(object) {
  
  if(class(object) == "glmerMod") {
    
    summary = summary(object)
    
    coefficients = summary$coefficients %>% 
      as.data.frame()
    
    estimate = coefficients %>% 
      select(Estimate) %>% 
      rename(est = Estimate)
    
    se = coefficients %>% 
      select(`Std. Error`) %>% 
      rename(se = `Std. Error`)
    
    odds_ratio = exp(estimate) %>% 
      rename(odds_ratio = est)
    
    or_lower = exp(estimate + qnorm(0.025) * se) %>% 
      rename(or_lower = est)
    or_upper = exp(estimate + qnorm(0.975) * se) %>% 
      rename(or_upper = est)
    
    names = rownames(coefficients)
    
    z_val = coefficients %>% 
      select(`z value`) %>% 
      rename(z_stat = `z value`)
    p_val = coefficients %>% 
      select(`Pr(>|z|)`) %>% 
      rename(p_value = `Pr(>|z|)`)
    
    sum_table = data.frame(est = estimate,
                           odds_ratio = odds_ratio,
                           or_lower = or_lower,
                           or_upper = or_upper,
                           se = se,
                           z_stat = z_val,
                           p_value = p_val) %>% 
      mutate(text = paste0("(OR = ", sprintf('%.3f', odds_ratio),
                           ", z = ", sprintf('%.3f', z_stat),
                           ", 95% CI [",sprintf('%.3f', or_lower),", ",sprintf('%.3f', or_upper),
                           "], p ", ifelse(p_value < 0.001, "< 0.001",
                                           paste("=",sprintf('%.3f', p_value))),")")) %>% 
      select(est, odds_ratio, se, 
             or_lower, or_upper,
             everything()) %>% 
      rownames_to_column(var = "variable")
    
    vif_table = performance::check_collinearity(object) %>% 
      as.data.frame()
    
    min_vif = data.frame(Term = "MIN",
                         VIF = min(vif_table$VIF),
                         VIF_CI_low = min(vif_table$VIF_CI_low),
                         VIF_CI_high = min(vif_table$VIF_CI_high),
                         SE_factor = min(vif_table$SE_factor),
                         Tolerance = min(vif_table$Tolerance),
                         Tolerance_CI_low = min(vif_table$Tolerance_CI_low),
                         Tolerance_CI_high = min(vif_table$Tolerance_CI_high))
    max_vif = data.frame(Term = "MAX",
                         VIF = max(vif_table$VIF),
                         VIF_CI_low = max(vif_table$VIF_CI_low),
                         VIF_CI_high = max(vif_table$VIF_CI_high),
                         SE_factor = max(vif_table$SE_factor),
                         Tolerance = max(vif_table$Tolerance),
                         Tolerance_CI_low = max(vif_table$Tolerance_CI_low),
                         Tolerance_CI_high = max(vif_table$Tolerance_CI_high))
    text_vif = data.frame(Term = "TEXT",
                          VIF = paste0("(VIF from ",
                                       sprintf('%.3f', min(vif_table$VIF)),
                                       " to ",
                                       sprintf('%.3f', max(vif_table$VIF)),
                                       ")"),
                          VIF_CI_low = "",
                          VIF_CI_high = "",
                          SE_factor = paste0("(SE factor from ",
                                             sprintf('%.3f', min(vif_table$SE_factor)),
                                             " to ",
                                             sprintf('%.3f', max(vif_table$SE_factor)),
                                             ")"),
                          Tolerance = paste0("(Tolerance from ",
                                             sprintf('%.3f', min(vif_table$Tolerance)),
                                             " to ",
                                             sprintf('%.3f', max(vif_table$Tolerance)),
                                             ")"),
                          Tolerance_CI_low = "",
                          Tolerance_CI_high = "")
    
    vif_table = vif_table %>% 
      rbind(min_vif,
            max_vif,
            text_vif)
    
    reg_table = sum_table %>% 
      mutate(OR = paste0(sprintf('%.3f', odds_ratio),
                         ifelse(p_value < 0.001, "***",
                                ifelse(p_value < 0.01, "**",
                                       ifelse(p_value < 0.05, "*", "")))),
             "95% CI" = paste0("[",
                               sprintf('%.3f', or_lower),
                               ", ",
                               sprintf('%.3f', or_upper),
                               "]")) %>% 
      select(variable, OR, "95% CI")
    
  }
  
  if(class(object) == "lmerModLmerTest") {
    
    lmer_mod = summary(object)
    
    estimate = lmer_mod$coefficients %>% 
      as.data.frame() %>% 
      select(Estimate) %>% 
      rename(est = Estimate)
    
    sd_outcome = sd(getME(object,"y"))
    sd_predictors = apply(getME(object,"X"), 2, sd)
    
    std_beta = fixef(object)*sd_predictors/sd_outcome
    
    se_fixedeffect = coef(summary(object))[,"Std. Error"]
    
    se = se_fixedeffect*sd_predictors/sd_outcome
    
    names = names(std_beta)
    
    summary = summary(object)
    coefficients = summary$coefficients %>% as.data.frame()
    
    df_val = coefficients$df
    t_val = coefficients$`t value`
    p_val = coefficients$`Pr(>|t|)`
    
    names(df_val) = names(std_beta)
    names(t_val) = names(std_beta)
    names(p_val) = names(std_beta)
    
    sum_table = data.frame(est = estimate,
                           std_est = std_beta,
                           std_se = se,
                           df = df_val,
                           t_stat = t_val,
                           p_value = p_val) %>% 
      mutate(ci_lower = std_est - (std_se*qnorm(0.975)),
             ci_upper = std_est + (std_se*qnorm(0.975)),
             text = paste0("(β = ", sprintf('%.3f', std_est),
                           ", t(", sprintf('%.3f', df),
                           ") = ", sprintf('%.3f', t_stat),
                           ", 95% CI [",sprintf('%.3f', ci_lower),", ",sprintf('%.3f', ci_upper),
                           "], p ", ifelse(p_value < 0.001, "< 0.001",
                                           paste("=",sprintf('%.3f', p_value))),")")) %>% 
      select(est, std_est, std_se, ci_lower, ci_upper, everything()) %>% 
      rownames_to_column(var = "variable")
    
    vif_table = performance::check_collinearity(object) %>% 
      as.data.frame()
    
    min_vif = data.frame(Term = "MIN",
                         VIF = min(vif_table$VIF),
                         VIF_CI_low = min(vif_table$VIF_CI_low),
                         VIF_CI_high = min(vif_table$VIF_CI_high),
                         SE_factor = min(vif_table$SE_factor),
                         Tolerance = min(vif_table$Tolerance),
                         Tolerance_CI_low = min(vif_table$Tolerance_CI_low),
                         Tolerance_CI_high = min(vif_table$Tolerance_CI_high))
    max_vif = data.frame(Term = "MAX",
                         VIF = max(vif_table$VIF),
                         VIF_CI_low = max(vif_table$VIF_CI_low),
                         VIF_CI_high = max(vif_table$VIF_CI_high),
                         SE_factor = max(vif_table$SE_factor),
                         Tolerance = max(vif_table$Tolerance),
                         Tolerance_CI_low = max(vif_table$Tolerance_CI_low),
                         Tolerance_CI_high = max(vif_table$Tolerance_CI_high))
    text_vif = data.frame(Term = "TEXT",
                          VIF = paste0("(VIF from ",
                                       sprintf('%.3f', min(vif_table$VIF)),
                                       " to ",
                                       sprintf('%.3f', max(vif_table$VIF)),
                                       ")"),
                          VIF_CI_low = "",
                          VIF_CI_high = "",
                          SE_factor = paste0("(SE factor from ",
                                             sprintf('%.3f', min(vif_table$SE_factor)),
                                             " to ",
                                             sprintf('%.3f', max(vif_table$SE_factor)),
                                             ")"),
                          Tolerance = paste0("(Tolerance from ",
                                             sprintf('%.3f', min(vif_table$Tolerance)),
                                             " to ",
                                             sprintf('%.3f', max(vif_table$Tolerance)),
                                             ")"),
                          Tolerance_CI_low = "",
                          Tolerance_CI_high = "")
    
    vif_table = vif_table %>% 
      rbind(min_vif,
            max_vif,
            text_vif)
    
    reg_table = sum_table %>% 
      mutate(Beta = paste0(sprintf('%.3f', std_beta),
                           ifelse(p_value < 0.001, "***",
                                  ifelse(p_value < 0.01, "**",
                                         ifelse(p_value < 0.05, "*", "")))),
             "95% CI" = paste0("[",
                               sprintf('%.3f', ci_lower),
                               ", ",
                               sprintf('%.3f', ci_upper),
                               "]")) %>% 
      select(variable, Beta, "95% CI")
    
  } 
  
  if(class(object) == "lm") {
    
    library(broom)
    
    estimate = object$coefficients
    
    summary = summary(object)
    
    sd_outcome = sd(object$model[[1]])
    sd_predictors = apply(object$model[-1] %>% 
                            mutate(`(Intercept)` = 1) %>% 
                            select(`(Intercept)`, everything()), 2, sd)
    
    se_fixedeffect = summary$coefficients[,"Std. Error"]
    
    std_beta = estimate*sd_predictors/sd_outcome
    
    se = se_fixedeffect*sd_predictors/sd_outcome
    
    std_ci = as.data.frame(confint(object)*(sd_predictors/sd_outcome)) %>% 
      rename(ci_lower = "2.5 %",
             ci_upper = "97.5 %")
    
    names = names(std_beta)
    
    coefficients = summary$coefficients %>% as.data.frame() %>% 
      mutate(df = summary$df[2])
    
    df_val = coefficients$df
    t_val = coefficients$`t value`
    p_val = coefficients$`Pr(>|t|)`
    
    names(df_val) = names(std_beta)
    names(t_val) = names(std_beta)
    names(p_val) = names(std_beta)
    
    sum_table = data.frame(est = estimate,
                           std_est = std_beta,
                           std_se = se,
                           ci = std_ci,
                           df = df_val,
                           t_stat = t_val,
                           p_value = p_val) %>%
      rename(ci_lower = "ci.ci_lower",
             ci_upper = "ci.ci_upper") %>% 
      mutate(df = df_val,
             text = paste0("(β = ", sprintf('%.3f', std_est),
                           ", t(", sprintf('%.f', df),
                           ") = ", sprintf('%.3f', t_stat),
                           ", 95% CI [",sprintf('%.3f', ci_lower),", ",sprintf('%.3f', ci_upper),
                           "], p ", ifelse(p_value < 0.001, "< 0.001",
                                           paste("=",sprintf('%.3f', p_value))),")")) %>%
      select(est, std_est, std_se, ci_lower, ci_upper, everything()) %>% 
      rownames_to_column(var = "variable")
    
    vif_table = performance::check_collinearity(object) %>% 
      as.data.frame()
    
    min_vif = data.frame(Term = "MIN",
                         VIF = min(vif_table$VIF),
                         VIF_CI_low = min(vif_table$VIF_CI_low),
                         VIF_CI_high = min(vif_table$VIF_CI_high),
                         SE_factor = min(vif_table$SE_factor),
                         Tolerance = min(vif_table$Tolerance),
                         Tolerance_CI_low = min(vif_table$Tolerance_CI_low),
                         Tolerance_CI_high = min(vif_table$Tolerance_CI_high))
    max_vif = data.frame(Term = "MAX",
                         VIF = max(vif_table$VIF),
                         VIF_CI_low = max(vif_table$VIF_CI_low),
                         VIF_CI_high = max(vif_table$VIF_CI_high),
                         SE_factor = max(vif_table$SE_factor),
                         Tolerance = max(vif_table$Tolerance),
                         Tolerance_CI_low = max(vif_table$Tolerance_CI_low),
                         Tolerance_CI_high = max(vif_table$Tolerance_CI_high))
    text_vif = data.frame(Term = "TEXT",
                          VIF = paste0("(VIF from ",
                                       sprintf('%.3f', min(vif_table$VIF)),
                                       " to ",
                                       sprintf('%.3f', max(vif_table$VIF)),
                                       ")"),
                          VIF_CI_low = "",
                          VIF_CI_high = "",
                          SE_factor = paste0("(SE factor from ",
                                             sprintf('%.3f', min(vif_table$SE_factor)),
                                             " to ",
                                             sprintf('%.3f', max(vif_table$SE_factor)),
                                             ")"),
                          Tolerance = paste0("(Tolerance from ",
                                             sprintf('%.3f', min(vif_table$Tolerance)),
                                             " to ",
                                             sprintf('%.3f', max(vif_table$Tolerance)),
                                             ")"),
                          Tolerance_CI_low = "",
                          Tolerance_CI_high = "")
    
    vif_table = vif_table %>% 
      rbind(min_vif,
            max_vif,
            text_vif)
    
    reg_table = sum_table %>% 
      mutate(Beta = paste0(sprintf('%.3f', std_beta),
                           ifelse(p_value < 0.001, "***",
                                  ifelse(p_value < 0.01, "**",
                                         ifelse(p_value < 0.05, "*", "")))),
             "95% CI" = paste0("[",
                               sprintf('%.3f', ci_lower),
                               ", ",
                               sprintf('%.3f', ci_upper),
                               "]")) %>% 
      select(variable, Beta, "95% CI")
    
    fit_table = data.frame(R2 = summary$r.squared,
                           R2_adj = summary$adj.r.squared,
                           F_stat = summary$fstatistic[[1]],
                           num_df = summary$fstatistic[[2]],
                           den_df = summary$fstatistic[[3]])
    
  }
  
  sheets = list("COEF" = sum_table,
                "FIT" = fit_table,
                "VIF" = vif_table,
                "TABLE" = reg_table)
  
  return(sheets)
  
}


# ANOVA WRAPPER -----------------------------------------------------------

anova_summary = function(data,
                         outcomes,
                         factors,
                         covariates = NULL) {
  
  # Create empty list variables
  main_list =   list()
  ph_list_f1 =  list()
  ph_list_f2 =  list()
  ph_list_f3 =  list()
  emm_list_f1 = list()
  emm_list_f2 = list()
  emm_list_f3 = list()
  
  # Loop for every outcome; Different based on interaction or single
  if(length(factors) > 1){
    
    for(x in outcomes){
      
      # If there are covariates, factor them into the formula.
      if(!is.null(covariates)){
        
        # Create formula
        formula <- as.formula(paste0(x, " ~ ", paste0(c(factors,
                                                        covariates,
                                                        paste0(factors, collapse = ":")),
                                                      collapse = " + ")))
        phformula <- as.formula(paste0(" ~ ", paste0(c(factors,
                                                       paste0(factors, collapse = ":")),
                                                     collapse = " + ")))
        
      } else {
        
        # Create formula
        formula <- as.formula(paste0(x, " ~ ", paste0(c(factors,
                                                        paste0(factors, collapse = ":")),
                                                      collapse = " + ")))
        phformula <- as.formula(paste0(" ~ ", paste0(c(factors,
                                                       paste0(factors, collapse = ":")),
                                                     collapse = " + ")))
        
      }
      
      
      # Run Model
      mod = jmv::ANOVA(
        formula = formula,
        data = data,
        effectSize = c("partEta", "eta", "omega"),
        modelTest = TRUE,
        postHoc = phformula,
        postHocCorr = c("tukey", "scheffe"),
        postHocES = "d",
        postHocEsCi = TRUE,
        emMeans = phformula,
        emmTables = T)
      
      
      # Get main statistics as dataframe
      maindf <- mod$main$asDF %>% 
        rownames_to_column(var = "outcome")
      
      # Get degrees of freedom for error term (residuals)
      df_n <- maindf %>% 
        filter(name == "Residuals") %>% 
        select(df) %>% 
        as.numeric()
      
      # Get main statistics
      main <- maindf %>% 
        mutate(outcome = x,
               text = ifelse(name == "Overall model",
                             paste0("(SS = ",
                                    sprintf('%.3f', ss),
                                    ", MS = ",
                                    sprintf('%.3f', ms),
                                    ", F(", df, ",", df_n,
                                    ") = ",
                                    sprintf('%.3f', `F`),
                                    ", ",
                                    ifelse(p < .001,
                                           "p < .001",
                                           paste0("p = ", sprintf('%.3f', p))),
                                    ")"),
                             ifelse(name == "Residuals",
                                    "",
                                    paste0("(SS = ",
                                           sprintf('%.3f', ss),
                                           ", MS = ",
                                           sprintf('%.3f', ms),
                                           ", F(", df, ",", df_n,
                                           ") = ",
                                           sprintf('%.3f', `F`),
                                           ", ",
                                           ifelse(p < .001,
                                                  "p < .001",
                                                  paste0("p = ", sprintf('%.3f', p))),
                                           ", etaSqP = ",
                                           sprintf('%.3f', etaSqP),
                                           ")"))))
      
      
      # Get main effects for first factor
      ph_factor1 = mod$postHoc[[1]]$asDF %>% 
        mutate(outcome = x,
               text_tukey = paste0("(t(",
                                   df,
                                   ") = ",
                                   sprintf('%.3f', t),
                                   ", ",
                                   ifelse(ptukey < .001,
                                          "p < .001",
                                          paste0("p = ", sprintf('%.3f', ptukey))),
                                   ", d = ",
                                   sprintf('%.3f', d),
                                   ")"),
               text_scheffe = paste0("(t(",
                                     df,
                                     ") = ",
                                     sprintf('%.3f', t),
                                     ", ",
                                     ifelse(ptukey < .001,
                                            "p < .001",
                                            paste0("p = ", sprintf('%.3f', pscheffe))),
                                     ", d = ",
                                     sprintf('%.3f', d),
                                     ")")) %>% 
        select(outcome, everything())
      
      
      
      # -------------------------- #
      # -- POST-HOC COMPARISONS -- #
      # -------------------------- #
      
      # Get main effects for second factor
      ph_factor2 = mod$postHoc[[2]]$asDF %>% 
        mutate(outcome = x,
               text_tukey = paste0("(t(",
                                   df,
                                   ") = ",
                                   sprintf('%.3f', t),
                                   ", ",
                                   ifelse(ptukey < .001,
                                          "p < .001",
                                          paste0("p = ", sprintf('%.3f', ptukey))),
                                   ", d = ",
                                   sprintf('%.3f', d),
                                   ")"),
               text_scheffe = paste0("(t(",
                                     df,
                                     ") = ",
                                     sprintf('%.3f', t),
                                     ", ",
                                     ifelse(ptukey < .001,
                                            "p < .001",
                                            paste0("p = ", sprintf('%.3f', pscheffe))),
                                     ", d = ",
                                     sprintf('%.3f', d),
                                     ")")) %>% 
        select(outcome, everything())
      
      
      # Get main effects for interaction
      ph_factor3 = mod$postHoc[[3]]$asDF %>% 
        mutate(outcome = x,
               text_tukey = paste0("(t(",
                                   df,
                                   ") = ",
                                   sprintf('%.3f', t),
                                   ", ",
                                   ifelse(ptukey < .001,
                                          "p < .001",
                                          paste0("p = ", sprintf('%.3f', ptukey))),
                                   ", d = ",
                                   sprintf('%.3f', d),
                                   ")"),
               text_scheffe = paste0("(t(",
                                     df,
                                     ") = ",
                                     sprintf('%.3f', t),
                                     ", ",
                                     ifelse(ptukey < .001,
                                            "p < .001",
                                            paste0("p = ", sprintf('%.3f', pscheffe))),
                                     ", d = ",
                                     sprintf('%.3f', d),
                                     ")")) %>% 
        select(outcome, everything())
      
      
      
      # ------------------------------ #
      # -- ESTIMATED MARGINAL MEANS -- #
      # ------------------------------ #
      
      # Estimated marginal means for factor 1
      emm_factor1 = mod$emm[[1]]$emmTable$asDF %>% 
        mutate(full_text = paste0("(M ± SE = ",
                                  sprintf("%.3f", mean),
                                  " ± ",
                                  sprintf("%.3f", se),
                                  ", 95% CI [",
                                  sprintf("%.3f", lower),
                                  ", ",
                                  sprintf("%.3f", upper),
                                  "])"),
               MSE = paste0("(M ± SE = ",
                            sprintf("%.3f", mean),
                            " ± ",
                            sprintf("%.3f", se),
                            ")"),
               M_SE = paste0("(M = ",
                             sprintf("%.3f", mean),
                             ", SE = ",
                             sprintf("%.3f", se),
                             ")"),
               outcome = x) %>% 
        select(outcome, everything())
      
      
      # Estimated marginal means for factor 2
      emm_factor2 = mod$emm[[2]]$emmTable$asDF %>% 
        mutate(full_text = paste0("(M ± SE = ",
                                  sprintf("%.3f", mean),
                                  " ± ",
                                  sprintf("%.3f", se),
                                  ", 95% CI [",
                                  sprintf("%.3f", lower),
                                  ", ",
                                  sprintf("%.3f", upper),
                                  "])"),
               MSE = paste0("(M ± SE = ",
                            sprintf("%.3f", mean),
                            " ± ",
                            sprintf("%.3f", se),
                            ")"),
               M_SE = paste0("(M = ",
                             sprintf("%.3f", mean),
                             ", SE = ",
                             sprintf("%.3f", se),
                             ")"),
               outcome = x) %>% 
        select(outcome, everything())
      
      
      # Estimated marginal means for interaction
      emm_factor3 = mod$emm[[3]]$emmTable$asDF %>% 
        mutate(full_text = paste0("(M ± SE = ",
                                  sprintf("%.3f", mean),
                                  " ± ",
                                  sprintf("%.3f", se),
                                  ", 95% CI [",
                                  sprintf("%.3f", lower),
                                  ", ",
                                  sprintf("%.3f", upper),
                                  "])"),
               MSE = paste0("(M ± SE = ",
                            sprintf("%.3f", mean),
                            " ± ",
                            sprintf("%.3f", se),
                            ")"),
               M_SE = paste0("(M = ",
                             sprintf("%.3f", mean),
                             ", SE = ",
                             sprintf("%.3f", se),
                             ")"),
               outcome = x) %>% 
        select(outcome, everything())
      
      
      # Save to list
      main_list[[x]]   <- main
      ph_list_f1[[x]]  <- ph_factor1
      ph_list_f2[[x]]  <- ph_factor2
      ph_list_f3[[x]]  <- ph_factor3
      emm_list_f1[[x]] <- emm_factor1
      emm_list_f2[[x]] <- emm_factor2
      emm_list_f3[[x]] <- emm_factor3
      
    }
    
  } else {
    
    for(x in outcomes){
      
      # If there are covariates, factor them into the formula.
      if(!is.null(covariates)){
        
        # Create formula
        formula <- as.formula(paste0(x, " ~ ", paste0(c(covariates,
                                                        paste0(factors,
                                                               collapse = ":")),
                                                      collapse = " + ")))
        phformula <- as.formula(paste0(" ~ ", paste0(c(paste0(factors,
                                                              collapse = ":")),
                                                     collapse = " + ")))
        
      } else {
        
        # Create formula
        formula <- as.formula(paste0(x, " ~ ", paste0(c(paste0(factors,
                                                               collapse = ":")),
                                                      collapse = " + ")))
        phformula <- as.formula(paste0(" ~ ", paste0(c(paste0(factors,
                                                              collapse = ":")),
                                                     collapse = " + ")))
        
      }
      
      # Run Model
      mod = jmv::ANOVA(
        formula = formula,
        data = data,
        effectSize = c("partEta", "eta", "omega"),
        modelTest = TRUE,
        postHoc = phformula,
        postHocCorr = c("tukey", "scheffe"),
        postHocES = "d",
        postHocEsCi = TRUE,
        emMeans = phformula,
        emmTables = T)
      
      
      # Get main statistics as dataframe
      maindf <- mod$main$asDF %>% 
        rownames_to_column(var = "outcome")
      
      # Get degrees of freedom for error term (residuals)
      df_n <- maindf %>% 
        filter(name == "Residuals") %>% 
        select(df) %>% 
        as.numeric()
      
      # Get main statistics
      main <- maindf %>% 
        mutate(outcome = x,
               text = ifelse(name == "Overall model",
                             paste0("(SS = ",
                                    sprintf('%.3f', ss),
                                    ", MS = ",
                                    sprintf('%.3f', ms),
                                    ", F(", df, ",", df_n,
                                    ") = ",
                                    sprintf('%.3f', `F`),
                                    ", ",
                                    ifelse(p < .001,
                                           "p < .001",
                                           paste0("p = ", sprintf('%.3f', p))),
                                    ")"),
                             ifelse(name == "Residuals",
                                    "",
                                    paste0("(SS = ",
                                           sprintf('%.3f', ss),
                                           ", MS = ",
                                           sprintf('%.3f', ms),
                                           ", F(", df, ",", df_n,
                                           ") = ",
                                           sprintf('%.3f', `F`),
                                           ", ",
                                           ifelse(p < .001,
                                                  "p < .001",
                                                  paste0("p = ", sprintf('%.3f', p))),
                                           ", etaSqP = ",
                                           sprintf('%.3f', etaSqP),
                                           ")"))))
      
      
      
      # -------------------------- #
      # -- POST-HOC COMPARISONS -- #
      # -------------------------- #
      
      
      # Get main effects for first factor
      ph_factor1 = mod$postHoc[[1]]$asDF %>% 
        mutate(outcome = x,
               text_tukey = paste0("(t(",
                                   df,
                                   ") = ",
                                   sprintf('%.3f', t),
                                   ", ",
                                   ifelse(ptukey < .001,
                                          "p < .001",
                                          paste0("p = ", sprintf('%.3f', ptukey))),
                                   ", d = ",
                                   sprintf('%.3f', d),
                                   ")"),
               text_scheffe = paste0("(t(",
                                     df,
                                     ") = ",
                                     sprintf('%.3f', t),
                                     ", ",
                                     ifelse(ptukey < .001,
                                            "p < .001",
                                            paste0("p = ", sprintf('%.3f', pscheffe))),
                                     ", d = ",
                                     sprintf('%.3f', d),
                                     ")")) %>% 
        select(outcome, everything())
      
      
      
      # ------------------------------ #
      # -- ESTIMATED MARGINAL MEANS -- #
      # ------------------------------ #
      
      # Estimated marginal means for factor 1
      emm_factor1 = mod$emm[[1]]$emmTable$asDF %>% 
        mutate(full_text = paste0("(M ± SE = ",
                                  sprintf("%.3f", mean),
                                  " ± ",
                                  sprintf("%.3f", se),
                                  ", 95% CI [",
                                  sprintf("%.3f", lower),
                                  ", ",
                                  sprintf("%.3f", upper),
                                  "])"),
               MSE = paste0("(M ± SE = ",
                            sprintf("%.3f", mean),
                            " ± ",
                            sprintf("%.3f", se),
                            ")"),
               M_SE = paste0("(M = ",
                             sprintf("%.3f", mean),
                             ", SE = ",
                             sprintf("%.3f", se),
                             ")"),
               outcome = x) %>% 
        select(outcome, everything())
      
      
      # Save to list
      main_list[[x]]   <- main
      ph_list_f1[[x]]  <- ph_factor1
      emm_list_f1[[x]] <- emm_factor1
      
      
    }
    
  }
  
  
  # Export depending on single factor or factorial design
  if(length(factors) > 1){
    
    # Reduce accordingly
    anova_main   <- purrr::reduce(main_list, rbind)
    
    anova_ph_f1  <- purrr::reduce(ph_list_f1, rbind)
    anova_ph_f2  <- purrr::reduce(ph_list_f2, rbind)
    anova_ph_f3  <- purrr::reduce(ph_list_f3, rbind)
    
    anova_emm_f1 <- purrr::reduce(emm_list_f1, rbind)
    anova_emm_f2 <- purrr::reduce(emm_list_f2, rbind)
    anova_emm_f3 <- purrr::reduce(emm_list_f3, rbind)
    
    # Create exportable list
    sheets = list("main" = anova_main,
                  "posthoc_f1" = anova_ph_f1,
                  "posthoc_f2" = anova_ph_f2,
                  "posthoc_int" = anova_ph_f3,
                  "emm_f1" = anova_emm_f1,
                  "emm_f2" = anova_emm_f2,
                  "emm_int" = anova_emm_f3)
    
  } else {
    
    # Reduce accordingly
    anova_main   <- purrr::reduce(main_list, rbind)
    
    anova_ph_f1  <- purrr::reduce(ph_list_f1, rbind)
    
    anova_emm_f1 <- purrr::reduce(emm_list_f1, rbind)
    
    # Create exportable list
    sheets = list("main" = anova_main,
                  "posthoc" = anova_ph_f1,
                  "emm" = anova_emm_f1)
    
  }
  
  return(sheets)
  
}


# ANOVA REPEATED MEASURES WRAPPER -----------------------------------------

#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param comparisons list of named vectors containing the repeated-measures variables

rm_anova = function(data = .,
                    comparisons){
  
  # Empty list to store results
  main_list = list()
  pairwise_list = list()
  emm_list = list()
  
  for(n in seq_along(comparisons)){
    
    # Define repeated-measures variables
    outcomes <- c(comparisons[[n]])
    
    # Create list of measure and cell parameters for jmv::anovaRM arguments
    rmCells <- list()
    for(i in seq_along(outcomes)){
      
      rmCell <- list(measure = outcomes[i],
                     cell = outcomes[i])
      
      rmCells[[i]] <- rmCell
      
    }
    
    # Run Repeated Measures ANOVA
    rm_anova <- jmv::anovaRM(
      data = data,
      rm = list(
        list(
          label = "RM Factor ",
          levels = outcomes)),
      rmCells = rmCells,
      effectSize = c("ges", "eta", "partEta"), # Default to grabbing all available effect sizes
      rmTerms = ~ `RM Factor `,
      spherTests = TRUE,
      spherCorr = c("none", "GG", "HF"), # Default to sphericity correction
      postHoc = list(
        "RM Factor "),
      postHocCorr = c("tukey", "scheffe"),
      emMeans = ~ `RM Factor `,
      emmPlots = FALSE,
      emmTables = TRUE)
    
    
    # Get dataframe table of main repeated measures for no sphericity correction
    none_rmTable <- rm_anova$rmTable$asDF %>% 
      select(ends_with("[none]")) %>% 
      magrittr::set_colnames(gsub('\\[none]',
                                  "",
                                  names(.)))
    
    # Get degrees of freedom for repeated measures
    none_df1 <- none_rmTable$df[1]
    # Get degrees of freedom for residuals
    none_df2 <- none_rmTable$df[2]
    
    # Create a copy-and-paste in-text version of the results
    none_rmTable <- none_rmTable %>% 
      mutate(text = ifelse(F == "", "", paste0("(ss = ",
                                               sprintf('%.3f', ss),
                                               ", MS = ",
                                               sprintf("%.3f", ms),
                                               ", F(",
                                               ifelse(none_df1 == floor(none_df1), none_df1, sprintf('%.3f', none_df1)),
                                               ", ",
                                               ifelse(none_df2 == floor(none_df2), none_df2, sprintf('%.3f', none_df2)),
                                               ") = ",
                                               sprintf("%.3f", F),
                                               ", p ",
                                               ifelse(p < .001, "< .001",
                                                      sprintf('%.3f', p)),
                                               ", partial Eta = ",
                                               sprintf('%.3f', partEta),
                                               ")")))
    
    
    # Get dataframe table of main repeated measures for Greenhouse-Geisser sphericity correction
    GG_rmTable <- rm_anova$rmTable$asDF %>% 
      select(ends_with("[GG]")) %>% 
      magrittr::set_colnames(gsub('\\[GG]',
                                  "",
                                  names(.)))
    
    # Get degrees of freedom for repeated measures
    GG_df1 <- GG_rmTable$df[1]
    # Get degrees of freedom for residuals
    GG_df2 <- GG_rmTable$df[2]
    
    # Create a copy-and-paste in-text version of the results
    GG_rmTable <- GG_rmTable %>% 
      mutate(text = ifelse(F == "", "", paste0("(ss = ",
                                               sprintf('%.3f', ss),
                                               ", MS = ",
                                               sprintf("%.3f", ms),
                                               ", F(",
                                               ifelse(GG_df1 == floor(GG_df1), GG_df1, sprintf('%.3f', GG_df1)),
                                               ", ",
                                               ifelse(GG_df2 == floor(GG_df2), GG_df2, sprintf('%.3f', GG_df2)),
                                               ") = ",
                                               sprintf("%.3f", F),
                                               ", p ",
                                               ifelse(p < .001, "< .001",
                                                      sprintf('%.3f', p)),
                                               ", partial Eta = ",
                                               sprintf('%.3f', partEta),
                                               ")")))
    
    
    
    # Get dataframe table of main repeated measures for Huynh-Feldt sphericity correction
    HF_rmTable <- rm_anova$rmTable$asDF %>% 
      select(ends_with("[HF]")) %>% 
      magrittr::set_colnames(gsub('\\[HF]',
                                  "",
                                  names(.)))
    
    # Get degrees of freedom for repeated measures
    HF_df1 <- HF_rmTable$df[1]
    # Get degrees of freedom for residuals
    HF_df2 <- HF_rmTable$df[2]
    
    # Create a copy-and-paste in-text version of the results
    HF_rmTable <- HF_rmTable %>% 
      mutate(text = ifelse(F == "", "", paste0("(ss = ",
                                               sprintf('%.3f', ss),
                                               ", MS = ",
                                               sprintf("%.3f", ms),
                                               ", F(",
                                               ifelse(HF_df1 == floor(HF_df1), HF_df1, sprintf('%.3f', HF_df1)),
                                               ", ",
                                               ifelse(HF_df2 == floor(HF_df2), HF_df2, sprintf('%.3f', HF_df2)),
                                               ") = ",
                                               sprintf("%.3f", F),
                                               ", p ",
                                               ifelse(p < .001, "< .001",
                                                      sprintf('%.3f', p)),
                                               ", partial Eta = ",
                                               sprintf('%.3f', partEta),
                                               ")")))
    
    
    # Collapse into one dataframe table
    rmTable <- rbind(none_rmTable,
                     GG_rmTable,
                     HF_rmTable)
    
    # Appendable notes
    rmNotes <- data.frame(name = "Note: SS = Sum of Squares, MS = Mean Squares, df = Degrees of Freedom, GES = Generalized Eta Squared, ETA = Eta Squared, partETA = Partial Eta Squared.")
    rmNotes[, setdiff(names(rmTable),
                      names(rmNotes))] <- NA
    
    # Append the notes
    rmTable <- rbind(rmTable,
                     rmNotes) %>% 
      rownames_to_column(var = "Comparison") %>% 
      mutate(Comparison = paste(outcomes, collapse = " :: "))
    
    
    # Get Estimated Marginal Means table with in-text reference to mean and SD
    rmEMM <- rm_anova$emm[[1]]$emmTable$asDF %>% 
      mutate(sd = se * sqrt(nrow(data)),
             text = paste0("(M = ",
                           sprintf('%.3f', mean),
                           ", SD = ",
                           sprintf('%.3f', sd),
                           ")"))
    
    
    # Get dataframe of pairwise comparisons
    pairwise = rm_anova$postHoc[[1]]$asDF %>%
      rename(Variable_1 = "RM Factor 1",
             Variable_2 = "RM Factor 2")
    
    # Get every pairwise combination of outcomes
    comb = combn(outcomes, 2) %>% 
      t() %>% 
      as.data.frame()
    
    # Define a function to apply to each element of the list
    pairwise_function <- function(p){
      
      d <- meta_d(data = data,
                  paired = T,
                  conditions = c(comb$V1[p], comb$V2[p])) %>% 
        separate(comparison, c("Variable_1", "Variable_2"),
                 sep = " - ")
      return(d)
      
    }
    
    # Use lapply to apply the function to each element of the list
    pairwise_d <- lapply(seq(nrow(comb)), pairwise_function)
    
    # Combine the results into a single dataframe
    pairwise_d <- do.call(rbind, pairwise_d)
    
    # Merge with the Anova pairwise results
    rmPairwise <- pairwise %>% 
      inner_join(pairwise_d) %>% 
      mutate(text = paste0("(d = ",
                           sprintf('%.3f', abs(d)),
                           ", ptukey ",
                           ifelse(ptukey < .001, "< .001",
                                  paste0("= ", sprintf('%.3f', ptukey))),
                           ")"))
    
    
    # Assign to main lists
    main_list[[n]] <- rmTable
    pairwise_list[[n]] <- rmPairwise
    emm_list[[n]] <- rmEMM
    
  }
  
  # Reduce/collapse into single dataframes
  main_table <- purrr::reduce(main_list,
                              rbind)
  
  pairwise_table <- purrr::reduce(pairwise_list,
                                  rbind)
  
  emm_table <- purrr::reduce(emm_list,
                             rbind)
  
  # Compile into single list
  rm_sheets <- list(Main = main_table,
                    Pairwise = pairwise_table,
                    EMM = emm_table)
  
  # Return results
  return(rm_sheets)
  
}


anova_rm_summary = function(df,
                            label = NULL,
                            rm_vars,
                            bs_var){
  
  levels = stringr::str_to_sentence(rm_vars)
  
  if(!is.null(label)){
    
    rm_label = label
    
  } else {
    
    rm_label = "RM_Var"  
    
  }
  
  jmv::anovaRM(
    data = df,
    rm = list(
      list(
        label= rm_label,
        levels= levels)),
    rmCells = list(
      list(
        measure="nepotism",
        cell="Nepotism"),
      list(
        measure="cronyism",
        cell="Cronyism")),
    bs = bs_var,
    effectSize = c("ges", "eta", "partEta"),
    rmTerms = ~ rm_label,
    bsTerms = ~ bs_var,
    spherTests = TRUE,
    spherCorr = c("none", "GG"),
    postHoc = list(
      "Favoritism",
      "ethn",
      c("Favoritism", "ethn")),
    postHocCorr = c("tukey", "scheffe"),
    emMeans = ~ Favoritism:ethn,
    emmTables = TRUE)
  
}

# T-TEST INDEPENDENT WRAPPER ----------------------------------------------

#' @param data dataframe
#' @param outcomes vector of variables
#' @param factor group or experimental condition

ttest_summary <- function(data = .,
                          outcomes,
                          factor){
  
  ttest_list = list()
  norm_list  = list()
  eqv_list   = list()
  desc_list  = list()
  
  student_list <- list()
  welch_list   <- list()
  mann_list    <- list()
  bayes_list   <- list()
  
  for(o in outcomes){
    
    formula <- as.formula(paste(o, "~", factor))
    
    # T-test
    ttest <- jmv::ttestIS(
      formula = formula,
      data = data,
      vars = vars(!!sym(o)),
      bf = TRUE,           # Bayes Factor
      welchs = TRUE,       # Welch's T-test
      mann = TRUE,         # Mann-whitney U Test
      norm = TRUE,         # Normality assumption
      eqv = TRUE,          # Homogeneity test
      meanDiff = TRUE,     # Get mean difference
      ci = TRUE,           # Confidence interval
      effectSize = TRUE,   # Effect size
      ciES = TRUE,         # Confidence interval of effect size
      desc = TRUE)         # Descriptives
    
    # Student's t-test
    stutest <- ttest$ttest$asDF %>% 
      select(ends_with("[stud]")) %>% 
      magrittr::set_colnames(.,
                             gsub("\\[stud\\]",
                                  "",
                                  names(.)))
    
    # Welch's t-test
    weltest <- ttest$ttest$asDF %>% 
      select(ends_with("[welc]")) %>% 
      magrittr::set_colnames(.,
                             gsub("\\[welc\\]",
                                  "",
                                  names(.)))
    
    # Mann-Whitney U-test
    mantest <- ttest$ttest$asDF %>% 
      select(ends_with("[mann]")) %>% 
      magrittr::set_colnames(.,
                             gsub("\\[mann\\]",
                                  "",
                                  names(.))) %>% 
      mutate(df = NA, .before = "p")
    
    # Bayesian t-test
    baytest <- ttest$ttest$asDF %>% 
      select(ends_with("[bf]")) %>% 
      magrittr::set_colnames(.,
                             gsub("\\[bf\\]",
                                  "",
                                  names(.)))
    
    # Generate table
    ttest_bind <- rbind(stutest,
                        weltest,
                        mantest,
                        baytest)
    
    # Append to t-test list
    ttest_list[[o]] <- ttest_bind
    
    
    
    # ------------------------------ #
    # -- T-TEST ASSUMPTION CHECKS -- #
    # ------------------------------ #
    
    # -- Normality assumption -- #
    
    normtest <- ttest$assum$norm$asDF
    
    # Append to norm list
    norm_list[[o]] <- normtest
    
    # -- Homogeneity assumption -- #
    
    eqvtest <- ttest$assum$eqv$asDF
    
    # Append to homogeneity list
    eqv_list[[o]] <- eqvtest
    
    
    
    # ------------------ #
    # -- DESCRIPTIVES -- #
    # ------------------ #
    
    # Get descriptives
    desc <- ttest$desc$asDF
    
    # Get descriptives for group 1
    g1desc <- desc %>% 
      select(dep,
             ends_with("[1]")) %>% 
      magrittr::set_colnames(.,
                             gsub("\\[1\\]",
                                  "",
                                  names(.)))
    
    # Get descriptives for group 2
    g2desc <- desc %>% 
      select(dep,
             ends_with("[2]")) %>%
      magrittr::set_colnames(.,
                             gsub("\\[2\\]",
                                  "",
                                  names(.)))
    
    # Append groups 1 and 2 descriptives
    desc_table <- rbind(g1desc,
                        g2desc) %>% 
      mutate(text = paste0("(M \u00B1 SD = ",
                           sprintf("%.3f", mean),
                           " \u00B1 ",
                           sprintf('%.3f', sd),
                           ")"))
    
    # Append to descriptives list
    desc_list[[o]] <- desc_table
    
    
    
    # ----------------------- #
    # -- REPORTABLE TABLES -- #
    # ----------------------- #
    
    # Get M and SD of groups
    g1_msd <- paste0(sprintf('%.3f', g1desc$mean),
                     " \u00B1 ",
                     sprintf('%.3f', g1desc$sd))
    
    g2_msd <- paste0(sprintf('%.3f', g2desc$mean),
                     " \u00B1 ",
                     sprintf('%.3f', g2desc$sd))
    
    # Get the group names
    g1 <- g1desc$group
    g2 <- g2desc$group
    
    msd <- data.frame(g1_msd, g2_msd) %>% 
      magrittr::set_colnames(.,
                             c(paste(g1, "M \u00B1 SD"),
                               paste(g2, "M \u00B1 SD")))
    
    
    # -- STUDENT'S T-TEST -- #
    
    student <- msd %>% 
      cbind(stutest %>% 
              rename(t = stat,
                     m_diff = md,
                     md_lower = cil,
                     md_upper = ciu,
                     d = es,
                     d_lower = ciles,
                     d_upper = ciues) %>% 
              select(var, t, df, m_diff, md_lower, md_upper, p, d, d_lower, d_upper,
                     -c(err, name, sed, esType))) %>% 
      select(var,
             everything()) %>% 
      mutate(text = paste0("(t(",
                           df,
                           ") = ",
                           sprintf('%.3f', t),
                           ", p ",
                           ifelse(p < .001, "< .001",
                                  paste0("= ", sprintf('%.3f', p))),
                           ", d = ",
                           sprintf('%.3f', d),
                           ")"))
    
    # Append
    student_list[[o]] <- student
    
    
    # -- WELCH'S T-TEST -- #
    
    welch <- msd %>% 
      cbind(weltest %>% 
              rename(t = stat,
                     m_diff = md,
                     md_lower = cil,
                     md_upper = ciu,
                     d = es,
                     d_lower = ciles,
                     d_upper = ciues) %>% 
              select(var, t, df, m_diff, md_lower, md_upper, p, d, d_lower, d_upper,
                     -c(err, name, sed, esType))) %>% 
      select(var,
             everything()) %>% 
      mutate(text = paste0("(t(",
                           sprintf('%.3f', df),
                           ") = ",
                           sprintf('%.3f', t),
                           ", p ",
                           ifelse(p < .001, "< .001",
                                  paste0("= ", sprintf('%.3f', p))),
                           ", d = ",
                           sprintf('%.3f', d),
                           ")"))
    
    # Append
    welch_list[[o]] <- welch
    
    
    # -- MANN-WHITNEY U TEST -- #
    
    mann <- msd %>% 
      cbind(mantest %>% 
              rename(U = stat,
                     m_diff = md,
                     md_lower = cil,
                     md_upper = ciu,
                     r_rankbiserial = es,
                     r_lower = ciles,
                     r_upper = ciues) %>% 
              select(var, U, m_diff, md_lower, md_upper, p, r_rankbiserial, r_lower, r_upper,
                     -c(err, name, sed, esType))) %>% 
      select(var,
             everything()) %>% 
      mutate(text = paste0("(U = ",
                           U,
                           ", p ",
                           ifelse(p < .001, "< .001",
                                  paste0("= ", sprintf('%.3f', p))),
                           ", rrb = ",
                           sprintf('%.3f', r_rankbiserial),
                           ")"))
    
    # Append
    mann_list[[o]] <- mann
    
    
    # -- BAYESIAN T-TEST -- #
    
    bayes <- msd %>% 
      cbind(baytest %>% 
              rename(BF10 = stat) %>% 
              select(var, BF10, err,
                     -c(md, cil, ciu, es, ciles, ciues, name, sed, esType))) %>% 
      select(var,
             everything()) %>% 
      mutate(text = paste0("(Bayes Factor10 = ",
                           ifelse(BF10 > 9999, formatC(BF10,
                                                       format = "e",
                                                       digits = 3),
                                  sprintf('%.3f', BF10)),
                           " \u00B1% ",
                           ifelse(err < .001, formatC(err,
                                                      format = "e",
                                                      digits = 3),
                                  sprintf('%.3f', err)),
                           ")")) %>% 
      rename("\u00B1%" = err)
    
    # Append
    bayes_list[[o]] <- bayes
    
    
  }
  
  # Reduce into working tables
  ttest_table <- purrr::reduce(ttest_list,
                               rbind)
  norm_table  <- purrr::reduce(norm_list,
                               rbind)
  eqv_table   <- purrr::reduce(eqv_list,
                               rbind)
  desc_table  <- purrr::reduce(desc_list,
                               rbind)
  
  student_table <- purrr::reduce(student_list,
                                 rbind)
  welch_table   <- purrr::reduce(welch_list,
                                 rbind)
  mann_table    <- purrr::reduce(mann_list,
                                 rbind)
  bayes_table   <- purrr::reduce(bayes_list,
                                 rbind)
  
  # Export sheet
  test_sheet <- list(student = student_table,
                     welch   = welch_table,
                     mann    = mann_table,
                     bayes   = bayes_table,
                     ttest   = ttest_table,
                     norm    = norm_table,
                     eqv     = eqv_table,
                     desc    = desc_table)
  
  return(test_sheet)
  
}

# T-TEST PAIRED WRAPPER ---------------------------------------------------

ttestps_summary = function(data = .,
                           outcomes){
  
  mod = jmv::ttestPS(
    data = data,
    pairs = list(
      list(
        i1= outcomes[1],
        i2= outcomes[2])),
    wilcoxon = TRUE,
    meanDiff = TRUE,
    ci = TRUE,
    effectSize = TRUE,
    ciES = TRUE,
    desc = TRUE)
  
  means_stud = mod$ttest$asDF %>% 
    select(ends_with("[stud]")) %>% 
    magrittr::set_colnames(gsub('\\[stud]', "", names(.))) %>% 
    mutate(text = paste0("(t(",
                         df,
                         ") = ",
                         sprintf('%.3f', stat),
                         ", p ",
                         ifelse(p < .001, "< .001",
                                sprintf('%.3f', p)),
                         ", d = ",
                         sprintf('%.3f', es),
                         ")"))
  
  means_wilc = mod$ttest$asDF %>% 
    select(ends_with("[wilc]")) %>% 
    magrittr::set_colnames(gsub('\\[wilc]', "", names(.))) %>% 
    mutate(text = paste0("(W_wilcoxon = ",
                         sprintf('%.3f', stat),
                         ", p ",
                         ifelse(p < .001, "< .001",
                                sprintf('%.3f', p)),
                         ", r_rb = ",
                         sprintf('%.3f', es),
                         ")"))
  
  means_mod = plyr::rbind.fill(means_stud, means_wilc)
  
  desc = mod$desc$asDF %>% 
    mutate(text_msd = paste0("(M ± SD = ",
                             sprintf('%.3f', m),
                             " ± ",
                             sprintf('%.3f', sd),
                             ")"),
           text_m_sd = paste0("(M = ",
                              sprintf('%.3f', m),
                              ", SD = ",
                              sprintf('%.3f', sd),
                              ")"))
  
  sheets = list("PS_Ttest" = means_mod,
                "Desc" = desc)
  
  return(sheets)
  
}



# PROPORTIONS TEST --------------------------------------------------------

#' PROPORTIONS TEST WRAPPER
#' 
#' @description
#' A simple wrapper that automates the running and extracting of common report
#' metrics for a chi-square independent proportions test.
#' Automatically applies holm p adjustment to multiple comparisons. Returns
#' model statistic and also pairwise comparison results. 
#' 
#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param group Group variable.
#' @param outcomes Vector of outcome variable names (strings)
#' 
#' @export
proportion_test <- function(data = .,
                            group,
                            outcomes){
  
  # Make empty lists
  stat_list <- list()
  prop_list <- list()
  pairwise_list <- list()
  
  # Get unique number of groups
  group_pairs <- combn(unique(data[[group]]), 2, simplify = FALSE)
  
  
  for(ovar in outcomes){
    
    # Create crosstab
    table <- table(data[[group]],
                   data[[ovar]])
    
    # Run proportions test
    result <- prop.test(table,
                        correct = T)
    
    # Get proportions test statistics dataframe
    stats <- data.frame(chisq = result$statistic,
                        df = result$parameter,
                        p = result$p.value,
                        method = result$method) %>% 
      mutate(text = paste0("X2(",
                           df,
                           ") = ",
                           sprintf('%.3f', chisq),
                           ", p ",
                           ifelse(p < .001,
                                  "< .001", paste0("= ", sprintf('%.3f', p))),
                           ")")) %>% 
      rownames_to_column(var = "Outcome") %>% 
      mutate(Outcome = ovar)
    
    # Get percentage proportion
    proportions <- result$estimate %>% 
      as.data.frame() %>% 
      rename(Proportion_grp1 = ".") %>% 
      rownames_to_column(var = "Outcome") %>% 
      mutate(Outcome = ovar,
             Proportion_grp2 = 1-Proportion_grp1)
    
    # Join together
    table <- table %>% 
      as.data.frame() %>% 
      pivot_wider(names_from = Var2, values_from = Freq) %>% 
      rename(Group = "Var1") %>% 
      cbind(proportions)
    
    
    # Load into list
    stat_list[[ovar]] <- stats
    prop_list[[ovar]] <- table
    
    
    
    # ------------------------- #
    # -- PAIRWISE COMPARISON -- #
    # ------------------------- #
    
    for(pair in seq_along(group_pairs)){
      
      # Define groups
      group1 <- group_pairs[[pair]][[1]]
      group2 <- group_pairs[[pair]][[2]]
      
      # Subset the data for the current pair of groups
      data_subset <- data %>% filter(!!sym(group) %in% c(group1, group2))
      
      # Create crosstab
      table_pair <- table(data_subset[[group]],
                          data_subset[[ovar]])
      
      # Run proportions test
      result_pair <- prop.test(table_pair,
                               correct = T)
      
      # Get proportions test statistics dataframe
      stats_pair <- data.frame(chisq = result_pair$statistic,
                               df = result_pair$parameter,
                               p = result_pair$p.value) %>% 
        rownames_to_column(var = "Outcome") %>% 
        mutate(Outcome = ovar,
               Group_1 = group_pairs[[pair]][[1]],
               Group_2 = group_pairs[[pair]][[2]]) %>% 
        select(Outcome, Group_1, Group_2, everything())
      
      # Save to list
      name <- paste0(ovar,
                     "_",
                     paste0(pair, collapse = "_"))
      pairwise_list[[name]] <- stats_pair
      
    }
    
  }
  
  # Reduce to working export dataframe
  stat_export <- purrr::reduce(stat_list, rbind)
  prop_export <- purrr::reduce(prop_list, rbind)
  pairwise_export <- purrr::reduce(pairwise_list, rbind)
  
  
  # Correct p value in pairwise
  corrected_p <- list()
  
  # Loop separately for every outcome
  for(ovar in outcomes){
    
    # Filter to specific outcome
    holm_df <- pairwise_export %>%
      filter(Outcome %in% ovar)
    
    # Apply Holm p adjustment
    holm_p <- p.adjust(holm_df$p,
                       method = "holm") %>% 
      as.data.frame() %>% 
      rename("p_holm" = ".")
    
    # Assign
    corrected_p[[ovar]] <- holm_p
    
  }
  
  # Reduce to single dataframe
  corrected_p_df <- purrr::reduce(corrected_p,
                                  rbind)
  
  # Merge with pairwise export dataframe and apply pasteable text
  pairwise_export <- pairwise_export %>% 
    cbind(corrected_p_df) %>% 
    mutate(text = paste0("X2(",
                         df,
                         ") = ",
                         sprintf('%.3f', chisq),
                         ", p_holm ",
                         ifelse(p < .001,
                                "< .001", paste0("= ", sprintf('%.3f', p_holm))),
                         ")"))
  
  # Aggregate to reportable sheet
  results_sheet <- list(stat = stat_export,
                        prop = prop_export,
                        pair = pairwise_export)
  
  # Return results
  return(results_sheet)
  
}



# COHEN'S D FOR META-ANALYSIS ---------------------------------------------

#' FUNCTION FOR DERIVING T-TEST COHEN'S D FOR META-ANALYSIS
#' 
#' @description
#' A quick function for calculating Cohen's D effect size for independent
#' samples and paired samples t-tests. 
#' 
#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param paired Default is FALSE to indicate that this is an independent
#' samples t-test. Specify as TRUE to indicate that this is a paired samples
#' t-test. If using paired samples t-test, then specify the columns containing
#' the within samples condition 1 and 2.
#' @param alpha A numeric value indicating the alpha value to determine
#' statistical significance. Default is 0.05.
#' @param conditions A required single string or vector of character strings
#' specifying the condition variable or within sample conditions. If `paired`
#' equals FALSE, for independent samples t-test, this parameter should only
#' contain one character string to reflect the column containing the condition
#' variable. If `paired` equals TRUE, for paired samples t-test, this parameter
#' should be a vector of two character strings to reflect the two columns
#' containing the first and second within samples exposure conditions, e.g.,
#' pre and post intervention measurements.
#' @param outcomes Required parameter for independent samples t-tests where
#' `paired = FALSE`. Specify the column as a single or vector of characters
#' containing the dependent variable(s).
#' 
#' @examples
#' # For independent samples t-test
#' meta_d(data, conditions = c("test_condition"), outcomes = c("dv1", "dv2"))
#' 
#' # For paired samples t-test
#' meta_d(data, conditions = c("pre_test", "post_test"))
#' 
#' @details
#' For paired samples t-test, the Cohen's D is calculated manually but the 
#' confidence interval is calculated using the `psych` package's `cohen.d.ci`
#' function. For most meta-analyses, usually the standard errors are needed.
#' Because standard error for within subjects Cohen's D is not very
#' straightforward, this is also manually calculated following the approach
#' used by the JASP team's paired t-test function. The standard error of D is
#' calculated as the following:
#' 
#' deqn{d_variance = ((1/n) + (d^2 / (2\*n))) \* (2\*(1 - cor(c1, c2)))}
#' In this formula,
#' d_variance is the variance of Cohen's D.
#' n is the sample size.
#' d is Cohen's D.
#' cor(c1, c2) is the correlation between conditions 1 and 2.
#' 
#' deqn{d_variance = sum(ns)/prod(ns) + (as.numeric(d)^2 / (2*sum(ni)))}
#' 
#' @export
#' 



#' FUNCTION FOR DERIVING T-TEST COHEN'S D FOR META-ANALYSIS - PAIRED T-TEST
#' 
#' @description
#' A quick function for calculating Cohen's D effect size for paired samples
#' t-tests. 
#' 
#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param alpha A numeric value indicating the alpha value to determine
#' statistical significance. Default is 0.05.
#' @param conditions A vector of two character strings to reflect the two
#' columns containing the first and second within samples exposure conditions,
#' e.g., pre and post intervention measurements.
#' 
#' @examples
#' meta_d(data, conditions = c("pre_test", "post_test"))
#' 
#' @details
#' Cohen's D is calculated as the mean of differences divided by the standard
#' deviation of differences. The confidence interval is calculated using the
#' `psych` package's `cohen.d.ci` function. For most meta-analyses, usually the
#' standard errors are needed. Because standard error for within subjects
#' Cohen's D is not very straightforward, this is also manually calculated
#' following the approach used by the JASP team's paired t-test function. The
#' standard error of D is calculated as the following:
#' 
#' deqn{d_variance = ((1/n) + (d^2 / (2\*n))) \* (2\*(1 - cor(c1, c2)))}
#' In this formula,
#' d_variance is the variance of Cohen's D.
#' n is the sample size.
#' d is Cohen's D.
#' cor(c1, c2) is the correlation between conditions 1 and 2.
#' 
#' @returns A dataframe containing the comparison, Cohen's d, standard error
#' of Cohen's d, and confidence interval of Cohen's d. 
#' 
#' @export
meta_d_pairedt <- function(data = .,
                           conditions,
                           alpha = .05){
  
  c1 <- data[[conditions[1]]]
  c2 <- data[[conditions[2]]]
  
  # Calculate differences between outcomes
  differences <- c1 - c2
  
  # Calculate Cohen's d
  d <- mean(differences) / sd(differences)
  
  # Get sample size
  n <- length(differences)
  
  # Get CI of Cohen's d
  ci_d <- psych::cohen.d.ci(d,
                            n1 = n,
                            alpha = alpha)
  
  # Get SE of Cohen's d
  # From Line 254 from JASP ttestpairedsamples.R
  # https://github.com/jasp-stats/jaspTTests/blob/master/R/ttestpairedsamples.R#L254-L257
  d_var <- ((1/n) + (as.numeric(d)^2 / (2*n))) * (2*(1-cor(c1,c2)))
  d_se <- sqrt(d_var)
  
  name <- paste0(conditions[1],
                 " - ",
                 conditions[2])
  
  output <- data.frame(comparison = name,
                       d = d,
                       d_se = d_se,
                       ci_lower = ci_d[1],
                       ci_upper = ci_d[3])
  
  return(output)
  
}


#' FUNCTION FOR DERIVING T-TEST COHEN'S D FOR META-ANALYSIS - IND SAMPLES T-TEST
#' 
#' @description
#' A quick function for calculating Cohen's D effect size for independent
#' samples t-tests. 
#' 
#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param alpha A numeric value indicating the alpha value to determine
#' statistical significance. Default is 0.05.
#' @param conditions A required single string or vector of character strings
#' specifying the condition variable. This parameter should only contain one
#' character string to reflect the column containing the condition variable. 
#' @param outcomes Required parameter. Specify the column as a single or vector
#' of characters containing the dependent variable(s).
#' 
#' @examples
#' meta_d(data, conditions = c("test_condition"), outcomes = c("dv1", "dv2"))
#' 
#' @details
#' For independent samples t-test, the Cohen's D is calculated manually but the 
#' confidence interval is calculated using the `psych` package's `cohen.d.ci`
#' function. For most meta-analyses, usually the standard errors are needed.
#' Because standard error for within subjects Cohen's D is not very
#' straightforward, this is also manually calculated following the approach
#' used by the JASP team's paired t-test function. The standard error of D is
#' calculated as the following:
#' 
#' deqn{d_variance = ((1/n) + (d^2 / (2\*n))) \* (2\*(1 - cor(c1, c2)))}
#' In this formula,
#' d_variance is the variance of Cohen's D.
#' n is the sample size.
#' d is Cohen's D.
#' cor(c1, c2) is the correlation between conditions 1 and 2.
#' 
#' deqn{d_variance = sum(ns)/prod(ns) + (as.numeric(d)^2 / (2*sum(ni)))}
#' 
#' @export
meta_d_indt <- function(data = .,
                        condition,
                        outcome,
                        alpha = .05){
  
  dv <- data[[outcomes]]
  grp <- data[[conditions]]
  
  
  # Grab levels of Condition
  l1 <- unique(data[[conditions]])[1]
  l2 <- unique(data[[conditions]])[2]
  
  # Get vectors of data between two groups
  c1 <- data[data[[conditions]] == l1,][[outcomes]]
  c2 <- data[data[[conditions]] == l2,][[outcomes]]
  
  
  # ----------------------------------------------------------- #
  # -- Step 1: Compute the difference between the two groups -- #
  # ----------------------------------------------------------- #
  
  mean_diff <- mean(c1) - mean(c2)
  
  
  
  # ----------------------------------------------------- #
  # -- Step 2: Calculate the pooled standard deviation -- #
  # ----------------------------------------------------- #
  
  # Calculate sample sizes
  n1 <- length(c1)
  n2 <- length(c2)
  
  # Standard deviation
  sd1 <- sd(c1)
  sd2 <- sd(c2)
  
  # Calculate pooled standard deviation
  # NOTE: Assume unequal variance
  pooled_sd <- sqrt(((n1 - 1) * var(c1) + (n2 - 1) * var(c2)) / (n1 + n2 - 2))
  
  
  
  # ----------------------------------------------------------------------------- #
  # -- Step 3: Divide the difference in means by the pooled standard deviation -- #
  # ----------------------------------------------------------------------------- #
  
  d <- mean_diff / pooled_sd
  
  
  
  
  # ----------------------------------------- #
  # -- Step 4: Compute confidence interval -- #
  # ----------------------------------------- #
  
  # Reference: see <https://stats.stackexchange.com/questions/495015/what-is-the-formula-for-the-standard-error-of-cohens-d>
  # Compute Scale factor
  n <- n1*n2 / (n1+n2)
  
  # Calculate the degrees of freedom
  df <- n1 + n2 - 2
  
  # A suitable grid 'ds' for a grid search based on...
  var_est <- n^(-1) + ((d^2) / 2 / df)
  ds <- seq(d - 4*sqrt(var_est),
            d + 4*sqrt(var_est),
            sqrt(var_est)/(10^4))
  
  # Boundaries based on limits of t-distribution with ncp parameter
  # For which the observed d will be in the 95% CI
  upper <- min(ds[which(pt(d*sqrt(n),df,ds*sqrt(n))<0.025)])
  lower <- max(ds[which(pt(d*sqrt(n),df,ds*sqrt(n))>0.975)])
  
  
  # Calculate SE of Cohen's d
  ns <- tapply(dv, grp, function(dv) length(na.omit(dv)))
  ni <- ns
  
  # https://github.com/jasp-stats/jaspTTests/blob/master/R/ttestindependentsamples.R#L347-L349
  d_var <- sum(ns)/prod(ns) + (as.numeric(d)^2 / (2*sum(ni)))
  
  # Introduction to Meta-Analysis. Michael Borenstein, L. V. Hedges,
  # J. P. T. Higgins and H. R. Rothstein (2009). Chapter 4, equation
  # (4.20/4.24).
  
  d_se <- sqrt(d_var)
  
  name <- paste0(unique(grp)[1],
                 " - ",
                 unique(grp)[2])
  
  output <- data.frame(comparison = name,
                       d = d,
                       d_se = d_se,
                       ci_lower = lower,
                       ci_upper = upper)
  
  return(output)
  
}


meta_d = function(data = .,
                  paired = F,
                  alpha = 0.05,
                  conditions,
                  outcomes = NULL){
  
  # Paired samples t-test
  
  if(paired == T){
    
    c1 <- data[[conditions[1]]]
    c2 <- data[[conditions[2]]]
    
    # Calculate differences between outcomes
    differences <- c1 - c2
    
    # Calculate Cohen's d
    d <- mean(differences) / sd(differences)
    
    # Get sample size
    n <- length(differences)
    
    # Get CI of Cohen's d
    ci_d <- psych::cohen.d.ci(d,
                              n1 = n,
                              alpha = alpha)
    
    # Get SE of Cohen's d
    # From Line 254 from JASP ttestpairedsamples.R
    # https://github.com/jasp-stats/jaspTTests/blob/master/R/ttestpairedsamples.R#L254-L257
    d_var <- ((1/n) + (as.numeric(d)^2 / (2*n))) * (2*(1-cor(c1,c2)))
    d_se <- sqrt(d_var)
    
    name <- paste0(conditions[1],
                   " - ",
                   conditions[2])
    
    output <- data.frame(comparison = name,
                         d = d,
                         d_se = d_se,
                         ci_lower = ci_d[1],
                         ci_upper = ci_d[3])
    
  }
  
  # Independent Samples t-test
  
  if(paired == F){
    
    dv <- data[[outcomes]]
    grp <- data[[conditions]]
    
    
    # Grab levels of Condition
    l1 <- unique(data[[conditions]])[1]
    l2 <- unique(data[[conditions]])[2]
    
    # Get vectors of data between two groups
    c1 <- data[data[[conditions]] == l1,][[outcomes]]
    c2 <- data[data[[conditions]] == l2,][[outcomes]]
    
    
    # ----------------------------------------------------------- #
    # -- Step 1: Compute the difference between the two groups -- #
    # ----------------------------------------------------------- #
    
    mean_diff <- mean(c1) - mean(c2)
    
    ttest <- t.test(c1, c2)
    
    
    
    # ----------------------------------------------------- #
    # -- Step 2: Calculate the pooled standard deviation -- #
    # ----------------------------------------------------- #
    
    # Calculate sample sizes
    n1 <- length(c1)
    n2 <- length(c2)
    
    # Standard deviation
    sd1 <- sd(c1)
    sd2 <- sd(c2)
    
    # Calculate pooled standard deviation
    # NOTE: Assume unequal variance
    pooled_sd <- sqrt(((n1 - 1) * var(c1) + (n2 - 1) * var(c2)) / (n1 + n2 - 2))
    
    
    
    # ----------------------------------------------------------------------------- #
    # -- Step 3: Divide the difference in means by the pooled standard deviation -- #
    # ----------------------------------------------------------------------------- #
    
    d <- mean_diff / pooled_sd
    
    
    
    
    # ----------------------------------------- #
    # -- Step 4: Compute confidence interval -- #
    # ----------------------------------------- #
    
    # Reference: see <https://stats.stackexchange.com/questions/495015/what-is-the-formula-for-the-standard-error-of-cohens-d>
    # Compute Scale factor
    n <- n1*n2 / (n1+n2)
    
    # Calculate the degrees of freedom
    df <- n1 + n2 - 2
    
    # A suitable grid 'ds' for a grid search based on...
    var_est <- n^(-1) + ((d^2) / 2 / df)
    ds <- seq(d - 4*sqrt(var_est),
              d + 4*sqrt(var_est),
              sqrt(var_est)/(10^4))
    
    # Boundaries based on limits of t-distribution with ncp parameter
    # For which the observed d will be in the 95% CI
    upper <- min(ds[which(pt(d*sqrt(n),df,ds*sqrt(n))<0.025)])
    lower <- max(ds[which(pt(d*sqrt(n),df,ds*sqrt(n))>0.975)])
    
    
    # Calculate SE of Cohen's d
    ns <- tapply(dv, grp, function(dv) length(na.omit(dv)))
    ni <- ns
    
    # https://github.com/jasp-stats/jaspTTests/blob/master/R/ttestindependentsamples.R#L347-L349
    d_var <- sum(ns)/prod(ns) + (as.numeric(d)^2 / (2*sum(ni)))
    
    # Introduction to Meta-Analysis. Michael Borenstein, L. V. Hedges,
    # J. P. T. Higgins and H. R. Rothstein (2009). Chapter 4, equation
    # (4.20/4.24).
    
    d_se <- sqrt(d_var)
    
    name <- paste0(unique(grp)[1],
                   " - ",
                   unique(grp)[2])
    
    output <- data.frame(comparison = name,
                         d = d,
                         d_se = d_se,
                         ci_lower = lower,
                         ci_upper = upper)
  }
  
  return(output)
  
}


# CROSSTABS ---------------------------------------------------------------

#' FREQUENCY TABLE
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



# FREQUENCIES -------------------------------------------------------------

#' FREQUENCY TABLE
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


# TRAIN, VALIDATION, TEST DATA SPLITTER -----------------------------------

#' SPLIT DATA INTO TRAIN, VALIDATION, AND TEST SETS
#' 
#' @description
#' Automated wrapper function to split a dataframe into training, validation,
#' and testing sets for machine learning or cross-validation. Validation and
#' test sets are equally split from the remaining dataframe after subsetting
#' the training set. Training, validation, and test sets are saved as
#' dataframes in a returned list.
#' 
#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param group If there are subgroups in the dataset that also need to be
#' equally split into training, validation, or test sets, specify the column
#' name reflecting the group as a character. Default is NULL
#' @param fraction Percentage of the dataset that should be subset as the
#' training set. Default is 0.6 to indicate that 60% of the dataset will be
#' randomly sampled to comprise the training set.
#' @param validation Default is FALSE. If there is a reason to have a
#' validation set in addition to a test set, specify this as TRUE. The
#' remaining data left from partitioning the training set are then randomly
#' and equally split into the validation and test sets. 
#' 
#' @return A list of 2 or 3 dataframes representing the training, validation,
#' and test sets.
#' 
#' @export
test_split <- function(data = .,
                       group = NULL,
                       proportion = .6,
                       validation = FALSE){
  
  # If no groups exist, then split on total data
  if(is.null(group)){
    
    if(isTRUE(validation)){
      
      # Get training data
      train <- data %>% 
        sample_frac(proportion)
      
      # Get validation data
      validation <- data %>% 
        anti_join(train) %>% 
        sample_frac(.5)
      
      # Get test data
      test <- data %>% 
        anti_join(train) %>% 
        anti_join(validation)
      
    } else {
      
      # Get training data
      train = data %>%
        sample_frac(proportion)
      
      # Get test data
      test = data %>% 
        anti_join(train)
      
    }
    
    # If separate groups exist, then split by group:
  } else {
    
    if(isTRUE(validation)){
      
      # Get training data
      train = data %>%
        group_by(!!sym(group)) %>% 
        sample_frac(proportion)
      
      # Get validation data
      validation = data %>% 
        group_by(!!sym(group)) %>% 
        anti_join(train) %>% 
        sample_frac(.5)
      
      # Get test data
      test = data %>% 
        group_by(!!sym(group)) %>% 
        anti_join(train) %>% 
        anti_join(validation)
      
    } else {
      
      # Get training data
      train = data %>%
        group_by(!!sym(group)) %>% 
        sample_frac(proportion)
      
      # Get test data
      test = data %>% 
        group_by(!!sym(group)) %>% 
        anti_join(train)
      
    }
    
  }
  
  # If validation is TRUE
  if(isTRUE(validation)){
    
    ml_list = list(train = train,
                   valid = validation,
                   test = test)
    
  } else {
    
    ml_list = list(train = train,
                   test = test)
    
  }
  
  # Return results as list
  return(ml_list)
  
}


# KNN IMPUTATION WRAPPER --------------------------------------------------

#' KNN IMPUTATION WRAPPER
#' 
#' @description
#' A function to automate k nearest neighbor (KNN) imputation for missing
#' data. Imputing the dataframe with both the predictor and outcome variables
#' can inflate the association between the two batteries because the algorithm
#' is using information from the predictors to impute the outcome, and vice
#' versa. To prevent this, the outcome and predictor variables should be
#' imputed independently, or with a third, unrelated battery of variables.
#' This function automates this process and returns an imputed dataframe where
#' the outcome and predictor variables are imputed independently. The KNN
#' imputation process is done through the `VIM` package's `kNN()` function.
#' 
#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param outcomes A required vector of column names representing the 
#' outcome variables.
#' @param demographics An optional vector of column names representing the
#' demographic variables. Specify this argument to leverage demographic or
#' other variables to impute missing data. If any of the demographic variables
#' data are also missing, then the demographic variables are imputed twice,
#' once with the predictor variables and once with the outcome variables. Only
#' the imputed data for the predictor variables are kept in the returned
#' dataset. Default is NULL.
#' @param id A required character string indicating the response ID variable
#' to be used for merging imputed outcome and predictor variables.
#' @param k The number of nearest neighbors to use when imputing data. Default
#' is 10.
#' 
#' @return An imputed dataframe.
#' 
#' @references Alexander Kowarik, Matthias Templ (2016). Imputation with the R
#' Package VIM. _Journal of Statistical Software_, *74*(7), 1-16.
#' doi:10.18637/jss.v074.i07
#' 
#' @export
knn_impute <- function(data = .,
                       outcomes,
                       demographics = NULL,
                       id,
                       k = 10){
  
  # If demographic variables are specified, include them in partitioning the
  # outcome variables. Otherwise, only grab the outcome variables. 
  if(!is.null(demographics)){
    
    outcome <- data %>% 
      dplyr::select(all_of(id),
                    all_of(demographics),
                    all_of(outcomes))
    
    subdata <- data %>% 
      dplyr::select(-all_of(outcomes))
    
  } else {
    
    outcome <- data %>% 
      dplyr::select(all_of(id),
                    all_of(outcomes))
    
    subdata <- data %>% 
      dplyr::select(-all_of(outcomes))
    
  }
  
  # Impute data
  knn_sub <- VIM::kNN(subdata, k = k, imp_var = F)
  knn_out <- VIM::kNN(outcome, k = k, imp_var = F)
  
  # Merge with the id variable
  knn_data <- knn_sub %>% 
    inner_join(knn_out %>% 
                 dplyr::select(all_of(outcomes), id),
               by = id)
  
  # Return imputed dataset
  return(knn_data)
  
}


