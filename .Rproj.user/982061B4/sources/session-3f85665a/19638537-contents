

# ----------------------- #
# WRAPPER FUNCTIONS INDEX #
# ----------------------- #


# source("C:\\Users\\imhoh\\OneDrive\\Academia\\Projects\\RScript_Wrap_v1.1.R")

#' @author Hohjin Im, PhD
#' Version 0.1.0
#' Beta version.



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


