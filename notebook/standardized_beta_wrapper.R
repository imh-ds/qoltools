
#' STANDARDIZED COEFFICIENT EXTRACTOR
#' 
#' @description
#' A wrapper function that extracts the standardized coefficients of regression
#' models defined through \code{lm}, \code{lmer}, and \code{glmer}.
#' 
#' @param model The regression object.
#' 
#' 
#' 
std_coef <- function(model) {
  
  # If regression model is logistic multilevel
  if(class(model) == "glmerMod") {
    
    summary = summary(model)
    
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
    
    vif_table = performance::check_collinearity(model) %>% 
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
  
  # If regression model is multilevel
  if(class(model) == "lmerModLmerTest") {
    
    lmer_mod = summary(model)
    
    estimate = lmer_mod$coefficients %>% 
      as.data.frame() %>% 
      select(Estimate) %>% 
      rename(est = Estimate)
    
    sd_outcome = sd(lme4::getME(model,"y"))
    sd_predictors = apply(lme4::getME(model,"X"), 2, sd)
    
    std_beta = lme4::fixef(model)*sd_predictors/sd_outcome
    
    se_fixedeffect = coef(summary(model))[,"Std. Error"]
    
    se = se_fixedeffect*sd_predictors/sd_outcome
    
    names = names(std_beta)
    
    summary = summary(model)
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
    
    vif_table = performance::check_collinearity(model) %>% 
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
  
  # If regression model is basic regression
  if(class(model) == "lm") {
    
    print("Detected Model: Linear Model (lm)")
    
    # Get model summary and coefficients
    model_summary <- summary(model)
    model_coefficients <- model[["coefficients"]]
    
    # Calculate standard deviations of outcome and predictors
    sd_outcome <- sd(model[["model"]][[1]])
    sd_predictors <- apply(model$model[-1] %>% 
                             mutate(`(Intercept)` = 1) %>% 
                             select(`(Intercept)`, everything()), 2, sd)
    
    # Calculate standard errors and standardized betas
    se_fixedeffect <- model_summary[["coefficients"]][,"Std. Error"]
    std_beta <- model_coefficients * sd_predictors / sd_outcome
    se <- se_fixedeffect * sd_predictors / sd_outcome
    
    # Calculate confidence intervals
    std_ci <- confint(model) * (sd_predictors / sd_outcome)
    
    # Prepare data for summary table
    coefficients <- as.data.frame(model_summary[["coefficients"]])
    coefficients[["df"]] <- model_summary[["df"]][2]
    
    sum_table <- data.frame(
      est = model_coefficients,
      std_est = std_beta,
      std_se = se,
      ci_lower = std_ci[, "2.5 %"],
      ci_upper = std_ci[, "97.5 %"],
      df = coefficients[["df"]],
      t_stat = coefficients[["t value"]],
      p_value = coefficients[["Pr(>|t|)"]]
    )
    
    # Add text column to summary table
    sum_table[["text"]] <- sprintf(
      "(β = %.3f, t(%.f) = %.3f, 95%% CI [%.3f, %.3f], p %s)",
      sum_table[["std_est"]],
      sum_table[["df"]],
      sum_table[["t_stat"]],
      sum_table[["ci_lower"]],
      sum_table[["ci_upper"]],
      ifelse(sum_table[["p_value"]] < .001, "< .001", sprintf("= %.3f", sum_table[["p_value"]]))
    )
    
    # Convert row names to a column
    sum_table <- tibble::rownames_to_column(sum_table, var = "variable")
    
    
    # Calculate VIF table
    vif_table <- performance::check_collinearity(model) %>%
      as.data.frame()
    
    # Calculate min and max values
    min_vif = data.frame(Term = "MIN",
                         VIF = min(vif_table[["VIF"]]),
                         VIF_CI_low = min(vif_table[["VIF_CI_low"]]),
                         VIF_CI_high = min(vif_table[["VIF_CI_high"]]),
                         SE_factor = min(vif_table[["SE_factor"]]),
                         Tolerance = min(vif_table[["Tolerance"]]),
                         Tolerance_CI_low = min(vif_table[["Tolerance_CI_low"]]),
                         Tolerance_CI_high = min(vif_table[["Tolerance_CI_high"]]))
    max_vif = data.frame(Term = "MAX",
                         VIF = max(vif_table[["VIF"]]),
                         VIF_CI_low = max(vif_table[["VIF_CI_low"]]),
                         VIF_CI_high = max(vif_table[["VIF_CI_high"]]),
                         SE_factor = max(vif_table[["SE_factor"]]),
                         Tolerance = max(vif_table[["Tolerance"]]),
                         Tolerance_CI_low = max(vif_table[["Tolerance_CI_low"]]),
                         Tolerance_CI_high = max(vif_table[["Tolerance_CI_high"]]))
    
    # Generate text for VIF, SE factor, and Tolerance
    text_values <- c(
      Term = "TEXT",
      VIF = sprintf("(VIF from %.3f to %.3f)", min_vif[["VIF"]], max_vif[["VIF"]]),
      VIF_CI_low = "",
      VIF_CI_high = "",
      SE_factor = sprintf("(SE factor from %.3f to %.3f)", min_vif[["SE_factor"]], max_vif[["SE_factor"]]),
      Tolerance = sprintf("(Tolerance from %.3f to %.3f)", min_vif[["Tolerance"]], max_vif[["Tolerance"]]),
      Tolerance_CI_low = "",
      Tolerance_CI_high = ""
    )
    
    # Add min, max, and text rows to VIF table
    vif_table <- rbind(vif_table, min_vif, max_vif, text_values)
    
    
    
    # ---------------------- #
    # -- REGRESSION TABLE -- #
    # ---------------------- #
    
    # Create wide regression table
    reg_table <- sum_table %>% 
      mutate(Beta = paste0(sprintf('%.3f', std_beta),
                           ifelse(p_value < 0.001, "***",
                                  ifelse(p_value < 0.01, "**",
                                         ifelse(p_value < 0.05, "*", "")))),
             "95% CI" = paste0("(",
                               sprintf('%.3f', ci_lower),
                               ", ",
                               sprintf('%.3f', ci_upper),
                               ")")) %>% 
      select(variable, Beta, "95% CI")
    
    # Extract regression fits
    fit_table <- data.frame(R2 = summary[["r.squared"]],
                            R2_adj = summary[["adj.r.squared"]],
                            F_stat = summary[["fstatistic"]][[1]],
                            num_df = summary[["fstatistic"]][[2]],
                            den_df = summary[["fstatistic"]][[3]])
    
  }
  
  sheets = list("coef" = sum_table,
                "fit" = fit_table,
                "vif" = vif_table,
                "table" = reg_table)
  
  return(sheets)
  
}


data <- carData::WVS

multimodel <- lmerTest::lmer(as.numeric(poverty) ~ degree + age + gender + (1|country),
                             data = data)
std_coef(multimodel)

multilogit <- 