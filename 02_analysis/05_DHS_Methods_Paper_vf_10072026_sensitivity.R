# EBF Prevalence - Methods Paper 
# Run with the working directory set to this folder (02_analysis/).
# Sensitivity Analysis

####################################################################
# Load data for DHS surveys 2000-2022

library(haven)
DHS_methods_analysis_2000_2022 <- read_dta("../data/restricted/DHS_methods_analysis_2000-2022.dta")
# View(DHS_methods_analysis_2000_2022)  # interactive viewer; not needed to reproduce results

####################################################################

# UNICEF EBF definition so coding missing as no EBF instead of yes EBF - under 6 months

library(dplyr)
library(survey)
library(broom)
library(openxlsx)

df <- DHS_methods_analysis_2000_2022 %>%
  mutate(
    v005 = v005 / 1000000,
    agemonths_sq = agemonths^2,
    agemonths_cub = agemonths^3,
    agemonths_quar = agemonths^4,
    survey_id = as.numeric(as.factor(survey))
  )

results_list <- list()
unique_surveys <- unique(df$survey_id)

invlogit <- function(x) exp(x) / (1 + exp(x))

for (sid in unique_surveys) {
  cat("Running model for survey", sid, "\n")
  
  # Step 1: restrict to <12 months
  df_survey <- df %>% filter(survey_id == sid, agemonths < 12)
  
  if (nrow(df_survey) == 0) {
    cat("No data for survey", sid, "- skipping\n")
    next
  }
  
  dstrata <- svydesign(ids = ~1, weights = ~v005, data = df_survey, nest = TRUE)
  
  fit <- tryCatch({
    svyglm(
      EBF_strict ~ agemonths + agemonths_sq + agemonths_cub + agemonths_quar,
      design = dstrata,
      family = quasibinomial()
    )
  }, error = function(e) {
    cat("Model failed for survey", sid, ":", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(fit)) next
  
  if (any(!is.finite(coef(fit))) || any(is.na(coef(fit)))) {
    cat("Non-converged model for survey", sid, "- skipping\n")
    next
  }
  
  # Step 2: restrict to <6 months when building covariate averages
  df_under6 <- df_survey %>% filter(agemonths < 6)
  if (nrow(df_under6) == 0) {
    cat("No <6 months data for survey", sid, "- skipping\n")
    next
  }
  
  # Handle complete separation / zero-event surveys
  n_events <- sum(df_under6$EBF_strict == 1, na.rm = TRUE)
  
  if (sum(df_under6$EBF_strict == 1, na.rm = TRUE) == 0) {
    cat("Complete separation (0% prevalence) for survey", sid, "- applying correction\n")
    n <- nrow(df_under6)
    prev_corrected <- 0.5 / n        # Haldane-Anscombe correction
    ci_upper <- qbeta(0.975, 1, n)   # exact binomial upper bound
    fit_link <- qlogis(prev_corrected)
    se_link <- (qlogis(ci_upper) - qlogis(prev_corrected)) / 1.96
    
  } else {
    mean_vals <- colMeans(
      df_under6 %>% select(agemonths, agemonths_sq, agemonths_cub, agemonths_quar),
      na.rm = TRUE
    )
    newdata <- as.data.frame(t(mean_vals))
    
    # Prediction on link scale
    X_new <- model.matrix(~ agemonths + agemonths_sq + agemonths_cub + agemonths_quar, newdata)
    beta <- coef(fit)
    V <- vcov(fit)
    
    # Skip if variance-covariance matrix is unusable
    if (any(!is.finite(V)) || any(is.na(V))) {
      cat("Invalid variance-covariance matrix for survey", sid, "- skipping\n")
      next
    }
    
    fit_link <- as.numeric(X_new %*% beta)
    se_link <- sqrt(as.numeric(X_new %*% V %*% t(X_new)))
  }
  
  survey_name <- as.character(df_survey$survey[1])
  
  results_list[[length(results_list) + 1]] <- data.frame(
    survey = survey_name,
    survey_id = sid,
    n_under6 = nrow(df_under6),
    n_events_under6 = n_events,
    se_link_UNICEF_under6 = se_link,
    fit_link_UNICEF_under6 = fit_link
  )
}

prevalence_UNICEF_under6 <- bind_rows(results_list) %>%
  arrange(survey)

print(prevalence_UNICEF_under6)

# Average prevalence across surveys with combined CI
summary_average_UNICEF_under6 <- prevalence_UNICEF_under6 %>%
  summarise(
    K = sum(!is.na(fit_link_UNICEF_under6)),
    mean_link = mean(fit_link_UNICEF_under6, na.rm = TRUE),
    sd_between = sd(fit_link_UNICEF_under6, na.rm = TRUE),
    se_between = sd_between / sqrt(K),
    se_within = sqrt(sum(se_link_UNICEF_under6^2, na.rm = TRUE) / (K^2)),
    se_combined = sqrt(se_between^2 + se_within^2),
    ci_lower_link = mean_link - 1.96 * se_combined,
    ci_upper_link = mean_link + 1.96 * se_combined,
    # Back-transform to probability scale at the end
    mean_prev = invlogit(mean_link),
    ci_lower = invlogit(ci_lower_link),
    ci_upper = invlogit(ci_upper_link)
  )

print(summary_average_UNICEF_under6)

cat(
  "\nAverage prevalence (mean_prediction_UNICEF_under6):",
  sprintf("%.3f", summary_average_UNICEF_under6$mean_prev),
  "(95% CI:",
  sprintf("%.3f", summary_average_UNICEF_under6$ci_lower), "-",
  sprintf("%.3f", summary_average_UNICEF_under6$ci_upper), ")\n"
)

####################################################################

# DHS EBF definition so coding missing as no EBF instead of yes EBF - under 6 months

library(dplyr)
library(survey)
library(broom)
library(openxlsx)

df_DHS_under6 <- DHS_methods_analysis_2000_2022 %>%
  mutate(
    v005 = v005 / 1000000,
    agemonths_sq = agemonths^2,
    agemonths_cub = agemonths^3,
    agemonths_quar = agemonths^4,
    survey_id = as.numeric(as.factor(survey))
  )

results_list_DHS_under6 <- list()
unique_surveys_DHS_under6 <- unique(df_DHS_under6$survey_id)

invlogit <- function(x) exp(x) / (1 + exp(x))

for (sid in unique_surveys_DHS_under6) {
  cat("Running model for survey", sid, "\n")
  
  # Step 1: restrict to <12 months
  df_survey <- df_DHS_under6 %>% filter(survey_id == sid, agemonths < 12)
  
  if (nrow(df_survey) == 0) {
    cat("No data for survey", sid, "- skipping\n")
    next
  }
  
  dstrata <- svydesign(ids = ~1, weights = ~v005, data = df_survey, nest = TRUE)
  
  fit <- tryCatch({
    svyglm(
      EBF_DHS ~ agemonths + agemonths_sq + agemonths_cub + agemonths_quar,
      design = dstrata,
      family = quasibinomial()
    )
  }, error = function(e) {
    cat("Model failed for survey", sid, ":", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(fit)) next
  
  # Optional: skip obviously non-converged / unusable models
  if (any(!is.finite(coef(fit))) || any(is.na(coef(fit)))) {
    cat("Non-converged model for survey", sid, "- skipping\n")
    next
  }
  
  # Step 2: restrict to <6 months when building covariate averages
  df_under6 <- df_survey %>% filter(agemonths < 6)
  if (nrow(df_under6) == 0) {
    cat("No <6 months data for survey", sid, "- skipping\n")
    next
  }
  
  n_events <- sum(df_under6$EBF_DHS == 1, na.rm = TRUE)
  
  if (sum(df_under6$EBF_DHS == 1, na.rm = TRUE) == 0) {
    cat("Complete separation (0% prevalence) for survey", sid, "- applying correction\n")
    n <- nrow(df_under6)
    prev_corrected <- 0.5 / n        # Haldane-Anscombe correction
    ci_upper <- qbeta(0.975, 1, n)   # exact binomial upper bound
    fit_link <- qlogis(prev_corrected)
    se_link <- (qlogis(ci_upper) - qlogis(prev_corrected)) / 1.96
    
    # The Haldane-Anscombe correction (0.5/n) is a standard epidemiological approach for zero-cell proportions —
    # it adds 0.5 to the numerator, giving a small non-zero prevalence that scales with sample size,
    # is numerically stable, and has a principled justification rather than being arbitrary.
    # Larger surveys get a smaller correction, which makes intuitive sense.
    
  } else {
    mean_vals <- colMeans(
      df_under6 %>% select(agemonths, agemonths_sq, agemonths_cub, agemonths_quar),
      na.rm = TRUE
    )
    newdata <- as.data.frame(t(mean_vals))
    X_new <- model.matrix(~ agemonths + agemonths_sq + agemonths_cub + agemonths_quar, newdata)
    beta <- coef(fit)
    V <- vcov(fit)
    
    # Skip if variance-covariance matrix is unusable
    if (any(!is.finite(V)) || any(is.na(V))) {
      cat("Invalid variance-covariance matrix for survey", sid, "- skipping\n")
      next
    }
    
    fit_link <- as.numeric(X_new %*% beta)
    se_link <- sqrt(as.numeric(X_new %*% V %*% t(X_new)))
  }
  
  survey_name <- as.character(df_survey$survey[1])
  
  results_list_DHS_under6[[length(results_list_DHS_under6) + 1]] <- data.frame(
    survey = survey_name,
    survey_id = sid,
    n_under6 = nrow(df_under6),
    n_events_under6 = n_events,
    se_link_DHS_under6 = se_link,
    fit_link_DHS_under6 = fit_link
  )
}

prevalence_DHS_under6 <- bind_rows(results_list_DHS_under6) %>%
  arrange(survey)

print(prevalence_DHS_under6)

# Average prevalence across surveys with combined CI
summary_average_DHS_under6 <- prevalence_DHS_under6 %>%
  summarise(
    K = sum(!is.na(fit_link_DHS_under6)),
    mean_prev_link = mean(fit_link_DHS_under6, na.rm = TRUE),
    sd_between = sd(fit_link_DHS_under6, na.rm = TRUE),
    se_between = sd_between / sqrt(K),
    se_within = sqrt(sum(se_link_DHS_under6^2, na.rm = TRUE) / (K^2)),
    se_combined = sqrt(se_between^2 + se_within^2),
    ci_lower_link = mean_prev_link - 1.96 * se_combined,
    ci_upper_link = mean_prev_link + 1.96 * se_combined,
    mean_prev = invlogit(mean_prev_link),
    ci_lower = invlogit(ci_lower_link),
    ci_upper = invlogit(ci_upper_link)
  )

print(summary_average_DHS_under6)

cat(
  "\nAverage prevalence (mean_prediction_DHS_under6):",
  sprintf("%.3f", summary_average_DHS_under6$mean_prev),
  "(95% CI:",
  sprintf("%.3f", summary_average_DHS_under6$ci_lower), "-",
  sprintf("%.3f", summary_average_DHS_under6$ci_upper), ")\n"
)

####################################################################

# DHS complete case EBF definition - under 6 months

library(dplyr)
library(survey)
library(broom)
library(openxlsx)

DHS.df <- DHS_methods_analysis_2000_2022 %>%
  mutate(
    v005 = v005 / 1000000,
    agemonths_sq = agemonths^2,
    agemonths_cub = agemonths^3,
    agemonths_quar = agemonths^4
  )

# Complete case filter: drop missing feeding variables
df_DHS_CC_under6 <- DHS.df %>%
  filter(!is.na(food) & !is.na(liquid) & !is.na(BF)) %>%
  mutate(
    EBF_DHS = factor(case_when(
      BF == 0 ~ 0,
      food == 1 ~ 0,
      liquid == 1 ~ 0,
      TRUE ~ 1
    )),
    survey_id = as.numeric(as.factor(survey))
  )

results_list_DHS_CC_under6 <- list()
unique_surveys_DHS_CC_under6 <- unique(df_DHS_CC_under6$survey_id)

invlogit <- function(x) exp(x) / (1 + exp(x))

for (sid in unique_surveys_DHS_CC_under6) {
  cat("Running model for survey", sid, "\n")
  
  # Step 1: restrict to <12 months
  df_survey <- df_DHS_CC_under6 %>% filter(survey_id == sid, agemonths < 12)
  
  if (nrow(df_survey) == 0) {
    cat("No data for survey", sid, "- skipping\n")
    next
  }
  
  dstrata <- svydesign(ids = ~1, weights = ~v005, data = df_survey, nest = TRUE)
  
  fit <- tryCatch({
    svyglm(
      EBF_DHS ~ agemonths + agemonths_sq + agemonths_cub + agemonths_quar,
      design = dstrata,
      family = quasibinomial()
    )
  }, error = function(e) {
    cat("Model failed for survey", sid, ":", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(fit)) next
  
  # Optional: skip obviously non-converged / unusable models
  if (any(!is.finite(coef(fit))) || any(is.na(coef(fit)))) {
    cat("Non-converged model for survey", sid, "- skipping\n")
    next
  }
  
  # Step 2: restrict to <6 months when building covariate averages
  df_under6 <- df_survey %>% filter(agemonths < 6)
  if (nrow(df_under6) == 0) {
    cat("No <6 months data for survey", sid, "- skipping\n")
    next
  }
  
  # Handle complete separation / zero-event surveys
  n_events <- sum(df_under6$EBF_DHS == 1, na.rm = TRUE)
  
  if (sum(df_under6$EBF_DHS == 1, na.rm = TRUE) == 0) {
    cat("Complete separation (0% prevalence) for survey", sid, "- applying correction\n")
    n <- nrow(df_under6)
    prev_corrected <- 0.5 / n        # Haldane-Anscombe correction
    ci_upper <- qbeta(0.975, 1, n)   # exact binomial upper bound
    fit_link <- qlogis(prev_corrected)
    se_link <- (qlogis(ci_upper) - qlogis(prev_corrected)) / 1.96
    
    # The Haldane-Anscombe correction (0.5/n) is a standard epidemiological approach for zero-cell proportions —
    # it adds 0.5 to the numerator, giving a small non-zero prevalence that scales with sample size,
    # is numerically stable, and has a principled justification rather than being arbitrary.
    # Larger surveys get a smaller correction, which makes intuitive sense.
    
  } else {
    mean_vals <- colMeans(
      df_under6 %>% select(agemonths, agemonths_sq, agemonths_cub, agemonths_quar),
      na.rm = TRUE
    )
    newdata <- as.data.frame(t(mean_vals))
    
    # Prediction on link scale
    X_new <- model.matrix(~ agemonths + agemonths_sq + agemonths_cub + agemonths_quar, newdata)
    beta <- coef(fit)
    V <- vcov(fit)
    
    # Skip if variance-covariance matrix is unusable
    if (any(!is.finite(V)) || any(is.na(V))) {
      cat("Invalid variance-covariance matrix for survey", sid, "- skipping\n")
      next
    }
    
    fit_link <- as.numeric(X_new %*% beta)
    se_link <- sqrt(as.numeric(X_new %*% V %*% t(X_new)))
  }
  
  survey_name <- as.character(df_survey$survey[1])
  
  results_list_DHS_CC_under6[[length(results_list_DHS_CC_under6) + 1]] <- data.frame(
    survey = survey_name,
    survey_id = sid,
    # mean_prediction_DHS_CC_under6 = mean_pred,
    # ci_lower_DHS_CC_under6 = ci_lower,
    # ci_upper_DHS_CC_under6 = ci_upper,
    se_link_DHS_CC_under6 = se_link,
    fit_link_DHS_CC_under6 = fit_link
  )
}

prevalence_DHS_CC_under6 <- bind_rows(results_list_DHS_CC_under6) %>%
  arrange(survey)

print(prevalence_DHS_CC_under6)

# Average prevalence across surveys with combined CI
summary_average_DHS_CC_under6 <- prevalence_DHS_CC_under6 %>%
  summarise(
    K = sum(!is.na(fit_link_DHS_CC_under6)),
    mean_prev_link = mean(fit_link_DHS_CC_under6, na.rm = TRUE),
    sd_between = sd(fit_link_DHS_CC_under6, na.rm = TRUE),
    se_between = sd_between / sqrt(K),
    se_within = sqrt(sum(se_link_DHS_CC_under6^2, na.rm = TRUE) / (K^2)),
    se_combined = sqrt(se_between^2 + se_within^2),
    ci_lower_link = mean_prev_link - 1.96 * se_combined,
    ci_upper_link = mean_prev_link + 1.96 * se_combined,
    mean_prev = invlogit(mean_prev_link),
    ci_lower = invlogit(ci_lower_link),
    ci_upper = invlogit(ci_upper_link)
  )

print(summary_average_DHS_CC_under6)

cat(
  "\nAverage prevalence (mean_prediction_DHS_CC_under6):",
  sprintf("%.3f", summary_average_DHS_CC_under6$mean_prev),
  "(95% CI:",
  sprintf("%.3f", summary_average_DHS_CC_under6$ci_lower), "-",
  sprintf("%.3f", summary_average_DHS_CC_under6$ci_upper), ")\n"
)

####################################################################

# DHS-MI EBF definition - under 6 months

library(tidyverse)
library(haven)
library(survey)
library(mice)
library(openxlsx)

DHS.df <- DHS_methods_analysis_2000_2022 %>%
  mutate(
    v005 = v005 / 1000000,
    agemonths_sq = agemonths^2,
    agemonths_cub = agemonths^3,
    agemonths_quar = agemonths^4
  )

# Imputation setup
imputation_vars_DHS_MI_under6 <- DHS.df %>%
  select(agemonths, BF, v012, v025, b4, food, liquid, edu) %>%
  mutate(across(-c(agemonths, v012), factor))

# Perform multiple imputation
imputed_data_DHS_MI_under6 <- mice(
  imputation_vars_DHS_MI_under6,
  m = 20,
  method = "logreg",
  seed = 1452,
  printFlag = FALSE
)

# Add necessary variables back
add_var_DHS_MI_under6 <- DHS.df %>%
  select(v005, survey, agemonths_sq, agemonths_cub, agemonths_quar)

imputed_dfs_DHS_MI_under6 <- lapply(1:imputed_data_DHS_MI_under6$m, function(i) {
  complete(imputed_data_DHS_MI_under6, i) %>%
    bind_cols(add_var_DHS_MI_under6) %>%
    mutate(
      EBF_DHS_MI = case_when(
        BF == 0 ~ 0,
        food == 1 ~ 0,
        liquid == 1 ~ 0,
        TRUE ~ 1
      )
    )
})

# Prevalence calculation function
prev_calculator_DHS_MI_under6 <- function(imp.list) {
  all_preds <- list()
  
  for (i in seq_along(imp.list)) {
    cat("Imputation", i, "\n")
    
    df <- imp.list[[i]] %>%
      filter(
        !is.na(agemonths),
        agemonths < 12,
        !is.na(v005), v005 > 0,
        !is.na(survey)
      ) %>%
      mutate(
        imputation_id = i,
        survey_id = as.numeric(as.factor(survey))
      )
    
    survey_ids <- unique(df$survey_id)
    preds <- list()
    
    for (sid in survey_ids) {
      cat("Running model for survey", sid, "in imputation", i, "\n")
      
      df_survey <- df %>% filter(survey_id == sid, agemonths < 12)
      
      if (nrow(df_survey) == 0) {
        cat("No data for survey", sid, "in imputation", i, "- skipping\n")
        next
      }
      
      df_under6 <- df_survey %>% filter(agemonths < 6)
      
      if (nrow(df_under6) == 0) {
        cat("No <6 months data for survey", sid, "in imputation", i, "- skipping\n")
        next
      }
      
      n <- nrow(df_under6)
      n_events <- sum(df_under6$EBF_DHS_MI == 1, na.rm = TRUE)
      
      # Haldane-Anscombe correction for zero prevalence only
      if (n_events == 0) {
        cat("Zero prevalence for survey", sid, "in imputation", i, "- applying Haldane correction\n")
        
        prev_corrected <- 0.5 / n
        ci_upper <- qbeta(0.975, 1, n)
        fit_link <- qlogis(prev_corrected)
        se_link <- (qlogis(ci_upper) - qlogis(prev_corrected)) / 1.96
        
      } else {
        dstrata <- svydesign(
          ids = ~1,
          weights = ~v005,
          data = df_survey,
          nest = TRUE
        )
        
        fit <- tryCatch({
          svyglm(
            EBF_DHS_MI ~ agemonths + agemonths_sq + agemonths_cub + agemonths_quar,
            design = dstrata,
            family = quasibinomial()
          )
        }, error = function(e) {
          cat("Model failed for survey", sid, "in imputation", i, ":", e$message, "\n")
          return(NULL)
        })
        
        if (is.null(fit)) next
        
        if (any(!is.finite(coef(fit))) || any(is.na(coef(fit)))) {
          cat("Non-converged model for survey", sid, "in imputation", i, "- skipping\n")
          next
        }
        
        # Predict on link scale for under-6 children
        df_under6$pred_link <- tryCatch({
          as.numeric(predict(fit, newdata = df_under6, type = "link"))
        }, error = function(e) {
          cat("Prediction failed for survey", sid, "in imputation", i, ":", e$message, "\n")
          return(NULL)
        })
        
        if (all(is.na(df_under6$pred_link))) {
          cat("All predictions missing for survey", sid, "in imputation", i, "- skipping\n")
          next
        }
        
        design_under6 <- svydesign(
          ids = ~1,
          weights = ~v005,
          data = df_under6,
          nest = TRUE
        )
        
        marginal_link <- tryCatch({
          svymean(~pred_link, design_under6, na.rm = TRUE)
        }, error = function(e) {
          cat("Weighted marginal mean failed for survey", sid, "in imputation", i, ":", e$message, "\n")
          return(NULL)
        })
        
        if (is.null(marginal_link)) next
        
        fit_link <- as.numeric(coef(marginal_link))
        se_link <- as.numeric(SE(marginal_link))
        
        if (!is.finite(fit_link) || is.na(fit_link) || !is.finite(se_link) || is.na(se_link)) {
          cat("Invalid marginal estimate for survey", sid, "in imputation", i, "- skipping\n")
          next
        }
      }
      
      survey_name <- as.character(df_survey$survey[1])
      
      preds[[length(preds) + 1]] <- tibble(
        imputation_id = i,
        survey_id = sid,
        survey = survey_name,
        n_under6 = n,
        n_events_under6 = n_events,
        fit_link = fit_link,
        se_link = se_link
      )
    }
    
    all_preds[[i]] <- bind_rows(preds)
  }
  
  pooled_with_link <- bind_rows(all_preds) %>%
    group_by(survey_id, survey) %>%
    summarise(
      m = n(),
      n_under6 = mean(n_under6, na.rm = TRUE),
      n_events_under6 = mean(n_events_under6, na.rm = TRUE),
      fit_link_DHS_MI_under6 = mean(fit_link, na.rm = TRUE),
      u_bar = mean(se_link^2, na.rm = TRUE),
      b = var(fit_link, na.rm = TRUE),
      b = ifelse(is.na(b), 0, b),
      t_var = u_bar + (1 + 1 / m) * b,
      se_link_DHS_MI_under6 = sqrt(t_var),
      .groups = "drop"
    ) %>%
    arrange(survey)
  
  pooled_with_link
}

prevalence_DHS_MI_under6 <- prev_calculator_DHS_MI_under6(imputed_dfs_DHS_MI_under6)

print(prevalence_DHS_MI_under6)

# Average prevalence across surveys with combined CI
summary_average_DHS_MI_under6 <- prevalence_DHS_MI_under6 %>%
  summarise(
    K = sum(!is.na(fit_link_DHS_MI_under6)),
    mean_link = mean(fit_link_DHS_MI_under6, na.rm = TRUE),
    sd_between = sd(fit_link_DHS_MI_under6, na.rm = TRUE),
    se_between = sd_between / sqrt(K),
    se_within = sqrt(sum(se_link_DHS_MI_under6^2, na.rm = TRUE) / (K^2)),
    se_combined = sqrt(se_between^2 + se_within^2),
    ci_lower_link = mean_link - 1.96 * se_combined,
    ci_upper_link = mean_link + 1.96 * se_combined,
    mean_prev = plogis(mean_link),
    ci_lower = plogis(ci_lower_link),
    ci_upper = plogis(ci_upper_link)
  )

print(summary_average_DHS_MI_under6)

cat(
  "\nAverage prevalence (DHS-MI under6):",
  sprintf("%.3f", summary_average_DHS_MI_under6$mean_prev),
  "(95% CI:",
  sprintf("%.3f", summary_average_DHS_MI_under6$ci_lower), "-",
  sprintf("%.3f", summary_average_DHS_MI_under6$ci_upper), ")\n"
)

####################################################################

# SE for CC and MI for comparison - back transform

# Delta method approximation to convert link-scale SE to probability scale
se_to_prob <- function(se_link, mean_prev) {
  se_link * mean_prev * (1 - mean_prev)
}

SE_DHS_CC_prob <- se_to_prob(
  summary_average_DHS_CC_under6$se_combined,
  summary_average_DHS_CC_under6$mean_prev
)

SE_DHS_MI_prob <- se_to_prob(
  summary_average_DHS_MI_under6$se_combined,
  summary_average_DHS_MI_under6$mean_prev
)

cat("SE for DHS-CC under 6 months (% points):", sprintf("%.2f", 100 * SE_DHS_CC_prob), "\n")
cat("SE for DHS-MI under 6 months (% points):", sprintf("%.2f", 100 * SE_DHS_MI_prob), "\n")

####################################################################

# WHO-MI EBF definition - under 6 months

library(dplyr)
library(survey)
library(openxlsx)
library(mice)

DHS.df <- DHS_methods_analysis_2000_2022 %>%
  mutate(
    v005 = v005 / 1000000,
    agemonths_sq = agemonths^2,
    agemonths_cub = agemonths^3,
    agemonths_quar = agemonths^4
  )

# Imputation setup
imputation_vars_WHO_MI_under6 <- DHS.df %>%
  select(agemonths, BF, v012, v025, b4, food, liquid, prelacteal, edu) %>%
  mutate(across(-c(agemonths, v012), factor))

# Perform multiple imputation
imputed_data_WHO_MI_under6 <- mice(
  imputation_vars_WHO_MI_under6,
  m = 20,
  method = "logreg",
  seed = 1452,
  printFlag = FALSE
)

# Add necessary variables back
add_var_WHO_MI_under6 <- DHS.df %>%
  select(v005, survey, agemonths_sq, agemonths_cub, agemonths_quar)

imputed_dfs_WHO_MI_under6 <- lapply(1:imputed_data_WHO_MI_under6$m, function(i) {
  complete(imputed_data_WHO_MI_under6, i) %>%
    bind_cols(add_var_WHO_MI_under6) %>%
    mutate(
      EBF_WHO_MI = case_when(
        BF == 0 ~ 0,
        food == 1 ~ 0,
        liquid == 1 ~ 0,
        prelacteal == 1 ~ 0,
        TRUE ~ 1
      )
    )
})

# Prevalence calculation function
prev_calculator_WHO_MI_under6 <- function(imp.list) {
  all_preds <- list()
  
  for (i in seq_along(imp.list)) {
    cat("Imputation", i, "\n")
    
    df <- imp.list[[i]] %>%
      filter(
        !is.na(agemonths),
        agemonths < 12,
        !is.na(v005), v005 > 0,
        !is.na(survey)
      ) %>%
      mutate(
        imputation_id = i,
        survey_id = as.numeric(as.factor(survey))
      )
    
    survey_ids <- unique(df$survey_id)
    preds <- list()
    
    for (sid in survey_ids) {
      cat("Running model for survey", sid, "in imputation", i, "\n")
      
      df_survey <- df %>% filter(survey_id == sid, agemonths < 12)
      
      if (nrow(df_survey) == 0) {
        cat("No data for survey", sid, "in imputation", i, "- skipping\n")
        next
      }
      
      df_under6 <- df_survey %>% filter(agemonths < 6)
      
      if (nrow(df_under6) == 0) {
        cat("No <6 months data for survey", sid, "in imputation", i, "- skipping\n")
        next
      }
      
      n <- nrow(df_under6)
      n_events <- sum(df_under6$EBF_WHO_MI == 1, na.rm = TRUE)
      
      # Haldane-Anscombe correction for zero prevalence only
      if (n_events == 0) {
        cat("Zero prevalence for survey", sid, "in imputation", i, "- applying Haldane correction\n")
        
        prev_corrected <- 0.5 / n
        ci_upper <- qbeta(0.975, 1, n)
        fit_link <- qlogis(prev_corrected)
        se_link <- (qlogis(ci_upper) - qlogis(prev_corrected)) / 1.96
        
      } else {
        dstrata <- svydesign(
          ids = ~1,
          weights = ~v005,
          data = df_survey,
          nest = TRUE
        )
        
        fit <- tryCatch({
          svyglm(
            EBF_WHO_MI ~ agemonths + agemonths_sq + agemonths_cub + agemonths_quar,
            design = dstrata,
            family = quasibinomial()
          )
        }, error = function(e) {
          cat("Model failed for survey", sid, "in imputation", i, ":", e$message, "\n")
          return(NULL)
        })
        
        if (is.null(fit)) next
        
        if (any(!is.finite(coef(fit))) || any(is.na(coef(fit)))) {
          cat("Non-converged model for survey", sid, "in imputation", i, "- skipping\n")
          next
        }
        
        # Predict on link scale for under-6 children
        df_under6$pred_link <- tryCatch({
          as.numeric(predict(fit, newdata = df_under6, type = "link"))
        }, error = function(e) {
          cat("Prediction failed for survey", sid, "in imputation", i, ":", e$message, "\n")
          return(NULL)
        })
        
        if (all(is.na(df_under6$pred_link))) {
          cat("All predictions missing for survey", sid, "in imputation", i, "- skipping\n")
          next
        }
        
        design_under6 <- svydesign(
          ids = ~1,
          weights = ~v005,
          data = df_under6,
          nest = TRUE
        )
        
        marginal_link <- tryCatch({
          svymean(~pred_link, design_under6, na.rm = TRUE)
        }, error = function(e) {
          cat("Weighted marginal mean failed for survey", sid, "in imputation", i, ":", e$message, "\n")
          return(NULL)
        })
        
        if (is.null(marginal_link)) next
        
        fit_link <- as.numeric(coef(marginal_link))
        se_link <- as.numeric(SE(marginal_link))
        
        if (!is.finite(fit_link) || is.na(fit_link) || !is.finite(se_link) || is.na(se_link)) {
          cat("Invalid marginal estimate for survey", sid, "in imputation", i, "- skipping\n")
          next
        }
      }
      
      survey_name <- as.character(df_survey$survey[1])
      
      preds[[length(preds) + 1]] <- tibble(
        imputation_id = i,
        survey_id = sid,
        survey = survey_name,
        n_under6 = n,
        n_events_under6 = n_events,
        fit_link_WHO_MI_under6 = fit_link,
        se_link_WHO_MI_under6 = se_link
      )
    }
    
    all_preds[[i]] <- bind_rows(preds)
  }
  
  pooled_with_link <- bind_rows(all_preds) %>%
    group_by(survey_id, survey) %>%
    summarise(
      m = n(),
      n_under6 = mean(n_under6, na.rm = TRUE),
      n_events_under6 = mean(n_events_under6, na.rm = TRUE),
      fit_link_WHO_MI_under6 = mean(fit_link_WHO_MI_under6, na.rm = TRUE),
      u_bar = mean(se_link_WHO_MI_under6^2, na.rm = TRUE),
      b = var(fit_link_WHO_MI_under6, na.rm = TRUE),
      b = ifelse(is.na(b), 0, b),
      t_var = u_bar + (1 + 1 / m) * b,
      se_link_WHO_MI_under6 = sqrt(t_var),
      .groups = "drop"
    ) %>%
    arrange(survey)
  
  pooled_with_link
}

prevalence_WHO_MI_under6 <- prev_calculator_WHO_MI_under6(imputed_dfs_WHO_MI_under6)

print(prevalence_WHO_MI_under6)

# Average prevalence across surveys with combined CI
summary_average_WHO_MI_under6 <- prevalence_WHO_MI_under6 %>%
  summarise(
    K = sum(!is.na(fit_link_WHO_MI_under6)),
    mean_link = mean(fit_link_WHO_MI_under6, na.rm = TRUE),
    sd_between = sd(fit_link_WHO_MI_under6, na.rm = TRUE),
    se_between = sd_between / sqrt(K),
    se_within = sqrt(sum(se_link_WHO_MI_under6^2, na.rm = TRUE) / (K^2)),
    se_combined = sqrt(se_between^2 + se_within^2),
    ci_lower_link = mean_link - 1.96 * se_combined,
    ci_upper_link = mean_link + 1.96 * se_combined,
    mean_prev = plogis(mean_link),
    ci_lower = plogis(ci_lower_link),
    ci_upper = plogis(ci_upper_link)
  )

print(summary_average_WHO_MI_under6)

cat(
  "\nAverage prevalence (WHO-MI under6):",
  sprintf("%.3f", summary_average_WHO_MI_under6$mean_prev),
  "(95% CI:",
  sprintf("%.3f", summary_average_WHO_MI_under6$ci_lower), "-",
  sprintf("%.3f", summary_average_WHO_MI_under6$ci_upper), ")\n"
)

####################################################################
# WHO-MI EBF definition - under 6 months + uniform age weights (months 0-5)

library(dplyr)
library(survey)
library(openxlsx)
library(mice)

DHS.df <- DHS_methods_analysis_2000_2022 %>%
  mutate(
    v005_s = v005 / 1000000,
    agemonths_sq = agemonths^2,
    agemonths_cub = agemonths^3,
    agemonths_quar = agemonths^4
  )

# Uniform age-group multipliers (months 0 to 5) per survey
age_mult_0_5 <- DHS.df %>%
  filter(
    !is.na(agemonths),
    agemonths >= 0, agemonths < 6,
    !is.na(survey),
    !is.na(v005_s), v005_s > 0
  ) %>%
  mutate(agegrp = as.integer(agemonths)) %>%
  group_by(survey, agegrp) %>%
  summarise(w_sum = sum(v005_s, na.rm = TRUE), .groups = "drop") %>%
  group_by(survey) %>%
  mutate(
    K = n(),
    target = sum(w_sum) / K,
    w_age_mult = target / w_sum
  ) %>%
  ungroup() %>%
  select(survey, agegrp, w_age_mult)

# Imputation setup
imputation_vars_WHO_MI_under6_agewt <- DHS.df %>%
  select(agemonths, BF, v012, v025, b4, food, liquid, prelacteal, edu) %>%
  mutate(across(-c(agemonths, v012), factor))

imputed_data_WHO_MI_under6_agewt <- mice(
  imputation_vars_WHO_MI_under6_agewt,
  m = 20,
  method = "logreg",
  seed = 1452,
  printFlag = FALSE
)

# Add necessary variables back
add_var_WHO_MI_under6_agewt <- DHS.df %>%
  select(v005_s, survey, agemonths_sq, agemonths_cub, agemonths_quar)

imputed_dfs_WHO_MI_under6_agewt <- lapply(1:imputed_data_WHO_MI_under6_agewt$m, function(i) {
  temp.df <- complete(imputed_data_WHO_MI_under6_agewt, i) %>%
    bind_cols(add_var_WHO_MI_under6_agewt) %>%
    mutate(
      EBF_WHO_MI = case_when(
        BF == 0 ~ 0,
        food == 1 ~ 0,
        liquid == 1 ~ 0,
        prelacteal == 1 ~ 0,
        TRUE ~ 1
      )
    )
  temp.df
})

prev_calculator_WHO_MI_under6_uniform <- function(imp.list, age_mult_tbl) {
  all_results <- list()
  
  for (i in seq_along(imp.list)) {
    cat("Running imputation", i, "\n")
    
    df_imp <- imp.list[[i]] %>%
      filter(
        !is.na(agemonths),
        !is.na(v005_s), v005_s > 0,
        !is.na(survey)
      ) %>%
      mutate(
        imputation_id = i,
        survey_id = as.numeric(as.factor(survey)),
        agegrp = as.integer(agemonths)
      ) %>%
      left_join(age_mult_tbl, by = c("survey", "agegrp")) %>%
      mutate(
        w_age_mult = ifelse(agegrp >= 0 & agegrp < 6, w_age_mult, 1),
        w_age_mult = ifelse(is.na(w_age_mult), 1, w_age_mult),
        w_final = v005_s * w_age_mult
      )
    
    results_list <- list()
    unique_surveys <- unique(df_imp$survey_id)
    
    for (sid in unique_surveys) {
      cat("Running model for survey", sid, "in imputation", i, "\n")
      
      # Step 1: restrict to <12 months
      df_survey <- df_imp %>% filter(survey_id == sid, agemonths < 12)
      
      if (nrow(df_survey) == 0) {
        cat("No data for survey", sid, "in imputation", i, "- skipping\n")
        next
      }
      
      # Step 2: restrict to <6 months
      df_under6 <- df_survey %>% filter(agemonths < 6)
      
      if (nrow(df_under6) == 0) {
        cat("No <6 months data for survey", sid, "in imputation", i, "- skipping\n")
        next
      }
      
      n <- nrow(df_under6)
      n_events <- sum(df_under6$EBF_WHO_MI == 1, na.rm = TRUE)
      
      # Haldane-Anscombe correction for zero prevalence only
      if (n_events == 0) {
        cat("Zero prevalence for survey", sid, "in imputation", i, "- applying Haldane correction\n")
        
        prev_corrected <- 0.5 / n
        ci_upper <- qbeta(0.975, 1, n)
        fit_link <- qlogis(prev_corrected)
        se_link <- (qlogis(ci_upper) - qlogis(prev_corrected)) / 1.96
        
      } else {
        # Step 3: fit model on <12 months with age-adjusted weights
        dstrata <- svydesign(
          ids = ~1,
          weights = ~w_final,
          data = df_survey,
          nest = TRUE
        )
        
        fit <- tryCatch({
          svyglm(
            EBF_WHO_MI ~ agemonths + agemonths_sq + agemonths_cub + agemonths_quar,
            design = dstrata,
            family = quasibinomial()
          )
        }, error = function(e) {
          cat("Model failed for survey", sid, "in imputation", i, ":", e$message, "\n")
          return(NULL)
        })
        
        if (is.null(fit)) next
        
        if (any(!is.finite(coef(fit))) || any(is.na(coef(fit)))) {
          cat("Non-converged model for survey", sid, "in imputation", i, "- skipping\n")
          next
        }
        
        V <- vcov(fit)
        if (any(!is.finite(V)) || any(is.na(V))) {
          cat("Invalid variance-covariance matrix for survey", sid, "in imputation", i, "- skipping\n")
          next
        }
        
        # Step 4: weighted marginal average over under-6 children
        df_under6$pred_link <- predict(fit, newdata = df_under6, type = "link")
        
        design_under6 <- svydesign(
          ids = ~1,
          weights = ~w_final,
          data = df_under6,
          nest = TRUE
        )
        
        marginal_prev <- svymean(~pred_link, design_under6)
        
        fit_link <- as.numeric(coef(marginal_prev))
        se_link <- as.numeric(SE(marginal_prev))
      }
      
      survey_name <- as.character(df_survey$survey[1])
      
      results_list[[length(results_list) + 1]] <- data.frame(
        imputation_id = i,
        survey = survey_name,
        survey_id = sid,
        n_under6 = n,
        n_events_under6 = n_events,
        fit_link_WHO_MI_under6_agewt = fit_link,
        se_link_WHO_MI_under6_agewt = se_link
      )
    }
    
    all_results[[i]] <- bind_rows(results_list)
  }
  
  pooled <- bind_rows(all_results) %>%
    group_by(survey, survey_id) %>%
    summarise(
      m = n(),
      n_under6 = mean(n_under6, na.rm = TRUE),
      n_events_under6 = mean(n_events_under6, na.rm = TRUE),
      fit_link_WHO_MI_under6_agewt = mean(fit_link_WHO_MI_under6_agewt, na.rm = TRUE),
      u_bar = mean(se_link_WHO_MI_under6_agewt^2, na.rm = TRUE),
      b = var(fit_link_WHO_MI_under6_agewt, na.rm = TRUE),
      b = ifelse(is.na(b), 0, b),
      t_var = u_bar + (1 + 1 / m) * b,
      se_link_WHO_MI_under6_agewt = sqrt(t_var),
      .groups = "drop"
    ) %>%
    arrange(survey)
  
  pooled
}

prevalence_WHO_MI_under6_uniform <- prev_calculator_WHO_MI_under6_uniform(
  imputed_dfs_WHO_MI_under6_agewt,
  age_mult_0_5
)

print(prevalence_WHO_MI_under6_uniform)

# Average prevalence across surveys with combined CI
summary_average_WHO_MI_under6_agewt <- prevalence_WHO_MI_under6_uniform %>%
  summarise(
    K = sum(!is.na(fit_link_WHO_MI_under6_agewt)),
    mean_link = mean(fit_link_WHO_MI_under6_agewt, na.rm = TRUE),
    sd_between = sd(fit_link_WHO_MI_under6_agewt, na.rm = TRUE),
    se_between = sd_between / sqrt(K),
    se_within = sqrt(sum(se_link_WHO_MI_under6_agewt^2, na.rm = TRUE) / (K^2)),
    se_combined = sqrt(se_between^2 + se_within^2),
    ci_lower_link = mean_link - 1.96 * se_combined,
    ci_upper_link = mean_link + 1.96 * se_combined,
    mean_prev = plogis(mean_link),
    ci_lower = plogis(ci_lower_link),
    ci_upper = plogis(ci_upper_link)
  )

print(summary_average_WHO_MI_under6_agewt)

cat(
  "\nAverage prevalence (uniform age-weighted WHO-MI under6):",
  sprintf("%.3f", summary_average_WHO_MI_under6_agewt$mean_prev),
  "(95% CI:",
  sprintf("%.3f", summary_average_WHO_MI_under6_agewt$ci_lower), "-",
  sprintf("%.3f", summary_average_WHO_MI_under6_agewt$ci_upper), ")\n"
)

####################################################################

# WHO-MI EBF definition - at 6 months

library(dplyr)
library(survey)
library(openxlsx)
library(mice)

DHS.df <- DHS_methods_analysis_2000_2022 %>%
  mutate(
    v005 = v005 / 1000000,
    agemonths_sq = agemonths^2,
    agemonths_cub = agemonths^3,
    agemonths_quar = agemonths^4
  )

# Imputation setup
imputation_vars_WHO_MI_at6 <- DHS.df %>%
  select(agemonths, BF, v012, v025, b4, food, liquid, prelacteal, edu) %>%
  mutate(across(-c(agemonths, v012), factor))

imputed_data_WHO_MI_at6 <- mice(
  imputation_vars_WHO_MI_at6,
  m = 20,
  method = "logreg",
  seed = 1452,
  printFlag = FALSE
)

add_var_WHO_MI_at6 <- DHS.df %>%
  select(v005, survey, agemonths_sq, agemonths_cub, agemonths_quar)

imputed_dfs_WHO_MI_at6 <- lapply(1:imputed_data_WHO_MI_at6$m, function(i) {
  temp.df <- complete(imputed_data_WHO_MI_at6, i) %>%
    bind_cols(add_var_WHO_MI_at6) %>%
    mutate(
      EBF_WHO_MI = case_when(
        BF == 0 ~ 0,
        food == 1 ~ 0,
        liquid == 1 ~ 0,
        prelacteal == 1 ~ 0,
        TRUE ~ 1
      )
    )
  temp.df
})

prev_calculator_WHO_MI_at6 <- function(imp.list) {
  all_results <- list()
  
  for (i in seq_along(imp.list)) {
    cat("Running imputation", i, "\n")
    
    df_imp <- imp.list[[i]] %>%
      filter(
        !is.na(agemonths),
        !is.na(v005),
        v005 > 0,
        !is.na(survey)
      ) %>%
      mutate(
        imputation_id = i,
        survey_id = as.numeric(as.factor(survey))
      )
    
    results_list <- list()
    unique_surveys <- unique(df_imp$survey_id)
    
    for (sid in unique_surveys) {
      cat("Running model for survey", sid, "in imputation", i, "\n")
      
      df_survey <- df_imp %>%
        filter(survey_id == sid, agemonths < 12)
      
      if (nrow(df_survey) == 0) {
        cat("No data for survey", sid, "in imputation", i, "- skipping\n")
        next
      }
      
      dstrata <- svydesign(ids = ~1, weights = ~v005, data = df_survey, nest = TRUE)
      
      fit <- tryCatch({
        svyglm(
          EBF_WHO_MI ~ agemonths + agemonths_sq + agemonths_cub + agemonths_quar,
          design = dstrata,
          family = quasibinomial()
        )
      }, error = function(e) {
        cat("Model failed for survey", sid, "in imputation", i, ":", e$message, "\n")
        return(NULL)
      })
      
      if (is.null(fit)) next
      
      if (any(!is.finite(coef(fit))) || any(is.na(coef(fit)))) {
        cat("Non-converged model for survey", sid, "in imputation", i, "- skipping\n")
        next
      }
      
      n <- nrow(df_survey)
      n_events <- sum(df_survey$EBF_WHO_MI == 1, na.rm = TRUE)
      
      # Haldane-Anscombe correction for zero prevalence only
      if (n_events == 0) {
        cat("Zero prevalence for survey", sid, "in imputation", i, "- applying Haldane correction\n")
        
        prev_corrected <- 0.5 / n
        ci_upper <- qbeta(0.975, 1, n)
        
        fit_link <- qlogis(prev_corrected)
        se_link <- (qlogis(ci_upper) - qlogis(prev_corrected)) / 1.96
      } else {
        newdata <- data.frame(
          agemonths = 6,
          agemonths_sq = 6^2,
          agemonths_cub = 6^3,
          agemonths_quar = 6^4
        )
        
        X_new <- model.matrix(
          ~ agemonths + agemonths_sq + agemonths_cub + agemonths_quar,
          newdata
        )
        
        beta <- coef(fit)
        V <- vcov(fit)
        
        if (any(!is.finite(V)) || any(is.na(V))) {
          cat("Invalid variance-covariance matrix for survey", sid, "in imputation", i, "- skipping\n")
          next
        }
        
        fit_link <- as.numeric(X_new %*% beta)
        se_link <- sqrt(as.numeric(X_new %*% V %*% t(X_new)))
      }
      
      survey_name <- as.character(df_survey$survey[1])
      
      results_list[[length(results_list) + 1]] <- data.frame(
        imputation_id = i,
        survey = survey_name,
        survey_id = sid,
        n_under12 = n,
        n_events_under12 = n_events,
        se_link_WHO_MI_at6 = se_link,
        fit_link_WHO_MI_at6 = fit_link
      )
    }
    
    all_results[[i]] <- bind_rows(results_list)
  }
  
  pooled <- bind_rows(all_results) %>%
    group_by(survey, survey_id) %>%
    summarise(
      m = n(),
      n_under12 = mean(n_under12, na.rm = TRUE),
      n_events_under12 = mean(n_events_under12, na.rm = TRUE),
      fit_link_WHO_MI_at6 = mean(fit_link_WHO_MI_at6, na.rm = TRUE),
      u_bar = mean(se_link_WHO_MI_at6^2, na.rm = TRUE),
      b = var(fit_link_WHO_MI_at6, na.rm = TRUE),
      b = ifelse(is.na(b), 0, b),
      t_var = u_bar + (1 + 1 / m) * b,
      se_link_WHO_MI_at6 = sqrt(t_var),
      .groups = "drop"
    ) %>%
    arrange(survey)
  
  pooled
}

prevalence_WHO_MI_at6 <- prev_calculator_WHO_MI_at6(imputed_dfs_WHO_MI_at6)

print(prevalence_WHO_MI_at6)

# Average prevalence across surveys with combined CI
summary_average_WHO_MI_at6 <- prevalence_WHO_MI_at6 %>%
  summarise(
    K = sum(!is.na(fit_link_WHO_MI_at6)),
    mean_link = mean(fit_link_WHO_MI_at6, na.rm = TRUE),
    sd_between = sd(fit_link_WHO_MI_at6, na.rm = TRUE),
    se_between = sd_between / sqrt(K),
    se_within = sqrt(sum(se_link_WHO_MI_at6^2, na.rm = TRUE) / (K^2)),
    se_combined = sqrt(se_between^2 + se_within^2),
    ci_lower_link = mean_link - 1.96 * se_combined,
    ci_upper_link = mean_link + 1.96 * se_combined,
    mean_prev = plogis(mean_link),
    ci_lower = plogis(ci_lower_link),
    ci_upper = plogis(ci_upper_link)
  )

print(summary_average_WHO_MI_at6)

cat(
  "\nAverage prevalence (mean_prediction_WHO_MI_at6):",
  sprintf("%.3f", summary_average_WHO_MI_at6$mean_prev),
  "(95% CI:",
  sprintf("%.3f", summary_average_WHO_MI_at6$ci_lower),
  "-",
  sprintf("%.3f", summary_average_WHO_MI_at6$ci_upper),
  ")\n"
)

####################################################################

# ISOLATED METHODOLOGICAL EFFECTS (each adjustment applied alone, directly to the DHS baseline, with no other adjustment stacked on top).
# These are in addition to the sequential/cumulative analyses above and are used to show each methodological change's standalone effect vs. DHS, per Reviewers request to see separate effects before combined ones

####################################################################

# DHS + Prelacteal only EBF definition (no MI, no age weighting) - under 6 months
# EBF_DHS_p codes missing food/liquid the same way as EBF_DHS (still counted as EBF), but additionally counts prelacteal feeding as breaking exclusive breastfeeding
# This variable already exists in DHS_methods_analysis_2000_2022 (see 03_Methods_Paper_Final.do, line 28)

library(dplyr)
library(survey)
library(broom)
library(openxlsx)

df_DHS_p_under6 <- DHS_methods_analysis_2000_2022 %>%
  mutate(
    v005 = v005 / 1000000,
    agemonths_sq = agemonths^2,
    agemonths_cub = agemonths^3,
    agemonths_quar = agemonths^4,
    survey_id = as.numeric(as.factor(survey))
  )

results_list_DHS_p_under6 <- list()
unique_surveys_DHS_p_under6 <- unique(df_DHS_p_under6$survey_id)

for (sid in unique_surveys_DHS_p_under6) {
  cat("Running model for survey", sid, "\n")
  
  # Step 1: restrict to <12 months
  df_survey <- df_DHS_p_under6 %>% filter(survey_id == sid, agemonths < 12)
  
  if (nrow(df_survey) == 0) {
    cat("No data for survey", sid, "- skipping\n")
    next
  }
  
  dstrata <- svydesign(ids = ~1, weights = ~v005, data = df_survey, nest = TRUE)
  
  fit <- tryCatch({
    svyglm(
      EBF_DHS_p ~ agemonths + agemonths_sq + agemonths_cub + agemonths_quar,
      design = dstrata,
      family = quasibinomial()
    )
  }, error = function(e) {
    cat("Model failed for survey", sid, ":", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(fit)) next
  
  if (any(!is.finite(coef(fit))) || any(is.na(coef(fit)))) {
    cat("Non-converged model for survey", sid, "- skipping\n")
    next
  }
  
  # Step 2: restrict to <6 months when building covariate averages
  df_under6 <- df_survey %>% filter(agemonths < 6)
  if (nrow(df_under6) == 0) {
    cat("No <6 months data for survey", sid, "- skipping\n")
    next
  }
  
  n_events <- sum(df_under6$EBF_DHS_p == 1, na.rm = TRUE)
  
  if (n_events == 0) {
    cat("Complete separation (0% prevalence) for survey", sid, "- applying correction\n")
    n <- nrow(df_under6)
    prev_corrected <- 0.5 / n        # Haldane-Anscombe correction
    ci_upper <- qbeta(0.975, 1, n)
    fit_link <- qlogis(prev_corrected)
    se_link <- (qlogis(ci_upper) - qlogis(prev_corrected)) / 1.96
    
  } else {
    mean_vals <- colMeans(
      df_under6 %>% select(agemonths, agemonths_sq, agemonths_cub, agemonths_quar),
      na.rm = TRUE
    )
    newdata <- as.data.frame(t(mean_vals))
    X_new <- model.matrix(~ agemonths + agemonths_sq + agemonths_cub + agemonths_quar, newdata)
    beta <- coef(fit)
    V <- vcov(fit)
    
    if (any(!is.finite(V)) || any(is.na(V))) {
      cat("Invalid variance-covariance matrix for survey", sid, "- skipping\n")
      next
    }
    
    fit_link <- as.numeric(X_new %*% beta)
    se_link <- sqrt(as.numeric(X_new %*% V %*% t(X_new)))
  }
  
  survey_name <- as.character(df_survey$survey[1])
  
  results_list_DHS_p_under6[[length(results_list_DHS_p_under6) + 1]] <- data.frame(
    survey = survey_name,
    survey_id = sid,
    n_under6 = nrow(df_under6),
    n_events_under6 = n_events,
    se_link_DHS_p_under6 = se_link,
    fit_link_DHS_p_under6 = fit_link
  )
}

prevalence_DHS_p_under6 <- bind_rows(results_list_DHS_p_under6) %>%
  arrange(survey)

print(prevalence_DHS_p_under6)

# Average prevalence across surveys with combined CI
summary_average_DHS_p_under6 <- prevalence_DHS_p_under6 %>%
  summarise(
    K = sum(!is.na(fit_link_DHS_p_under6)),
    mean_prev_link = mean(fit_link_DHS_p_under6, na.rm = TRUE),
    sd_between = sd(fit_link_DHS_p_under6, na.rm = TRUE),
    se_between = sd_between / sqrt(K),
    se_within = sqrt(sum(se_link_DHS_p_under6^2, na.rm = TRUE) / (K^2)),
    se_combined = sqrt(se_between^2 + se_within^2),
    ci_lower_link = mean_prev_link - 1.96 * se_combined,
    ci_upper_link = mean_prev_link + 1.96 * se_combined,
    mean_prev = invlogit(mean_prev_link),
    ci_lower = invlogit(ci_lower_link),
    ci_upper = invlogit(ci_upper_link)
  )

print(summary_average_DHS_p_under6)

cat(
  "\nAverage prevalence (DHS + prelacteal only, under 6 months):",
  sprintf("%.3f", summary_average_DHS_p_under6$mean_prev),
  "(95% CI:",
  sprintf("%.3f", summary_average_DHS_p_under6$ci_lower), "-",
  sprintf("%.3f", summary_average_DHS_p_under6$ci_upper), ")\n"
)

####################################################################

# DHS + Age-weighting only EBF definition (no MI, no prelacteal) - under 6 months
# Isolates the effect of age weighting alone, applied directly to the DHS missing-data approach
# Applies it here to EBF_DHS instead of EBF_WHO_MI. NOTE: this block must run after age_mult_0_5 has been created (see "Uniform age-group multipliers" earlier in this script).

library(dplyr)
library(survey)

df_DHS_agewt <- DHS_methods_analysis_2000_2022 %>%
  mutate(
    v005 = v005 / 1000000,
    agemonths_sq = agemonths^2,
    agemonths_cub = agemonths^3,
    agemonths_quar = agemonths^4,
    survey_id = as.numeric(as.factor(survey)),
    agegrp = as.integer(agemonths)
  ) %>%
  filter(
    !is.na(agemonths),
    !is.na(v005), v005 > 0,
    !is.na(survey)
  ) %>%
  left_join(age_mult_0_5, by = c("survey", "agegrp")) %>%
  mutate(
    w_age_mult = ifelse(agegrp >= 0 & agegrp < 6, w_age_mult, 1),
    w_age_mult = ifelse(is.na(w_age_mult), 1, w_age_mult),
    w_final = v005 * w_age_mult
  )

prev_calculator_DHS_agewt <- function(df_in) {
  results_list <- list()
  unique_surveys <- unique(df_in$survey_id)
  
  for (sid in unique_surveys) {
    cat("Running model for survey", sid, "\n")
    
    # Step 1: restrict to <12 months
    df_survey <- df_in %>% filter(survey_id == sid, agemonths < 12)
    
    if (nrow(df_survey) == 0) {
      cat("No data for survey", sid, "- skipping\n")
      next
    }
    
    # Step 2: restrict to <6 months
    df_under6 <- df_survey %>% filter(agemonths < 6)
    
    if (nrow(df_under6) == 0) {
      cat("No <6 months data for survey", sid, "- skipping\n")
      next
    }
    
    n <- nrow(df_under6)
    n_events <- sum(df_under6$EBF_DHS == 1, na.rm = TRUE)
    
    # Haldane-Anscombe correction for zero prevalence only
    if (n_events == 0) {
      cat("Zero prevalence for survey", sid, "- applying Haldane correction\n")
      
      prev_corrected <- 0.5 / n
      ci_upper <- qbeta(0.975, 1, n)
      fit_link <- qlogis(prev_corrected)
      se_link <- (qlogis(ci_upper) - qlogis(prev_corrected)) / 1.96
      
    } else {
      # Step 3: fit model on <12 months with age-adjusted weights
      dstrata <- svydesign(
        ids = ~1,
        weights = ~w_final,
        data = df_survey,
        nest = TRUE
      )
      
      fit <- tryCatch({
        svyglm(
          EBF_DHS ~ agemonths + agemonths_sq + agemonths_cub + agemonths_quar,
          design = dstrata,
          family = quasibinomial()
        )
      }, error = function(e) {
        cat("Model failed for survey", sid, ":", e$message, "\n")
        return(NULL)
      })
      
      if (is.null(fit)) next
      
      if (any(!is.finite(coef(fit))) || any(is.na(coef(fit)))) {
        cat("Non-converged model for survey", sid, "- skipping\n")
        next
      }
      
      V <- vcov(fit)
      if (any(!is.finite(V)) || any(is.na(V))) {
        cat("Invalid variance-covariance matrix for survey", sid, "- skipping\n")
        next
      }
      
      # Step 4: weighted marginal average over under-6 children
      df_under6$pred_link <- predict(fit, newdata = df_under6, type = "link")
      
      design_under6 <- svydesign(
        ids = ~1,
        weights = ~w_final,
        data = df_under6,
        nest = TRUE
      )
      
      marginal_prev <- svymean(~pred_link, design_under6)
      
      fit_link <- as.numeric(coef(marginal_prev))
      se_link <- as.numeric(SE(marginal_prev))
    }
    
    survey_name <- as.character(df_survey$survey[1])
    
    results_list[[length(results_list) + 1]] <- data.frame(
      survey = survey_name,
      survey_id = sid,
      n_under6 = n,
      n_events_under6 = n_events,
      fit_link_DHS_agewt = fit_link,
      se_link_DHS_agewt = se_link
    )
  }
  
  bind_rows(results_list) %>%
    arrange(survey)
}

prevalence_DHS_agewt <- prev_calculator_DHS_agewt(df_DHS_agewt)

print(prevalence_DHS_agewt)

# Average prevalence across surveys with combined CI
summary_average_DHS_agewt <- prevalence_DHS_agewt %>%
  summarise(
    K = sum(!is.na(fit_link_DHS_agewt)),
    mean_prev_link = mean(fit_link_DHS_agewt, na.rm = TRUE),
    sd_between = sd(fit_link_DHS_agewt, na.rm = TRUE),
    se_between = sd_between / sqrt(K),
    se_within = sqrt(sum(se_link_DHS_agewt^2, na.rm = TRUE) / (K^2)),
    se_combined = sqrt(se_between^2 + se_within^2),
    ci_lower_link = mean_prev_link - 1.96 * se_combined,
    ci_upper_link = mean_prev_link + 1.96 * se_combined,
    mean_prev = invlogit(mean_prev_link),
    ci_lower = invlogit(ci_lower_link),
    ci_upper = invlogit(ci_upper_link)
  )

print(summary_average_DHS_agewt)

cat(
  "\nAverage prevalence (DHS + age weighting only, under 6 months):",
  sprintf("%.3f", summary_average_DHS_agewt$mean_prev),
  "(95% CI:",
  sprintf("%.3f", summary_average_DHS_agewt$ci_lower), "-",
  sprintf("%.3f", summary_average_DHS_agewt$ci_upper), ")\n"
)

####################################################################

# DHS + At 6 months only EBF definition (no MI, no prelacteal, no age
# weighting)
# Isolates the effect of measuring EBF at exactly 6 months


library(dplyr)
library(survey)

df_DHS_at6 <- DHS_methods_analysis_2000_2022 %>%
  mutate(
    v005 = v005 / 1000000,
    agemonths_sq = agemonths^2,
    agemonths_cub = agemonths^3,
    agemonths_quar = agemonths^4,
    survey_id = as.numeric(as.factor(survey))
  ) %>%
  filter(
    !is.na(agemonths),
    !is.na(v005), v005 > 0,
    !is.na(survey)
  )

prev_calculator_DHS_at6 <- function(df_in) {
  results_list <- list()
  unique_surveys <- unique(df_in$survey_id)
  
  for (sid in unique_surveys) {
    cat("Running model for survey", sid, "\n")
    
    df_survey <- df_in %>% filter(survey_id == sid, agemonths < 12)
    
    if (nrow(df_survey) == 0) {
      cat("No data for survey", sid, "- skipping\n")
      next
    }
    
    dstrata <- svydesign(ids = ~1, weights = ~v005, data = df_survey, nest = TRUE)
    
    fit <- tryCatch({
      svyglm(
        EBF_DHS ~ agemonths + agemonths_sq + agemonths_cub + agemonths_quar,
        design = dstrata,
        family = quasibinomial()
      )
    }, error = function(e) {
      cat("Model failed for survey", sid, ":", e$message, "\n")
      return(NULL)
    })
    
    if (is.null(fit)) next
    
    if (any(!is.finite(coef(fit))) || any(is.na(coef(fit)))) {
      cat("Non-converged model for survey", sid, "- skipping\n")
      next
    }
    
    n <- nrow(df_survey)
    n_events <- sum(df_survey$EBF_DHS == 1, na.rm = TRUE)
    
    # Haldane-Anscombe correction for zero prevalence only
    if (n_events == 0) {
      cat("Zero prevalence for survey", sid, "- applying Haldane correction\n")
      
      prev_corrected <- 0.5 / n
      ci_upper <- qbeta(0.975, 1, n)
      fit_link <- qlogis(prev_corrected)
      se_link <- (qlogis(ci_upper) - qlogis(prev_corrected)) / 1.96
    } else {
      newdata <- data.frame(
        agemonths = 6,
        agemonths_sq = 6^2,
        agemonths_cub = 6^3,
        agemonths_quar = 6^4
      )
      
      X_new <- model.matrix(
        ~ agemonths + agemonths_sq + agemonths_cub + agemonths_quar,
        newdata
      )
      
      beta <- coef(fit)
      V <- vcov(fit)
      
      if (any(!is.finite(V)) || any(is.na(V))) {
        cat("Invalid variance-covariance matrix for survey", sid, "- skipping\n")
        next
      }
      
      fit_link <- as.numeric(X_new %*% beta)
      se_link <- sqrt(as.numeric(X_new %*% V %*% t(X_new)))
    }
    
    survey_name <- as.character(df_survey$survey[1])
    
    results_list[[length(results_list) + 1]] <- data.frame(
      survey = survey_name,
      survey_id = sid,
      n_under12 = n,
      n_events_under12 = n_events,
      se_link_DHS_at6 = se_link,
      fit_link_DHS_at6 = fit_link
    )
  }
  
  bind_rows(results_list) %>%
    arrange(survey)
}

prevalence_DHS_at6 <- prev_calculator_DHS_at6(df_DHS_at6)

print(prevalence_DHS_at6)

# Average prevalence across surveys with combined CI
summary_average_DHS_at6 <- prevalence_DHS_at6 %>%
  summarise(
    K = sum(!is.na(fit_link_DHS_at6)),
    mean_prev_link = mean(fit_link_DHS_at6, na.rm = TRUE),
    sd_between = sd(fit_link_DHS_at6, na.rm = TRUE),
    se_between = sd_between / sqrt(K),
    se_within = sqrt(sum(se_link_DHS_at6^2, na.rm = TRUE) / (K^2)),
    se_combined = sqrt(se_between^2 + se_within^2),
    ci_lower_link = mean_prev_link - 1.96 * se_combined,
    ci_upper_link = mean_prev_link + 1.96 * se_combined,
    mean_prev = invlogit(mean_prev_link),
    ci_lower = invlogit(ci_lower_link),
    ci_upper = invlogit(ci_upper_link)
  )

print(summary_average_DHS_at6)

cat(
  "\nAverage prevalence (DHS + at 6 months only):",
  sprintf("%.3f", summary_average_DHS_at6$mean_prev),
  "(95% CI:",
  sprintf("%.3f", summary_average_DHS_at6$ci_lower), "-",
  sprintf("%.3f", summary_average_DHS_at6$ci_upper), ")\n"
)

####################################################################

# MI EBF definition - under 6 months + uniform age weights (months 0-5)
# WITHOUT prelacteal feeding (Section C of Table 1)

library(dplyr)
library(survey)
library(openxlsx)
library(mice)

DHS.df <- DHS_methods_analysis_2000_2022 %>%
  mutate(
    v005_s = v005 / 1000000,
    agemonths_sq = agemonths^2,
    agemonths_cub = agemonths^3,
    agemonths_quar = agemonths^4
  )

# Uniform age-group multipliers (months 0 to 5) per survey
age_mult_0_5_nop <- DHS.df %>%
  filter(
    !is.na(agemonths),
    agemonths >= 0, agemonths < 6,
    !is.na(survey),
    !is.na(v005_s), v005_s > 0
  ) %>%
  mutate(agegrp = as.integer(agemonths)) %>%
  group_by(survey, agegrp) %>%
  summarise(w_sum = sum(v005_s, na.rm = TRUE), .groups = "drop") %>%
  group_by(survey) %>%
  mutate(
    K = n(),
    target = sum(w_sum) / K,
    w_age_mult = target / w_sum
  ) %>%
  ungroup() %>%
  select(survey, agegrp, w_age_mult)

# Imputation setup
imputation_vars_MI_under6_agewt_nop <- DHS.df %>%
  select(agemonths, BF, v012, v025, b4, food, liquid, edu) %>%
  mutate(across(-c(agemonths, v012), factor))

imputed_data_MI_under6_agewt_nop <- mice(
  imputation_vars_MI_under6_agewt_nop,
  m = 20,
  method = "logreg",
  seed = 1452,
  printFlag = FALSE
)

add_var_MI_under6_agewt_nop <- DHS.df %>%
  select(v005_s, survey, agemonths_sq, agemonths_cub, agemonths_quar)

imputed_dfs_MI_under6_agewt_nop <- lapply(1:imputed_data_MI_under6_agewt_nop$m, function(i) {
  temp.df <- complete(imputed_data_MI_under6_agewt_nop, i) %>%
    bind_cols(add_var_MI_under6_agewt_nop) %>%
    mutate(
      EBF_MI_nop = case_when(
        BF == 0 ~ 0,
        food == 1 ~ 0,
        liquid == 1 ~ 0,
        TRUE ~ 1
      )
    )
  temp.df
})

prev_calculator_MI_under6_agewt_nop <- function(imp.list, age_mult_tbl) {
  all_results <- list()
  
  for (i in seq_along(imp.list)) {
    cat("Running imputation", i, "\n")
    
    df_imp <- imp.list[[i]] %>%
      filter(
        !is.na(agemonths),
        !is.na(v005_s), v005_s > 0,
        !is.na(survey)
      ) %>%
      mutate(
        imputation_id = i,
        survey_id = as.numeric(as.factor(survey)),
        agegrp = as.integer(agemonths)
      ) %>%
      left_join(age_mult_tbl, by = c("survey", "agegrp")) %>%
      mutate(
        w_age_mult = ifelse(agegrp >= 0 & agegrp < 6, w_age_mult, 1),
        w_age_mult = ifelse(is.na(w_age_mult), 1, w_age_mult),
        w_final = v005_s * w_age_mult
      )
    
    results_list <- list()
    unique_surveys <- unique(df_imp$survey_id)
    
    for (sid in unique_surveys) {
      cat("Running model for survey", sid, "in imputation", i, "\n")
      
      df_survey <- df_imp %>% filter(survey_id == sid, agemonths < 12)
      
      if (nrow(df_survey) == 0) {
        cat("No data for survey", sid, "in imputation", i, "- skipping\n")
        next
      }
      
      df_under6 <- df_survey %>% filter(agemonths < 6)
      
      if (nrow(df_under6) == 0) {
        cat("No <6 months data for survey", sid, "in imputation", i, "- skipping\n")
        next
      }
      
      n <- nrow(df_under6)
      n_events <- sum(df_under6$EBF_MI_nop == 1, na.rm = TRUE)
      
      if (n_events == 0) {
        cat("Zero prevalence for survey", sid, "in imputation", i, "- applying Haldane correction\n")
        prev_corrected <- 0.5 / n
        ci_upper <- qbeta(0.975, 1, n)
        fit_link <- qlogis(prev_corrected)
        se_link <- (qlogis(ci_upper) - qlogis(prev_corrected)) / 1.96
        
      } else {
        dstrata <- svydesign(
          ids = ~1,
          weights = ~w_final,
          data = df_survey,
          nest = TRUE
        )
        
        fit <- tryCatch({
          svyglm(
            EBF_MI_nop ~ agemonths + agemonths_sq + agemonths_cub + agemonths_quar,
            design = dstrata,
            family = quasibinomial()
          )
        }, error = function(e) {
          cat("Model failed for survey", sid, "in imputation", i, ":", e$message, "\n")
          return(NULL)
        })
        
        if (is.null(fit)) next
        
        if (any(!is.finite(coef(fit))) || any(is.na(coef(fit)))) {
          cat("Non-converged model for survey", sid, "in imputation", i, "- skipping\n")
          next
        }
        
        V <- vcov(fit)
        if (any(!is.finite(V)) || any(is.na(V))) {
          cat("Invalid variance-covariance matrix for survey", sid, "in imputation", i, "- skipping\n")
          next
        }
        
        df_under6$pred_link <- predict(fit, newdata = df_under6, type = "link")
        
        design_under6 <- svydesign(
          ids = ~1,
          weights = ~w_final,
          data = df_under6,
          nest = TRUE
        )
        
        marginal_prev <- svymean(~pred_link, design_under6)
        
        fit_link <- as.numeric(coef(marginal_prev))
        se_link <- as.numeric(SE(marginal_prev))
      }
      
      survey_name <- as.character(df_survey$survey[1])
      
      results_list[[length(results_list) + 1]] <- data.frame(
        imputation_id = i,
        survey = survey_name,
        survey_id = sid,
        n_under6 = n,
        n_events_under6 = n_events,
        fit_link_MI_under6_agewt_nop = fit_link,
        se_link_MI_under6_agewt_nop = se_link
      )
    }
    
    all_results[[i]] <- bind_rows(results_list)
  }
  
  pooled <- bind_rows(all_results) %>%
    group_by(survey, survey_id) %>%
    summarise(
      m = n(),
      n_under6 = mean(n_under6, na.rm = TRUE),
      n_events_under6 = mean(n_events_under6, na.rm = TRUE),
      fit_link_MI_under6_agewt_nop = mean(fit_link_MI_under6_agewt_nop, na.rm = TRUE),
      u_bar = mean(se_link_MI_under6_agewt_nop^2, na.rm = TRUE),
      b = var(fit_link_MI_under6_agewt_nop, na.rm = TRUE),
      b = ifelse(is.na(b), 0, b),
      t_var = u_bar + (1 + 1 / m) * b,
      se_link_MI_under6_agewt_nop = sqrt(t_var),
      .groups = "drop"
    ) %>%
    arrange(survey)
  
  pooled
}

prevalence_MI_under6_agewt_nop <- prev_calculator_MI_under6_agewt_nop(
  imputed_dfs_MI_under6_agewt_nop,
  age_mult_0_5_nop
)

print(prevalence_MI_under6_agewt_nop)

summary_average_MI_under6_agewt_nop <- prevalence_MI_under6_agewt_nop %>%
  summarise(
    K = sum(!is.na(fit_link_MI_under6_agewt_nop)),
    mean_link = mean(fit_link_MI_under6_agewt_nop, na.rm = TRUE),
    sd_between = sd(fit_link_MI_under6_agewt_nop, na.rm = TRUE),
    se_between = sd_between / sqrt(K),
    se_within = sqrt(sum(se_link_MI_under6_agewt_nop^2, na.rm = TRUE) / (K^2)),
    se_combined = sqrt(se_between^2 + se_within^2),
    ci_lower_link = mean_link - 1.96 * se_combined,
    ci_upper_link = mean_link + 1.96 * se_combined,
    mean_prev = plogis(mean_link),
    ci_lower = plogis(ci_lower_link),
    ci_upper = plogis(ci_upper_link)
  )

print(summary_average_MI_under6_agewt_nop)

cat(
  "\nAverage prevalence (MI + age-weighting, no prelacteal, under 6 months):",
  sprintf("%.3f", summary_average_MI_under6_agewt_nop$mean_prev),
  "(95% CI:",
  sprintf("%.3f", summary_average_MI_under6_agewt_nop$ci_lower), "-",
  sprintf("%.3f", summary_average_MI_under6_agewt_nop$ci_upper), ")\n"
)

####################################################################

# MI EBF definition - at exactly 6 months WITHOUT prelacteal feeding (Section C of Table 1)

library(dplyr)
library(survey)
library(openxlsx)
library(mice)

DHS.df <- DHS_methods_analysis_2000_2022 %>%
  mutate(
    v005 = v005 / 1000000,
    agemonths_sq = agemonths^2,
    agemonths_cub = agemonths^3,
    agemonths_quar = agemonths^4
  )

# Imputation setup
imputation_vars_MI_at6_nop <- DHS.df %>%
  select(agemonths, BF, v012, v025, b4, food, liquid, edu) %>%
  mutate(across(-c(agemonths, v012), factor))

imputed_data_MI_at6_nop <- mice(
  imputation_vars_MI_at6_nop,
  m = 20,
  method = "logreg",
  seed = 1452,
  printFlag = FALSE
)

add_var_MI_at6_nop <- DHS.df %>%
  select(v005, survey, agemonths_sq, agemonths_cub, agemonths_quar)

imputed_dfs_MI_at6_nop <- lapply(1:imputed_data_MI_at6_nop$m, function(i) {
  temp.df <- complete(imputed_data_MI_at6_nop, i) %>%
    bind_cols(add_var_MI_at6_nop) %>%
    mutate(
      EBF_MI_nop = case_when(
        BF == 0 ~ 0,
        food == 1 ~ 0,
        liquid == 1 ~ 0,
        TRUE ~ 1
      )
    )
  temp.df
})

prev_calculator_MI_at6_nop <- function(imp.list) {
  all_results <- list()
  
  for (i in seq_along(imp.list)) {
    cat("Running imputation", i, "\n")
    
    df_imp <- imp.list[[i]] %>%
      filter(
        !is.na(agemonths),
        !is.na(v005),
        v005 > 0,
        !is.na(survey)
      ) %>%
      mutate(
        imputation_id = i,
        survey_id = as.numeric(as.factor(survey))
      )
    
    results_list <- list()
    unique_surveys <- unique(df_imp$survey_id)
    
    for (sid in unique_surveys) {
      cat("Running model for survey", sid, "in imputation", i, "\n")
      
      df_survey <- df_imp %>%
        filter(survey_id == sid, agemonths < 12)
      
      if (nrow(df_survey) == 0) {
        cat("No data for survey", sid, "in imputation", i, "- skipping\n")
        next
      }
      
      dstrata <- svydesign(ids = ~1, weights = ~v005, data = df_survey, nest = TRUE)
      
      fit <- tryCatch({
        svyglm(
          EBF_MI_nop ~ agemonths + agemonths_sq + agemonths_cub + agemonths_quar,
          design = dstrata,
          family = quasibinomial()
        )
      }, error = function(e) {
        cat("Model failed for survey", sid, "in imputation", i, ":", e$message, "\n")
        return(NULL)
      })
      
      if (is.null(fit)) next
      
      if (any(!is.finite(coef(fit))) || any(is.na(coef(fit)))) {
        cat("Non-converged model for survey", sid, "in imputation", i, "- skipping\n")
        next
      }
      
      n <- nrow(df_survey)
      n_events <- sum(df_survey$EBF_MI_nop == 1, na.rm = TRUE)
      
      if (n_events == 0) {
        cat("Zero prevalence for survey", sid, "in imputation", i, "- applying Haldane correction\n")
        prev_corrected <- 0.5 / n
        ci_upper <- qbeta(0.975, 1, n)
        fit_link <- qlogis(prev_corrected)
        se_link <- (qlogis(ci_upper) - qlogis(prev_corrected)) / 1.96
      } else {
        newdata <- data.frame(
          agemonths = 6,
          agemonths_sq = 6^2,
          agemonths_cub = 6^3,
          agemonths_quar = 6^4
        )
        
        X_new <- model.matrix(
          ~ agemonths + agemonths_sq + agemonths_cub + agemonths_quar,
          newdata
        )
        
        beta <- coef(fit)
        V <- vcov(fit)
        
        if (any(!is.finite(V)) || any(is.na(V))) {
          cat("Invalid variance-covariance matrix for survey", sid, "in imputation", i, "- skipping\n")
          next
        }
        
        fit_link <- as.numeric(X_new %*% beta)
        se_link <- sqrt(as.numeric(X_new %*% V %*% t(X_new)))
      }
      
      survey_name <- as.character(df_survey$survey[1])
      
      results_list[[length(results_list) + 1]] <- data.frame(
        imputation_id = i,
        survey = survey_name,
        survey_id = sid,
        n_under12 = n,
        n_events_under12 = n_events,
        se_link_MI_at6_nop = se_link,
        fit_link_MI_at6_nop = fit_link
      )
    }
    
    all_results[[i]] <- bind_rows(results_list)
  }
  
  pooled <- bind_rows(all_results) %>%
    group_by(survey, survey_id) %>%
    summarise(
      m = n(),
      n_under12 = mean(n_under12, na.rm = TRUE),
      n_events_under12 = mean(n_events_under12, na.rm = TRUE),
      fit_link_MI_at6_nop = mean(fit_link_MI_at6_nop, na.rm = TRUE),
      u_bar = mean(se_link_MI_at6_nop^2, na.rm = TRUE),
      b = var(fit_link_MI_at6_nop, na.rm = TRUE),
      b = ifelse(is.na(b), 0, b),
      t_var = u_bar + (1 + 1 / m) * b,
      se_link_MI_at6_nop = sqrt(t_var),
      .groups = "drop"
    ) %>%
    arrange(survey)
  
  pooled
}

prevalence_MI_at6_nop <- prev_calculator_MI_at6_nop(imputed_dfs_MI_at6_nop)

print(prevalence_MI_at6_nop)

summary_average_MI_at6_nop <- prevalence_MI_at6_nop %>%
  summarise(
    K = sum(!is.na(fit_link_MI_at6_nop)),
    mean_link = mean(fit_link_MI_at6_nop, na.rm = TRUE),
    sd_between = sd(fit_link_MI_at6_nop, na.rm = TRUE),
    se_between = sd_between / sqrt(K),
    se_within = sqrt(sum(se_link_MI_at6_nop^2, na.rm = TRUE) / (K^2)),
    se_combined = sqrt(se_between^2 + se_within^2),
    ci_lower_link = mean_link - 1.96 * se_combined,
    ci_upper_link = mean_link + 1.96 * se_combined,
    mean_prev = plogis(mean_link),
    ci_lower = plogis(ci_lower_link),
    ci_upper = plogis(ci_upper_link)
  )

print(summary_average_MI_at6_nop)

cat(
  "\nAverage prevalence (MI + at 6 months, no prelacteal):",
  sprintf("%.3f", summary_average_MI_at6_nop$mean_prev),
  "(95% CI:",
  sprintf("%.3f", summary_average_MI_at6_nop$ci_lower), "-",
  sprintf("%.3f", summary_average_MI_at6_nop$ci_upper), ")\n"
)

####################################################################

# SUMMARY TABLE (Table 1) - pooled EBF prevalence under every definition
# tested in the main 2000-2022 (months-based) analysis, to appear at the
# start of the Results section, ahead of Figure 3. Excludes the 183-days/
# 2015-2022 supplementary analyses, which are reported separately.
# Organized into four sections:
#   A. Missing-data handling approaches (cf. Figure 3)
#   B. Each adjustment applied in isolation, directly vs. the DHS
#      baseline, with nothing else changed (cf. Figures 4-6 content)
#   C. Cumulative chain of adjustments WITHOUT prelacteal feeding
#   D. Cumulative chain of adjustments WITH prelacteal feeding (cf. Figure 7)

library(dplyr)
library(tibble)

make_summary_row <- function(summary_df, category, definition) {
  prevalence <- summary_df$mean_prev * 100
  ci_lower <- summary_df$ci_lower * 100
  ci_upper <- summary_df$ci_upper * 100
  
  tibble(
    category = category,
    definition = definition,
    n_surveys = summary_df$K,
    prevalence_CI = sprintf("%.1f (%.1f-%.1f)", prevalence, ci_lower, ci_upper),
    prevalence = prevalence,
    ci_lower = ci_lower,
    ci_upper = ci_upper
  )
}

summary_table_main <- bind_rows(
  make_summary_row(summary_average_DHS_under6, "A. Missing-data handling", "DHS (missing = still EBF) [reference]"),
  make_summary_row(summary_average_UNICEF_under6, "A. Missing-data handling", "UNICEF/WHO (missing = not EBF)"),
  make_summary_row(summary_average_DHS_CC_under6, "A. Missing-data handling", "Complete case"),
  make_summary_row(summary_average_DHS_MI_under6, "A. Missing-data handling", "Multiple imputation (MI)"),
  
  make_summary_row(summary_average_DHS_p_under6, "B. Independent effect", "DHS + Prelacteal only"),
  make_summary_row(summary_average_DHS_agewt, "B. Independent effect", "DHS + Age-weighting only"),
  make_summary_row(summary_average_DHS_at6, "B. Independent effect", "DHS + At-6-months only"),
  
  make_summary_row(summary_average_MI_under6_agewt_nop, "C. Cumulative effect without Prelacteal", "MI + Age-weighting"),
  make_summary_row(summary_average_MI_at6_nop, "C. Cumulative effect without Prelacteal", "MI + At-6-months"),
  
  make_summary_row(summary_average_WHO_MI_under6, "D. Cumulative effect with Prelacteal", "MI-Prelacteal"),
  make_summary_row(summary_average_WHO_MI_under6_agewt, "D. Cumulative effect with Prelacteal", "MI-Prelacteal + Age-weighting"),
  make_summary_row(summary_average_WHO_MI_at6, "D. Cumulative effect with Prelacteal", "MI-Prelacteal + At-6-months")
) %>%
  select(category, definition, n_surveys, prevalence_CI, prevalence, ci_lower, ci_upper)

print(summary_table_main, n = Inf)

####################################################################

# TABLE 1B - same definitions as summary_table_main, with an added column showing each definition's prevalence minus the DHS reference prevalence
# Positive = higher than DHS; negative = lower than DHS

dhs_reference_prev <- summary_table_main %>%
  filter(definition == "DHS (missing = still EBF) [reference]") %>%
  pull(prevalence)

summary_table_diff <- summary_table_main %>%
  mutate(
    diff_from_DHS = prevalence - dhs_reference_prev
  ) %>%
  select(category, definition, n_surveys, prevalence_CI, prevalence, diff_from_DHS)

print(summary_table_diff, n = Inf)

####################################################################

# Figure 1 and 2 in ppt or mapchart

####################################################################

# Formatting for paper figures

library(ggplot2)
library(grid)

# Shared formatting for all figures
theme_ebf <- function() {
  theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(
        size = 14, face = "bold", hjust = 0.5,
        margin = margin(b = 10)
      ),
      axis.title.x = element_text(
        size = 14, face = "bold",
        margin = margin(t = 12)
      ),
      axis.title.y = element_text(
        size = 14, face = "bold",
        margin = margin(r = 12)
      ),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9),
      legend.key.size = unit(0.8, "lines"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.margin = margin(10, 12, 10, 10)
    )
}

####################################################################

# Figure 3 - bar plot of average prevalence across surveys for DHS, UNICEF, CC, and MI

figure_3_bar <- bind_rows(
  summary_average_DHS_under6 %>%
    transmute(
      definition = "DHS",
      prevalence = mean_prev * 100,
      ci_lower = ci_lower * 100,
      ci_upper = ci_upper * 100
    ),
  summary_average_UNICEF_under6 %>%
    transmute(
      definition = "UNICEF/WHO",
      prevalence = mean_prev * 100,
      ci_lower = ci_lower * 100,
      ci_upper = ci_upper * 100
    ),
  summary_average_DHS_CC_under6 %>%
    transmute(
      definition = "CC",
      prevalence = mean_prev * 100,
      ci_lower = ci_lower * 100,
      ci_upper = ci_upper * 100
    ),
  summary_average_DHS_MI_under6 %>%
    transmute(
      definition = "MI",
      prevalence = mean_prev * 100,
      ci_lower = ci_lower * 100,
      ci_upper = ci_upper * 100
    )
) %>%
  mutate(
    definition = factor(definition, levels = c("DHS", "UNICEF/WHO", "CC", "MI"))
  )

p3 <- ggplot(figure_3_bar, aes(x = definition, y = prevalence, fill = definition)) +
  geom_col(width = 0.7, alpha = 0.85) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.15,
    linewidth = 0.6
  ) +
  labs(
    title = "Exclusive Breastfeeding Prevalence\n(<6 months of age)",
    x = "EBF Definition",
    y = "Average Prevalence (%)"
  ) +
  scale_y_continuous(
    limits = c(0, 60),
    breaks = seq(0, 60, by = 10),
    expand = expansion(mult = c(0, 0.02))
  ) +
  theme_ebf() +
  theme(legend.position = "none")

print(p3)

###################################################

# Figure 4 - uses all surveys 2000 to 2022 to show a difference between MI and MI-Prelacteal density plot

figure_4 <- bind_rows(
  prevalence_DHS_MI_under6 %>%
    transmute(
      survey,
      definition = "MI",
      prevalence = plogis(fit_link_DHS_MI_under6),
      ci_lower = plogis(fit_link_DHS_MI_under6 - 1.96 * se_link_DHS_MI_under6),
      ci_upper = plogis(fit_link_DHS_MI_under6 + 1.96 * se_link_DHS_MI_under6)
    ),
  prevalence_WHO_MI_under6 %>%
    transmute(
      survey,
      definition = "MI-Prelacteal",
      prevalence = plogis(fit_link_WHO_MI_under6),
      ci_lower = plogis(fit_link_WHO_MI_under6 - 1.96 * se_link_WHO_MI_under6),
      ci_upper = plogis(fit_link_WHO_MI_under6 + 1.96 * se_link_WHO_MI_under6)
    )
) %>%
  mutate(
    prevalence = prevalence * 100,
    ci_lower = ci_lower * 100,
    ci_upper = ci_upper * 100,
    definition = factor(definition, levels = c("MI", "MI-Prelacteal")),
    prevalence_with_CI = sprintf("%.1f (%.1f–%.1f)", prevalence, ci_lower, ci_upper)
  )

p4 <- ggplot(figure_4, aes(x = prevalence, fill = definition)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Exclusive Breastfeeding Prevalence\n(<6 months of age)",
    x = "Prevalence (%)",
    y = "Density",
    fill = "EBF Definition"
  ) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20)
  ) +
  theme_ebf() +
  theme(legend.position = "right")

print(p4)

###################################################

# Supp. Figure 1 - Histogram of age ratios across surveys

library(tidyr)

ratio_tbl <- DHS_methods_analysis_2000_2022 %>%
  mutate(
    age_group = case_when(
      agemonths <= 1 ~ "0_1",
      agemonths >= 4 & agemonths <= 5 ~ "4_5",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(age_group)) %>%
  group_by(survey, age_group) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = age_group,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(
    ratio_01_45 = ifelse(`4_5` > 0, `0_1` / `4_5`, NA_real_)
  ) %>%
  filter(!is.na(ratio_01_45))

p_s1 <- ggplot(ratio_tbl, aes(x = ratio_01_45)) +
  geom_histogram(
    binwidth = 0.05,
    fill = "#00BFC4",
    color = "black",
    alpha = 0.85
  ) +
  geom_vline(
    xintercept = 1,
    color = "red",
    linetype = "dashed"
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 10)
  ) +
  labs(
    title = "Distribution of age ratios across surveys",
    x = "Ratio: children 0–1 / 4–5 months",
    y = "Number of surveys"
  ) +
  theme_ebf()

print(p_s1)

# Overall ratio across the full 2000-2022 dataset
ratio_overall_2000_2022 <- with(
  DHS_methods_analysis_2000_2022,
  sum(agemonths <= 1, na.rm = TRUE) /
    sum(agemonths >= 4 & agemonths <= 5, na.rm = TRUE)
)

ratio_overall_2000_2022

###################################################

# Figure 5 - paired scatter plot for WHO-MI under 6 months vs WHO-MI under 6 months (age-weighted)

paired_df_5 <- prevalence_WHO_MI_under6 %>%
  transmute(
    survey,
    who_mi_under6_pct = plogis(fit_link_WHO_MI_under6) * 100
  ) %>%
  inner_join(
    prevalence_WHO_MI_under6_uniform %>%
      transmute(
        survey,
        who_mi_under6_agewt_pct = plogis(fit_link_WHO_MI_under6_agewt) * 100
      ),
    by = "survey"
  )

p5 <- ggplot(paired_df_5, aes(x = who_mi_under6_agewt_pct, y = who_mi_under6_pct)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(color = "#00BFC4", size = 2.5, alpha = 0.85) +
  scale_x_continuous(
    breaks = seq(0, 100, 20)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 20)
  ) +
  coord_equal(xlim = c(0, 100), ylim = c(0, 100)) +
  labs(
    x = "Prevalence Under 6 Months (Age-Weighted) (%)",
    y = "Prevalence Under 6 Months (%)",
    title = "Survey-level EBF prevalence: Under 6 (age-weighted) vs Under 6 months"
  ) +
  theme_ebf()

print(p5)

###################################################

# Figure 6 - paired scatter plot for WHO-MI under 6 months (age-weighted) vs WHO-MI at 6 months

paired_df <- prevalence_WHO_MI_under6_uniform %>%
  transmute(
    survey,
    under6_pct = plogis(fit_link_WHO_MI_under6_agewt) * 100
  ) %>%
  inner_join(
    prevalence_WHO_MI_at6 %>%
      transmute(
        survey,
        at6_pct = plogis(fit_link_WHO_MI_at6) * 100
      ),
    by = "survey"
  )

p6 <- ggplot(paired_df, aes(x = under6_pct, y = at6_pct)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(color = "#00BFC4", size = 2.5, alpha = 0.85) +
  scale_x_continuous(
    breaks = seq(0, 100, 20)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 20)
  ) +
  coord_equal(xlim = c(0, 100), ylim = c(0, 100)) +
  labs(
    x = "Prevalence Under 6 Months (Age-Weighted) (%)",
    y = "Prevalence At 6 Months (%)",
    title = "Survey-level EBF prevalence: Under 6 (age-weighted) vs At 6 months"
  ) +
  theme_ebf()

print(p6)

###################################################

# Figure 7 - summary bar graph (DHS, MI, MI-Prelacteal, Age weights, at 6 months)

library(dplyr)
library(ggplot2)

fig7 <- bind_rows(
  summary_average_DHS_under6 %>%
    transmute(
      Definition = "DHS\nUnder 6M",
      Prev = mean_prev * 100,
      L = ci_lower * 100,
      U = ci_upper * 100
    ),
  summary_average_DHS_MI_under6 %>%
    transmute(
      Definition = "MI\nUnder 6M",
      Prev = mean_prev * 100,
      L = ci_lower * 100,
      U = ci_upper * 100
    ),
  summary_average_WHO_MI_under6 %>%
    transmute(
      Definition = "MI-Prelacteal\nUnder 6M",
      Prev = mean_prev * 100,
      L = ci_lower * 100,
      U = ci_upper * 100
    ),
  summary_average_WHO_MI_under6_agewt %>%
    transmute(
      Definition = "MI-Prelacteal\n(age-weighted)\nUnder 6M",
      Prev = mean_prev * 100,
      L = ci_lower * 100,
      U = ci_upper * 100
    ),
  summary_average_WHO_MI_at6 %>%
    transmute(
      Definition = "MI-Prelacteal\nAt 6M",
      Prev = mean_prev * 100,
      L = ci_lower * 100,
      U = ci_upper * 100
    )
) %>%
  mutate(
    Definition = factor(
      Definition,
      levels = c(
        "DHS\nUnder 6M",
        "MI\nUnder 6M",
        "MI-Prelacteal\nUnder 6M",
        "MI-Prelacteal\n(age-weighted)\nUnder 6M",
        "MI-Prelacteal\nAt 6M"
      )
    )
  )

p7 <- ggplot(fig7, aes(x = Definition, y = Prev, fill = Definition)) +
  geom_col(
    width = 0.7,
    alpha = 0.85
  ) +
  geom_errorbar(
    aes(ymin = L, ymax = U),
    width = 0.15,
    linewidth = 0.6
  ) +
  scale_y_continuous(
    limits = c(0, 60),
    breaks = seq(0, 60, by = 10),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_manual(
    values = c(
      "DHS\nUnder 6M" = "#FF7F7F",
      "MI\nUnder 6M" = "#7CAE00",
      "MI-Prelacteal\nUnder 6M" = "#00BFC4",
      "MI-Prelacteal\n(age-weighted)\nUnder 6M" = "#C77CFF",
      "MI-Prelacteal\nAt 6M" = "#F1C40F"
    )
  ) +
  labs(
    title = "Average EBF prevalence across definitions",
    x = "EBF Definition",
    y = "Average prevalence (%)"
  ) +
  theme_ebf() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      size = 11,
      lineheight = 0.9,
      hjust = 0.5
    )
  )

print(p7)

###################################################


# Supp. Tables 2,3,4 - export pooled survey prevalences

library(openxlsx)
library(dplyr)
library(tibble)

# survey-level export table

make_export_table <- function(data, fit_col, se_col,
                              survey_col = "survey",
                              survey_id_col = "survey_id",
                              n_col = NULL,
                              events_col = NULL) {
  
  out <- data %>%
    transmute(
      survey = .data[[survey_col]],
      survey_id = .data[[survey_id_col]],
      pooled_fit_link = .data[[fit_col]],
      pooled_se_link = .data[[se_col]],
      pooled_prevalence = plogis(.data[[fit_col]]) * 100,
      lower_CI = plogis(.data[[fit_col]] - 1.96 * .data[[se_col]]) * 100,
      upper_CI = plogis(.data[[fit_col]] + 1.96 * .data[[se_col]]) * 100
    )
  
  if (!is.null(n_col) && n_col %in% names(data)) {
    out <- out %>% mutate(n = data[[n_col]])
  }
  
  if (!is.null(events_col) && events_col %in% names(data)) {
    out <- out %>% mutate(n_events = data[[events_col]])
  }
  
  out %>%
    mutate(
      prevalence_CI = sprintf("%.1f (%.1f-%.1f)", pooled_prevalence, lower_CI, upper_CI)
    ) %>%
    select(
      survey,
      survey_id,
      any_of(c("n", "n_events")),
      prevalence_CI,
      pooled_prevalence,
      lower_CI,
      upper_CI,
      pooled_fit_link,
      pooled_se_link
    ) %>%
    arrange(survey)
}

# overall summary export table
make_overall_export <- function(summary_df, definition_name) {
  
  mean_link_val <- if ("mean_link" %in% names(summary_df)) {
    summary_df$mean_link
  } else if ("mean_prev_link" %in% names(summary_df)) {
    summary_df$mean_prev_link
  } else {
    NA_real_
  }
  
  pooled_prevalence <- summary_df$mean_prev * 100
  lower_CI <- summary_df$ci_lower * 100
  upper_CI <- summary_df$ci_upper * 100
  
  tibble(
    definition = definition_name,
    n_surveys = summary_df$K,
    prevalence_CI = sprintf("%.1f (%.1f-%.1f)", pooled_prevalence, lower_CI, upper_CI),
    pooled_prevalence = pooled_prevalence,
    lower_CI = lower_CI,
    upper_CI = upper_CI,
    mean_link = mean_link_val,
    se_combined = summary_df$se_combined
  )
}


# Optional styling helper

write_sheet_with_style <- function(wb, sheet_name, df) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet = sheet_name, x = df)
  
  header_style <- createStyle(
    textDecoration = "bold",
    halign = "center",
    border = "Bottom"
  )
  
  addStyle(
    wb, sheet = sheet_name, style = header_style,
    rows = 1, cols = 1:ncol(df), gridExpand = TRUE
  )
  
  setColWidths(wb, sheet = sheet_name, cols = 1:ncol(df), widths = "auto")
}

# Build survey-level export sheets

sheet_UNICEF_under6 <- make_export_table(
  prevalence_UNICEF_under6,
  fit_col = "fit_link_UNICEF_under6",
  se_col = "se_link_UNICEF_under6",
  n_col = "n_under6",
  events_col = "n_events_under6"
)

sheet_DHS_under6 <- make_export_table(
  prevalence_DHS_under6,
  fit_col = "fit_link_DHS_under6",
  se_col = "se_link_DHS_under6",
  n_col = "n_under6",
  events_col = "n_events_under6"
)

sheet_DHS_CC_under6 <- make_export_table(
  prevalence_DHS_CC_under6,
  fit_col = "fit_link_DHS_CC_under6",
  se_col = "se_link_DHS_CC_under6",
  n_col = "n_under6",
  events_col = "n_events_under6"
)

sheet_DHS_MI_under6 <- make_export_table(
  prevalence_DHS_MI_under6,
  fit_col = "fit_link_DHS_MI_under6",
  se_col = "se_link_DHS_MI_under6",
  n_col = "n_under6",
  events_col = "n_events_under6"
)

sheet_WHO_MI_under6 <- make_export_table(
  prevalence_WHO_MI_under6,
  fit_col = "fit_link_WHO_MI_under6",
  se_col = "se_link_WHO_MI_under6",
  n_col = "n_under6",
  events_col = "n_events_under6"
)

sheet_WHO_MI_under6_agewt <- make_export_table(
  prevalence_WHO_MI_under6_uniform,
  fit_col = "fit_link_WHO_MI_under6_agewt",
  se_col = "se_link_WHO_MI_under6_agewt",
  n_col = "n_under6",
  events_col = "n_events_under6"
)

sheet_WHO_MI_at6 <- make_export_table(
  prevalence_WHO_MI_at6,
  fit_col = "fit_link_WHO_MI_at6",
  se_col = "se_link_WHO_MI_at6",
  n_col = "n_under12",
  events_col = "n_events_under12"
)

sheet_DHS_p_under6 <- make_export_table(
  prevalence_DHS_p_under6,
  fit_col = "fit_link_DHS_p_under6",
  se_col = "se_link_DHS_p_under6",
  n_col = "n_under6",
  events_col = "n_events_under6"
)

sheet_DHS_agewt <- make_export_table(
  prevalence_DHS_agewt,
  fit_col = "fit_link_DHS_agewt",
  se_col = "se_link_DHS_agewt",
  n_col = "n_under6",
  events_col = "n_events_under6"
)

sheet_DHS_at6 <- make_export_table(
  prevalence_DHS_at6,
  fit_col = "fit_link_DHS_at6",
  se_col = "se_link_DHS_at6",
  n_col = "n_under12",
  events_col = "n_events_under12"
)

sheet_MI_under6_agewt_nop <- make_export_table(
  prevalence_MI_under6_agewt_nop,
  fit_col = "fit_link_MI_under6_agewt_nop",
  se_col = "se_link_MI_under6_agewt_nop",
  n_col = "n_under6",
  events_col = "n_events_under6"
)

sheet_MI_at6_nop <- make_export_table(
  prevalence_MI_at6_nop,
  fit_col = "fit_link_MI_at6_nop",
  se_col = "se_link_MI_at6_nop",
  n_col = "n_under12",
  events_col = "n_events_under12"
)

# Overall pooled summary sheet (one row per definition)
overall_summary <- bind_rows(
  make_overall_export(summary_average_DHS_under6, "DHS (missing = still EBF) - under 6M [reference]"),
  make_overall_export(summary_average_UNICEF_under6, "UNICEF/WHO (missing = not EBF) - under 6M"),
  make_overall_export(summary_average_DHS_CC_under6, "Complete case - under 6M"),
  make_overall_export(summary_average_DHS_MI_under6, "Multiple imputation (MI) - under 6M"),
  make_overall_export(summary_average_DHS_p_under6, "DHS + Prelacteal only - under 6M"),
  make_overall_export(summary_average_DHS_agewt, "DHS + Age-weighting only - under 6M"),
  make_overall_export(summary_average_DHS_at6, "DHS + At-6-months only"),
  make_overall_export(summary_average_MI_under6_agewt_nop, "MI + Age-weighting (no prelacteal) - under 6M"),
  make_overall_export(summary_average_MI_at6_nop, "MI + At-6-months (no prelacteal)"),
  make_overall_export(summary_average_WHO_MI_under6, "MI-Prelacteal - under 6M"),
  make_overall_export(summary_average_WHO_MI_under6_agewt, "MI-Prelacteal + Age-weighting - under 6M"),
  make_overall_export(summary_average_WHO_MI_at6, "MI-Prelacteal + At-6-months")
)

# Write everything to one Excel workbook

wb <- createWorkbook()

write_sheet_with_style(wb, "Table1_summary", summary_table_main)
write_sheet_with_style(wb, "Table1_diff_from_DHS", summary_table_diff)
write_sheet_with_style(wb, "UNICEF_under6", sheet_UNICEF_under6)
write_sheet_with_style(wb, "DHS_under6", sheet_DHS_under6)
write_sheet_with_style(wb, "DHS_CC_under6", sheet_DHS_CC_under6)
write_sheet_with_style(wb, "DHS_MI_under6", sheet_DHS_MI_under6)
write_sheet_with_style(wb, "WHO_MI_under6", sheet_WHO_MI_under6)
write_sheet_with_style(wb, "WHO_MI_under6_agewt", sheet_WHO_MI_under6_agewt)
write_sheet_with_style(wb, "WHO_MI_at6", sheet_WHO_MI_at6)
write_sheet_with_style(wb, "DHS_plus_prelacteal_only", sheet_DHS_p_under6)
write_sheet_with_style(wb, "DHS_plus_agewt_only", sheet_DHS_agewt)
write_sheet_with_style(wb, "DHS_plus_at6_only", sheet_DHS_at6)
write_sheet_with_style(wb, "MI_agewt_no_prelacteal", sheet_MI_under6_agewt_nop)
write_sheet_with_style(wb, "MI_at6_no_prelacteal", sheet_MI_at6_nop)
write_sheet_with_style(wb, "overall_summary", overall_summary)

saveWorkbook(
  wb,
  file = "EBF_pooled_survey_prevalences.xlsx",
  overwrite = TRUE
)

####################################################################