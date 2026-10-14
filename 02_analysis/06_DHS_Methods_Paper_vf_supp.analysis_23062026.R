# Using DHS surveys 2015 to 2022 - Supplementary Analysis (age in days)
# Run with the working directory set to this folder (02_analysis/).

####################################################################

# Load data for DHS surveys 2015-2022

library(tidyverse)
library(haven)
library(survey)
library(mice)
library(openxlsx)

DHS_methods_analysis_2015_2022 <- read_dta(
  "../data/restricted/DHS_methods_analysis.dta"
)

# view(DHS_methods_analysis_2015_2022)  # interactive viewer; not needed to reproduce results

DHS.df <- DHS_methods_analysis_2015_2022 %>%
  mutate(
    v005 = v005 / 1000000,
    agemonths_sq = agemonths^2,
    agemonths_cub = agemonths^3,
    agemonths_quar = agemonths^4,
    agedays_sq = agedays^2,
    agedays_cub = agedays^3,
    agedays_quar = agedays^4
  )

####################################################################

# WHO-MI EBF definition - at 6 months

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

# WHO-MI EBF definition - at 183 days

imputation_vars_WHO_MI_at183 <- DHS.df %>%
  select(agedays, BF, v012, v025, b4, food, liquid, prelacteal, edu) %>%
  mutate(across(-c(agedays, v012), factor))

imputed_data_WHO_MI_at183 <- mice(
  imputation_vars_WHO_MI_at183,
  m = 20,
  method = "logreg",
  seed = 1452,
  printFlag = FALSE
)

add_var_WHO_MI_at183 <- DHS.df %>%
  select(v005, survey, agedays_sq, agedays_cub, agedays_quar)

imputed_dfs_WHO_MI_at183 <- lapply(1:imputed_data_WHO_MI_at183$m, function(i) {
  temp.df <- complete(imputed_data_WHO_MI_at183, i) %>%
    bind_cols(add_var_WHO_MI_at183) %>%
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

prev_calculator_WHO_MI_at183 <- function(imp.list) {
  all_results <- list()
  
  for (i in seq_along(imp.list)) {
    cat("Running imputation", i, "\n")
    
    df_imp <- imp.list[[i]] %>%
      filter(
        !is.na(agedays),
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
        filter(survey_id == sid, agedays < 365)
      
      if (nrow(df_survey) == 0) {
        cat("No data for survey", sid, "in imputation", i, "- skipping\n")
        next
      }
      
      dstrata <- svydesign(
        ids = ~1,
        weights = ~v005,
        data = df_survey,
        nest = TRUE
      )
      
      fit <- tryCatch({
        svyglm(
          EBF_WHO_MI ~ agedays + agedays_sq + agedays_cub + agedays_quar,
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
          agedays = 183,
          agedays_sq = 183^2,
          agedays_cub = 183^3,
          agedays_quar = 183^4
        )
        
        X_new <- model.matrix(
          ~ agedays + agedays_sq + agedays_cub + agedays_quar,
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
        n_under365 = n,
        n_events_under365 = n_events,
        se_link_WHO_MI_at183 = se_link,
        fit_link_WHO_MI_at183 = fit_link
      )
    }
    
    all_results[[i]] <- bind_rows(results_list)
  }
  
  pooled <- bind_rows(all_results) %>%
    group_by(survey, survey_id) %>%
    summarise(
      m = n(),
      n_under365 = mean(n_under365, na.rm = TRUE),
      n_events_under365 = mean(n_events_under365, na.rm = TRUE),
      fit_link_WHO_MI_at183 = mean(fit_link_WHO_MI_at183, na.rm = TRUE),
      u_bar = mean(se_link_WHO_MI_at183^2, na.rm = TRUE),
      b = var(fit_link_WHO_MI_at183, na.rm = TRUE),
      b = ifelse(is.na(b), 0, b),
      t_var = u_bar + (1 + 1 / m) * b,
      se_link_WHO_MI_at183 = sqrt(t_var),
      .groups = "drop"
    ) %>%
    arrange(survey)
  
  pooled
}

prevalence_WHO_MI_at183 <- prev_calculator_WHO_MI_at183(imputed_dfs_WHO_MI_at183)

print(prevalence_WHO_MI_at183)

summary_average_WHO_MI_at183 <- prevalence_WHO_MI_at183 %>%
  summarise(
    K = sum(!is.na(fit_link_WHO_MI_at183)),
    mean_link = mean(fit_link_WHO_MI_at183, na.rm = TRUE),
    sd_between = sd(fit_link_WHO_MI_at183, na.rm = TRUE),
    se_between = sd_between / sqrt(K),
    se_within = sqrt(sum(se_link_WHO_MI_at183^2, na.rm = TRUE) / (K^2)),
    se_combined = sqrt(se_between^2 + se_within^2),
    ci_lower_link = mean_link - 1.96 * se_combined,
    ci_upper_link = mean_link + 1.96 * se_combined,
    mean_prev = plogis(mean_link),
    ci_lower = plogis(ci_lower_link),
    ci_upper = plogis(ci_upper_link)
  )

print(summary_average_WHO_MI_at183)

cat(
  "\nAverage prevalence (mean_prediction_WHO_MI_at183):",
  sprintf("%.3f", summary_average_WHO_MI_at183$mean_prev),
  "(95% CI:",
  sprintf("%.3f", summary_average_WHO_MI_at183$ci_lower),
  "-",
  sprintf("%.3f", summary_average_WHO_MI_at183$ci_upper),
  ")\n"
)

####################################################################

# ISOLATED DAYS-MEASUREMENT EFFECT: DHS reference definition (no MI, no
# prelacteal) at exactly 183 days, compared against the DHS reference
# definition under 6 months - both computed on this SAME 2015-2022 survey
# subsample, so the comparison isolates the effect of switching the age
# measurement from "under 6 months" to "exactly 183 days," without also
# mixing in a different set of surveys (agedays only exists from 2015
# onward, so a fair comparison must restrict the under-6-months estimate
# to the same surveys, not the full 2000-2022 set used in the main text).

####################################################################

# DHS reference - under 6 months

library(dplyr)
library(survey)

df_DHS_2015_under6 <- DHS.df %>%
  filter(
    !is.na(agemonths),
    !is.na(v005), v005 > 0,
    !is.na(survey)
  ) %>%
  mutate(survey_id = as.numeric(as.factor(survey)))

prev_calculator_DHS_2015_under6 <- function(df_in) {
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
    
    df_under6 <- df_survey %>% filter(agemonths < 6)
    if (nrow(df_under6) == 0) {
      cat("No <6 months data for survey", sid, "- skipping\n")
      next
    }
    
    n_events <- sum(df_under6$EBF_DHS == 1, na.rm = TRUE)
    
    if (n_events == 0) {
      cat("Complete separation (0% prevalence) for survey", sid, "- applying correction\n")
      n <- nrow(df_under6)
      prev_corrected <- 0.5 / n
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
    
    results_list[[length(results_list) + 1]] <- data.frame(
      survey = survey_name,
      survey_id = sid,
      n_under6 = nrow(df_under6),
      n_events_under6 = n_events,
      se_link_DHS_2015_under6 = se_link,
      fit_link_DHS_2015_under6 = fit_link
    )
  }
  
  bind_rows(results_list) %>%
    arrange(survey)
}

prevalence_DHS_2015_under6 <- prev_calculator_DHS_2015_under6(df_DHS_2015_under6)

print(prevalence_DHS_2015_under6)

summary_average_DHS_2015_under6 <- prevalence_DHS_2015_under6 %>%
  summarise(
    K = sum(!is.na(fit_link_DHS_2015_under6)),
    mean_prev_link = mean(fit_link_DHS_2015_under6, na.rm = TRUE),
    sd_between = sd(fit_link_DHS_2015_under6, na.rm = TRUE),
    se_between = sd_between / sqrt(K),
    se_within = sqrt(sum(se_link_DHS_2015_under6^2, na.rm = TRUE) / (K^2)),
    se_combined = sqrt(se_between^2 + se_within^2),
    ci_lower_link = mean_prev_link - 1.96 * se_combined,
    ci_upper_link = mean_prev_link + 1.96 * se_combined,
    mean_prev = plogis(mean_prev_link),
    ci_lower = plogis(ci_lower_link),
    ci_upper = plogis(ci_upper_link)
  )

print(summary_average_DHS_2015_under6)

cat(
  "\nAverage prevalence (DHS reference, under 6 months, 2015-2022 subsample):",
  sprintf("%.3f", summary_average_DHS_2015_under6$mean_prev),
  "(95% CI:",
  sprintf("%.3f", summary_average_DHS_2015_under6$ci_lower), "-",
  sprintf("%.3f", summary_average_DHS_2015_under6$ci_upper), ")\n"
)

####################################################################

# DHS reference - at exactly 183 days

library(dplyr)
library(survey)

df_DHS_at183 <- DHS.df %>%
  filter(
    !is.na(agedays),
    !is.na(v005), v005 > 0,
    !is.na(survey)
  ) %>%
  mutate(survey_id = as.numeric(as.factor(survey)))

prev_calculator_DHS_at183 <- function(df_in) {
  results_list <- list()
  unique_surveys <- unique(df_in$survey_id)
  
  for (sid in unique_surveys) {
    cat("Running model for survey", sid, "\n")
    
    df_survey <- df_in %>% filter(survey_id == sid, agedays < 365)
    
    if (nrow(df_survey) == 0) {
      cat("No data for survey", sid, "- skipping\n")
      next
    }
    
    dstrata <- svydesign(ids = ~1, weights = ~v005, data = df_survey, nest = TRUE)
    
    fit <- tryCatch({
      svyglm(
        EBF_DHS ~ agedays + agedays_sq + agedays_cub + agedays_quar,
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
    
    if (n_events == 0) {
      cat("Zero prevalence for survey", sid, "- applying Haldane correction\n")
      prev_corrected <- 0.5 / n
      ci_upper <- qbeta(0.975, 1, n)
      fit_link <- qlogis(prev_corrected)
      se_link <- (qlogis(ci_upper) - qlogis(prev_corrected)) / 1.96
    } else {
      newdata <- data.frame(
        agedays = 183,
        agedays_sq = 183^2,
        agedays_cub = 183^3,
        agedays_quar = 183^4
      )
      
      X_new <- model.matrix(
        ~ agedays + agedays_sq + agedays_cub + agedays_quar,
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
      n_under365 = n,
      n_events_under365 = n_events,
      se_link_DHS_at183 = se_link,
      fit_link_DHS_at183 = fit_link
    )
  }
  
  bind_rows(results_list) %>%
    arrange(survey)
}

prevalence_DHS_at183 <- prev_calculator_DHS_at183(df_DHS_at183)

print(prevalence_DHS_at183)

summary_average_DHS_at183 <- prevalence_DHS_at183 %>%
  summarise(
    K = sum(!is.na(fit_link_DHS_at183)),
    mean_prev_link = mean(fit_link_DHS_at183, na.rm = TRUE),
    sd_between = sd(fit_link_DHS_at183, na.rm = TRUE),
    se_between = sd_between / sqrt(K),
    se_within = sqrt(sum(se_link_DHS_at183^2, na.rm = TRUE) / (K^2)),
    se_combined = sqrt(se_between^2 + se_within^2),
    ci_lower_link = mean_prev_link - 1.96 * se_combined,
    ci_upper_link = mean_prev_link + 1.96 * se_combined,
    mean_prev = plogis(mean_prev_link),
    ci_lower = plogis(ci_lower_link),
    ci_upper = plogis(ci_upper_link)
  )

print(summary_average_DHS_at183)

cat(
  "\nAverage prevalence (DHS reference, at 183 days only, 2015-2022 subsample):",
  sprintf("%.3f", summary_average_DHS_at183$mean_prev),
  "(95% CI:",
  sprintf("%.3f", summary_average_DHS_at183$ci_lower), "-",
  sprintf("%.3f", summary_average_DHS_at183$ci_upper), ")\n"
)

####################################################################

# Formatting for paper figures

library(ggplot2)
library(grid)   # for unit()

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

# Supplementary Figure 2 - compare at 6 months vs at 183 days using DHS surveys 2015-2022
# (unchanged - kept as-is)

paired_df_183 <- prevalence_WHO_MI_at6 %>%
  transmute(
    survey,
    at6_pct = plogis(fit_link_WHO_MI_at6) * 100
  ) %>%
  inner_join(
    prevalence_WHO_MI_at183 %>%
      transmute(
        survey,
        at183_pct = plogis(fit_link_WHO_MI_at183) * 100
      ),
    by = "survey"
  )

p_s2 <- ggplot(paired_df_183, aes(x = at6_pct, y = at183_pct)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(color = "#00BFC4", size = 2.5, alpha = 0.85) +
  scale_x_continuous(limits = c(0, 60), breaks = seq(0, 60, 10)) +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, 10)) +
  labs(
    x = "Prevalence at 6 Months (%)",
    y = "Prevalence At 183 Days (%)",
    title = "Survey-level EBF prevalence: 6 months vs 183 days"
  ) +
  coord_equal() +
  theme_ebf()

print(p_s2)

####################################################################

# Table - export pooled survey prevalence

library(openxlsx)
library(dplyr)
library(tibble)

# Helper: survey-level export table
make_export_table <- function(data, fit_col, se_col,
                              survey_col = "survey",
                              survey_id_col = "survey_id",
                              n_col = NULL,
                              events_col = NULL) {
  
  # Start from original data and carry optional columns directly
  keep_cols <- c(survey_col, survey_id_col, fit_col, se_col)
  if (!is.null(n_col) && n_col %in% names(data)) keep_cols <- c(keep_cols, n_col)
  if (!is.null(events_col) && events_col %in% names(data)) keep_cols <- c(keep_cols, events_col)
  
  out <- data %>%
    select(all_of(keep_cols)) %>%
    transmute(
      survey = .data[[survey_col]],
      survey_id = .data[[survey_id_col]],
      n = if (!is.null(n_col) && n_col %in% names(cur_data())) .data[[n_col]] else NA_real_,
      n_events = if (!is.null(events_col) && events_col %in% names(cur_data())) .data[[events_col]] else NA_real_,
      pooled_fit_link = .data[[fit_col]],
      pooled_se_link = .data[[se_col]],
      pooled_prevalence = plogis(.data[[fit_col]]) * 100,
      lower_CI = plogis(.data[[fit_col]] - 1.96 * .data[[se_col]]) * 100,
      upper_CI = plogis(.data[[fit_col]] + 1.96 * .data[[se_col]]) * 100
    ) %>%
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
  
  # Remove n / n_events if they were not actually supplied
  if (is.null(n_col) || !(n_col %in% names(data))) {
    out <- out %>% select(-any_of("n"))
  }
  if (is.null(events_col) || !(events_col %in% names(data))) {
    out <- out %>% select(-any_of("n_events"))
  }
  
  out
}

# overall summary export table
make_overall_export <- function(summary_df, definition_name) {
  
  mean_link_val <- if ("mean_link" %in% names(summary_df)) {
    summary_df$mean_link
  } else {
    summary_df$mean_prev_link
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
sheet_WHO_MI_at6 <- make_export_table(
  data = prevalence_WHO_MI_at6,
  fit_col = "fit_link_WHO_MI_at6",
  se_col = "se_link_WHO_MI_at6",
  n_col = "n_under12",
  events_col = "n_events_under12"
)

sheet_WHO_MI_at183 <- make_export_table(
  data = prevalence_WHO_MI_at183,
  fit_col = "fit_link_WHO_MI_at183",
  se_col = "se_link_WHO_MI_at183",
  n_col = "n_under365",
  events_col = "n_events_under365"
)

sheet_DHS_2015_under6 <- make_export_table(
  data = prevalence_DHS_2015_under6,
  fit_col = "fit_link_DHS_2015_under6",
  se_col = "se_link_DHS_2015_under6",
  n_col = "n_under6",
  events_col = "n_events_under6"
)

sheet_DHS_at183 <- make_export_table(
  data = prevalence_DHS_at183,
  fit_col = "fit_link_DHS_at183",
  se_col = "se_link_DHS_at183",
  n_col = "n_under365",
  events_col = "n_events_under365"
)

# Build overall summary sheet
overall_summary <- bind_rows(
  make_overall_export(summary_average_WHO_MI_at6, "WHO_MI_at6_2015sample"),
  make_overall_export(summary_average_WHO_MI_at183, "WHO_MI_at183"),
  make_overall_export(summary_average_DHS_2015_under6, "DHS_under6_2015sample"),
  make_overall_export(summary_average_DHS_at183, "DHS_at183_only")
)

# Write everything to one Excel workbook
wb <- createWorkbook()

write_sheet_with_style(wb, "WHO_MI_at6_2015sample", sheet_WHO_MI_at6)
write_sheet_with_style(wb, "WHO_MI_at183", sheet_WHO_MI_at183)
write_sheet_with_style(wb, "DHS_under6_2015sample", sheet_DHS_2015_under6)
write_sheet_with_style(wb, "DHS_at183_only", sheet_DHS_at183)
write_sheet_with_style(wb, "overall_summary", overall_summary)

saveWorkbook(
  wb,
  file = "EBF_pooled_survey_prevalence_2015-2022.xlsx",
  overwrite = TRUE
)

####################################################################