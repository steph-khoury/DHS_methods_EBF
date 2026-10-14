# EBF Prevalence - Methods Paper
# Run with the working directory set to this folder (02_analysis/).
# All under-6-month estimates use simple weighted
# At-6-months estimates remain model-based (logistic regression)

####################################################################
# Load data

library(haven)
DHS_methods_analysis_2000_2022 <- read_dta("../data/restricted/DHS_methods_analysis_2000-2022.dta")

library(dplyr)
library(survey)
library(mice)
library(openxlsx)
library(tibble)
library(tidyr)

invlogit <- function(x) exp(x) / (1 + exp(x))

####################################################################
# simple weighted proportion per survey

simple_prev_by_survey <- function(df, outcome_var, weight_var = "v005",
                                  survey_var = "survey", survey_id_var = "survey_id",
                                  n_col = "n_under6", events_col = "n_events_under6") {
  results <- list()
  for (sid in unique(df[[survey_id_var]])) {
    df_s <- df[df[[survey_id_var]] == sid, ]
    if (nrow(df_s) == 0) next
    dstrata <- svydesign(ids = ~1, weights = as.formula(paste0("~", weight_var)),
                         data = df_s, nest = TRUE)
    est <- tryCatch(svymean(as.formula(paste0("~", outcome_var)), dstrata, na.rm = TRUE),
                    error = function(e) NULL)
    if (is.null(est)) next
    results[[length(results) + 1]] <- data.frame(
      survey      = as.character(df_s[[survey_var]][1]),
      survey_id   = sid,
      n           = nrow(df_s),
      n_events    = sum(df_s[[outcome_var]] == 1, na.rm = TRUE),
      prev        = as.numeric(coef(est)),
      se_prev     = as.numeric(SE(est))
    )
  }
  bind_rows(results) %>% arrange(survey)
}

# Pool simple averages across surveys
pool_simple <- function(prev_df) {
  prev_df %>%
    summarise(
      K          = n(),
      mean_prev  = mean(prev, na.rm = TRUE),
      sd_between = sd(prev, na.rm = TRUE),
      se_between = sd_between / sqrt(K),
      se_within  = sqrt(sum(se_prev^2, na.rm = TRUE) / (K^2)),
      se_combined = sqrt(se_between^2 + se_within^2),
      ci_lower   = mean_prev - 1.96 * se_combined,
      ci_upper   = mean_prev + 1.96 * se_combined
    )
}

# Pool simple averages across MI imputations then across surveys
pool_mi_simple <- function(imp_prev_list) {
  bind_rows(imp_prev_list) %>%
    group_by(survey, survey_id) %>%
    summarise(
      m          = n(),
      n          = mean(n),
      n_events   = mean(n_events),
      pooled_prev = mean(prev, na.rm = TRUE),
      u_bar      = mean(se_prev^2, na.rm = TRUE),
      b          = var(prev, na.rm = TRUE),
      b          = ifelse(is.na(b), 0, b),
      t_var      = u_bar + (1 + 1/m) * b,
      se_prev    = sqrt(t_var),
      .groups    = "drop"
    ) %>%
    rename(prev = pooled_prev) %>%
    arrange(survey)
}

####################################################################
# SECTION A: Missing-data handling - simple weighted proportions

DHS.df <- DHS_methods_analysis_2000_2022 %>%
  mutate(
    v005 = v005 / 1000000,
    survey_id = as.numeric(as.factor(survey))
  ) %>%
  filter(!is.na(agemonths), agemonths < 6, !is.na(v005), v005 > 0,
         !is.na(survey), !is.na(EBF_DHS))

# DHS simple average
prev_DHS_simple <- simple_prev_by_survey(DHS.df, "EBF_DHS")
summary_DHS_simple <- pool_simple(prev_DHS_simple)
cat("\nDHS simple average:", sprintf("%.3f (%.3f-%.3f)",
                                     summary_DHS_simple$mean_prev, summary_DHS_simple$ci_lower, summary_DHS_simple$ci_upper), "\n")

# UNICEF/WHO simple average
DHS_strict.df <- DHS_methods_analysis_2000_2022 %>%
  mutate(v005 = v005 / 1000000, survey_id = as.numeric(as.factor(survey))) %>%
  filter(!is.na(agemonths), agemonths < 6, !is.na(v005), v005 > 0,
         !is.na(survey), !is.na(EBF_strict))

prev_UNICEF_simple <- simple_prev_by_survey(DHS_strict.df, "EBF_strict")
summary_UNICEF_simple <- pool_simple(prev_UNICEF_simple)
cat("UNICEF/WHO simple average:", sprintf("%.3f (%.3f-%.3f)",
                                          summary_UNICEF_simple$mean_prev, summary_UNICEF_simple$ci_lower, summary_UNICEF_simple$ci_upper), "\n")

# CC simple average
DHS_CC.df <- DHS_methods_analysis_2000_2022 %>%
  mutate(v005 = v005 / 1000000, survey_id = as.numeric(as.factor(survey))) %>%
  filter(!is.na(agemonths), agemonths < 6, !is.na(v005), v005 > 0,
         !is.na(survey), !is.na(food), !is.na(liquid), !is.na(BF)) %>%
  mutate(EBF_CC = as.numeric(BF == 1 & food == 0 & liquid == 0))

prev_CC_simple <- simple_prev_by_survey(DHS_CC.df, "EBF_CC")
summary_CC_simple <- pool_simple(prev_CC_simple)
cat("CC simple average:", sprintf("%.3f (%.3f-%.3f)",
                                  summary_CC_simple$mean_prev, summary_CC_simple$ci_lower, summary_CC_simple$ci_upper), "\n")

# MI simple average - pool svymean across 20 imputed datasets
DHS_MI.df <- DHS_methods_analysis_2000_2022 %>%
  mutate(
    v005 = v005 / 1000000,
    agemonths_sq = agemonths^2, agemonths_cub = agemonths^3, agemonths_quar = agemonths^4
  )

imputation_vars_MI <- DHS_MI.df %>%
  select(agemonths, BF, v012, v025, b4, food, liquid, edu) %>%
  mutate(across(-c(agemonths, v012), factor))

imputed_data_MI <- mice(imputation_vars_MI, m = 20, method = "logreg",
                        seed = 1452, printFlag = FALSE)

add_var_MI <- DHS_MI.df %>% select(v005, survey)

imputed_dfs_MI <- lapply(1:imputed_data_MI$m, function(i) {
  complete(imputed_data_MI, i) %>%
    bind_cols(add_var_MI) %>%
    mutate(
      survey_id = as.numeric(as.factor(survey)),
      EBF_MI = as.numeric(BF == 1 & food == 0 & liquid == 0)
    ) %>%
    filter(!is.na(agemonths), agemonths < 6, !is.na(v005), v005 > 0, !is.na(survey))
})

prev_MI_simple_list <- lapply(imputed_dfs_MI, function(df)
  simple_prev_by_survey(df, "EBF_MI"))

prev_MI_simple <- pool_mi_simple(prev_MI_simple_list)
summary_MI_simple <- pool_simple(prev_MI_simple)
cat("MI simple average:", sprintf("%.3f (%.3f-%.3f)",
                                  summary_MI_simple$mean_prev, summary_MI_simple$ci_lower, summary_MI_simple$ci_upper), "\n")

####################################################################
# SECTION B: Independent effects - simple weighted proportions

# DHS + Prelacteal only
DHS_p.df <- DHS_methods_analysis_2000_2022 %>%
  mutate(v005 = v005 / 1000000, survey_id = as.numeric(as.factor(survey))) %>%
  filter(!is.na(agemonths), agemonths < 6, !is.na(v005), v005 > 0,
         !is.na(survey), !is.na(EBF_DHS_p))

prev_DHS_p_simple <- simple_prev_by_survey(DHS_p.df, "EBF_DHS_p")
summary_DHS_p_simple <- pool_simple(prev_DHS_p_simple)
cat("\nDHS + Prelacteal only:", sprintf("%.3f (%.3f-%.3f)",
                                        summary_DHS_p_simple$mean_prev, summary_DHS_p_simple$ci_lower, summary_DHS_p_simple$ci_upper), "\n")

# DHS + Age-weighting only - simple weighted proportion with age-adjusted weights
DHS_agewt.df <- DHS_methods_analysis_2000_2022 %>%
  mutate(
    v005_s = v005 / 1000000,
    survey_id = as.numeric(as.factor(survey)),
    agegrp = as.integer(agemonths)
  ) %>%
  filter(!is.na(agemonths), agemonths < 6, !is.na(v005_s), v005_s > 0, !is.na(survey))

age_mult_0_5 <- DHS_agewt.df %>%
  group_by(survey, agegrp) %>%
  summarise(w_sum = sum(v005_s, na.rm = TRUE), .groups = "drop") %>%
  group_by(survey) %>%
  mutate(K = n(), target = sum(w_sum) / K, w_age_mult = target / w_sum) %>%
  ungroup() %>%
  select(survey, agegrp, w_age_mult)

DHS_agewt.df <- DHS_agewt.df %>%
  left_join(age_mult_0_5, by = c("survey", "agegrp")) %>%
  mutate(
    w_age_mult = ifelse(is.na(w_age_mult), 1, w_age_mult),
    w_final = v005_s * w_age_mult
  )

prev_DHS_agewt_simple <- simple_prev_by_survey(DHS_agewt.df, "EBF_DHS",
                                               weight_var = "w_final")
summary_DHS_agewt_simple <- pool_simple(prev_DHS_agewt_simple)
cat("DHS + Age-weighting only:", sprintf("%.3f (%.3f-%.3f)",
                                         summary_DHS_agewt_simple$mean_prev, summary_DHS_agewt_simple$ci_lower,
                                         summary_DHS_agewt_simple$ci_upper), "\n")

# DHS + At 6 months - MODEL BASED (cannot do simple average at a point)
DHS_at6.df <- DHS_methods_analysis_2000_2022 %>%
  mutate(
    v005 = v005 / 1000000,
    agemonths_sq = agemonths^2, agemonths_cub = agemonths^3, agemonths_quar = agemonths^4,
    survey_id = as.numeric(as.factor(survey))
  ) %>%
  filter(!is.na(agemonths), !is.na(v005), v005 > 0, !is.na(survey))

prev_DHS_at6_list <- list()
for (sid in unique(DHS_at6.df$survey_id)) {
  df_s <- DHS_at6.df %>% filter(survey_id == sid, agemonths < 12)
  if (nrow(df_s) == 0) next
  dstrata <- svydesign(ids = ~1, weights = ~v005, data = df_s, nest = TRUE)
  fit <- tryCatch(svyglm(EBF_DHS ~ agemonths + agemonths_sq + agemonths_cub + agemonths_quar,
                         design = dstrata, family = quasibinomial()), error = function(e) NULL)
  if (is.null(fit)) next
  if (any(!is.finite(coef(fit))) || any(is.na(coef(fit)))) next
  n_events <- sum(df_s$EBF_DHS == 1, na.rm = TRUE)
  if (n_events == 0) {
    n <- nrow(df_s)
    fit_link <- qlogis(0.5/n)
    se_link <- (qlogis(qbeta(0.975,1,n)) - fit_link) / 1.96
  } else {
    nd <- data.frame(agemonths=6, agemonths_sq=36, agemonths_cub=216, agemonths_quar=1296)
    X <- model.matrix(~agemonths+agemonths_sq+agemonths_cub+agemonths_quar, nd)
    V <- vcov(fit)
    if (any(!is.finite(V))) next
    fit_link <- as.numeric(X %*% coef(fit))
    se_link <- sqrt(as.numeric(X %*% V %*% t(X)))
  }
  prev_DHS_at6_list[[length(prev_DHS_at6_list)+1]] <- data.frame(
    survey = as.character(df_s$survey[1]), survey_id = sid,
    fit_link_DHS_at6 = fit_link, se_link_DHS_at6 = se_link)
}
prev_DHS_at6 <- bind_rows(prev_DHS_at6_list) %>% arrange(survey)
summary_DHS_at6 <- prev_DHS_at6 %>%
  summarise(
    K = n(), mean_prev_link = mean(fit_link_DHS_at6, na.rm=TRUE),
    sd_between = sd(fit_link_DHS_at6, na.rm=TRUE), se_between = sd_between/sqrt(K),
    se_within = sqrt(sum(se_link_DHS_at6^2, na.rm=TRUE)/(K^2)),
    se_combined = sqrt(se_between^2 + se_within^2),
    mean_prev = invlogit(mean_prev_link),
    ci_lower = invlogit(mean_prev_link - 1.96*se_combined),
    ci_upper = invlogit(mean_prev_link + 1.96*se_combined)
  )
cat("DHS + At 6 months (model-based):", sprintf("%.3f (%.3f-%.3f)",
                                                summary_DHS_at6$mean_prev, summary_DHS_at6$ci_lower, summary_DHS_at6$ci_upper), "\n")

####################################################################
# SECTION C: Cumulative without prelacteal - simple weighted proportions

# MI + Age-weighting (no prelacteal) - simple proportion with age weights

imputed_dfs_MI_agewt_nop <- lapply(1:imputed_data_MI$m, function(i) {
  complete(imputed_data_MI, i) %>%
    bind_cols(add_var_MI %>% rename(v005_s = v005)) %>%
    mutate(
      survey_id = as.numeric(as.factor(survey)),
      agegrp = as.integer(agemonths),
      EBF_MI_nop = as.numeric(BF == 1 & food == 0 & liquid == 0)
    ) %>%
    filter(!is.na(agemonths), agemonths < 6, !is.na(v005_s), v005_s > 0, !is.na(survey)) %>%
    left_join(age_mult_0_5, by = c("survey", "agegrp")) %>%
    mutate(
      w_age_mult = ifelse(is.na(w_age_mult), 1, w_age_mult),
      w_final = v005_s * w_age_mult
    )
})

prev_MI_agewt_nop_list <- lapply(imputed_dfs_MI_agewt_nop, function(df)
  simple_prev_by_survey(df, "EBF_MI_nop", weight_var = "w_final"))
prev_MI_agewt_nop <- pool_mi_simple(prev_MI_agewt_nop_list)
summary_MI_agewt_nop <- pool_simple(prev_MI_agewt_nop)
cat("\nMI + Age-weighting (no prelacteal):", sprintf("%.3f (%.3f-%.3f)",
                                                     summary_MI_agewt_nop$mean_prev, summary_MI_agewt_nop$ci_lower, summary_MI_agewt_nop$ci_upper), "\n")

# MI + At 6 months (no prelacteal) - MODEL BASED
DHS_MI_at6.df <- DHS_methods_analysis_2000_2022 %>%
  mutate(
    v005 = v005 / 1000000,
    agemonths_sq = agemonths^2, agemonths_cub = agemonths^3, agemonths_quar = agemonths^4
  )
add_var_MI_at6 <- DHS_MI_at6.df %>% select(v005, survey, agemonths_sq, agemonths_cub, agemonths_quar)

imputed_dfs_MI_at6_nop <- lapply(1:imputed_data_MI$m, function(i) {
  temp.df <- complete(imputed_data_MI, i) %>%
    bind_cols(add_var_MI_at6) %>%
    mutate(EBF_WHO_MI = case_when(BF==0~0, food==1~0, liquid==1~0, TRUE~1))
  temp.df
})

prev_MI_at6_nop_list <- list()
for (i in seq_along(imputed_dfs_MI_at6_nop)) {
  df_imp <- imputed_dfs_MI_at6_nop[[i]] %>%
    filter(!is.na(agemonths), !is.na(v005), v005>0, !is.na(survey)) %>%
    mutate(survey_id = as.numeric(as.factor(survey)))
  res <- list()
  for (sid in unique(df_imp$survey_id)) {
    df_s <- df_imp %>% filter(survey_id==sid, agemonths<12)
    if (nrow(df_s)==0) next
    dstrata <- svydesign(ids=~1, weights=~v005, data=df_s, nest=TRUE)
    fit <- tryCatch(svyglm(EBF_WHO_MI~agemonths+agemonths_sq+agemonths_cub+agemonths_quar,
                           design=dstrata, family=quasibinomial()), error=function(e) NULL)
    if (is.null(fit)) next
    if (any(!is.finite(coef(fit)))||any(is.na(coef(fit)))) next
    n_events <- sum(df_s$EBF_WHO_MI==1, na.rm=TRUE)
    if (n_events==0) {
      fit_link <- qlogis(0.5/nrow(df_s))
      se_link <- (qlogis(qbeta(0.975,1,nrow(df_s)))-fit_link)/1.96
    } else {
      nd <- data.frame(agemonths=6, agemonths_sq=36, agemonths_cub=216, agemonths_quar=1296)
      X <- model.matrix(~agemonths+agemonths_sq+agemonths_cub+agemonths_quar, nd)
      V <- vcov(fit)
      if (any(!is.finite(V))) next
      fit_link <- as.numeric(X %*% coef(fit))
      se_link <- sqrt(as.numeric(X %*% V %*% t(X)))
    }
    res[[length(res)+1]] <- data.frame(
      imputation_id=i, survey=as.character(df_s$survey[1]), survey_id=sid,
      fit_link_MI_at6_nop=fit_link, se_link_MI_at6_nop=se_link)
  }
  prev_MI_at6_nop_list[[i]] <- bind_rows(res)
}
prev_MI_at6_nop <- bind_rows(prev_MI_at6_nop_list) %>%
  group_by(survey, survey_id) %>%
  summarise(
    m=n(), fit_link_MI_at6_nop=mean(fit_link_MI_at6_nop, na.rm=TRUE),
    u_bar=mean(se_link_MI_at6_nop^2, na.rm=TRUE), b=var(fit_link_MI_at6_nop, na.rm=TRUE),
    b=ifelse(is.na(b),0,b), t_var=u_bar+(1+1/m)*b,
    se_link_MI_at6_nop=sqrt(t_var), .groups="drop") %>%
  arrange(survey)
summary_MI_at6_nop <- prev_MI_at6_nop %>%
  summarise(
    K=n(), mean_link=mean(fit_link_MI_at6_nop, na.rm=TRUE),
    sd_between=sd(fit_link_MI_at6_nop, na.rm=TRUE), se_between=sd_between/sqrt(K),
    se_within=sqrt(sum(se_link_MI_at6_nop^2, na.rm=TRUE)/(K^2)),
    se_combined=sqrt(se_between^2+se_within^2),
    mean_prev=plogis(mean_link),
    ci_lower=plogis(mean_link-1.96*se_combined), ci_upper=plogis(mean_link+1.96*se_combined))
cat("MI + At 6 months (no prelacteal, model-based):", sprintf("%.3f (%.3f-%.3f)",
                                                              summary_MI_at6_nop$mean_prev, summary_MI_at6_nop$ci_lower, summary_MI_at6_nop$ci_upper), "\n")

####################################################################
# SECTION D: Cumulative with prelacteal - simple weighted proportions

# MI-Prelacteal imputed datasets
imputation_vars_WHO_MI <- DHS_MI.df %>%
  select(agemonths, BF, v012, v025, b4, food, liquid, prelacteal, edu) %>%
  mutate(across(-c(agemonths, v012), factor))

imputed_data_WHO_MI <- mice(imputation_vars_WHO_MI, m=20, method="logreg",
                            seed=1452, printFlag=FALSE)

add_var_WHO_MI <- DHS_MI.df %>% select(v005, survey)

# MI-Prelacteal under 6 months - simple weighted proportion
imputed_dfs_WHO_MI <- lapply(1:imputed_data_WHO_MI$m, function(i) {
  complete(imputed_data_WHO_MI, i) %>%
    bind_cols(add_var_WHO_MI) %>%
    mutate(
      survey_id = as.numeric(as.factor(survey)),
      EBF_WHO_MI = as.numeric(BF==1 & food==0 & liquid==0 & prelacteal==0)
    ) %>%
    filter(!is.na(agemonths), agemonths<6, !is.na(v005), v005>0, !is.na(survey))
})

prev_WHO_MI_list <- lapply(imputed_dfs_WHO_MI, function(df)
  simple_prev_by_survey(df, "EBF_WHO_MI"))
prev_WHO_MI_simple <- pool_mi_simple(prev_WHO_MI_list)
summary_WHO_MI_simple <- pool_simple(prev_WHO_MI_simple)
cat("\nMI-Prelacteal simple average:", sprintf("%.3f (%.3f-%.3f)",
                                               summary_WHO_MI_simple$mean_prev, summary_WHO_MI_simple$ci_lower,
                                               summary_WHO_MI_simple$ci_upper), "\n")

# MI-Prelacteal + Age-weighting - simple weighted proportion with age weights
DHS_WHO_MI_agewt.df <- DHS_methods_analysis_2000_2022 %>%
  mutate(v005_s = v005/1000000, agegrp = as.integer(agemonths))

imputed_dfs_WHO_MI_agewt <- lapply(1:imputed_data_WHO_MI$m, function(i) {
  complete(imputed_data_WHO_MI, i) %>%
    bind_cols(add_var_WHO_MI %>% rename(v005_s = v005)) %>%
    mutate(
      survey_id = as.numeric(as.factor(survey)),
      agegrp = as.integer(agemonths),
      EBF_WHO_MI = as.numeric(BF==1 & food==0 & liquid==0 & prelacteal==0)
    ) %>%
    filter(!is.na(agemonths), agemonths<6, !is.na(v005_s), v005_s>0, !is.na(survey)) %>%
    left_join(age_mult_0_5, by=c("survey","agegrp")) %>%
    mutate(w_age_mult=ifelse(is.na(w_age_mult),1,w_age_mult), w_final=v005_s*w_age_mult)
})

prev_WHO_MI_agewt_list <- lapply(imputed_dfs_WHO_MI_agewt, function(df)
  simple_prev_by_survey(df, "EBF_WHO_MI", weight_var="w_final"))
prev_WHO_MI_agewt_simple <- pool_mi_simple(prev_WHO_MI_agewt_list)
summary_WHO_MI_agewt_simple <- pool_simple(prev_WHO_MI_agewt_simple)
cat("MI-Prelacteal + Age-weighting simple average:", sprintf("%.3f (%.3f-%.3f)",
                                                             summary_WHO_MI_agewt_simple$mean_prev, summary_WHO_MI_agewt_simple$ci_lower,
                                                             summary_WHO_MI_agewt_simple$ci_upper), "\n")

# MI-Prelacteal + At 6 months - MODEL BASED
DHS_MI_WHO_df <- DHS_methods_analysis_2000_2022 %>%
  mutate(
    v005 = v005/1000000,
    agemonths_sq = agemonths^2, agemonths_cub = agemonths^3, agemonths_quar = agemonths^4
  )
add_var_WHO_at6 <- DHS_MI_WHO_df %>% select(v005, survey, agemonths_sq, agemonths_cub, agemonths_quar)

imputed_dfs_WHO_MI_at6 <- lapply(1:imputed_data_WHO_MI$m, function(i) {
  complete(imputed_data_WHO_MI, i) %>%
    bind_cols(add_var_WHO_at6) %>%
    mutate(EBF_WHO_MI = case_when(BF==0~0, food==1~0, liquid==1~0, prelacteal==1~0, TRUE~1))
})

prev_WHO_MI_at6_list <- list()
for (i in seq_along(imputed_dfs_WHO_MI_at6)) {
  df_imp <- imputed_dfs_WHO_MI_at6[[i]] %>%
    filter(!is.na(agemonths), !is.na(v005), v005>0, !is.na(survey)) %>%
    mutate(survey_id = as.numeric(as.factor(survey)))
  res <- list()
  for (sid in unique(df_imp$survey_id)) {
    df_s <- df_imp %>% filter(survey_id==sid, agemonths<12)
    if (nrow(df_s)==0) next
    dstrata <- svydesign(ids=~1, weights=~v005, data=df_s, nest=TRUE)
    fit <- tryCatch(svyglm(EBF_WHO_MI~agemonths+agemonths_sq+agemonths_cub+agemonths_quar,
                           design=dstrata, family=quasibinomial()), error=function(e) NULL)
    if (is.null(fit)) next
    if (any(!is.finite(coef(fit)))||any(is.na(coef(fit)))) next
    n_events <- sum(df_s$EBF_WHO_MI==1, na.rm=TRUE)
    if (n_events==0) {
      fit_link <- qlogis(0.5/nrow(df_s))
      se_link <- (qlogis(qbeta(0.975,1,nrow(df_s)))-fit_link)/1.96
    } else {
      nd <- data.frame(agemonths=6, agemonths_sq=36, agemonths_cub=216, agemonths_quar=1296)
      X <- model.matrix(~agemonths+agemonths_sq+agemonths_cub+agemonths_quar, nd)
      V <- vcov(fit)
      if (any(!is.finite(V))) next
      fit_link <- as.numeric(X %*% coef(fit))
      se_link <- sqrt(as.numeric(X %*% V %*% t(X)))
    }
    res[[length(res)+1]] <- data.frame(
      imputation_id=i, survey=as.character(df_s$survey[1]), survey_id=sid,
      fit_link_WHO_MI_at6=fit_link, se_link_WHO_MI_at6=se_link)
  }
  prev_WHO_MI_at6_list[[i]] <- bind_rows(res)
}
prev_WHO_MI_at6 <- bind_rows(prev_WHO_MI_at6_list) %>%
  group_by(survey, survey_id) %>%
  summarise(
    m=n(), fit_link_WHO_MI_at6=mean(fit_link_WHO_MI_at6, na.rm=TRUE),
    u_bar=mean(se_link_WHO_MI_at6^2, na.rm=TRUE), b=var(fit_link_WHO_MI_at6, na.rm=TRUE),
    b=ifelse(is.na(b),0,b), t_var=u_bar+(1+1/m)*b,
    se_link_WHO_MI_at6=sqrt(t_var), .groups="drop") %>%
  arrange(survey)
summary_WHO_MI_at6 <- prev_WHO_MI_at6 %>%
  summarise(
    K=n(), mean_link=mean(fit_link_WHO_MI_at6, na.rm=TRUE),
    sd_between=sd(fit_link_WHO_MI_at6, na.rm=TRUE), se_between=sd_between/sqrt(K),
    se_within=sqrt(sum(se_link_WHO_MI_at6^2, na.rm=TRUE)/(K^2)),
    se_combined=sqrt(se_between^2+se_within^2),
    mean_prev=plogis(mean_link),
    ci_lower=plogis(mean_link-1.96*se_combined), ci_upper=plogis(mean_link+1.96*se_combined))
cat("MI-Prelacteal + At 6 months (model-based):", sprintf("%.3f (%.3f-%.3f)",
                                                          summary_WHO_MI_at6$mean_prev, summary_WHO_MI_at6$ci_lower, summary_WHO_MI_at6$ci_upper), "\n")

####################################################################
# TABLES AND FIGURES USING THE UPDATED SIMPLE-AVERAGE OBJECTS

library(ggplot2)
library(grid)

# Output folders. Change output_dir if you want the results saved elsewhere.
output_dir <- "EBF_methods_outputs"
table_dir  <- file.path(output_dir, "tables")
figure_dir <- file.path(output_dir, "figures")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

####################################################################
# TABLE 1 - pooled summary across surveys

make_summary_row <- function(summary_df, category, definition) {
  prevalence <- summary_df$mean_prev * 100
  ci_lower   <- pmax(summary_df$ci_lower, 0) * 100
  ci_upper   <- pmin(summary_df$ci_upper, 1) * 100
  
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
  make_summary_row(summary_DHS_simple,       "A. Missing-data handling", "DHS (missing = still EBF) [reference]"),
  make_summary_row(summary_UNICEF_simple,    "A. Missing-data handling", "UNICEF/WHO (missing = not EBF)"),
  make_summary_row(summary_CC_simple,        "A. Missing-data handling", "Complete case"),
  make_summary_row(summary_MI_simple,        "A. Missing-data handling", "Multiple imputation (MI)"),
  
  make_summary_row(summary_DHS_p_simple,     "B. Independent effect", "DHS + Prelacteal only"),
  make_summary_row(summary_DHS_agewt_simple, "B. Independent effect", "DHS + Age-weighting only"),
  make_summary_row(summary_DHS_at6,          "B. Independent effect", "DHS + At-6-months only*"),
  
  make_summary_row(summary_MI_agewt_nop,     "C. Cumulative effect without Prelacteal", "MI + Age-weighting"),
  make_summary_row(summary_MI_at6_nop,       "C. Cumulative effect without Prelacteal", "MI + At-6-months*"),
  
  make_summary_row(summary_WHO_MI_simple,       "D. Cumulative effect with Prelacteal", "MI-Prelacteal"),
  make_summary_row(summary_WHO_MI_agewt_simple, "D. Cumulative effect with Prelacteal", "MI-Prelacteal + Age-weighting"),
  make_summary_row(summary_WHO_MI_at6,          "D. Cumulative effect with Prelacteal", "MI-Prelacteal + At-6-months*")
) %>%
  select(category, definition, n_surveys, prevalence_CI,
         prevalence, ci_lower, ci_upper)

dhs_reference_prev <- summary_table_main %>%
  filter(definition == "DHS (missing = still EBF) [reference]") %>%
  pull(prevalence)

summary_table_diff <- summary_table_main %>%
  mutate(diff_from_DHS = prevalence - dhs_reference_prev) %>%
  select(category, definition, n_surveys, prevalence_CI,
         prevalence, diff_from_DHS)

print(summary_table_main, n = Inf)
print(summary_table_diff, n = Inf)

####################################################################
# Survey-level tables

make_simple_export_table <- function(data) {
  data %>%
    transmute(
      survey,
      survey_id,
      n,
      n_events,
      pooled_prevalence = prev * 100,
      lower_CI = pmax(prev - 1.96 * se_prev, 0) * 100,
      upper_CI = pmin(prev + 1.96 * se_prev, 1) * 100,
      pooled_se = se_prev * 100
    ) %>%
    mutate(
      prevalence_CI = sprintf("%.1f (%.1f-%.1f)",
                              pooled_prevalence, lower_CI, upper_CI)
    ) %>%
    select(survey, survey_id, n, n_events, prevalence_CI,
           pooled_prevalence, lower_CI, upper_CI, pooled_se) %>%
    arrange(survey)
}

make_logit_export_table <- function(data, fit_col, se_col) {
  data %>%
    transmute(
      survey,
      survey_id,
      pooled_fit_link = .data[[fit_col]],
      pooled_se_link = .data[[se_col]],
      pooled_prevalence = plogis(.data[[fit_col]]) * 100,
      lower_CI = plogis(.data[[fit_col]] - 1.96 * .data[[se_col]]) * 100,
      upper_CI = plogis(.data[[fit_col]] + 1.96 * .data[[se_col]]) * 100
    ) %>%
    mutate(
      prevalence_CI = sprintf("%.1f (%.1f-%.1f)",
                              pooled_prevalence, lower_CI, upper_CI)
    ) %>%
    select(survey, survey_id, prevalence_CI, pooled_prevalence,
           lower_CI, upper_CI, pooled_fit_link, pooled_se_link) %>%
    arrange(survey)
}

sheet_DHS_under6             <- make_simple_export_table(prev_DHS_simple)
sheet_UNICEF_under6          <- make_simple_export_table(prev_UNICEF_simple)
sheet_DHS_CC_under6          <- make_simple_export_table(prev_CC_simple)
sheet_DHS_MI_under6          <- make_simple_export_table(prev_MI_simple)
sheet_DHS_p_under6           <- make_simple_export_table(prev_DHS_p_simple)
sheet_DHS_agewt_under6       <- make_simple_export_table(prev_DHS_agewt_simple)
sheet_MI_agewt_nop_under6    <- make_simple_export_table(prev_MI_agewt_nop)
sheet_WHO_MI_under6          <- make_simple_export_table(prev_WHO_MI_simple)
sheet_WHO_MI_agewt_under6    <- make_simple_export_table(prev_WHO_MI_agewt_simple)

sheet_DHS_at6 <- make_logit_export_table(
  prev_DHS_at6, "fit_link_DHS_at6", "se_link_DHS_at6"
)
sheet_MI_at6_nop <- make_logit_export_table(
  prev_MI_at6_nop, "fit_link_MI_at6_nop", "se_link_MI_at6_nop"
)
sheet_WHO_MI_at6 <- make_logit_export_table(
  prev_WHO_MI_at6, "fit_link_WHO_MI_at6", "se_link_WHO_MI_at6"
)

write_sheet_with_style <- function(wb, sheet_name, df) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet = sheet_name, x = df)
  header_style <- createStyle(
    textDecoration = "bold", halign = "center", border = "Bottom"
  )
  addStyle(
    wb, sheet = sheet_name, style = header_style,
    rows = 1, cols = 1:ncol(df), gridExpand = TRUE
  )
  freezePane(wb, sheet = sheet_name, firstRow = TRUE)
  setColWidths(wb, sheet = sheet_name, cols = 1:ncol(df), widths = "auto")
}

wb <- createWorkbook()
write_sheet_with_style(wb, "Table1_summary", summary_table_main)
write_sheet_with_style(wb, "Table1_diff_from_DHS", summary_table_diff)
write_sheet_with_style(wb, "DHS_under6", sheet_DHS_under6)
write_sheet_with_style(wb, "UNICEF_under6", sheet_UNICEF_under6)
write_sheet_with_style(wb, "DHS_CC_under6", sheet_DHS_CC_under6)
write_sheet_with_style(wb, "DHS_MI_under6", sheet_DHS_MI_under6)
write_sheet_with_style(wb, "DHS_prelacteal_under6", sheet_DHS_p_under6)
write_sheet_with_style(wb, "DHS_agewt_under6", sheet_DHS_agewt_under6)
write_sheet_with_style(wb, "DHS_at6", sheet_DHS_at6)
write_sheet_with_style(wb, "MI_agewt_no_prelacteal", sheet_MI_agewt_nop_under6)
write_sheet_with_style(wb, "MI_at6_no_prelacteal", sheet_MI_at6_nop)
write_sheet_with_style(wb, "WHO_MI_under6", sheet_WHO_MI_under6)
write_sheet_with_style(wb, "WHO_MI_agewt_under6", sheet_WHO_MI_agewt_under6)
write_sheet_with_style(wb, "WHO_MI_at6", sheet_WHO_MI_at6)

excel_file <- file.path(table_dir, "EBF_pooled_survey_prevalences_simple_averages.xlsx")
saveWorkbook(wb, file = excel_file, overwrite = TRUE)

####################################################################
# Shared formatting for all figures

theme_ebf <- function() {
  theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(
        size = 14, face = "bold", hjust = 0.5,
        margin = margin(b = 10)
      ),
      axis.title.x = element_text(
        size = 14, face = "bold", margin = margin(t = 12)
      ),
      axis.title.y = element_text(
        size = 14, face = "bold", margin = margin(r = 12)
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
# Figure 3 - missing-data handling

figure_3_bar <- bind_rows(
  make_summary_row(summary_DHS_simple,    "", "DHS") %>%
    transmute(definition, prevalence, ci_lower, ci_upper),
  make_summary_row(summary_UNICEF_simple, "", "UNICEF/WHO") %>%
    transmute(definition, prevalence, ci_lower, ci_upper),
  make_summary_row(summary_CC_simple,     "", "CC") %>%
    transmute(definition, prevalence, ci_lower, ci_upper),
  make_summary_row(summary_MI_simple,     "", "MI") %>%
    transmute(definition, prevalence, ci_lower, ci_upper)
) %>%
  mutate(definition = factor(definition,
                             levels = c("DHS", "UNICEF/WHO", "CC", "MI")))

p3 <- ggplot(figure_3_bar, aes(x = definition, y = prevalence, fill = definition)) +
  geom_col(width = 0.7, alpha = 0.85) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.15, linewidth = 0.6) +
  labs(
    title = "Exclusive Breastfeeding Prevalence\n(<6 months of age)",
    x = "EBF Definition",
    y = "Average Prevalence (%)"
  ) +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, 10),
                     expand = expansion(mult = c(0, 0.02))) +
  theme_ebf() +
  theme(legend.position = "none")

####################################################################
# Figure 4 - survey distribution: MI vs MI-Prelacteal

figure_4 <- bind_rows(
  prev_MI_simple %>%
    transmute(survey, definition = "MI", prevalence = prev * 100),
  prev_WHO_MI_simple %>%
    transmute(survey, definition = "MI-Prelacteal", prevalence = prev * 100)
) %>%
  mutate(definition = factor(definition, levels = c("MI", "MI-Prelacteal")))

p4 <- ggplot(figure_4, aes(x = prevalence, fill = definition)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Exclusive Breastfeeding Prevalence\n(<6 months of age)",
    x = "Prevalence (%)",
    y = "Density",
    fill = "EBF Definition"
  ) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  theme_ebf() +
  theme(legend.position = "right")

####################################################################
# Supplementary Figure 1 - age-ratio distribution

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
  pivot_wider(names_from = age_group, values_from = n, values_fill = 0) %>%
  mutate(ratio_01_45 = ifelse(`4_5` > 0, `0_1` / `4_5`, NA_real_)) %>%
  filter(!is.na(ratio_01_45))

p_s1 <- ggplot(ratio_tbl, aes(x = ratio_01_45)) +
  geom_histogram(binwidth = 0.05, fill = "#00BFC4",
                 color = "black", alpha = 0.85) +
  geom_vline(xintercept = 1, color = "red", linetype = "dashed") +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  labs(
    title = "Distribution of age ratios across surveys",
    x = "Ratio: children 0-1 / 4-5 months",
    y = "Number of surveys"
  ) +
  theme_ebf()

ratio_overall_2000_2022 <- with(
  DHS_methods_analysis_2000_2022,
  sum(agemonths <= 1, na.rm = TRUE) /
    sum(agemonths >= 4 & agemonths <= 5, na.rm = TRUE)
)

####################################################################
# Figure 5 - WHO-MI under 6 months: standard vs age-weighted

paired_df_5 <- prev_WHO_MI_simple %>%
  transmute(survey, who_mi_under6_pct = prev * 100) %>%
  inner_join(
    prev_WHO_MI_agewt_simple %>%
      transmute(survey, who_mi_under6_agewt_pct = prev * 100),
    by = "survey"
  )

p5 <- ggplot(paired_df_5,
             aes(x = who_mi_under6_agewt_pct, y = who_mi_under6_pct)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(color = "#00BFC4", size = 2.5, alpha = 0.85) +
  scale_x_continuous(breaks = seq(0, 100, 20)) +
  scale_y_continuous(breaks = seq(0, 100, 20)) +
  coord_equal(xlim = c(0, 100), ylim = c(0, 100)) +
  labs(
    x = "Prevalence Under 6 Months (Age-Weighted) (%)",
    y = "Prevalence Under 6 Months (%)",
    title = "Survey-level EBF prevalence: age-weighted vs standard under-6 estimate"
  ) +
  theme_ebf()

####################################################################
# Figure 6 - WHO-MI age-weighted under 6 months vs exact 6 months

paired_df_6 <- prev_WHO_MI_agewt_simple %>%
  transmute(survey, under6_pct = prev * 100) %>%
  inner_join(
    prev_WHO_MI_at6 %>%
      transmute(survey, at6_pct = plogis(fit_link_WHO_MI_at6) * 100),
    by = "survey"
  )

p6 <- ggplot(paired_df_6, aes(x = under6_pct, y = at6_pct)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(color = "#00BFC4", size = 2.5, alpha = 0.85) +
  scale_x_continuous(breaks = seq(0, 100, 20)) +
  scale_y_continuous(breaks = seq(0, 100, 20)) +
  coord_equal(xlim = c(0, 100), ylim = c(0, 100)) +
  labs(
    x = "Prevalence Under 6 Months (Age-Weighted) (%)",
    y = "Prevalence At 6 Months (%)",
    title = "Survey-level EBF prevalence: under 6 months vs at 6 months"
  ) +
  theme_ebf()

####################################################################
# Figure 7 - cumulative methodological changes

fig7 <- bind_rows(
  make_summary_row(summary_DHS_simple, "", "DHS\nUnder 6M") %>%
    transmute(Definition = definition, Prev = prevalence, L = ci_lower, U = ci_upper),
  make_summary_row(summary_MI_simple, "", "MI\nUnder 6M") %>%
    transmute(Definition = definition, Prev = prevalence, L = ci_lower, U = ci_upper),
  make_summary_row(summary_WHO_MI_simple, "", "MI-Prelacteal\nUnder 6M") %>%
    transmute(Definition = definition, Prev = prevalence, L = ci_lower, U = ci_upper),
  make_summary_row(summary_WHO_MI_agewt_simple, "", "MI-Prelacteal\n(age-weighted)\nUnder 6M") %>%
    transmute(Definition = definition, Prev = prevalence, L = ci_lower, U = ci_upper),
  make_summary_row(summary_WHO_MI_at6, "", "MI-Prelacteal\nAt 6M") %>%
    transmute(Definition = definition, Prev = prevalence, L = ci_lower, U = ci_upper)
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
  geom_col(width = 0.7, alpha = 0.85) +
  geom_errorbar(aes(ymin = L, ymax = U), width = 0.15, linewidth = 0.6) +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, 10),
                     expand = expansion(mult = c(0, 0.05))) +
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
    axis.text.x = element_text(size = 11, lineheight = 0.9, hjust = 0.5)
  )

####################################################################
# Display figures in the R plotting pane

print(p3)
print(p4)
print(p_s1)
print(p5)
print(p6)
print(p7)

####################################################################
# Export all figures
# Each figure is saved as high-resolution PNG and TIFF, plus a vector PDF.

save_figure <- function(plot_object, filename, width, height, dpi = 600) {
  ggsave(
    filename = file.path(figure_dir, paste0(filename, ".png")),
    plot = plot_object, width = width, height = height,
    units = "in", dpi = dpi, bg = "white"
  )
  ggsave(
    filename = file.path(figure_dir, paste0(filename, ".tiff")),
    plot = plot_object, width = width, height = height,
    units = "in", dpi = dpi, compression = "lzw", bg = "white"
  )
  ggsave(
    filename = file.path(figure_dir, paste0(filename, ".pdf")),
    plot = plot_object, width = width, height = height,
    units = "in", bg = "white"
  )
}

save_figure(p3,   "Figure_3_missing_data_handling",      width = 7.0, height = 5.5)
save_figure(p4,   "Figure_4_MI_vs_MI_prelacteal",       width = 7.5, height = 5.5)
save_figure(p5,   "Figure_5_age_weighting_comparison",   width = 7.0, height = 6.5)
save_figure(p6,   "Figure_6_under6_vs_at6",              width = 7.0, height = 6.5)
save_figure(p7,   "Figure_7_cumulative_adjustments",     width = 9.0, height = 6.0)
save_figure(p_s1, "Supplementary_Figure_1_age_ratios",   width = 7.0, height = 5.5)

cat("\nAnalysis complete.\n")
cat("Tables saved to: ", normalizePath(table_dir, winslash = "/", mustWork = FALSE), "\n", sep = "")
cat("Figures saved to: ", normalizePath(figure_dir, winslash = "/", mustWork = FALSE), "\n", sep = "")
cat("* Under-6-month estimates use simple survey-weighted proportions.\n")
cat("* Exact-6-month estimates remain model-based logistic predictions.\n")

####################################################################