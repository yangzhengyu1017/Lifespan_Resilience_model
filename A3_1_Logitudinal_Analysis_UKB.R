# ==============================================================================
# Script: A3_1_Logitudinal_Analysis_UKB_V2.R
# Project: Lifespan Resilience Modeling (STM Submission)
# Purpose: Perform longitudinal trajectory analysis of depressive (PHQ) and anxiety (GAD) symptoms across resilience groups in UK Biobank.
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. Load necessary packages
# ------------------------------------------------------------------------------
library(data.table)
library(dplyr)
library(tidyr)
library(readr)
library(rstatix)
library(ggpubr)
library(caret)
library(emmeans)
library(bruceR)
library(nlme)
library(lme4)
library(forestploter)
library(survminer)
library(gridExtra)
library(gtsummary)
library(autoReg)
library(writexl)
library(MatchIt)
library(purrr)

# ------------------------------------------------------------------------------
# 1. Data Import and Preprocessing
# ------------------------------------------------------------------------------
resilience_group_R <- read_csv("data/UKB_dat_for_analysis_1226.csv")[,-1]

# Safely recode ethnic groups
resilience_group_R <- resilience_group_R %>%
  dplyr::mutate(
    Ethnic_group = dplyr::case_when(
      Ethnic_group == 2 ~ 6,
      Ethnic_group == 3 ~ 2,
      Ethnic_group == 5 ~ 2,
      Ethnic_group == 4 ~ 3,
      Ethnic_group == 6 ~ 4,
      TRUE ~ Ethnic_group
    ),
    Ethnic_group = Ethnic_group - 1
  )

# Calculate quantiles and categorize self_resilience (Executed once)
quantiles <- stats::quantile(resilience_group_R$self_resilience, probs = c(1/3, 2/3), na.rm = TRUE)

resilience_group_R <- resilience_group_R %>%
  dplyr::mutate(
    self_resilience = dplyr::case_when(
      self_resilience <= quantiles[1] ~ 0,
      self_resilience > quantiles[1] & self_resilience < quantiles[2] ~ 1,
      self_resilience >= quantiles[2] ~ 2
    ),
    self_resilience = as.factor(self_resilience)
  )

# ------------------------------------------------------------------------------
# 2. Baseline Characteristics Table (Table 1)
# ------------------------------------------------------------------------------
# Create base dataset for longitudinal comparison
resilience_test_base <- resilience_group_R %>%
  dplyr::transmute(
    eid, 
    Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")), 
    Age = age_BL, 
    Ethnic = factor(Ethnic_group, levels = c(0, 1, 2, 3), labels = c("1_White", "2_Asian", "3_Black", "4_Other")),  
    Education = Education_year,
    BMI = BMI_BL,
    site = factor(site),
    selfresilience = factor(self_resilience, levels = c(0, 1, 2), labels = c("1_Low", "2_Medium", "3_High")),
    data_FU1 = Mental_onine_date_FU1, 
    data_FU2 = Mental_onine_date_FU2, 
    PHQ_FU1 = PHQ9_Severity_FU1 - 1,
    PHQ_FU2 = `PHQ-9_FU2`,
    GAD_FU1 = General_Anxiety_Disorder_Severity_FU1,
    GAD_FU2 = General_Anxiety_Disorder_Severity_FU2
  ) %>%
  tidyr::drop_na()

# Generate Demographic Table (Excluding 'site' to keep the table clean)
unmatched_group_table <- resilience_test_base %>% 
  dplyr::select(-site) %>%
  gtsummary::tbl_summary(
    by = selfresilience,
    statistic = list(
      gtsummary::all_continuous() ~ "{mean} ({sd})",
      gtsummary::all_categorical() ~ "{n} ({p}%)"
    ),
    type = list(Age ~ "continuous", Education ~ "continuous")
  ) %>%
  gtsummary::add_p(test = list(gtsummary::all_continuous() ~ "aov")) %>% 
  gtsummary::add_overall()

print(unmatched_group_table)


################################################################################
# PART 1: PHQ (Depression) Longitudinal Analysis
################################################################################
cat("\n--- Running PHQ Longitudinal Regression Models ---\n")

resilience_test_PHQ <- resilience_test_base %>%
  dplyr::mutate(PHQ_Change = PHQ_FU2 - PHQ_FU1)

# Model 1.1: Baseline PHQ (PHQ_FU1)
lme_PHQ1 <- stats::lm(PHQ_FU1 ~ selfresilience + Age + Sex + Ethnic + BMI + Education + site, data = resilience_test_PHQ)
# autoReg::autoReg(lme_PHQ1) %>% autoReg::myft()

T1 <- broom::tidy(lme_PHQ1, conf.int = TRUE) %>%
  dplyr::mutate(beta = paste0(round(estimate, 2), " (", round(conf.low, 2), ", ", round(conf.high, 2), ")")) %>%
  dplyr::select(term, beta, statistic, p.value) %>%
  dplyr::rename_with(~ paste0("PHQ_FU1_", .), -term)

# Model 1.2: Unadjusted PHQ Change
lme_PHQ2 <- stats::lm(PHQ_Change ~ selfresilience + Age + Sex + Ethnic + BMI + Education + site, data = resilience_test_PHQ)

T2 <- broom::tidy(lme_PHQ2, conf.int = TRUE) %>%
  dplyr::mutate(beta = paste0(round(estimate, 2), " (", round(conf.low, 2), ", ", round(conf.high, 2), ")")) %>%
  dplyr::select(term, beta, statistic, p.value) %>%
  dplyr::rename_with(~ paste0("PHQ_Change_", .), -term)

# Model 1.3: Baseline-adjusted PHQ Change
lme_PHQ3 <- stats::lm(PHQ_Change ~ PHQ_FU1 + selfresilience + Age + Sex + Ethnic + BMI + Education + site, data = resilience_test_PHQ)

T3 <- broom::tidy(lme_PHQ3, conf.int = TRUE) %>%
  dplyr::filter(term != "PHQ_FU1") %>% # Remove baseline covariate from table
  dplyr::mutate(beta = paste0(round(estimate, 2), " (", round(conf.low, 2), ", ", round(conf.high, 2), ")")) %>%
  dplyr::select(term, beta, statistic, p.value) %>%
  dplyr::rename_with(~ paste0("PHQ_Change_adj_", .), -term)

# Plotting PHQ Trajectory
tb_long_PHQ <- resilience_test_PHQ %>% 
  tidyr::pivot_longer(cols = c("PHQ_FU1", "PHQ_FU2"), names_to = c(".value", "FU"), names_sep = "_", values_drop_na = TRUE)

p_PHQ <- ggpubr::ggline(
  tb_long_PHQ, x = "FU", y = "PHQ", add = "mean_se",
  color = "selfresilience", palette = c("#E2C098", "#A6CAA8", "#85C3DC"), size = 1
) + 
  ggpubr::stat_compare_means(aes(group = selfresilience), label = "p.signif", label.y = c(5.1, 4.6), size = 4)


################################################################################
# PART 2: GAD (Anxiety) Longitudinal Analysis
################################################################################
cat("\n--- Running GAD Longitudinal Regression Models ---\n")

resilience_test_GAD <- resilience_test_base %>%
  dplyr::mutate(GAD_Change = GAD_FU2 - GAD_FU1)

# Model 2.1: Baseline GAD (GAD_FU1)
lme_GAD1 <- stats::lm(GAD_FU1 ~ selfresilience + Age + Sex + Ethnic + BMI + Education + site, data = resilience_test_GAD)

T4 <- broom::tidy(lme_GAD1, conf.int = TRUE) %>%
  dplyr::mutate(beta = paste0(round(estimate, 2), " (", round(conf.low, 2), ", ", round(conf.high, 2), ")")) %>%
  dplyr::select(term, beta, statistic, p.value) %>%
  dplyr::rename_with(~ paste0("GAD_FU1_", .), -term)

# Model 2.2: Unadjusted GAD Change
lme_GAD2 <- stats::lm(GAD_Change ~ selfresilience + Age + Sex + Ethnic + BMI + Education + site, data = resilience_test_GAD)

T5 <- broom::tidy(lme_GAD2, conf.int = TRUE) %>%
  dplyr::mutate(beta = paste0(round(estimate, 2), " (", round(conf.low, 2), ", ", round(conf.high, 2), ")")) %>%
  dplyr::select(term, beta, statistic, p.value) %>%
  dplyr::rename_with(~ paste0("GAD_Change_", .), -term)

# Model 2.3: Baseline-adjusted GAD Change
lme_GAD3 <- stats::lm(GAD_Change ~ GAD_FU1 + selfresilience + Age + Sex + Ethnic + BMI + Education + site, data = resilience_test_GAD)

T6 <- broom::tidy(lme_GAD3, conf.int = TRUE) %>%
  dplyr::filter(term != "GAD_FU1") %>% 
  dplyr::mutate(beta = paste0(round(estimate, 2), " (", round(conf.low, 2), ", ", round(conf.high, 2), ")")) %>%
  dplyr::select(term, beta, statistic, p.value) %>%
  dplyr::rename_with(~ paste0("GAD_Change_adj_", .), -term)

# Plotting GAD Trajectory
tb_long_GAD <- resilience_test_GAD %>% 
  tidyr::pivot_longer(cols = c("GAD_FU1", "GAD_FU2"), names_to = c(".value", "FU"), names_sep = "_", values_drop_na = TRUE)

p_GAD <- ggpubr::ggline(
  tb_long_GAD, x = "FU", y = "GAD", add = "mean_se",
  color = "selfresilience", palette = c("#E2C098", "#A6CAA8", "#85C3DC"), size = 1
) + 
  ggpubr::stat_compare_means(aes(group = selfresilience), label = "p.signif", label.y = c(3.5, 4), size = 4)


################################################################################
# PART 3: Compile and Display Final Results
################################################################################
# Use purrr::reduce to safely join all tables by 'term' to avoid column mismatch
Stastic_result <- list(T1, T2, T3, T4, T5, T6) %>%
  purrr::reduce(dplyr::full_join, by = "term")

cat("\n=========================================================\n")
cat("Final Merged Statistical Results (PHQ & GAD)\n")
cat("=========================================================\n")
print(Stastic_result)

# Display combined plots
gridExtra::grid.arrange(p_PHQ, p_GAD, ncol = 2, top = "Longitudinal Symptom Trajectories")