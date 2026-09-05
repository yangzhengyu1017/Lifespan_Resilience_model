# ==============================================================================
# Script: A3_2_Logitudinal_Analysis_UKB_matched_V2.R
# Project: Lifespan Resilience Modeling (STM Submission)
# Purpose: Conduct propensity-score matched longitudinal analysis of mental health trajectories across resilience groups to control for baseline confounding.
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. Load required packages
# ------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(MatchIt)
library(gtsummary)
library(ggplot2)
library(ggpubr)
library(broom)
library(gridExtra)
library(purrr)

# ------------------------------------------------------------------------------
# 1. Data Preparation (Pre-matching)
# ------------------------------------------------------------------------------
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

cat("\n[INFO] Preparing data for Propensity Score Matching...\n")

# Create dataset, filter out "Medium" resilience, and calculate change scores
resilience_test <- resilience_group_R %>% 
  dplyr::transmute(
    eid, 
    Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")), 
    Age = age_BL, 
    Ethnic = factor(Ethnic_group, levels = c(0, 1, 2, 3), labels = c("1_White", "2_Asian", "3_Black", "4_Other")),  
    Education = Education_year,
    BMI = BMI_BL,
    selfresilience = factor(self_resilience, levels = c(0, 1, 2), labels = c("1_Low", "2_Medium", "3_High")),
    data_FU1 = Mental_onine_date_FU1, 
    data_FU2 = Mental_onine_date_FU2, 
    PHQ_FU1 = PHQ9_Severity_FU1,
    PHQ_FU2 = `PHQ-9_FU2`,
    GAD_FU1 = General_Anxiety_Disorder_Severity_FU1,
    GAD_FU2 = General_Anxiety_Disorder_Severity_FU2
  ) %>%
  dplyr::filter(selfresilience != "2_Medium") %>% 
  dplyr::mutate(selfresilience = base::droplevels(selfresilience)) %>%
  stats::na.omit() %>%
  # Calculate symptom change (FU2 - FU1) for both PHQ and GAD
  dplyr::mutate(
    PHQ_Change = PHQ_FU2 - PHQ_FU1,
    GAD_Change = GAD_FU2 - GAD_FU1
  )

# --- Generate Pre-matching Baseline Characteristics (Table 1) ---
unmatch_group_table <- resilience_test %>% 
  gtsummary::tbl_summary(
    by = selfresilience,
    statistic = list(
      gtsummary::all_continuous() ~ "{mean} ({sd})",
      gtsummary::all_categorical() ~ "{n} ({p}%)"
    ),
    type = list(Age ~ "continuous", Education ~ "continuous")
  ) %>%
  gtsummary::add_p() %>% 
  gtsummary::add_overall()

cat("\n--- Unmatched Baseline Characteristics ---\n")
print(unmatch_group_table)


# ------------------------------------------------------------------------------
# 2. Propensity Score Matching (PSM)
# ------------------------------------------------------------------------------
cat("\n[INFO] Executing Propensity Score Matching (PSM)...\n")

m.out0 <- MatchIt::matchit(
  selfresilience ~ PHQ_FU1 + GAD_FU1 + Age + Sex + Ethnic + BMI + Education, 
  data = resilience_test,
  method = "nearest", 
  distance = "glm", 
  caliper = 0.000001 # Note: Highly strict caliper
)

# Extract matched data
resilience_matched <- MatchIt::match.data(m.out0) %>%
  dplyr::filter(weights == 1)

# --- Generate Post-matching Baseline Characteristics (Table 1) ---
matched_group_table <- resilience_matched %>% 
  dplyr::select(Sex, Age, Ethnic, Education, BMI, selfresilience, PHQ_FU1, GAD_FU1) %>%
  gtsummary::tbl_summary(
    by = selfresilience,
    statistic = list(
      gtsummary::all_continuous() ~ "{mean} ({sd})",
      gtsummary::all_categorical() ~ "{n} ({p}%)"
    ),
    type = list(Age ~ "continuous", Education ~ "continuous", PHQ_FU1 ~ "continuous", GAD_FU1 ~ "continuous")
  ) %>%
  gtsummary::add_p() %>% 
  gtsummary::add_overall()

cat("\n--- Matched Baseline Characteristics ---\n")
print(matched_group_table)


# ==============================================================================
# PART 1: Matched PHQ Longitudinal Analysis
# ==============================================================================
cat("\n[INFO] Running PHQ Linear Models...\n")

# Model M1: Baseline PHQ (PHQ_FU1)
lme_PHQ1 <- stats::lm(PHQ_FU1 ~ selfresilience + Age + Sex + Ethnic + BMI + Education, data = resilience_matched)
M1 <- broom::tidy(lme_PHQ1, conf.int = TRUE) %>%
  dplyr::mutate(beta = paste0(round(estimate, 2), " (", round(conf.low, 2), ", ", round(conf.high, 2), ")")) %>%
  dplyr::select(term, beta, statistic, p.value) %>%
  dplyr::rename_with(~ paste0("PHQ_FU1_Matched_", .), -term)

# Model M2: Unadjusted PHQ Change
lme_PHQ2 <- stats::lm(PHQ_Change ~ selfresilience + Age + Sex + Ethnic + BMI + Education, data = resilience_matched)
M2 <- broom::tidy(lme_PHQ2, conf.int = TRUE) %>%
  dplyr::mutate(beta = paste0(round(estimate, 2), " (", round(conf.low, 2), ", ", round(conf.high, 2), ")")) %>%
  dplyr::select(term, beta, statistic, p.value) %>%
  dplyr::rename_with(~ paste0("PHQ_Change_Matched_", .), -term)

# Model M3: Baseline-adjusted PHQ Change
lme_PHQ3 <- stats::lm(PHQ_Change ~ PHQ_FU1 + selfresilience + Age + Sex + Ethnic + BMI + Education, data = resilience_matched)
M3 <- broom::tidy(lme_PHQ3, conf.int = TRUE) %>%
  dplyr::filter(term != "PHQ_FU1") %>% # Remove baseline covariate from reporting
  dplyr::mutate(beta = paste0(round(estimate, 2), " (", round(conf.low, 2), ", ", round(conf.high, 2), ")")) %>%
  dplyr::select(term, beta, statistic, p.value) %>%
  dplyr::rename_with(~ paste0("PHQ_Change_Matched_adj_", .), -term)

# Plotting PHQ Trajectory (Matched)
tb_long_PHQ <- resilience_matched %>% 
  tidyr::pivot_longer(cols = c("PHQ_FU1", "PHQ_FU2"), names_to = c(".value", "FU"), names_sep = "_", values_drop_na = TRUE)

p_PHQ2 <- ggpubr::ggline(
  tb_long_PHQ, x = "FU", y = "PHQ", add = "mean_se",
  color = "selfresilience", palette = c("#E2C098", "#85C3DC"), size = 1
) + 
  ggpubr::stat_compare_means(aes(group = selfresilience), label = "p.signif", label.y = c(2.5, 2.6), size = 4)


# ==============================================================================
# PART 2: Matched GAD Longitudinal Analysis
# ==============================================================================
cat("\n[INFO] Running GAD Linear Models...\n")

# Model M4: Baseline GAD (GAD_FU1)
lme_GAD1 <- stats::lm(GAD_FU1 ~ selfresilience + Age + Sex + Ethnic + BMI + Education, data = resilience_matched)
M4 <- broom::tidy(lme_GAD1, conf.int = TRUE) %>%
  dplyr::mutate(beta = paste0(round(estimate, 2), " (", round(conf.low, 2), ", ", round(conf.high, 2), ")")) %>%
  dplyr::select(term, beta, statistic, p.value) %>%
  dplyr::rename_with(~ paste0("GAD_FU1_Matched_", .), -term)

# Model M5: Unadjusted GAD Change
lme_GAD2 <- stats::lm(GAD_Change ~ selfresilience + Age + Sex + Ethnic + BMI + Education, data = resilience_matched)
M5 <- broom::tidy(lme_GAD2, conf.int = TRUE) %>%
  dplyr::mutate(beta = paste0(round(estimate, 2), " (", round(conf.low, 2), ", ", round(conf.high, 2), ")")) %>%
  dplyr::select(term, beta, statistic, p.value) %>%
  dplyr::rename_with(~ paste0("GAD_Change_Matched_", .), -term)

# Model M6: Baseline-adjusted GAD Change
lme_GAD3 <- stats::lm(GAD_Change ~ GAD_FU1 + selfresilience + Age + Sex + Ethnic + BMI + Education, data = resilience_matched)
M6 <- broom::tidy(lme_GAD3, conf.int = TRUE) %>%
  dplyr::filter(term != "GAD_FU1") %>% # Remove baseline covariate from reporting
  dplyr::mutate(beta = paste0(round(estimate, 2), " (", round(conf.low, 2), ", ", round(conf.high, 2), ")")) %>%
  dplyr::select(term, beta, statistic, p.value) %>%
  dplyr::rename_with(~ paste0("GAD_Change_Matched_adj_", .), -term)

# Plotting GAD Trajectory (Matched)
tb_long_GAD <- resilience_matched %>% 
  tidyr::pivot_longer(cols = c("GAD_FU1", "GAD_FU2"), names_to = c(".value", "FU"), names_sep = "_", values_drop_na = TRUE)

p_GAD2 <- ggpubr::ggline(
  tb_long_GAD, x = "FU", y = "GAD", add = "mean_se",
  color = "selfresilience", palette = c("#E2C098", "#85C3DC"), size = 1
) + 
  ggpubr::stat_compare_means(aes(group = selfresilience), label = "p.signif", label.y = c(1.1, 2.3), size = 4)


# ==============================================================================
# Final: Combine Results and Plots
# ==============================================================================
cat("\n[INFO] Compiling final statistical tables and plots...\n")

# Safely join all matched model results by term using purrr::reduce
match_Stastic_result <- list(M1, M2, M3, M4, M5, M6) %>%
  purrr::reduce(dplyr::full_join, by = "term")

cat("\n--- Final Matched Statistical Results ---\n")
print(match_Stastic_result)

# Note: Ensure p_PHQ and p_GAD (from the unmatched analysis) exist in your environment 
# before running the grid.arrange code below.
combined_plot <- gridExtra::grid.arrange(p_PHQ, p_GAD, p_PHQ2, p_GAD2, ncol = 2)

# Display the combined plot
print(combined_plot)