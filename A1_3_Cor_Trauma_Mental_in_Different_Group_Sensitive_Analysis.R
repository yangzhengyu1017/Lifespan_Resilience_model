# ==============================================================================
# Script: A1_3_Cor_Trauma_Mental_in_Different_Group_Sensitive_Analysis_V2.R
# Project: Lifespan Resilience Modeling (STM Submission)
# Purpose: Perform sensitivity analysis testing continuous resilience x trauma exposure interaction effects on mental health outcomes across BL, FU1, and FU2.
# ==============================================================================

# -------------------------------
# 0. Load necessary libraries
# -------------------------------
library(readr)
library(dplyr)
library(tidyr)
library(stats)
library(broom)
library(autoReg)

# -------------------------------
# 1. Read data
# -------------------------------
resilience_group_R <- readr::read_csv("data/UKB_dat_for_analysis_1226.csv")[, -1]


# ==================================================================
# 2. Baseline (BL) Analysis
# ==================================================================
resilience_test_BL <- resilience_group_R %>%
  dplyr::select(
    eid, age_BL, gender, BMI_BL, Ethnic_group, site, Education_year,
    self_resilience, Depressive_Symptoms_PHQ4_BL, trauma_num
  ) %>%
  dplyr::mutate(
    # Format covariates
    Age = age_BL,
    Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")),
    BMI = BMI_BL,
    Education = Education_year,
    site = factor(site),
    
    # Recode Ethnic group safely
    Ethnic_group_tmp = dplyr::case_when(
      Ethnic_group == 1 ~ 0,
      Ethnic_group == 2 ~ 3,
      Ethnic_group == 3 ~ 1,
      Ethnic_group == 4 ~ 2,
      Ethnic_group == 5 ~ 1,
      Ethnic_group == 6 ~ 3,
      TRUE ~ Ethnic_group - 1 
    ),
    Ethnic = factor(Ethnic_group_tmp, levels = c(0, 1, 2, 3), labels = c("1_White", "2_Asian", "3_Black", "4_Other")),
    
    # Calculate target variables and apply cutoffs
    PHQ_BL = Depressive_Symptoms_PHQ4_BL - 4,
    Trauma_exposure = ifelse(trauma_num >= 5, 4, as.numeric(trauma_num))
  ) %>%
  # Keep only model variables and drop missing values
  dplyr::select(eid, Age, Sex, Ethnic, BMI, Education, site, self_resilience, PHQ_BL, Trauma_exposure) %>%
  tidyr::drop_na()

# Linear regression: Interaction model
lme_BL <- stats::lm(PHQ_BL ~ self_resilience * Trauma_exposure + Age + Sex + Ethnic + BMI + site + Education, data = resilience_test_BL)

summary(lme_BL)
tidy_results_BL <- broom::tidy(lme_BL, conf.int = TRUE)
autoReg::autoReg(lme_BL) %>% autoReg::myft()


# ==================================================================
# 3. Follow-Up 1 (FU1) Analysis
# ==================================================================
resilience_test_FU1 <- resilience_group_R %>%
  dplyr::select(
    eid, age_BL, gender, BMI_BL, Ethnic_group, site, Education_year,
    self_resilience, PHQ9_Severity_FU1, General_Anxiety_Disorder_Severity_FU1, FU_recent_trauma
  ) %>%
  dplyr::mutate(
    # Format covariates
    Age = age_BL,
    Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")),
    BMI = BMI_BL,
    Education = Education_year,
    site = factor(site),
    
    # Recode Ethnic group safely
    Ethnic_group_tmp = dplyr::case_when(
      Ethnic_group == 1 ~ 0,
      Ethnic_group == 2 ~ 3,
      Ethnic_group == 3 ~ 1,
      Ethnic_group == 4 ~ 2,
      Ethnic_group == 5 ~ 1,
      Ethnic_group == 6 ~ 3,
      TRUE ~ Ethnic_group - 1
    ),
    Ethnic = factor(Ethnic_group_tmp, levels = c(0, 1, 2, 3), labels = c("1_White", "2_Asian", "3_Black", "4_Other")),
    
    # Calculate target variables and apply cutoffs
    Mental_symptom_scores_FU1 = ((PHQ9_Severity_FU1 - 1) / 27) + (General_Anxiety_Disorder_Severity_FU1 / 21),
    Trauma_exposure = ifelse(FU_recent_trauma >= 3, 2, as.numeric(FU_recent_trauma))
  ) %>%
  # Keep only model variables and drop missing values
  dplyr::select(eid, Age, Sex, Ethnic, BMI, Education, site, self_resilience, Mental_symptom_scores_FU1, Trauma_exposure) %>%
  tidyr::drop_na()

# Linear regression: Interaction model
lme_FU1 <- stats::lm(Mental_symptom_scores_FU1 ~ self_resilience * Trauma_exposure + Age + Sex + Ethnic + BMI + site + Education, data = resilience_test_FU1)

summary(lme_FU1)
tidy_results_FU1 <- broom::tidy(lme_FU1, conf.int = TRUE)
autoReg::autoReg(lme_FU1) %>% autoReg::myft()


# ==================================================================
# 4. Follow-Up 2 (FU2) Analysis
# ==================================================================
resilience_test_FU2 <- resilience_group_R %>%
  dplyr::select(
    eid, age_BL, gender, BMI_BL, Ethnic_group, site, Education_year,
    self_resilience, `PHQ-9_FU2`, General_Anxiety_Disorder_Severity_FU2, FU2_recent_trauma
  ) %>%
  dplyr::mutate(
    # Format covariates
    Age = age_BL,
    Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")),
    BMI = BMI_BL,
    Education = Education_year,
    site = factor(site),
    
    # Recode Ethnic group safely
    Ethnic_group_tmp = dplyr::case_when(
      Ethnic_group == 1 ~ 0,
      Ethnic_group == 2 ~ 3,
      Ethnic_group == 3 ~ 1,
      Ethnic_group == 4 ~ 2,
      Ethnic_group == 5 ~ 1,
      Ethnic_group == 6 ~ 3,
      TRUE ~ Ethnic_group - 1
    ),
    Ethnic = factor(Ethnic_group_tmp, levels = c(0, 1, 2, 3), labels = c("1_White", "2_Asian", "3_Black", "4_Other")),
    
    # Calculate target variables and apply cutoffs
    Mental_symptom_scores_FU2 = (`PHQ-9_FU2` / 27) + (General_Anxiety_Disorder_Severity_FU2 / 21),
    Trauma_exposure = ifelse(FU2_recent_trauma >= 5, 4, as.numeric(FU2_recent_trauma))
  ) %>%
  # Keep only model variables and drop missing values
  dplyr::select(eid, Age, Sex, Ethnic, BMI, Education, site, self_resilience, Mental_symptom_scores_FU2, Trauma_exposure) %>%
  tidyr::drop_na()

# Linear regression: Interaction model
lme_FU2 <- stats::lm(Mental_symptom_scores_FU2 ~ self_resilience * Trauma_exposure + Age + Sex + Ethnic + BMI + site + Education, data = resilience_test_FU2)

summary(lme_FU2)
tidy_results_FU2 <- broom::tidy(lme_FU2, conf.int = TRUE)
autoReg::autoReg(lme_FU2) %>% autoReg::myft()