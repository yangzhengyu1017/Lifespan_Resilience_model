# ==============================================================================
# Script: A1_1_Cor_Trauma_Mental_all_Group.R
# Project: Lifespan Resilience Modeling (STM Submission)
# Purpose: Analyze correlation between trauma exposure and mental health outcomes across all UK Biobank participants at Baseline (BL), Follow-Up 1 (FU1), and Follow-Up 2 (FU2).
# ==============================================================================

# Load required packages explicitly
library(readr)
library(dplyr)
library(tidyr)
library(stats)
library(ppcor)

# ------------------------------------------------------------------------------
# 1. Global Data Import & Preprocessing
# ------------------------------------------------------------------------------
# Read data and remove the first column (rownames)
resilience_raw <- readr::read_csv("data/UKB_resilience_corr_rename_dat_11221.csv")[, -1]
env_mental_raw <- utils::read.csv("data/UKB_BL_FU_trauma_mental_env_dat.csv")[, -1]

# Extract specific columns from the environmental dataset
env_mental_sub <- env_mental_raw[, c(1, 71, 85, 79)]

# Merge datasets and perform global recoding
df_global <- resilience_raw %>%
  dplyr::left_join(env_mental_sub, by = "eid") %>%
  dplyr::mutate(
    # Continuous Resilience Score
    self_resilience = (self_resilience + 6) / 6,
    
    # Standardize demographics
    Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")),
    Age = age_BL,
    BMI = BMI_BL,
    Education = Education_year,
    site = factor(site),
    
    # ========================================================
    # Recode ethnic group into 4 standard categories
    # ========================================================
    Ethnic_group_tmp = dplyr::case_when(
      Ethnic_group == 1 ~ 0, # White
      Ethnic_group == 2 ~ 3, # Other
      Ethnic_group == 3 ~ 1, # Asian
      Ethnic_group == 4 ~ 2, # Black
      Ethnic_group == 5 ~ 1, # Asian
      Ethnic_group == 6 ~ 3, # Other
      TRUE ~ Ethnic_group - 1
    ),
        Ethnic = factor(Ethnic_group_tmp, levels = c(0, 1, 2, 3), labels = c("1_White", "2_Asian", "3_Black", "4_Other")),
    
    # Adjust FU2 trauma score globally
    FU2_recent_trauma = FU2_recent_trauma * 10
  )

# ------------------------------------------------------------------------------
# 2. Baseline (BL) Stage Analysis
# Note: PHQ score is direct. Trauma >= 5 is recoded to 4.
# ------------------------------------------------------------------------------
cat("\n--- Running Baseline (BL) Analysis ---\n")

df_BL <- df_global %>%
  dplyr::transmute(
    eid, Age, Sex, Ethnic, BMI, Education, site,
    selfresilience = self_resilience,
    PHQ_BL = Depressive_Symptoms_PHQ4_BL, 
    Trauma_exposure = trauma_num
  ) %>%
  # Apply trauma cutoff
  dplyr::mutate(Trauma_exposure = ifelse(Trauma_exposure >= 5, 4, Trauma_exposure)) %>%
  tidyr::drop_na()

# Fit Linear Model
model_BL <- stats::lm(PHQ_BL ~ Trauma_exposure + Education + BMI + Sex + Age + site, data = df_BL)
print(summary(model_BL)$coefficients[, "Pr(>|t|)"])

# Calculate Standardized Beta
std_beta_BL <- stats::coef(model_BL)["Trauma_exposure"] * stats::sd(df_BL$Trauma_exposure) / stats::sd(df_BL$PHQ_BL)
cat("Standardized Beta (BL):", std_beta_BL, "\n")

# Calculate True Partial Correlation (Recommended for reporting)
covar_mat_BL <- stats::model.matrix(~ Education + BMI + Sex + Age + site, data = df_BL)[, -1]
pcor_BL <- ppcor::pcor.test(df_BL$PHQ_BL, df_BL$Trauma_exposure, covar_mat_BL, method = "pearson")
cat("Partial Correlation (BL):", pcor_BL$estimate, "\n")


# ------------------------------------------------------------------------------
# 3. Follow-Up 1 (FU1) Stage Analysis
# Note: PHQ score - 1, divided by 27. Trauma == 3 is recoded to 2.
# ------------------------------------------------------------------------------
cat("\n--- Running Follow-Up 1 (FU1) Analysis ---\n")

df_FU1 <- df_global %>%
  dplyr::transmute(
    eid, Age, Sex, Ethnic, BMI, Education, site,
    selfresilience = self_resilience,
    Mental_symptom_scores_FU1 = ((PHQ9_Severity_FU1 - 1) / 27 + General_Anxiety_Disorder_Severity_FU1 / 21) * 10,
    Trauma_exposure = FU_recent_trauma
  ) %>%
  # Apply trauma cutoff
  dplyr::mutate(Trauma_exposure = ifelse(Trauma_exposure == 3, 2, Trauma_exposure)) %>%
  tidyr::drop_na()

# Fit Linear Model
model_FU1 <- stats::lm(Mental_symptom_scores_FU1 ~ Trauma_exposure + Education + BMI + Sex + Age + site, data = df_FU1)
print(summary(model_FU1)$coefficients[, "Pr(>|t|)"])

# Calculate Standardized Beta
std_beta_FU1 <- stats::coef(model_FU1)["Trauma_exposure"] * stats::sd(df_FU1$Trauma_exposure) / stats::sd(df_FU1$Mental_symptom_scores_FU1)
cat("Standardized Beta (FU1):", std_beta_FU1, "\n")

# Calculate True Partial Correlation
covar_mat_FU1 <- stats::model.matrix(~ Education + BMI + Sex + Age + site, data = df_FU1)[, -1]
pcor_FU1 <- ppcor::pcor.test(df_FU1$Mental_symptom_scores_FU1, df_FU1$Trauma_exposure, covar_mat_FU1, method = "pearson")
cat("Partial Correlation (FU1):", pcor_FU1$estimate, "\n")


# ------------------------------------------------------------------------------
# 4. Follow-Up 2 (FU2) Stage Analysis
# Note: PHQ score NOT subtracted by 1, divided by 27. Trauma >= 5 is recoded to 4.
# ------------------------------------------------------------------------------
cat("\n--- Running Follow-Up 2 (FU2) Analysis ---\n")

df_FU2 <- df_global %>%
  dplyr::transmute(
    eid, Age, Sex, Ethnic, BMI, Education, site,
    selfresilience = self_resilience,
    Mental_symptom_scores_FU2 = (`PHQ-9_FU2` / 27 + General_Anxiety_Disorder_Severity_FU2 / 21) * 10,
    Trauma_exposure = FU2_recent_trauma
  ) %>%
  # Apply trauma cutoff
  dplyr::mutate(Trauma_exposure = ifelse(Trauma_exposure >= 5, 4, Trauma_exposure)) %>%
  tidyr::drop_na()

# Fit Linear Model
model_FU2 <- stats::lm(Mental_symptom_scores_FU2 ~ Trauma_exposure + Education + BMI + Sex + Age + site, data = df_FU2)
print(summary(model_FU2)$coefficients[, "Pr(>|t|)"])

# Calculate Standardized Beta
std_beta_FU2 <- stats::coef(model_FU2)["Trauma_exposure"] * stats::sd(df_FU2$Trauma_exposure) / stats::sd(df_FU2$Mental_symptom_scores_FU2)
cat("Standardized Beta (FU2):", std_beta_FU2, "\n")

# Calculate True Partial Correlation
covar_mat_FU2 <- stats::model.matrix(~ Education + BMI + Sex + Age + site, data = df_FU2)[, -1]
pcor_FU2 <- ppcor::pcor.test(df_FU2$Mental_symptom_scores_FU2, df_FU2$Trauma_exposure, covar_mat_FU2, method = "pearson")
cat("Partial Correlation (FU2):", pcor_FU2$estimate, "\n")