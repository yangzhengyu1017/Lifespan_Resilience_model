# ----------------------------------------------------------------------------------
# Purpose:
# This script performs partial correlation analyses between trauma exposure and 
# mental health symptoms across three stages: Baseline (BL), Follow-up 1 (FU1), 
# and Follow-up 2 (FU2). The analyses control for covariates including age, sex, 
# education, and BMI. Standardized regression coefficients are used to estimate 
# the partial correlations, and p-values are reported.
# ----------------------------------------------------------------------------------

# -------------------------------
# Load required libraries
# -------------------------------
library(data.table)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(caret)
library(visreg)
library(rms)
library(gtsummary)
library(autoReg)
library(ggplot2)
library(ggbeeswarm)
library(patchwork)
library(gridExtra)
library(skimr)
library(janitor)
library(dplyr)
library(emmeans)
library(bruceR)
library(ppcor)
library(QuantPsyc)

# -------------------------------
# Load data
# -------------------------------
resilience_group_R <- read_csv("~/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/UKB_dat_for_analysis_1226.csv")[,-1]

# -------------------------------
# Define partial correlation function
# -------------------------------
compute_partial_correlation <- function(df, outcome, trauma_var) {
  # Fit linear regression: outcome ~ trauma + covariates
  model <- lm(as.formula(paste(outcome, "~", trauma_var, "+ Education + BMI + Sex + Age")), data = df)
  
  # Standardized coefficient to estimate partial correlation
  beta <- model$coefficients[trauma_var]
  correlation <- beta * sd(df[[trauma_var]]) / sd(df[[outcome]])
  
  # Extract p-value for trauma effect
  p_value <- summary(model)$coefficients[trauma_var, "Pr(>|t|)"]
  
  list(model_summary = summary(model), correlation = correlation, p_value = p_value)
}


resilience_group_R_cor <- resilience_group_R[, c("eid", "age_BL", "gender", "BMI_BL", "Ethnic_group", "IMD",
                                                 "self_resilience", "Depressive_Symptoms_PHQ4_BL", "PHQ9_Severity_FU1", "PHQ-9_FU2",
                                                 "General_Anxiety_Disorder_Severity_FU1", "General_Anxiety_Disorder_Severity_FU2",
                                                 "trauma_num", "FU_recent_trauma", "FU2_recent_trauma", "Education_year")]
resilience_group_R_cor <- na.omit(resilience_group_R_cor)
# -------------------------------
# Baseline (BL) analysis
# -------------------------------
resilience_BL <- resilience_group_R_cor %>%
  transmute(
    eid,
    Sex = factor(gender, levels = c(0,1), labels = c("Female","Male")),
    Age = age_BL,
    Ethnic = factor(Ethnic_group, levels = c("0","1","2","3"), labels = c("1_White","2_Asian","3_Black","4_Other")),
    Education = Education_year,
    BMI = BMI_BL,
    site = factor(site),
    selfresilience = self_resilience,
    PHQ_BL = Depressive_Symptoms_PHQ4_BL,
    Trauma_exposure = trauma_num
  )

# Cap trauma exposure at 4
resilience_BL$Trauma_exposure[resilience_BL$Trauma_exposure >= 5] <- 4

# Remove missing data
resilience_BL <- na.omit(resilience_BL)

# Adjust baseline PHQ scores
resilience_BL$PHQ_BL <- resilience_BL$PHQ_BL - 4

# Compute partial correlation
BL_results <- compute_partial_correlation(resilience_BL, "PHQ_BL", "Trauma_exposure")
BL_results

# -------------------------------
# Follow-up 1 (FU1) analysis
# -------------------------------
resilience_FU1 <- resilience_group_R_cor %>%
  transmute(
    eid,
    Sex = factor(gender, levels = c(0,1), labels = c("Female","Male")),
    Age = age_BL,
    Ethnic = factor(Ethnic_group, levels = c("0","1","2","3"), labels = c("1_White","2_Asian","3_Black","4_Other")),
    Education = Education_year,
    BMI = BMI_BL,
    site = factor(site),
    selfresilience = self_resilience,
    # Combined mental symptom score: PHQ9 + GAD
    Mental_symptom_scores_FU1 = (PHQ9_Severity_FU1 / 28 + General_Anxiety_Disorder_Severity_FU1 / 21) * 10,
    Trauma_exposure = FU_recent_trauma
  )

# Recode FU1 trauma categories
resilience_FU1$Trauma_exposure[resilience_FU1$Trauma_exposure == 3] <- 2

# Remove missing data
resilience_FU1 <- na.omit(resilience_FU1)

# Compute partial correlation
FU1_results <- compute_partial_correlation(resilience_FU1, "Mental_symptom_scores_FU1", "Trauma_exposure")
FU1_results

# -------------------------------
# Follow-up 2 (FU2) analysis
# -------------------------------
resilience_FU2 <- resilience_group_R_cor %>%
  transmute(
    eid,
    Sex = factor(gender, levels = c(0,1), labels = c("Female","Male")),
    Age = age_BL,
    Ethnic = factor(Ethnic_group, levels = c("0","1","2","3"), labels = c("1_White","2_Asian","3_Black","4_Other")),
    Education = Education_year,
    BMI = BMI_BL,
    site = factor(site),
    selfresilience = self_resilience,
    # Combined mental symptom score: PHQ9 + GAD
    Mental_symptom_scores_FU2 = (`PHQ-9_FU2` / 28 + General_Anxiety_Disorder_Severity_FU2 / 21) * 10,
    Trauma_exposure = FU2_recent_trauma
  )

# Cap trauma exposure at 4
resilience_FU2$Trauma_exposure[resilience_FU2$Trauma_exposure >= 5] <- 4

# Remove missing data
resilience_FU2 <- na.omit(resilience_FU2)

# Compute partial correlation
FU2_results <- compute_partial_correlation(resilience_FU2, "Mental_symptom_scores_FU2", "Trauma_exposure")
FU2_results