# ============================================================
# Project: UK Biobank – Blood Protein × Resilience Interaction Analysis
# Author: Zhengyu Yang
# Date: 2025-10-23
# ============================================================
# Purpose:
#   This script examines the associations between blood proteins 
#   and depressive symptoms (PHQ), focusing on:
#     (1) the main effects of individual proteins,
#     (2) the interaction effects between protein levels and self-reported resilience.
#   All models adjust for demographic and physiological covariates,
#   and FDR correction is applied to identify significant effects.
# ============================================================


# ---------- 1. Load Required Packages ----------
library(R.matlab)
library(tidyverse)
library(broom)
library(dplyr)


# ---------- 2. Load and Prepare Protein Data ----------
# Load normalized protein matrix (.mat)
file_path <- "~/Documents/Data/UKB/UKB_Protein_norm.mat"
mat_data <- readMat(file_path)
proteinic_dat <- as.data.frame(mat_data$ukb.protein.norm)
proteinic_dat <- proteinic_dat[, -1]  # Remove redundant column
colnames(proteinic_dat)[1] <- "eid"

# Load protein names
proteinic_name <- read_csv("~/Documents/Data/UKB/id.csv")
new_colnames <- as.character(proteinic_name[[1]])

# Assign column names and validate
if (length(new_colnames) == (2921 - 2 + 1)) {
  colnames(proteinic_dat)[2:2921] <- new_colnames
} else {
  stop("Number of protein names does not match the dataset columns.")
}


# ---------- 3. Merge with Main Dataset ----------
resilience_group_R <- read.csv("/public/home/yangzy/Documents/Data/UKB/resilience_group_R.csv")
resilience_group_R <- merge(resilience_group_R, proteinic_dat, by = "eid", all.x = TRUE)


# ---------- 4. Select Biomarkers from Mediation Results ----------
file_path <- "/public/home/yangzy/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/protein_mediation_data.csv"
mediation_results_table <- read.csv(file_path)
mediation_results_table_short <- mediation_results_table %>% filter(P_value < 0.001)

biomarker_list <- mediation_results_table_short$Biomarker


# ---------- 5. Initialize Result Tables ----------
interact_result_table <- data.frame()     # Full regression results
I_results_table <- data.frame()           # Interaction term results
main_effect_table <- data.frame()         # Main effect results


# ---------- 6. Loop Over Each Biomarker ----------
for (b in biomarker_list) {
  
  # Prepare dataset for regression
  resilience_test <- resilience_group_R %>%
    transmute(
      eid,
      Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")),
      Age = age_BL,
      Ethnic = factor(Ethnic_group, levels = c("0", "1", "2", "3"),
                      labels = c("1_White", "2_Asian", "3_Black", "4_Other")),
      Education = Education_year,
      site = factor(site),
      BMI = BMI_BL,
      selfresilience = self_resilience,
      PHQ = Depressive_Symptoms_PHQ4_BL,
      !!b := !!sym(b)
    ) %>%
    filter(selfresilience != 1) %>%   # Exclude participants in the "medium" group
    na.omit()                         # Remove missing values
  
  # Linear regression model:
  # PHQ ~ biomarker + resilience + biomarker*resilience + covariates
  fit <- lm(
    as.formula(paste0(
      "PHQ ~ `", b, "` + selfresilience + `", b, "`*selfresilience + ",
      "Age + Sex + Ethnic + BMI + Education + site"
    )),
    data = resilience_test
  )
  
  # Extract regression results
  fit_result <- tidy(fit)
  
  # Store full regression output
  interact_result_table <- bind_rows(interact_result_table, fit_result)
  
  # Extract interaction term (biomarker × selfresilience)
  inter_term <- fit_result %>%
    filter(grepl(paste0(b, ".*selfresilience"), term)) %>%
    mutate(Biomarker = b)
  I_results_table <- bind_rows(I_results_table, inter_term)
  
  # Extract main effect (biomarker)
  main_term <- fit_result %>%
    filter(term == b) %>%
    mutate(Biomarker = b)
  main_effect_table <- bind_rows(main_effect_table, main_term)
}


# ---------- 7. Add Stage Labels ----------
I_results_table$Stage <- "Blood Protein"
main_effect_table$Stage <- "Blood Protein"


# ---------- 8. Multiple Comparison Correction (FDR) ----------
# FDR correction for interaction terms
I_results_table$adjusted_p <- p.adjust(I_results_table$p.value, method = "BH")
I_results_table_sig <- I_results_table %>% filter(adjusted_p < 0.05)

# FDR correction for main effects
main_effect_table$adjusted_p <- p.adjust(main_effect_table$p.value, method = "BH")
main_effect_table_sig <- main_effect_table %>% filter(adjusted_p < 0.05)


# ---------- 9. Display Results ----------
cat("Significant interaction effects (FDR < 0.05):\n")
print(head(I_results_table_sig))

cat("\nSignificant main effects (FDR < 0.05):\n")
print(head(main_effect_table_sig))