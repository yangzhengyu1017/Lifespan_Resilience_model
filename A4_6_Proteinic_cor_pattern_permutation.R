# ==========================================
# Project: UK Biobank — Protein Correlates of Trauma and Resilience
# Script: protein_permutation_analysis.R
# Author: Zhengyu Yang
# Date: 2025-10-23
# ==========================================
# Purpose:
# This script investigates the relationship between circulating protein biomarkers,
# trauma exposure (trauma_num), and self-reported resilience (self_resilience)
# in the UK Biobank cohort. 
#
# Specifically, it performs the following steps:
# 1. Loads normalized protein expression data and merges it with the main resilience dataset.
# 2. Selects protein biomarkers identified as significant mediators (P < 0.001) from a prior mediation analysis.
# 3. Computes the correlation coefficients (r_T and r_R) between each protein and
#    trauma exposure or resilience, adjusting for demographic and health covariates.
# 4. Estimates the overall association (S = corr(r_T, r_R)) between protein–trauma and protein–resilience patterns.
# 5. Conducts a permutation test (N = 1000) to assess the significance of the observed S value.
# 6. Visualizes the permutation distribution and saves the results for further interpretation.
#
# The analysis aims to identify whether proteins showing strong associations with
# trauma exposure also tend to show similar or inverse associations with resilience,
# thus elucidating potential biological mechanisms underlying psychological adaptation.
# ==========================================

# ---------- Load Required Packages ----------
library(R.matlab)
library(data.table)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(caret)
library(dplyr)
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
library(lavaan)
library(broom)

# ---------- 1. Load and Prepare Protein Data ----------
# Load normalized protein matrix (.mat)
file_path <- "~/Documents/Data/UKB/UKB_Protein_norm.mat"
mat_data <- readMat(file_path)
proteinic_dat <- as.data.frame(mat_data$ukb.protein.norm)
proteinic_dat <- proteinic_dat[, -1]  # Remove redundant column
colnames(proteinic_dat)[1] <- "eid"

# Load protein names
proteinic_name <- read_csv("~/Documents/Data/UKB/id.csv")
new_colnames <- as.character(proteinic_name[[1]])

# Assign column names and check match
if (length(new_colnames) == (2921 - 2 + 1)) {
  colnames(proteinic_dat)[2:2921] <- new_colnames
} else {
  stop("Number of protein names does not match the dataset columns.")
}

# ---------- 2. Merge with Main Dataset ----------
resilience_group_R <- read.csv("/public/home/yangzy/Documents/Data/UKB/resilience_group_R.csv")
resilience_group_R <- merge(resilience_group_R, proteinic_dat, by = "eid", all.x = TRUE)

# ---------- 3. Prepare Data for Permutation ----------
# Import mediation results and select biomarkers with P < 0.001
file_path <- "/public/home/yangzy/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/protein_mediation_data.csv"
mediation_results_table <- read.csv(file_path)
mediation_results_table_short <- mediation_results_table %>% filter(P_value < 0.001)

# Select biomarker names
M_biomarker <- mediation_results_table_short$Biomarker
num_bio <- length(M_biomarker)

# Subset relevant biomarker columns
selected_columns <- select(resilience_group_R, all_of(M_biomarker))

# Construct permutation dataset
blood_permut_data <- resilience_group_R %>%
  transmute(
    eid,
    Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")),
    Age = age_BL,
    Ethnic = factor(Ethnic_group, levels = c("0", "1", "2", "3"),
                    labels = c("1_White", "2_Asian", "3_Black", "4_Other")),
    Education = Education_year,
    BMI = BMI_BL,
    selfresilience = self_resilience,
    PHQ = Depressive_Symptoms_PHQ4_BL,
    site = factor(site),
    trauma_num
  ) %>%
  bind_cols(selected_columns) %>%
  filter(complete.cases(select(., eid, Sex, Age, Ethnic, Education, BMI,
                               selfresilience, site, trauma_num, PHQ)))

blood_list <- M_biomarker

# ---------- 4. Define Function to Compute Statistics ----------
calc_stat <- function(data, blood_cols, trauma_col, resil_col) {
  r_T <- numeric(length(blood_cols))
  r_R <- numeric(length(blood_cols))
  
  for (i in seq_along(blood_cols)) {
    b <- blood_cols[i]
    
    # Trauma association
    temp_data_trauma <- data %>%
      select(all_of(b), all_of(trauma_col), Age, Sex, Ethnic, BMI, Education, site) %>%
      na.omit()
    lme_trauma <- lm(as.formula(paste0("`", b, "` ~ ", trauma_col,
                                       " + Age + Sex + Ethnic + BMI + Education + site")),
                     data = temp_data_trauma)
    results_trauma <- tidy(lme_trauma)
    n <- nrow(temp_data_trauma)
    k <- length(coef(lme_trauma)) - 1
    df <- n - k - 1
    r_T[i] <- results_trauma %>%
      filter(term == trauma_col) %>%
      mutate(correlation_r = statistic / sqrt(statistic^2 + df)) %>%
      pull(correlation_r)
    
    # Resilience association
    temp_data_resil <- data %>%
      select(all_of(b), all_of(resil_col), Age, Sex, Ethnic, BMI, Education, site) %>%
      na.omit()
    lme_resil <- lm(as.formula(paste0("`", b, "` ~ ", resil_col,
                                      " + Age + Sex + Ethnic + BMI + Education + site")),
                    data = temp_data_resil)
    results_resil <- tidy(lme_resil)
    n <- nrow(temp_data_resil)
    k <- length(coef(lme_resil)) - 1
    df <- n - k - 1
    r_R[i] <- results_resil %>%
      filter(term == resil_col) %>%
      mutate(correlation_r = statistic / sqrt(statistic^2 + df)) %>%
      pull(correlation_r)
  }
  
  S <- cor(r_T, r_R, use = "complete.obs")
  return(list(S = S, r_T = r_T, r_R = r_R))
}

# ---------- 5. Observed Statistics ----------
results_obs <- calc_stat(blood_permut_data, blood_list, "trauma_num", "selfresilience")
S_obs <- results_obs$S
r_T_obs <- results_obs$r_T
r_R_obs <- results_obs$r_R

cat("Observed S (corr(r_T, r_R)):", S_obs, "\n\n")

r_T_table <- data.frame(Biomarker = blood_list, r_T = r_T_obs)
r_R_table <- data.frame(Biomarker = blood_list, r_R = r_R_obs)

# ---------- 6. Permutation Test ----------
n_perm <- 1000
S_perm <- numeric(n_perm)
set.seed(123)

for (i in 1:n_perm) {
  perm_idx <- sample(nrow(blood_permut_data))
  perm_data <- blood_permut_data
  perm_data[, c("trauma_num", "selfresilience", "Sex", "Age", "Ethnic",
                "Education", "BMI", "site")] <- 
    blood_permut_data[perm_idx, c("trauma_num", "selfresilience", "Sex", "Age",
                                  "Ethnic", "Education", "BMI", "site")]
  
  S_perm[i] <- calc_stat(perm_data, blood_list, "trauma_num", "selfresilience")$S
}

# Compute permutation p-value
p_value <- mean(S_perm <= S_obs, na.rm = TRUE)
cat("Permutation p-value:", p_value, "\n")

# ---------- 7. Visualization ----------
perm_df <- data.frame(S_perm = S_perm)
ggplot(perm_df, aes(x = S_perm)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "skyblue") +
  geom_vline(xintercept = S_obs, color = "red", linetype = "dashed", size = 1) +
  labs(title = "Permutation Distribution of S",
       x = "S (corr(r_T, r_R))", y = "Frequency") +
  theme_minimal()

# ---------- 8. Save Outputs ----------
write.csv(r_T_table, "correlation_trauma.csv", row.names = FALSE)
write.csv(r_R_table, "correlation_resilience.csv", row.names = FALSE)
write.csv(S_perm,
          "/public/home/yangzy/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/protein_permutation_trauma_resilience_1000.csv",
          row.names = FALSE)