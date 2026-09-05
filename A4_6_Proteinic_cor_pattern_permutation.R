# ==============================================================================
# Script: A4_6_Proteinic_cor_pattern_permutation.R
# Project: Lifespan Resilience Modeling (STM Submission)
# Purpose: Perform permutation analysis of plasma proteomic biomarker correlation patterns with trauma and resilience to assess pattern-level significance.
# ==============================================================================

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


# Ensure output directory exists
if (!dir.exists("results")) dir.create("results", recursive = TRUE)

# ---------- 1. Load and Prepare Protein Data ----------
# Load normalized protein matrix (.mat)
file_path <- "data/UKB_Protein_norm.mat"
mat_data <- readMat(file_path)
proteinic_dat <- as.data.frame(mat_data$ukb.protein.norm)
proteinic_dat <- proteinic_dat[, -1]  # Remove redundant column
colnames(proteinic_dat)[1] <- "eid"

# Load protein names
proteinic_name <- read_csv("data/id.csv")
new_colnames <- as.character(proteinic_name[[1]])

# Assign column names and check match
if (length(new_colnames) == (2921 - 2 + 1)) {
  colnames(proteinic_dat)[2:2921] <- new_colnames
} else {
  stop("Number of protein names does not match the dataset columns.")
}

# ---------- 2. Merge with Main Dataset ----------
resilience_group_R <- read_csv("data/UKB_dat_for_analysis_1226.csv")[,-1]
resilience_group_R <- merge(resilience_group_R, proteinic_dat, by = "eid", all.x = TRUE)

resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 2, 6, Ethnic_group))
resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 3, 2, Ethnic_group))
resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 5, 2, Ethnic_group))
resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 4, 3, Ethnic_group))
resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 6, 4, Ethnic_group))

resilience_group_R$Ethnic_group <- resilience_group_R$Ethnic_group - 1

# ---------- 3. Prepare Data for Permutation ----------
cat("Step 3: Preparing Mediation Subset...\n")
file_path_med <- "data/protein_mediation_data.csv"
mediation_results_table <- read.csv(file_path_med)
M_biomarker <- mediation_results_table %>% filter(P_value < 0.001) %>% pull(Biomarker)
blood_list <- M_biomarker

# Pre-filter participants with complete covariates and primary psychological variables
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
  bind_cols(dplyr::select(resilience_group_R, all_of(blood_list))) %>%
  filter(complete.cases(Age, Sex, Ethnic, BMI, Education, site, trauma_num, selfresilience, PHQ))

covariate_str <- "Age + Sex + Ethnic + BMI + Education + site"

# ---------- 4. Define Extreme-Fast Statistics Function ----------
# Require complete cases across protein, PHQ, trauma, and resilience
# Use fit$df.residual for exact model degrees of freedom
calc_stat_fast <- function(data, blood_cols, trauma_col, resil_col) {
  r_T <- numeric(length(blood_cols))
  r_R <- numeric(length(blood_cols))
  
  for (i in seq_along(blood_cols)) {
    b <- blood_cols[i]
    
    # Ensure complete alignment across protein expression, trauma, resilience, and depression
    temp_data <- data %>%
      dplyr::select(all_of(c(b, trauma_col, resil_col, "PHQ", "Age", "Sex", "Ethnic", "BMI", "Education", "site"))) %>%
      na.omit()
    
    if(nrow(temp_data) < 50) {
      r_T[i] <- NA; r_R[i] <- NA
      next
    }
    
    # Fast linear regression for trauma exposure
    fit_T <- lm(as.formula(paste0("`", b, "` ~ ", trauma_col, " + ", covariate_str)), data = temp_data)
    df_res <- fit_T$df.residual
    t_T <- summary(fit_T)$coefficients[trauma_col, "t value"]
    
    # Fast linear regression for resilience
    fit_R <- lm(as.formula(paste0("`", b, "` ~ ", resil_col, " + ", covariate_str)), data = temp_data)
    t_R <- summary(fit_R)$coefficients[resil_col, "t value"]
    
    # Convert t-statistic to correlation coefficient
    r_T[i] <- t_T / sqrt(t_T^2 + df_res)
    r_R[i] <- t_R / sqrt(t_R^2 + df_res)
  }
  
  S <- cor(r_T, r_R, use = "complete.obs")
  return(list(S = S, r_T = r_T, r_R = r_R))
}

# ---------- 5. Observed Statistics ----------
cat("Step 4: Calculating Observed Correlations...\n")
results_obs <- calc_stat_fast(blood_permut_data, blood_list, "trauma_num", "selfresilience")
S_obs <- results_obs$S

cat("Observed S (corr(r_T, r_R)):", S_obs, "\n\n")

r_T_table <- data.frame(Biomarker = blood_list, r_T = results_obs$r_T)
r_R_table <- data.frame(Biomarker = blood_list, r_R = results_obs$r_R)

# ---------- 6. Corrected Permutation Test ----------
cat("Step 5: Running Fast Permutation Test (N = 1000)...\n")
n_perm <- 1000
S_perm <- numeric(n_perm)

set.seed(123)
for (i in 1:n_perm) {
  perm_idx <- sample(nrow(blood_permut_data))
  perm_data <- blood_permut_data
  
  # Permute only psychological predictors to preserve biological covariance with covariates
  perm_data[, c("trauma_num", "selfresilience")] <- 
    blood_permut_data[perm_idx, c("trauma_num", "selfresilience")]
  
  # Inline fast computation extracting t-statistic directly
  r_T_p <- numeric(length(blood_list))
  r_R_p <- numeric(length(blood_list))
  
  for (j in seq_along(blood_list)) {
    b <- blood_list[j]
    temp <- perm_data %>% 
      dplyr::select(all_of(c(b, "trauma_num", "selfresilience", "PHQ", "Age", "Sex", "Ethnic", "BMI", "Education", "site"))) %>% 
      na.omit()
    
    if(nrow(temp) < 50) next
    
    fit_T <- lm(as.formula(paste0("`", b, "` ~ trauma_num + ", covariate_str)), data = temp)
    df_res <- fit_T$df.residual
    
    t_T <- summary(fit_T)$coefficients["trauma_num", "t value"]
    t_R <- summary(lm(as.formula(paste0("`", b, "` ~ selfresilience + ", covariate_str)), data = temp))$coefficients["selfresilience", "t value"]
    
    r_T_p[j] <- t_T / sqrt(t_T^2 + df_res)
    r_R_p[j] <- t_R / sqrt(t_R^2 + df_res)
  }
  
  S_perm[i] <- cor(r_T_p, r_R_p, use = "complete.obs")
}

# Calculate two-sided permutation p-value
p_value <- mean(abs(S_perm) >= abs(S_obs), na.rm = TRUE)
cat("Corrected Permutation p-value (Two-tailed):", p_value, "\n")

# ---------- 7. Visualization ----------
cat("Step 6: Visualizing & Saving Results...\n")
perm_df <- data.frame(S_perm = S_perm)

ggplot(perm_df, aes(x = S_perm)) +
  geom_histogram(bins = 40, fill = "#4DBBD5B2", color = "white", alpha = 0.8) +
  geom_vline(xintercept = S_obs, color = "#E64B35FF", linetype = "dashed", linewidth = 1.2) +
  labs(
    title = "Permutation Distribution of Global Pattern (S)",
    x = "Permuted Correlation S (r_Trauma vs r_Resilience)", 
    y = "Frequency"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )

ggsave("results/protein_permutation_plot_updated.png", 
       width = 8, height = 6)

# ---------- 8. Save Outputs ----------
write.csv(r_T_table, "results/protein_correlation_trauma_updated.csv", row.names = FALSE)
write.csv(r_R_table, "results/protein_correlation_resilience_updated.csv", row.names = FALSE)
write.csv(S_perm, "results/protein_permutation_trauma_resilience_1000_updated.csv", row.names = FALSE)

cat("Analysis completed. Results saved.\n")