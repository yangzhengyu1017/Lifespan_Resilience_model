# ==============================================================================
# Script: A4_2_Blood_cor_pattern_permutation.R
# Project: Lifespan Resilience Modeling (STM Submission)
# Purpose: Conduct permutation testing of blood biomarker correlation patterns with trauma exposure and resilience to evaluate global pattern consistency.
# ==============================================================================


# Ensure output directory exists
if (!dir.exists("results")) dir.create("results", recursive = TRUE)

## ============================================================
## Blood biomarker–resilience correlation and permutation test
## (Optimized Pairwise Deletion & Fast Permutation Version)
## ============================================================

## -------------------- Environment setup --------------------
if(!require(pacman)) install.packages("pacman")
pacman::p_load(data.table, tidyverse, broom, ggplot2)

## -------------------- Data import and integration --------------------
cat("Loading and processing data...\n")
resilience_group_R <- read_csv("data/UKB_dat_for_analysis_1226.csv")[,-1]

# Recode ethnic group categories
resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 2, 6, Ethnic_group))
resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 3, 2, Ethnic_group))
resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 5, 2, Ethnic_group))
resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 4, 3, Ethnic_group))
resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 6, 4, Ethnic_group))

resilience_group_R$Ethnic_group <- resilience_group_R$Ethnic_group - 1

Blood_count <- read.csv("data/Blood_count.csv")
blood_biochemistry <- read.csv("data/blood_biochemistry.csv")

# Merge blood count and biochemistry data
resilience_group_R <- resilience_group_R %>%
  merge(Blood_count, by = "eid", all.x = TRUE) %>%
  merge(blood_biochemistry, by = "eid", all.x = TRUE)

base::colnames(resilience_group_R) <- base::gsub(" ", "_", base::colnames(resilience_group_R))

# Read mediation analysis results
file_path <- "data/blood_mediation_data_1203.csv"
mediation_results_table <- read.csv(file_path)
M_biomarker <- mediation_results_table %>% filter(P_value < 0.001) %>% pull(Biomarker)
M_biomarker <- base::gsub(" ", "_", M_biomarker)
blood_list <- M_biomarker

## -------------------- Build blood dataset --------------------
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

## -------------------- Define correlation computation --------------------
# Enforce identical complete cases across trauma and resilience models
# Extract exact model residual degrees of freedom
calc_stat_fast <- function(data, blood_cols, trauma_col, resil_col) {
  r_T <- numeric(length(blood_cols))
  r_R <- numeric(length(blood_cols))
  
  for (i in seq_along(blood_cols)) {
    b <- blood_cols[i]
    
    # Pairwise complete cases for current biomarker
    temp_data <- data %>%
      dplyr::select(all_of(c(b, trauma_col, resil_col, "Age", "Sex", "Ethnic", "BMI", "Education", "site"))) %>%
      na.omit()
    
    if (nrow(temp_data) < 50) {
      r_T[i] <- NA; r_R[i] <- NA
      next
    }
    
    # Linear regression for trauma exposure
    lme_trauma <- lm(as.formula(paste0("`", b, "` ~ ", trauma_col, " + ", covariate_str)), data = temp_data)
    df_res <- lme_trauma$df.residual
    t_val_T <- summary(lme_trauma)$coefficients[trauma_col, "t value"]
    r_T[i] <- t_val_T / sqrt(t_val_T^2 + df_res)
    
    # Linear regression for self-reported resilience
    lme_resil <- lm(as.formula(paste0("`", b, "` ~ ", resil_col, " + ", covariate_str)), data = temp_data)
    t_val_R <- summary(lme_resil)$coefficients[resil_col, "t value"]
    r_R[i] <- t_val_R / sqrt(t_val_R^2 + df_res)
  }
  
  S <- cor(r_T, r_R, use = "complete.obs")
  return(list(S = S, r_T = r_T, r_R = r_R))
}

## -------------------- Observed correlation --------------------
cat("Calculating observed S value...\n")
results_obs <- calc_stat_fast(blood_permut_data, blood_list, "trauma_num", "selfresilience")
S_obs <- results_obs$S
r_T_obs <- results_obs$r_T
r_R_obs <- results_obs$r_R

cat("Observed S (corr(r_T, r_R)):", S_obs, "\n\n")

# Output correlation tables
r_T_table <- data.frame(Biomarker = blood_list, r_T = r_T_obs)
r_R_table <- data.frame(Biomarker = blood_list, r_R = r_R_obs)

## -------------------- Permutation test --------------------
cat("Starting extreme-fast Permutation loop (1000 iterations)...\n")
n_perm <- 1000
S_perm <- numeric(n_perm)

set.seed(123)
for (i in 1:n_perm) {
  perm_idx <- sample(nrow(blood_permut_data))
  perm_data <- blood_permut_data
  
  # Permute focal psychological predictor while preserving biomarker-covariate covariance structure
  perm_data[, c("trauma_num", "selfresilience")] <- 
    blood_permut_data[perm_idx, c("trauma_num", "selfresilience")]
  
  # Vectorized computation extracting t-statistics directly
  r_T_p <- numeric(length(blood_list))
  r_R_p <- numeric(length(blood_list))
  
  for (j in seq_along(blood_list)) {
    b <- blood_list[j]
    temp <- perm_data %>% 
      dplyr::select(all_of(c(b, "trauma_num", "selfresilience", "Age", "Sex", "Ethnic", "BMI", "Education", "site"))) %>% 
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

# Compute permutation-based p-value
p_value <- mean(abs(S_perm) >= abs(S_obs), na.rm = TRUE)
cat("Permutation p-value:", p_value, "\n")

## -------------------- Visualization --------------------
perm_df <- data.frame(S_perm = S_perm)

ggplot(perm_df, aes(x = S_perm)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "white") +
  geom_vline(xintercept = S_obs, color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Permutation Distribution of S (True Resilience)",
    x = "S (corr(r_T, r_R))",
    y = "Frequency"
  ) +
  theme_minimal()

ggsave("results/permutation_plot_updated.png", 
       width = 8, height = 6)

## -------------------- Save results --------------------
write.csv(r_T_table, "results/correlation_trauma_updated.csv", row.names = FALSE)
write.csv(r_R_table, "results/correlation_resilience_updated.csv", row.names = FALSE)
write.csv(S_perm, "results/blood_permutation_trauma_resilience_1000_updated.csv", row.names = FALSE)

cat("All operations completed successfully!\n")
