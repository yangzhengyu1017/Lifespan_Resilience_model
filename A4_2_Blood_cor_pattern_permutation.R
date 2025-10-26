## ============================================================
## Blood biomarker–resilience correlation and permutation test
## ============================================================

## -------------------- Environment setup --------------------
library(data.table)
library(tidyverse)
library(broom)
library(ggplot2)

## -------------------- Data import and integration --------------------
resilience_group_R <- read.csv("/public/home/yangzy/Documents/Data/UKB/resilience_group_R.csv")
Blood_count <- read.csv("/public/home/yangzy/Documents/Data/UKB/Blood_count.csv")
blood_biochemistry <- read.csv("/public/home/yangzy/Documents/Data/UKB/blood_biochemistry.csv")

# Merge blood count and biochemistry data
resilience_group_R <- resilience_group_R %>%
  merge(Blood_count, by = "eid", all.x = TRUE) %>%
  merge(blood_biochemistry, by = "eid", all.x = TRUE)

# Read mediation analysis results
file_path <- "/public/home/yangzy/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/blood_mediation_data_1203.csv"
mediation_results_table <- read.csv(file_path)
mediation_results_table_short <- mediation_results_table %>%
  filter(P_value < 0.001)

# Extract biomarkers identified as significant mediators
t <- "trauma_num"
M_biomarker <- mediation_results_table_short$Biomarker
num_bio <- length(M_biomarker)

## -------------------- Build blood dataset --------------------
selected_columns <- select(resilience_group_R, all_of(M_biomarker))

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
  filter(complete.cases(select(., eid, Sex, Age, Ethnic, Education, BMI, selfresilience, site, trauma_num, PHQ)))

blood_list <- M_biomarker

## -------------------- Define correlation computation --------------------
calc_stat <- function(data, blood_cols, trauma_col, resil_col) {
  r_T <- numeric(length(blood_cols))
  r_R <- numeric(length(blood_cols))
  
  for (i in seq_along(blood_cols)) {
    b <- blood_cols[i]
    
    # Correlation with trauma
    temp_data_trauma <- data %>%
      select(all_of(b), all_of(trauma_col), Age, Sex, Ethnic, BMI, Education, site) %>%
      na.omit()
    lme_trauma <- lm(as.formula(paste0("`", b, "` ~ ", trauma_col, " + Age + Sex + Ethnic + BMI + Education + site")),
                     data = temp_data_trauma)
    results_trauma <- tidy(lme_trauma)
    n <- nrow(temp_data_trauma)
    df <- n - (length(coef(lme_trauma)) - 1) - 1
    r_T[i] <- results_trauma %>%
      filter(term == trauma_col) %>%
      mutate(r = statistic / sqrt(statistic^2 + df)) %>%
      pull(r)
    
    # Correlation with self-resilience
    temp_data_resil <- data %>%
      select(all_of(b), all_of(resil_col), Age, Sex, Ethnic, BMI, Education, site) %>%
      na.omit()
    lme_resil <- lm(as.formula(paste0("`", b, "` ~ ", resil_col, " + Age + Sex + Ethnic + BMI + Education + site")),
                    data = temp_data_resil)
    results_resil <- tidy(lme_resil)
    n <- nrow(temp_data_resil)
    df <- n - (length(coef(lme_resil)) - 1) - 1
    r_R[i] <- results_resil %>%
      filter(term == resil_col) %>%
      mutate(r = statistic / sqrt(statistic^2 + df)) %>%
      pull(r)
  }
  
  S <- cor(r_T, r_R, use = "complete.obs")
  return(list(S = S, r_T = r_T, r_R = r_R))
}

## -------------------- Observed correlation --------------------
results_obs <- calc_stat(blood_permut_data, blood_list, "trauma_num", "selfresilience")
S_obs <- results_obs$S
r_T_obs <- results_obs$r_T
r_R_obs <- results_obs$r_R

cat("Observed S (corr(r_T, r_R)):", S_obs, "\n\n")

# Output correlation tables
r_T_table <- data.frame(Biomarker = blood_list, r_T = r_T_obs)
r_R_table <- data.frame(Biomarker = blood_list, r_R = r_R_obs)

## -------------------- Permutation test --------------------
n_perm <- 1000
S_perm <- numeric(n_perm)

set.seed(123)
for (i in 1:n_perm) {
  perm_idx <- sample(nrow(blood_permut_data))
  perm_data <- blood_permut_data
  perm_data[, c("trauma_num", "selfresilience", "Sex", "Age", "Ethnic", "Education", "BMI", "site")] <-
    blood_permut_data[perm_idx, c("trauma_num", "selfresilience", "Sex", "Age", "Ethnic", "Education", "BMI", "site")]
  
  S_perm[i] <- calc_stat(perm_data, blood_list, "trauma_num", "selfresilience")$S
}

# Compute permutation-based p-value
p_value <- mean(S_perm <= S_obs, na.rm = TRUE)
cat("Permutation p-value:", p_value, "\n")

## -------------------- Visualization --------------------
perm_df <- data.frame(S_perm = S_perm)

ggplot(perm_df, aes(x = S_perm)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "white") +
  geom_vline(xintercept = S_obs, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Permutation Distribution of S",
    x = "S (corr(r_T, r_R))",
    y = "Frequency"
  ) +
  theme_minimal()

ggsave("/public/home/yangzy/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/permutation_plot.png", 
       width = 8, height = 6)

## -------------------- Save results --------------------
write.csv(r_T_table, "/public/home/yangzy/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/correlation_trauma.csv", row.names = FALSE)
write.csv(r_R_table, "/public/home/yangzy/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/correlation_resilience.csv", row.names = FALSE)
write.csv(S_perm, "/public/home/yangzy/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/blood_permutation_trauma_resilience_1000.csv", row.names = FALSE)