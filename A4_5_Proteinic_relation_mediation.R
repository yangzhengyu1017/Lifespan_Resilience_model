# ============================================================
# Title: UK Biobank Protein Biomarker Analysis
# Author: Zhengyu Yang
# Purpose:
#   1. Load normalized protein data and merge with the resilience dataset.
#   2. Examine associations of protein biomarkers with:
#        - Trauma exposure
#        - Depressive symptoms (PHQ)
#        - Self-reported resilience
#   3. Identify overlapping biomarkers significant in both trauma and PHQ models.
#   4. Conduct mediation analyses testing whether selected proteins mediate
#      the relationship between trauma exposure and depressive symptoms.
#   5. Visualize the correlations between biomarkers and psychological variables
#      (trauma, PHQ, and resilience) using a heatmap.
# ============================================================

# ===============================
# Load Required Packages
# ===============================
library(R.matlab)
library(tidyverse)
library(dplyr)
library(broom)
library(rstatix)
library(lme4)
library(mediation)
library(ggplot2)

# ===============================
# 1. Load and Prepare Protein Data
# ===============================

file_path <- "~/Documents/Data/UKB/UKB_Protein_norm.mat"
mat_data <- readMat(file_path)
proteinic_dat <- as.data.frame(mat_data$ukb.protein.norm)
proteinic_dat <- proteinic_dat[, -1]
colnames(proteinic_dat)[1] <- "eid"

proteinic_name <- read_csv("~/Documents/Data/UKB/id.csv")
new_colnames <- as.character(proteinic_name[[1]])

if (length(new_colnames) == (2921 - 2 + 1)) {
  colnames(proteinic_dat)[2:2921] <- new_colnames
} else {
  stop("Number of protein names does not match dataset columns.")
}

# ===============================
# 2. Merge with Main Dataset
# ===============================
resilience_group_R <- read.csv("/public/home/yangzy/Documents/Data/UKB/resilience_group_R.csv")
resilience_group_R <- merge(resilience_group_R, proteinic_dat, by = "eid", all.x = TRUE)

# ===============================
# 3. Define Variables
# ===============================
trauma_var <- "trauma_num"
biomarker_list <- colnames(resilience_group_R[, 558:3477])

# ===============================
# 4. Association: Biomarkers ~ Trauma
# ===============================
trauma_result_table <- data.frame()

for (b in biomarker_list) {
  dat <- resilience_group_R %>%
    transmute(eid,
              Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")),
              Age = age_BL,
              Ethnic = factor(Ethnic_group, levels = c("0", "1", "2", "3"),
                              labels = c("1_White", "2_Asian", "3_Black", "4_Other")),
              Education = Education_year,
              BMI = BMI_BL,
              site = factor(site),
              selfresilience = self_resilience,
              PHQ = Depressive_Symptoms_PHQ4_BL,
              trauma_num,
              !!b := !!sym(b)) %>%
    na.omit()
  
  lm_model <- lm(as.formula(paste0("`", b, "` ~ ", trauma_var, " + Age + Sex + Ethnic + BMI + Education + site")), data = dat)
  res <- tidy(lm_model) %>% filter(term == trauma_var)
  
  n <- nrow(dat)
  k <- length(coef(lm_model)) - 1
  df <- n - k - 1
  res$correlation_r <- res$statistic / sqrt(res$statistic^2 + df)
  
  res$Biomarker <- b
  res$Stage <- "Protein"
  res$Subject_Count <- n
  trauma_result_table <- bind_rows(trauma_result_table, res)
}

trauma_result_table$adjusted_p <- p.adjust(trauma_result_table$p.value, method = "BH")

write.csv(trauma_result_table,
          "/public/home/yangzy/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/trauma_result_table.csv",
          row.names = FALSE)

# ===============================
# 5. Association: Biomarkers ~ PHQ
# ===============================
PHQ_results_table <- data.frame()

for (b in biomarker_list) {
  dat <- resilience_group_R %>%
    transmute(eid,
              Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")),
              Age = age_BL,
              Ethnic = factor(Ethnic_group, levels = c("0", "1", "2", "3"),
                              labels = c("1_White", "2_Asian", "3_Black", "4_Other")),
              Education = Education_year,
              BMI = BMI_BL,
              site = factor(site),
              PHQ = Depressive_Symptoms_PHQ4_BL,
              !!b := !!sym(b)) %>%
    na.omit()
  
  lm_model <- lm(as.formula(paste0("`", b, "` ~ PHQ + Age + Sex + Ethnic + BMI + Education + site")), data = dat)
  res <- tidy(lm_model) %>% filter(term == "PHQ")
  
  n <- nrow(dat)
  k <- length(coef(lm_model)) - 1
  df <- n - k - 1
  res$correlation_r <- res$statistic / sqrt(res$statistic^2 + df)
  
  res$Biomarker <- b
  res$Stage <- "Protein"
  res$Subject_Count <- n
  PHQ_results_table <- bind_rows(PHQ_results_table, res)
}

PHQ_results_table$adjusted_p <- p.adjust(PHQ_results_table$p.value, method = "BH")

write.csv(PHQ_results_table,
          "/public/home/yangzy/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/blood_protein_PHQ_results_table.csv",
          row.names = FALSE)

# ===============================
# 6. Identify Overlapping Significant Biomarkers
# ===============================
both_results_adj <- merge(trauma_result_table, PHQ_results_table, by = "Biomarker") %>%
  filter(adjusted_p.x < 0.05 & adjusted_p.y < 0.05)

write.csv(both_results_adj,
          "/public/home/yangzy/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/blood_protein_both_result.csv",
          row.names = FALSE)

# ===============================
# 7. Association: Biomarkers ~ Self-Resilience
# ===============================
R_results_table <- data.frame()

for (b in biomarker_list) {
  dat <- resilience_group_R %>%
    transmute(eid,
              Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")),
              Age = age_BL,
              Ethnic = factor(Ethnic_group, levels = c("0", "1", "2", "3"),
                              labels = c("1_White", "2_Asian", "3_Black", "4_Other")),
              Education = Education_year,
              BMI = BMI_BL,
              site = factor(site),
              selfresilience = self_resilience,
              !!b := !!sym(b)) %>%
    na.omit()
  
  lm_model <- lm(as.formula(paste0("`", b, "` ~ selfresilience + Age + Sex + Ethnic + BMI + Education + site")), data = dat)
  res <- tidy(lm_model) %>% filter(term == "selfresilience")
  
  n <- nrow(dat)
  k <- length(coef(lm_model)) - 1
  df <- n - k - 1
  res$correlation_r <- res$statistic / sqrt(res$statistic^2 + df)
  
  res$Biomarker <- b
  res$Stage <- "Protein"
  res$Subject_Count <- n
  R_results_table <- bind_rows(R_results_table, res)
}

R_results_table$adjusted_p <- p.adjust(R_results_table$p.value, method = "BH")

write.csv(R_results_table,
          "/public/home/yangzy/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/R_results_table_protein.csv",
          row.names = FALSE)

# ===============================
# 8. Mediation Analysis
# ===============================
M_vars <- both_results_adj$Biomarker
mediation_results_table <- data.frame()

for (m in M_vars) {
  dat <- resilience_group_R %>%
    transmute(eid,
              Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")),
              Age = age_BL,
              Ethnic = factor(Ethnic_group, levels = c("0", "1", "2", "3"),
                              labels = c("1_White", "2_Asian", "3_Black", "4_Other")),
              Education = Education_year,
              BMI = BMI_BL,
              selfresilience = self_resilience,
              site = factor(site),
              PHQ = Depressive_Symptoms_PHQ4_BL,
              trauma_num,
              !!m := !!sym(m)) %>%
    na.omit()
  
  model.M <- lm(as.formula(paste(m, "~ trauma_num + Sex + Age + Ethnic + Education + BMI")), dat)
  model.Y <- lm(as.formula(paste("PHQ ~ ", m, " + trauma_num + Sex + Age + Ethnic + Education + BMI")), dat)
  
  results <- mediate(model.M, model.Y, treat = "trauma_num", mediator = m, boot = TRUE, sims = 1000)
  summary_res <- summary(results)
  
  mediation_results_table <- bind_rows(mediation_results_table, data.frame(
    Biomarker = m,
    Estimate = summary_res$d0,
    CI_lower = summary_res$d0.ci[1],
    CI_upper = summary_res$d0.ci[2],
    P_value = summary_res$d0.p,
    Prop_Mediated = summary_res$n0,
    Prop_Mediated_ci_lower = summary_res$n0.ci[1],
    Prop_Mediated_ci_upper = summary_res$n0.ci[2]
  ))
}

write.csv(mediation_results_table,
          "/public/home/yangzy/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/protein_mediation_data.csv",
          row.names = FALSE)

# ===============================
# 9. Visualization: Correlation Heatmap
# ===============================
mediation_results_table_short <- mediation_results_table %>% filter(P_value < 0.05)
colnames(mediation_results_table_short)[5] <- "mediation_p"

# Merge for each term
phq_plot_dat <- merge(PHQ_results_table, mediation_results_table_short[, c("Biomarker", "mediation_p")], by = "Biomarker", all.x = TRUE)
phq_plot_dat$term <- "Affective Disorders"

trauma_plot_dat <- merge(trauma_result_table, mediation_results_table_short[, c("Biomarker", "mediation_p")], by = "Biomarker", all.x = TRUE)
trauma_plot_dat$term <- "Trauma Exposure"

resilience_plot_dat <- merge(R_results_table, mediation_results_table_short[, c("Biomarker", "mediation_p")], by = "Biomarker", all.x = TRUE)
resilience_plot_dat$term <- "Resilience"

# Combine data
blood_plot_dat <- rbind(phq_plot_dat, trauma_plot_dat, resilience_plot_dat) %>%
  arrange(factor(term, levels = c("Trauma Exposure", "Affective Disorders", "Resilience")), desc(statistic))

# Factor ordering
blood_plot_dat$term <- factor(blood_plot_dat$term,
                              levels = c("Trauma Exposure", "Affective Disorders", "Resilience"),
                              ordered = TRUE)

# Add significance labels
add_significance <- function(p) {
  ifelse(p < 0.001, "***",
         ifelse(p < 0.01, "**",
                ifelse(p < 0.05, "*", "")))
}

# Heatmap
ggplot(blood_plot_dat, aes(x = term, y = Biomarker, fill = correlation_r)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  geom_text(aes(label = paste(round(correlation_r, 3), add_significance(adjusted_p))), size = 3) +
  theme_minimal() +
  labs(x = "", y = "Biomarkers", fill = "Correlation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))