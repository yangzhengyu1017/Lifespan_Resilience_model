# ==============================================================================
# Script: A1_2_Cor_Trauma_Mental_in_Different_Group_V2.R
# Project: Lifespan Resilience Modeling (STM Submission)
# Purpose: Examine correlation differences between trauma exposure and mental health across resilience groups (Low, Medium, High) in UK Biobank.
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. Load Required Packages
# ------------------------------------------------------------------------------
library(readr)
library(dplyr)
library(tidyr)
library(stats)
library(ggplot2)
library(ggpubr)
library(autoReg)
library(broom)
library(cocor)
library(gridExtra)

# ------------------------------------------------------------------------------
# 1. Global Data Import & Preprocessing
# ------------------------------------------------------------------------------
cat("\n[1/5] Initializing global data and performing feature engineering...\n")

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
    self_resilience_cont = (self_resilience + 6) / 6,
    
    # Standardize demographics
    Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")),
    Age = age_BL,
    BMI = BMI_BL,
    Education = Education_year,
    site = factor(site),
    
    # Safely recode Ethnic group to avoid NA generation (Fixed Logic)
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
    
    # Standardize FU2 trauma score globally
    FU2_recent_trauma_adj = FU2_recent_trauma * 10
  )

# Calculate quantiles and create Categorical Resilience
quantiles <- stats::quantile(df_global$self_resilience_cont, probs = c(1/3, 2/3), na.rm = TRUE)

df_global <- df_global %>%
  dplyr::mutate(
    selfresilience = dplyr::case_when(
      self_resilience_cont <= quantiles[1] ~ "1_Low",
      self_resilience_cont > quantiles[1] & self_resilience_cont < quantiles[2] ~ "2_Medium",
      self_resilience_cont >= quantiles[2] ~ "3_High"
    ),
    selfresilience = factor(selfresilience, levels = c("1_Low", "2_Medium", "3_High"))
  )

resilience_levels <- levels(df_global$selfresilience)


# ==============================================================================
# 2. Baseline (BL) Stage Analysis
# Rule: Trauma >= 5 is recoded to 4
# ==============================================================================
cat("\n[2/5] Running Baseline (BL) Analysis...\n")

df_BL <- df_global %>%
  dplyr::transmute(
    eid, Age, Sex, Ethnic, BMI, Education, site, selfresilience,
    PHQ_BL = Depressive_Symptoms_PHQ4_BL, 
    Trauma_exposure = trauma_num
  ) %>%
  dplyr::mutate(Trauma_exposure = ifelse(Trauma_exposure >= 5, 4, Trauma_exposure)) %>%
  # Targeted drop_na to prevent Attrition Bias from unrelated variables
  tidyr::drop_na(PHQ_BL, Trauma_exposure, selfresilience, Age, Sex, Ethnic, BMI, Education, site)

# 2.1 Linear Mixed Model
lme_BL <- stats::lm(PHQ_BL ~ selfresilience * Trauma_exposure + Age + Sex + Ethnic + BMI + Education + site, data = df_BL)
print(summary(lme_BL))

# 2.2 Visualization (Line Plot)
max_vals_BL <- df_BL %>% dplyr::group_by(Trauma_exposure) %>% dplyr::summarise(max_y = mean(PHQ_BL))
L1 <- ggpubr::ggline(df_BL, x = "Trauma_exposure", y = "PHQ_BL", add = "mean_se", color = "selfresilience", palette = c("#E2C098", "#A6CAA8", "#85C3DC"), size = 1) +
  ggpubr::stat_compare_means(aes(group = selfresilience), label = "p.signif", label.y = max(max_vals_BL$max_y) + 1, size = 4) +
  ggplot2::labs(title = "Baseline Stage")

# 2.3 Partial Correlations (Using manual residual extraction for CIs)
cor_list_BL <- list()
for (lvl in resilience_levels) {
  sub_data <- df_BL %>% dplyr::filter(selfresilience == lvl)
  mod_m <- stats::lm(PHQ_BL ~ Age + Sex + Ethnic + BMI + Education + site, data = sub_data)
  mod_t <- stats::lm(Trauma_exposure ~ Age + Sex + Ethnic + BMI + Education + site, data = sub_data)
  cor_test <- stats::cor.test(stats::residuals(mod_m), stats::residuals(mod_t))
  
  cor_list_BL[[lvl]] <- data.frame(
    Timepoint = "BL", Resilience_Level = lvl, Correlation = cor_test$estimate, 
    Int_low = cor_test$conf.int[1], Int_high = cor_test$conf.int[2], Subject_Count = nrow(sub_data)
  )
}
cor_df_BL <- dplyr::bind_rows(cor_list_BL)

# 2.4 Cocor Fisher Z comparisons
cocor_list_BL <- list()
combos <- utils::combn(resilience_levels, 2)
for (i in 1:ncol(combos)) {
  g1 <- combos[1, i]; g2 <- combos[2, i]
  r1 <- cor_df_BL$Correlation[cor_df_BL$Resilience_Level == g1]; n1 <- cor_df_BL$Subject_Count[cor_df_BL$Resilience_Level == g1]
  r2 <- cor_df_BL$Correlation[cor_df_BL$Resilience_Level == g2]; n2 <- cor_df_BL$Subject_Count[cor_df_BL$Resilience_Level == g2]
  
  comp <- cocor::cocor.indep.groups(r1, r2, n1, n2, alternative = "two.sided")
  c_res <- cocor::get.cocor.results(comp)
  cocor_list_BL[[i]] <- data.frame(Stage = "BL", Group1 = g1, Group2 = g2, Z = c_res$fisher1925$statistic, p_value = c_res$fisher1925$p.value)
}


# ==============================================================================
# 3. Follow-Up 1 (FU1) Stage Analysis
# Rule: PHQ - 1. Trauma == 3 is recoded to 2
# ==============================================================================
cat("\n[3/5] Running Follow-Up 1 (FU1) Analysis...\n")

df_FU1 <- df_global %>%
  dplyr::transmute(
    eid, Age, Sex, Ethnic, BMI, Education, site, selfresilience,
    Mental_Symptom = ((PHQ9_Severity_FU1 - 1) / 27 + General_Anxiety_Disorder_Severity_FU1 / 21) * 10,
    Trauma_exposure = FU_recent_trauma
  ) %>%
  dplyr::mutate(Trauma_exposure = ifelse(Trauma_exposure == 3, 2, Trauma_exposure)) %>%
  tidyr::drop_na(Mental_Symptom, Trauma_exposure, selfresilience, Age, Sex, Ethnic, BMI, Education, site)

# 3.1 Linear Mixed Model
lme_FU1 <- stats::lm(Mental_Symptom ~ selfresilience * Trauma_exposure + Age + Sex + Ethnic + BMI + Education + site, data = df_FU1)
print(summary(lme_FU1))

# 3.2 Visualization (Line Plot)
max_vals_FU1 <- df_FU1 %>% dplyr::group_by(Trauma_exposure) %>% dplyr::summarise(max_y = mean(Mental_Symptom))
L2 <- ggpubr::ggline(df_FU1, x = "Trauma_exposure", y = "Mental_Symptom", add = "mean_se", color = "selfresilience", palette = c("#E2C098", "#A6CAA8", "#85C3DC"), size = 1) +
  ggpubr::stat_compare_means(aes(group = selfresilience), label = "p.signif", label.y = max(max_vals_FU1$max_y) + 1, size = 4) +
  ggplot2::labs(title = "FU1 Stage")

# 3.3 Partial Correlations
cor_list_FU1 <- list()
for (lvl in resilience_levels) {
  sub_data <- df_FU1 %>% dplyr::filter(selfresilience == lvl)
  mod_m <- stats::lm(Mental_Symptom ~ Age + Sex + Ethnic + BMI + Education + site, data = sub_data)
  mod_t <- stats::lm(Trauma_exposure ~ Age + Sex + Ethnic + BMI + Education + site, data = sub_data)
  cor_test <- stats::cor.test(stats::residuals(mod_m), stats::residuals(mod_t))
  
  cor_list_FU1[[lvl]] <- data.frame(
    Timepoint = "FU1", Resilience_Level = lvl, Correlation = cor_test$estimate, 
    Int_low = cor_test$conf.int[1], Int_high = cor_test$conf.int[2], Subject_Count = nrow(sub_data)
  )
}
cor_df_FU1 <- dplyr::bind_rows(cor_list_FU1)

# 3.4 Cocor Fisher Z comparisons
cocor_list_FU1 <- list()
for (i in 1:ncol(combos)) {
  g1 <- combos[1, i]; g2 <- combos[2, i]
  r1 <- cor_df_FU1$Correlation[cor_df_FU1$Resilience_Level == g1]; n1 <- cor_df_FU1$Subject_Count[cor_df_FU1$Resilience_Level == g1]
  r2 <- cor_df_FU1$Correlation[cor_df_FU1$Resilience_Level == g2]; n2 <- cor_df_FU1$Subject_Count[cor_df_FU1$Resilience_Level == g2]
  
  comp <- cocor::cocor.indep.groups(r1, r2, n1, n2, alternative = "two.sided")
  c_res <- cocor::get.cocor.results(comp)
  cocor_list_FU1[[i]] <- data.frame(Stage = "FU1", Group1 = g1, Group2 = g2, Z = c_res$fisher1925$statistic, p_value = c_res$fisher1925$p.value)
}


# ==============================================================================
# 4. Follow-Up 2 (FU2) Stage Analysis
# Rule: PHQ NO -1. Trauma >= 5 is recoded to 4
# ==============================================================================
cat("\n[4/5] Running Follow-Up 2 (FU2) Analysis...\n")

df_FU2 <- df_global %>%
  dplyr::transmute(
    eid, Age, Sex, Ethnic, BMI, Education, site, selfresilience,
    Mental_Symptom = (`PHQ-9_FU2` / 27 + General_Anxiety_Disorder_Severity_FU2 / 21) * 10,
    Trauma_exposure = FU2_recent_trauma_adj
  ) %>%
  dplyr::mutate(Trauma_exposure = ifelse(Trauma_exposure >= 5, 4, Trauma_exposure)) %>%
  tidyr::drop_na(Mental_Symptom, Trauma_exposure, selfresilience, Age, Sex, Ethnic, BMI, Education, site)

# 4.1 Linear Mixed Model
lme_FU2 <- stats::lm(Mental_Symptom ~ selfresilience * Trauma_exposure + Age + Sex + Ethnic + BMI + Education + site, data = df_FU2)
print(summary(lme_FU2))

# 4.2 Visualization (Line Plot)
max_vals_FU2 <- df_FU2 %>% dplyr::group_by(Trauma_exposure) %>% dplyr::summarise(max_y = mean(Mental_Symptom))
L3 <- ggpubr::ggline(df_FU2, x = "Trauma_exposure", y = "Mental_Symptom", add = "mean_se", color = "selfresilience", palette = c("#E2C098", "#A6CAA8", "#85C3DC"), size = 1) +
  ggpubr::stat_compare_means(aes(group = selfresilience), label = "p.signif", label.y = max(max_vals_FU2$max_y) + 3, size = 4) +
  ggplot2::labs(title = "FU2 Stage")

# 4.3 Partial Correlations
cor_list_FU2 <- list()
for (lvl in resilience_levels) {
  sub_data <- df_FU2 %>% dplyr::filter(selfresilience == lvl)
  mod_m <- stats::lm(Mental_Symptom ~ Age + Sex + Ethnic + BMI + Education + site, data = sub_data)
  mod_t <- stats::lm(Trauma_exposure ~ Age + Sex + Ethnic + BMI + Education + site, data = sub_data)
  cor_test <- stats::cor.test(stats::residuals(mod_m), stats::residuals(mod_t))
  
  cor_list_FU2[[lvl]] <- data.frame(
    Timepoint = "FU2", Resilience_Level = lvl, Correlation = cor_test$estimate, 
    Int_low = cor_test$conf.int[1], Int_high = cor_test$conf.int[2], Subject_Count = nrow(sub_data)
  )
}
cor_df_FU2 <- dplyr::bind_rows(cor_list_FU2)

# 4.4 Cocor Fisher Z comparisons
cocor_list_FU2 <- list()
for (i in 1:ncol(combos)) {
  g1 <- combos[1, i]; g2 <- combos[2, i]
  r1 <- cor_df_FU2$Correlation[cor_df_FU2$Resilience_Level == g1]; n1 <- cor_df_FU2$Subject_Count[cor_df_FU2$Resilience_Level == g1]
  r2 <- cor_df_FU2$Correlation[cor_df_FU2$Resilience_Level == g2]; n2 <- cor_df_FU2$Subject_Count[cor_df_FU2$Resilience_Level == g2]
  
  comp <- cocor::cocor.indep.groups(r1, r2, n1, n2, alternative = "two.sided")
  c_res <- cocor::get.cocor.results(comp)
  cocor_list_FU2[[i]] <- data.frame(Stage = "FU2", Group1 = g1, Group2 = g2, Z = c_res$fisher1925$statistic, p_value = c_res$fisher1925$p.value)
}


# ==============================================================================
# 5. Summary & Combined Visualizations
# ==============================================================================
cat("\n[5/5] Generating combined visualizations...\n")

# Combine the 3 line plots into one layout
combined_lines <- gridExtra::grid.arrange(L1, L2, L3, ncol = 3)

# Combine Correlation Data
cor_result_all <- dplyr::bind_rows(cor_df_BL, cor_df_FU1, cor_df_FU2) %>%
  dplyr::mutate(Timepoint = factor(Timepoint, levels = c("BL", "FU1", "FU2")))

# Bar Plot with Error Bars
cor_plot <- ggplot2::ggplot(cor_result_all) +
  aes(x = Resilience_Level, y = Correlation, fill = Resilience_Level, colour = Resilience_Level) +
  ggplot2::geom_col(width = 0.7) +
  ggplot2::geom_errorbar(aes(ymin = Int_low, ymax = Int_high), width = 0.3, color = "#6F6F6F", linewidth = 0.7) + 
  ggplot2::scale_fill_manual(values = c(`1_Low` = "#E2C098", `2_Medium` = "#A6CAA8", `3_High` = "#85C3DC")) +
  ggplot2::scale_color_manual(values = c(`1_Low` = "#E2C098", `2_Medium` = "#A6CAA8", `3_High` = "#85C3DC")) +
  ggplot2::theme_minimal() +
  ggplot2::facet_wrap(~ Timepoint) +
  ggplot2::theme(
    panel.spacing = unit(1.5, "lines"),
    panel.border = element_blank(),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_blank(),
    panel.background = element_rect(color = "white", fill = NA),
    panel.grid.major.y = element_line(color = "gray", linewidth = 0.5, linetype = "solid"),
    panel.grid.major.x = element_blank()
  )

print(cor_plot)

# Output final Cocor comparison table
cat("\n--- Final Fisher's Z Comparisons Across All Stages ---\n")
final_cocor_table <- dplyr::bind_rows(
  dplyr::bind_rows(cocor_list_BL),
  dplyr::bind_rows(cocor_list_FU1),
  dplyr::bind_rows(cocor_list_FU2)
)
print(final_cocor_table)