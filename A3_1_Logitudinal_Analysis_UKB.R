# ============================================================
# Longitudinal analysis for PHQ & GAD with Cohen’s d
# ============================================================

library(dplyr)
library(ggplot2)
library(gtsummary)
library(broom)
library(ggpubr)
library(tidyr)
library(gridExtra)
library(autoReg)



# ------------------------------------------------------------
# 数据准备
# ------------------------------------------------------------
resilience_test <- resilience_group_R %>%
  transmute(
    eid,
    Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")),
    Age = age_BL,
    Ethnic = factor(Ethnic_group, levels = c("0","1","2","3"), 
                    labels = c("1_White","2_Asian","3_Black","4_Other")),
    Education = Education_year,
    BMI = BMI_BL,
    selfresilience = factor(self_resilience, levels = c(0,1,2),
                            labels = c("1_Low","2_Medium","3_High")),
    data_FU1 = Mental_onine_date_FU1,
    data_FU2 = Mental_onine_date_FU2,
    PHQ_FU1 = PHQ9_Severity_FU1,
    PHQ_FU2 = `PHQ-9_FU2`,
    GAD_FU1 = General_Anxiety_Disorder_Severity_FU1,
    GAD_FU2 = General_Anxiety_Disorder_Severity_FU2
  ) %>%
  na.omit()

# ============================================================
# Part 1: PHQ longitudinal analysis
# ============================================================

resilience_test$PHQ_Change <- resilience_test$PHQ_FU2 - resilience_test$PHQ_FU1

# 线性模型
lm_PHQ_FU1 <- lm(PHQ_FU1 ~ selfresilience + Age + Sex + Ethnic + BMI + Education, data = resilience_test)
lm_PHQ_Change <- lm(PHQ_Change ~ selfresilience + Age + Sex + Ethnic + BMI + Education, data = resilience_test)
lm_PHQ_Change_adj <- lm(PHQ_Change ~ PHQ_FU1 + selfresilience + Age + Sex + Ethnic + BMI + Education, data = resilience_test)

# 提取结果表格
T1 <- tidy(lm_PHQ_FU1, conf.int = TRUE) %>%
  mutate(beta = paste0(round(estimate, 2), " (", round(conf.low, 2), ", ", round(conf.high, 2), ")")) %>%
  select(term, beta, p.value) %>%
  rename_with(~ paste0("PHQ_FU1_", .))

T2 <- tidy(lm_PHQ_Change, conf.int = TRUE) %>%
  mutate(beta = paste0(round(estimate, 2), " (", round(conf.low, 2), ", ", round(conf.high, 2), ")")) %>%
  select(term, beta, p.value) %>%
  rename_with(~ paste0("PHQ_Change_", .))

T3 <- tidy(lm_PHQ_Change_adj, conf.int = TRUE) %>%
  mutate(beta = paste0(round(estimate, 2), " (", round(conf.low, 2), ", ", round(conf.high, 2), ")")) %>%
  select(term, beta, p.value) %>%
  rename_with(~ paste0("PHQ_Change_adj_", .))

# ------------------------------------------------------------
# 长表 + 折线图
# ------------------------------------------------------------
tb_long_PHQ <- resilience_test %>%
  pivot_longer(cols = c("PHQ_FU1","PHQ_FU2"),
               names_to = c(".value","FU"),
               names_sep = "_",
               values_drop_na = TRUE)

p_PHQ <- ggline(tb_long_PHQ, x = "FU", y = "PHQ", add = "mean_se",
                color = "selfresilience", palette = c("#E2C098","#A6CAA8","#85C3DC"), size = 1) +
  stat_compare_means(aes(group = selfresilience), label = "p.signif", 
                     label.y = c(5.1, 4.6), size = 4) +
  labs(title = "PHQ-9 Score Change Across Follow-ups")

# ------------------------------------------------------------
# 两两比较 + Cohen’s d（包含未控制和控制 PHQ_FU1 的模型）
# ------------------------------------------------------------

results_PHQ <- list()

for (pair in comparisons) {
  group1 <- pair[1]; group2 <- pair[2]
  subset_data <- resilience_test %>%
    filter(selfresilience %in% c(group1, group2)) %>%
    mutate(selfresilience = factor(selfresilience, levels = c(group1, group2)))
  
  # 5a. Linear models
  lm_model_basic <- lm(PHQ_Change ~ selfresilience + Age + Sex + Ethnic + BMI + Education, data = subset_data)
  lm_model_baseline <- lm(PHQ_Change ~ PHQ_FU1 + selfresilience + Age + Sex + Ethnic + BMI + Education, data = subset_data)
  
  tidy_basic <- tidy(lm_model_basic, conf.int = TRUE) %>%
    filter(grepl("selfresilience", term)) %>%
    mutate(comparison = paste(group1, "vs", group2),
           beta = paste0(round(estimate, 2), " (", round(conf.low, 2), ", ", round(conf.high, 2), ")"))
  
  # 5b. Cohen's d
  # 1. raw
  x1_raw <- subset_data$PHQ_Change[subset_data$selfresilience == group1]
  x2_raw <- subset_data$PHQ_Change[subset_data$selfresilience == group2]
  sd_pooled_raw <- sqrt(((length(x1_raw)-1)*var(x1_raw) + (length(x2_raw)-1)*var(x2_raw)) / (length(x1_raw)+length(x2_raw)-2))
  cohens_d_raw <- (mean(x2_raw) - mean(x1_raw)) / sd_pooled_raw
  
  # 2. covariates
  cov_model <- lm(PHQ_Change ~ Age + Sex + Ethnic + BMI + Education, data = subset_data)
  adj_resid_cov <- resid(cov_model)
  x1_cov <- adj_resid_cov[subset_data$selfresilience == group1]
  x2_cov <- adj_resid_cov[subset_data$selfresilience == group2]
  sd_pooled_cov <- sqrt(((length(x1_cov)-1)*var(x1_cov) + (length(x2_cov)-1)*var(x2_cov)) / (length(x1_cov)+length(x2_cov)-2))
  cohens_d_cov <- (mean(x2_cov) - mean(x1_cov)) / sd_pooled_cov
  
  # 3. covariates + baseline
  cov_model_BL <- lm(PHQ_Change ~ PHQ_FU1 + Age + Sex + Ethnic + BMI + Education, data = subset_data)
  adj_resid_BL <- resid(cov_model_BL)
  x1_BL <- adj_resid_BL[subset_data$selfresilience == group1]
  x2_BL <- adj_resid_BL[subset_data$selfresilience == group2]
  sd_pooled_BL <- sqrt(((length(x1_BL)-1)*var(x1_BL) + (length(x2_BL)-1)*var(x2_BL)) / (length(x1_BL)+length(x2_BL)-2))
  cohens_d_BL <- (mean(x2_BL) - mean(x1_BL)) / sd_pooled_BL
  
  tidy_basic <- tidy_basic %>%
    mutate(
      cohens_d_raw = round(cohens_d_raw,2),
      cohens_d_cov = round(cohens_d_cov,2),
      cohens_d_cov_BL = round(cohens_d_BL,2)
    )
  
  results_PHQ[[paste(group1,"vs",group2)]] <- tidy_basic
}

results_PHQ_df <- bind_rows(results_PHQ) %>%
  mutate(p_adjusted = p.adjust(`p.value`, method = "fdr")) %>%
  rename(Comparison = comparison,
         Estimate = beta,
         `Raw p-value` = p.value,
         `FDR-adjusted p-value` = p_adjusted)

print(results_PHQ_df)

# ------------------------------------------------------------
# Bar 图
# ------------------------------------------------------------
summary_PHQ <- resilience_test %>%
  group_by(selfresilience) %>%
  summarise(mean_change = mean(PHQ_Change), se = sd(PHQ_Change) / sqrt(n()))

p_phq_bar <- ggplot(summary_PHQ, aes(x = selfresilience, y = mean_change, fill = selfresilience)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = mean_change - se, ymax = mean_change + se),
                width = 0.2, size = 0.8) +
  geom_hline(yintercept = 0, size = 0.8) +
  scale_fill_manual(values = c("#E2C098", "#A6CAA8", "#85C3DC")) +
  labs(x = "Resilience Group", y = "Change in PHQ-9 Score") +
  theme_classic() + theme(legend.position = "none")

# ============================================================
# Part 2: GAD longitudinal analysis (同样结构)
# ============================================================

resilience_test$GAD_Change <- resilience_test$GAD_FU2 - resilience_test$GAD_FU1

lm_GAD_FU1 <- lm(GAD_FU1 ~ selfresilience + Age + Sex + Ethnic + BMI + Education, data = resilience_test)
lm_GAD_Change <- lm(GAD_Change ~ selfresilience + Age + Sex + Ethnic + BMI + Education, data = resilience_test)
lm_GAD_Change_adj <- lm(GAD_Change ~ GAD_FU1 + selfresilience + Age + Sex + Ethnic + BMI + Education, data = resilience_test)

# 提取表格
T4 <- tidy(lm_GAD_FU1, conf.int = TRUE) %>%
  mutate(beta = paste0(round(estimate, 2), " (", round(conf.low, 2), ", ", round(conf.high, 2), ")")) %>%
  select(term, beta, p.value) %>%
  rename_with(~ paste0("GAD_FU1_", .))

T5 <- tidy(lm_GAD_Change, conf.int = TRUE) %>%
  mutate(beta = paste0(round(estimate, 2), " (", round(conf.low, 2), ", ", round(conf.high, 2), ")")) %>%
  select(term, beta, p.value) %>%
  rename_with(~ paste0("GAD_Change_", .))

T6 <- tidy(lm_GAD_Change_adj, conf.int = TRUE) %>%
  mutate(beta = paste0(round(estimate, 2), " (", round(conf.low, 2), ", ", round(conf.high, 2), ")")) %>%
  select(term, beta, p.value) %>%
  rename_with(~ paste0("GAD_Change_adj_", .))

# ------------------------------------------------------------
# 两两比较 + Cohen’s d（包含未控制和控制 GAD_FU1 的模型）
# ------------------------------------------------------------

results_GAD <- list()

for (pair in comparisons) {
  group1 <- pair[1]; group2 <- pair[2]
  subset_data <- resilience_test %>%
    filter(selfresilience %in% c(group1, group2)) %>%
    mutate(selfresilience = factor(selfresilience, levels = c(group1, group2)))
  
  # 5a. Linear models
  lm_model_basic <- lm(GAD_Change ~ selfresilience + Age + Sex + Ethnic + BMI + Education, data = subset_data)
  lm_model_baseline <- lm(GAD_Change ~ GAD_FU1 + selfresilience + Age + Sex + Ethnic + BMI + Education, data = subset_data)
  
  tidy_basic <- tidy(lm_model_basic, conf.int = TRUE) %>%
    filter(grepl("selfresilience", term)) %>%
    mutate(comparison = paste(group1, "vs", group2),
           beta = paste0(round(estimate, 2), " (", round(conf.low, 2), ", ", round(conf.high, 2), ")"))
  
  # 5b. Cohen's d
  # 1. raw
  x1_raw <- subset_data$GAD_Change[subset_data$selfresilience == group1]
  x2_raw <- subset_data$GAD_Change[subset_data$selfresilience == group2]
  sd_pooled_raw <- sqrt(((length(x1_raw)-1)*var(x1_raw) + (length(x2_raw)-1)*var(x2_raw)) / (length(x1_raw)+length(x2_raw)-2))
  cohens_d_raw <- (mean(x2_raw) - mean(x1_raw)) / sd_pooled_raw
  
  # 2. covariates
  cov_model <- lm(GAD_Change ~ Age + Sex + Ethnic + BMI + Education, data = subset_data)
  adj_resid_cov <- resid(cov_model)
  x1_cov <- adj_resid_cov[subset_data$selfresilience == group1]
  x2_cov <- adj_resid_cov[subset_data$selfresilience == group2]
  sd_pooled_cov <- sqrt(((length(x1_cov)-1)*var(x1_cov) + (length(x2_cov)-1)*var(x2_cov)) / (length(x1_cov)+length(x2_cov)-2))
  cohens_d_cov <- (mean(x2_cov) - mean(x1_cov)) / sd_pooled_cov
  
  # 3. covariates + baseline
  cov_model_BL <- lm(GAD_Change ~ GAD_FU1 + Age + Sex + Ethnic + BMI + Education, data = subset_data)
  adj_resid_BL <- resid(cov_model_BL)
  x1_BL <- adj_resid_BL[subset_data$selfresilience == group1]
  x2_BL <- adj_resid_BL[subset_data$selfresilience == group2]
  sd_pooled_BL <- sqrt(((length(x1_BL)-1)*var(x1_BL) + (length(x2_BL)-1)*var(x2_BL)) / (length(x1_BL)+length(x2_BL)-2))
  cohens_d_BL <- (mean(x2_BL) - mean(x1_BL)) / sd_pooled_BL
  
  tidy_basic <- tidy_basic %>%
    mutate(
      cohens_d_raw = round(cohens_d_raw,2),
      cohens_d_cov = round(cohens_d_cov,2),
      cohens_d_cov_BL = round(cohens_d_BL,2)
    )
  
  results_GAD[[paste(group1,"vs",group2)]] <- tidy_basic
}

results_GAD_df <- bind_rows(results_GAD) %>%
  mutate(p_adjusted = p.adjust(`p.value`, method = "fdr")) %>%
  rename(Comparison = comparison,
         Estimate = beta,
         `Raw p-value` = p.value,
         `FDR-adjusted p-value` = p_adjusted)

print(results_GAD_df)

# ------------------------------------------------------------
# Bar 图
# ------------------------------------------------------------
summary_GAD <- resilience_test %>%
  group_by(selfresilience) %>%
  summarise(mean_change = mean(GAD_Change), se = sd(GAD_Change) / sqrt(n()))

p_gad_bar <- ggplot(summary_GAD, aes(x = selfresilience, y = mean_change, fill = selfresilience)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = mean_change - se, ymax = mean_change + se),
                width = 0.2, size = 0.8) +
  geom_hline(yintercept = 0, size = 0.8) +
  scale_fill_manual(values = c("#E2C098", "#A6CAA8", "#85C3DC")) +
  labs(x = "Resilience Group", y = "Change in GAD-7 Score") +
  theme_classic() + theme(legend.position = "none")

# ============================================================
# 汇总结果与合并图
# ============================================================

combined_plot <- grid.arrange(p_phq_bar, p_gad_bar, ncol = 2)
print(combined_plot)

# 输出 pairwise 结果表
print(results_PHQ_df)
print(results_GAD_df)