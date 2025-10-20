# ============================================================
# Matched Analysis for PHQ & GAD with Cohen's d
# ============================================================

# -------------------------------
# 1. Libraries
# -------------------------------
library(dplyr)
library(ggplot2)
library(ggpubr)
library(MatchIt)
library(broom)
library(gtsummary)
library(gridExtra)
library(autoReg)

# -------------------------------
# 2. Prepare data (exclude Medium group)
# -------------------------------
resilience_test <- resilience_group_R %>%
  transmute(
    eid,
    Sex = factor(gender, levels = c(0,1), labels = c("Female","Male")),
    Age = age_BL,
    Ethnic = factor(Ethnic_group, levels = c("0","1","2","3"), labels = c("1_White","2_Asian","3_Black","4_Other")),
    Education = Education_year,
    BMI = BMI_BL,
    selfresilience = factor(self_resilience, levels = c(0,1,2), labels = c("1_Low","2_Medium","3_High")),
    data_FU1 = Mental_onine_date_FU1, data_FU2 = Mental_onine_date_FU2,
    PHQ_FU1 = PHQ9_Severity_FU1,
    PHQ_FU2 = `PHQ-9_FU2`,
    GAD_FU1 = General_Anxiety_Disorder_Severity_FU1,
    GAD_FU2 = General_Anxiety_Disorder_Severity_FU2
  ) %>%
  filter(selfresilience != "2_Medium") %>%
  droplevels() %>%
  na.omit() %>%
  mutate(
    PHQ_Change = PHQ_FU2 - PHQ_FU1,
    GAD_Change = GAD_FU2 - GAD_FU1
  )

# -------------------------------
# 3. Matching (nearest neighbor)
# -------------------------------
m.out0 <- matchit(selfresilience ~ PHQ_FU1 + GAD_FU1 + Age + Sex + Ethnic + BMI + Education,
                  data = resilience_test,
                  method = "nearest",
                  distance = "glm",
                  caliper = 0.000001)
resilience_test <- match.data(m.out0)
resilience_test <- subset(resilience_test, weights == 1)

# -------------------------------
# 4. Check group balance
# -------------------------------
matched_group <- resilience_test %>%
  tbl_summary(by = selfresilience,
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)")) %>%
  add_p() %>%
  add_overall()
matched_group_table <- as_tibble(matched_group)
print(matched_group_table)

# -------------------------------
# 5. Linear models for PHQ & GAD
# -------------------------------
run_lm <- function(var_change, covars = c("Age","Sex","Ethnic","BMI","Education")) {
  formula <- as.formula(paste(var_change, "~ selfresilience +", paste(covars, collapse = " + ")))
  lm(formula, data = resilience_test)
}

# PHQ models
lm_PHQ_FU1 <- run_lm("PHQ_FU1")
lm_PHQ_Change <- run_lm("PHQ_Change")

# GAD models
lm_GAD_FU1 <- run_lm("GAD_FU1")
lm_GAD_Change <- run_lm("GAD_Change")

# -------------------------------
# 6. Prepare tables with Beta + 95% CI
# -------------------------------
prepare_table <- function(lm_model, var_prefix) {
  tab <- tidy(lm_model)
  tab$confint <- confint(lm_model)
  tab <- tab %>%
    mutate(beta = paste0(round(estimate,2), " (", round(confint[,1],2), ", ", round(confint[,2],2), ")")) %>%
    select(term, beta, p.value) %>%
    rename_with(~ paste0(var_prefix, "_", .), -term)
  return(tab)
}

M_PHQ_FU1 <- prepare_table(lm_PHQ_FU1, "PHQ_FU1_Matched")
M_PHQ_Change <- prepare_table(lm_PHQ_Change, "PHQ_Change_Matched")
M_GAD_FU1 <- prepare_table(lm_GAD_FU1, "GAD_FU1_Matched")
M_GAD_Change <- prepare_table(lm_GAD_Change, "GAD_Change_Matched")

# -------------------------------
# 7. Cohen's d for Change Scores
# -------------------------------
compute_cohens_d <- function(change_var) {
  groups <- levels(resilience_test$selfresilience)
  x1 <- resilience_test[[change_var]][resilience_test$selfresilience == groups[1]]
  x2 <- resilience_test[[change_var]][resilience_test$selfresilience == groups[2]]
  n1 <- length(x1); n2 <- length(x2)
  sd_pooled <- sqrt(((n1-1)*var(x1) + (n2-1)*var(x2)) / (n1+n2-2))
  d <- (mean(x2) - mean(x1)) / sd_pooled
  return(round(d,2))
}

cohens_d_PHQ_Change <- compute_cohens_d("PHQ_Change")
cohens_d_GAD_Change <- compute_cohens_d("GAD_Change")

# -------------------------------
# 8. Pivot to long table for plotting
# -------------------------------
tb_long <- resilience_test %>%
  pivot_longer(
    cols = c("PHQ_FU1","PHQ_FU2","GAD_FU1","GAD_FU2"),
    names_to = c(".value","FU"),
    names_sep = "_",
    values_drop_na = TRUE
  )

# -------------------------------
# 9. Line plots
# -------------------------------
p_PHQ_line <- ggline(tb_long, x="FU", y="PHQ", add="mean_se",
                     color="selfresilience", palette=c("#E2C098","#85C3DC"), size=1) +
  stat_compare_means(aes(group=selfresilience), label="p.signif", label.y=c(2.5,2.6), size=4)

p_GAD_line <- ggline(tb_long, x="FU", y="GAD", add="mean_se",
                     color="selfresilience", palette=c("#E2C098","#85C3DC"), size=1) +
  stat_compare_means(aes(group=selfresilience), label="p.signif", label.y=c(1.1,2.3), size=4)

# -------------------------------
# 10. Bar plots for Change Scores
# -------------------------------
plot_bar <- function(var_change, y_label, colors=c("#E2C098","#85C3DC")) {
  summary_df <- resilience_test %>%
    group_by(selfresilience) %>%
    summarise(mean_change = mean(.data[[var_change]], na.rm=TRUE),
              se = sd(.data[[var_change]], na.rm=TRUE)/sqrt(n()))
  
  ggplot(summary_df, aes(x=selfresilience, y=mean_change, fill=selfresilience)) +
    geom_bar(stat="identity", width=0.4, color="black") +
    geom_errorbar(aes(ymin=mean_change-se, ymax=mean_change+se), width=0.2, size=0.8) +
    geom_hline(yintercept=0, color="black", size=0.8) +
    scale_fill_manual(values=colors) +
    labs(x="Resilience Group", y=y_label) +
    theme_classic() +
    theme(legend.position="none")
}

p_PHQ_bar <- plot_bar("PHQ_Change", "Change in PHQ-9 Score")
p_GAD_bar <- plot_bar("GAD_Change", "Change in GAD-7 Score")

# -------------------------------
# 11. Combine plots
# -------------------------------
combined_line_plot <- grid.arrange(p_PHQ_line, p_GAD_line, ncol=2)
combined_bar_plot <- grid.arrange(p_PHQ_bar, p_GAD_bar, ncol=2)

# -------------------------------
# 12. Combine statistics
# -------------------------------
match_results <- bind_cols(M_PHQ_FU1, M_PHQ_Change, M_GAD_FU1, M_GAD_Change) %>%
  mutate(
    Cohens_d_PHQ_Change = cohens_d_PHQ_Change,
    Cohens_d_GAD_Change = cohens_d_GAD_Change
  )

print(match_results)
print(combined_line_plot)
print(combined_bar_plot)