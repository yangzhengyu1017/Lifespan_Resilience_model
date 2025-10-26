# ===============================================================
# Project: Adolescent Resilience Modeling and Validation
# Purpose:
#   This R script builds a PLSR-based resilience prediction model
#   using the UK Biobank dataset and validates it in the IMAGEN dataset.
#   It then classifies individuals based on diagnostic and model-derived
#   resilience profiles, visualizes longitudinal trajectories of
#   internalizing disorder risks (ages 14–22), computes odds ratios
#   between groups, and conducts non-inferiority analyses.
# ===============================================================

# ================================
# Load libraries
# ================================
library(data.table)
library(tidyverse)
library(rstatix)
library(modeldata)  
library(pls)
library(broom)
library(ggplot2)
library(ggrepel)
library(cowplot)
library(janitor)

# ================================
# Load data
# ================================
resilience_corr_rename_dat <- read.csv("~/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/UKB_dat_for_resilience_model_1224.csv")
IMAGEN_test_dat <- read.csv("~/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/IMAGEN_test_dat.csv")
trauma_dat <- read.csv("~/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/IMAGEN_trauma_dat.csv")

# ================================
# Prepare train/test datasets
# ================================
BRS_train_dat <- resilience_corr_rename_dat %>%
  transmute(eid, gender, HH_Income_BL, Been_In_Confiding_Relationship_FU1, Social_Able_Confide_BL,
            Loneliness_BL, MET_Minutes_Per_Week_Vigorous_Activity_BL, Big5_warmth, Big5_diligence,
            Big5_nervousness, Big5_curiosity, Big5_sociability, Family_Relationship_Satisfaction_BL,
            Friendships_Satisfaction_BL, self_resilience) %>%
  na.omit()

IMAGEN_test1_dat <- IMAGEN_test_dat %>%
  transmute(eid, gender, HH_Income_BL, Been_In_Confiding_Relationship_FU1, Social_Able_Confide_BL,
            Loneliness_BL, MET_Minutes_Per_Week_Vigorous_Activity_BL, Big5_warmth, Big5_diligence,
            Big5_nervousness, Big5_curiosity, Big5_sociability, Family_Relationship_Satisfaction_BL,
            Friendships_Satisfaction_BL) %>%
  na.omit()

BRS_train_dat[, 2:15] <- scale(BRS_train_dat[, 2:15])
IMAGEN_test1_dat[, 2:14] <- scale(IMAGEN_test1_dat[, 2:14])

# ================================
# Prepare matrices for PLSR
# ================================
X_colnames <- colnames(BRS_train_dat)[2:14]
Y_colnames <- colnames(BRS_train_dat)[15]

X_train_matrix <- as.matrix(BRS_train_dat[X_colnames])
Y_train_matrix <- as.matrix(BRS_train_dat[Y_colnames])
X_test_matrix <- as.matrix(IMAGEN_test1_dat[X_colnames])

# ================================
# Fit PLSR
# ================================
my_plsr <- plsr(Y_train_matrix ~ X_train_matrix, ncomp = 13, scale = TRUE, validation = 'CV')
plot(RMSEP(my_plsr))
plot(my_plsr, plottype = "coef", ncomp = 1:13, legendpos = "bottomleft")

best_ncomp <- selectNcomp(my_plsr, method = "randomization", plot = TRUE)
best_model <- plsr(Y_train_matrix ~ X_train_matrix, ncomp = best_ncomp, scale = TRUE, validation = 'CV')
test_predictions <- as.matrix(predict(best_model, ncomp = best_ncomp, X_test_matrix))

predicted_IMAGEN_R_dat <- data.frame(eid = IMAGEN_test1_dat$eid, pred_resilience_IMAGEN = test_predictions)
trauma_dat <- merge(trauma_dat, predicted_IMAGEN_R_dat, by = "eid", all.x = TRUE)
trauma_dat <- na.omit(trauma_dat)

# ================================
# Descriptive statistics
# ================================
trauma_dat %>% tabyl(trauma) %>% adorn_totals("row") %>% adorn_pct_formatting()
age_stats <- summary(trauma_dat$Age_BL.x)
age_range <- range(trauma_dat$Age_BL.x)
age_sd <- sd(trauma_dat$Age_BL.x)
age_mean <- mean(trauma_dat$Age_BL.x)
results_age <- data.frame(
  Statistic = c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Maximum", "Range", "SD"),
  Value = c(age_stats["Min."], age_stats["1st Qu."], age_stats["Median"], age_mean, age_stats["3rd Qu."],
            age_stats["Max."], paste(age_range, collapse = " to "), age_sd)
)
print(results_age)

# ================================
# Define groups
# ================================
trauma_dat <- trauma_dat %>%
  mutate(Diag_resilience = case_when(
    AnyDisorder_BL == 0 & trauma == 0 ~ 0,
    AnyDisorder_BL == 1 ~ 2,
    AnyDisorder_BL == 0 & trauma == 1 ~ 1
  ))

percentile_high <- quantile(trauma_dat$pred_resilience_IMAGEN[trauma_dat$Diag_resilience == 1], 0.67)
percentile_low <- quantile(trauma_dat$pred_resilience_IMAGEN[trauma_dat$Diag_resilience == 1], 0.33)

trauma_dat <- trauma_dat %>%
  mutate(Model_resilience = case_when(
    AnyDisorder_BL == 0 & trauma == 0 ~ 0,
    AnyDisorder_BL == 1 ~ 3,
    AnyDisorder_BL == 0 & trauma == 1 & pred_resilience_IMAGEN >= percentile_high ~ 1,
    AnyDisorder_BL == 0 & trauma == 1 & pred_resilience_IMAGEN < percentile_low ~ 2
  ))

# ---------------------------
# DIAG-based grouping summary (FU1–FU3)
# ---------------------------

# Extract relevant columns and define diagnostic resilience groups
DAWBA_FU1_result <- trauma_dat %>%
  transmute(Internalizing_BL,
            Internalizing_FU1,
            Internalizing_FU2,
            Internalizing_FU3,
            Diag_resilience = factor(Diag_resilience, levels = c(0,1),
                                     labels = c("0_Low_Risk", "1_Diag_resilience"))) %>%
  filter(!is.na(Diag_resilience))

library(dplyr)
library(ggplot2)
library(tidyr)
library(ggrepel)

# Convert data to long format
long_data <- DAWBA_FU1_result %>%
  gather(key = "Variable", value = "Value", -Diag_resilience)

# Rename columns to represent age stages
long_data <- long_data %>%
  mutate(Variable = str_replace(Variable, "Internalizing_BL", "age 14")) %>%
  mutate(Variable = str_replace(Variable, "Internalizing_FU1", "age 16")) %>%
  mutate(Variable = str_replace(Variable, "Internalizing_FU2", "age 19")) %>%
  mutate(Variable = str_replace(Variable, "Internalizing_FU3", "age 22"))

# Ensure correct factor levels and ordering
long_data$Variable <- factor(long_data$Variable, levels = c("age 14", "age 16", "age 19", "age 22"))
long_data$Diag_resilience <- factor(long_data$Diag_resilience, levels = c("1_Diag_resilience","0_Low_Risk"))
long_data$Value <- as.numeric(long_data$Value)

# Check data structure
str(long_data)

# Calculate prevalence and sample size at each time point
diag_summary_data <- long_data %>%
  group_by(Diag_resilience, Variable) %>%
  summarize(
    Total_Subjects = n(),
    Disease_Cases = sum(Value),
    Disease_Rate = mean(Value)
  ) %>%
  ungroup()

# Display summary
print(diag_summary_data)

# Define functions for SEM-based error bars
topbar <- function(x){ return(mean(x) + sd(x)/sqrt(length(x))) }
bottombar <- function(x){ return(mean(x) - sd(x)/sqrt(length(x))) }

# Plot mean trajectories with error bars
p1 <- ggplot(long_data, aes(x = Variable, y = Value, group = Diag_resilience, color = Diag_resilience)) +
  geom_rect(aes(xmin = 0, xmax = 1.5, ymin = -Inf, ymax = Inf), fill = 'grey90', color = NA) +
  geom_vline(xintercept = 1.5, linetype = "dashed", size = 0.9) +
  stat_summary(geom = 'line', fun = 'mean', cex = 1.5, alpha = 1, position = position_dodge(width = 0.15)) +
  stat_summary(geom = 'errorbar', fun.min = bottombar, fun.max = topbar,
               width = 0.2, cex = 0.7, aes(fill = Diag_resilience),
               position = position_dodge(width = 0.15)) +
  stat_summary(geom = 'point', fun = 'mean', aes(fill = Diag_resilience),
               size = 4, pch = 21, stroke = 1, key_glyph = 'polygon',
               position = position_dodge(width = 0.15)) +
  labs(y = "Internalizing disorder risk", x = "") +
  scale_color_manual(values = c("#EAAA60", "#9EACB4"),
                     labels = c("1_Diag_resilience", "0_Low_Risk")) +
  scale_fill_manual(values = c("#EAAA60", "#9EACB4"),
                    labels = c("1_Diag_resilience", "0_Low_Risk")) +
  theme_classic(base_size = 15) +
  scale_x_discrete() +
  coord_cartesian(ylim = c(0, 0.3))

print(p1)

# --------------------------------------------------------------
# Calculate Odds Ratios (OR) for DIAG group comparisons
# --------------------------------------------------------------

results <- data.frame(
  Time_Point = character(),
  OR = numeric(),
  CI_Lower = numeric(),
  CI_Upper = numeric(),
  P_Value = numeric(),
  Compare = character(),
  stringsAsFactors = FALSE
)

# Add baseline
results <- results %>%
  add_row(Time_Point = "Internalizing_BL", OR = 1, CI_Lower = 1, CI_Upper = 1,
          P_Value = NA, Compare = "Diag_resilient-Low_risk")

# Compute ORs across FU1–FU3
for (time in colnames(DAWBA_FU1_result)[2:4]) {
  temp_data <- DAWBA_FU1_result %>%
    select(Diag_resilience, !!sym(time)) %>%
    rename(outcome = !!sym(time))
  
  model <- glm(outcome ~ Diag_resilience, data = temp_data, family = binomial)
  summary_result <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  
  OR <- summary_result$estimate[2]
  CI_Lower <- summary_result$conf.low[2]
  CI_Upper <- summary_result$conf.high[2]
  P_Value <- summary_result$p.value[2]
  
  results <- results %>%
    add_row(Time_Point = time, OR = OR, CI_Lower = CI_Lower, CI_Upper = CI_Upper,
            P_Value = P_Value, Compare = "Diag_resilient-Low_risk")
}

print(results)

# --------------------------------------------------------------
# MODEL-based grouping summary (FU1–FU3)
# --------------------------------------------------------------

DAWBA_FU1_result <- trauma_dat %>%
  transmute(Internalizing_BL,
            Internalizing_FU1,
            Internalizing_FU2,
            Internalizing_FU3,
            Model_resilience = factor(Model_resilience, levels = c(0,1,2),
                                      labels = c("0_Low_Risk", "1_Model_resilience", "2_Model_vulnerable"))) %>%
  filter(!is.na(Model_resilience))

# Convert to long format
long_data <- DAWBA_FU1_result %>%
  gather(key = "Variable", value = "Value", -Model_resilience)

# Rename columns to represent age stages
long_data <- long_data %>%
  mutate(Variable = str_replace(Variable, "Internalizing_BL", "age 14")) %>%
  mutate(Variable = str_replace(Variable, "Internalizing_FU1", "age 16")) %>%
  mutate(Variable = str_replace(Variable, "Internalizing_FU2", "age 19")) %>%
  mutate(Variable = str_replace(Variable, "Internalizing_FU3", "age 22"))

# Ensure correct order
long_data$Variable <- factor(long_data$Variable, levels = c("age 14", "age 16", "age 19", "age 22"))
long_data$Model_resilience <- factor(long_data$Model_resilience,
                                     levels = c("2_Model_vulnerable", "1_Model_resilience", "0_Low_Risk"))
long_data$Value <- as.numeric(long_data$Value)

str(long_data)

# Summarize prevalence and sample sizes
model_summary_data <- long_data %>%
  group_by(Model_resilience, Variable) %>%
  summarize(
    Total_Subjects = n(),
    Disease_Cases = sum(Value),
    Disease_Rate = mean(Value)
  ) %>%
  ungroup()

print(model_summary_data)

# Define SEM-based error bar functions
topbar <- function(x){ return(mean(x) + sd(x)/sqrt(length(x))) }
bottombar <- function(x){ return(mean(x) - sd(x)/sqrt(length(x))) }

# Plot MODEL-based trajectories
p2 <- ggplot(long_data, aes(x = Variable, y = Value, group = Model_resilience, color = Model_resilience)) +
  geom_rect(aes(xmin = 0, xmax = 1.5, ymin = -Inf, ymax = Inf), fill = 'grey90', color = NA) +
  geom_vline(xintercept = 1.5, linetype = "dashed", size = 0.9) +
  stat_summary(geom = 'line', fun = 'mean', cex = 1.5, alpha = 1, position = position_dodge(width = 0.15)) +
  stat_summary(geom = 'errorbar', fun.min = bottombar, fun.max = topbar,
               width = 0.2, cex = 0.7, aes(fill = Model_resilience),
               position = position_dodge(width = 0.15)) +
  stat_summary(geom = 'point', fun = 'mean', aes(fill = Model_resilience),
               size = 4, pch = 21, stroke = 1, key_glyph = 'polygon',
               position = position_dodge(width = 0.15)) +
  labs(y = "Internalizing disorder risk", x = "") +
  scale_color_manual(values = c("#E68B81", "#84C3B7", "#9EACB4"),
                     labels = c("2_Model_vulnerable", "1_Model_resilience", "0_Low_Risk")) +
  scale_fill_manual(values = c("#E68B81", "#84C3B7", "#9EACB4"),
                    labels = c("2_Model_vulnerable", "1_Model_resilience", "0_Low_Risk")) +
  theme_classic(base_size = 15) +
  scale_x_discrete() +
  coord_cartesian(ylim = c(0, 0.3))

print(p2)

# --------------------------------------------------------------
# Combine plots
# --------------------------------------------------------------
library(cowplot)
combined_plot <- plot_grid(p1, p2, ncol = 1)
print(combined_plot)

aligned_plots <- align_plots(p1 + theme(legend.position = "none"), 
                             p2 + theme(legend.position = "none"),
                             align = 'v', axis = 'l')
legend_p1 <- get_legend(p1)
legend_p2 <- get_legend(p2)

combined_plot <- plot_grid(
  plot_grid(aligned_plots[[1]], aligned_plots[[2]], ncol = 1),
  plot_grid(legend_p1, legend_p2, ncol = 1),
  ncol = 2, rel_heights = c(1, 0.1)
)

print(combined_plot)

# --------------------------------------------------------------
# Calculate pairwise OR comparisons (Model-based)
# --------------------------------------------------------------

# Resilient vs. Low Risk
results <- results %>%
  add_row(Time_Point = "Internalizing_BL", OR = 1, CI_Lower = 1, CI_Upper = 1,
          P_Value = NA, Compare = "Model_resilient-Low_risk")

for (time in colnames(DAWBA_FU1_result)[2:4]) {
  temp_data <- DAWBA_FU1_result %>%
    select(Model_resilience, !!sym(time)) %>%
    rename(outcome = !!sym(time)) %>%
    filter(Model_resilience != "2_Model_vulnerable")
  
  model <- glm(outcome ~ Model_resilience, data = temp_data, family = binomial)
  summary_result <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  
  results <- results %>%
    add_row(Time_Point = time,
            OR = summary_result$estimate[2],
            CI_Lower = summary_result$conf.low[2],
            CI_Upper = summary_result$conf.high[2],
            P_Value = summary_result$p.value[2],
            Compare = "Model_resilient-Low_risk")
}

# Vulnerable vs. Low Risk
results <- results %>%
  add_row(Time_Point = "Internalizing_BL", OR = 1, CI_Lower = 1, CI_Upper = 1,
          P_Value = NA, Compare = "Model_vulnerable-Low_risk")

for (time in colnames(DAWBA_FU1_result)[2:4]) {
  temp_data <- DAWBA_FU1_result %>%
    select(Model_resilience, !!sym(time)) %>%
    rename(outcome = !!sym(time)) %>%
    filter(Model_resilience != "1_Model_resilience")
  
  model <- glm(outcome ~ Model_resilience, data = temp_data, family = binomial)
  summary_result <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  
  results <- results %>%
    add_row(Time_Point = time,
            OR = summary_result$estimate[2],
            CI_Lower = summary_result$conf.low[2],
            CI_Upper = summary_result$conf.high[2],
            P_Value = summary_result$p.value[2],
            Compare = "Model_vulnerable-Low_risk")
}

# Vulnerable vs. Resilient
results <- results %>%
  add_row(Time_Point = "Internalizing_BL", OR = 1, CI_Lower = 1, CI_Upper = 1,
          P_Value = NA, Compare = "Model_vulnerable-Model_resilient")

for (time in colnames(DAWBA_FU1_result)[2:4]) {
  temp_data <- DAWBA_FU1_result %>%
    select(Model_resilience, !!sym(time)) %>%
    rename(outcome = !!sym(time)) %>%
    filter(Model_resilience != "0_Low_Risk")
  
  model <- glm(outcome ~ Model_resilience, data = temp_data, family = binomial)
  summary_result <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  
  results <- results %>%
    add_row(Time_Point = time,
            OR = summary_result$estimate[2],
            CI_Lower = summary_result$conf.low[2],
            CI_Upper = summary_result$conf.high[2],
            P_Value = summary_result$p.value[2],
            Compare = "Model_vulnerable-Model_resilient")
}

print(results)
view(results)

# --------------------------------------------------------------
# Plot OR results (horizontal and vertical)
# --------------------------------------------------------------

library(ggplot2)

# Vertical plot
or_results_plot <- results[-c(13:16),]
or_results_plot$Compare <- factor(or_results_plot$Compare, levels = rev(levels(or_results_plot$Compare)))

ggplot(or_results_plot, aes(x = Time_Point, y = OR, color = Compare)) +
  geom_rect(aes(xmin = 3.5, xmax = Inf, ymin = -Inf, ymax = Inf), fill = 'grey90', color = NA) +
  geom_vline(xintercept = 2.5, color = "gray", linetype = "dashed", size = 0.9) +
  geom_vline(xintercept = 1.5, color = "gray", linetype = "dashed", size = 0.9) +
  annotate("segment", x = 0, xend = Inf, y = 1, yend = 1,
           linetype = "dashed", color = "black", size = 0.9) +
  geom_point(stat = "identity", position = position_dodge(width = 0.8), size = 3.5) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0,
                position = position_dodge(width = 0.8)) +
  scale_color_manual(values = c("#E68B81", "#84C3B7", "#EAAA60")) +
  scale_y_continuous(limits = c(0, 8)) +
  labs(x = "Time Point", y = "Odds Ratio (OR)", fill = "Group") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line.y = element_line(color = "black")) +
  coord_flip() +
  xlim(rev(levels(or_results_plot$Time_Point)))


# ------------------ Non-Inferiority Analysis ------------------
# This section performs non-inferiority tests between two groups
# ("Model_resilience" vs "Low_risk") across multiple follow-up (FU) time points.

# ------------------ Two-Proportion Non-Inferiority Function ------------------
twoprop_test = function(p1, p2,
                        n1, n2,
                        null = NULL,
                        alpha = .05,
                        alternative = c("two.sided", "less", "greater", "equivalence", "minimal.effect"),
                        effect_size = c("difference", "odds.ratio", "risk.ratio")) {
  
  alternative = match.arg(alternative)
  effect_size = match.arg(effect_size)
  
  if(!is.numeric(alpha) || alpha <=0 || alpha >=1)
    stop("The alpha must be a numeric value between 0 and 1")
  if (any(c(p1, p2) > 1 | c(p1, p2) < 0))
    stop("Proportions (p1, p2) must be between 0 and 1")
  if(any(c(n1, n2) < 10))
    stop("Sample sizes must be greater than 9")
  if(any(c(n1, n2) <= 50))
    message("Small sample size in at least one group; proceed with caution")
  
  # Choose test based on effect size type
  res_tests = switch(effect_size,
                     difference = test_prop_dif(p1, p2, n1, n2, null, alternative, alpha),
                     odds.ratio = test_odds_ratio(p1, p2, n1, n2, null, alternative, alpha),
                     risk.ratio = test_risk_ratio(p1, p2, n1, n2, null, alternative, alpha))
  
  RVAL <- list(statistic = res_tests$STATISTIC,
               p.value = as.numeric(res_tests$PVAL),
               estimate = res_tests$ESTIMATE,
               null.value = res_tests$NVAL,
               conf.int = res_tests$CINT,
               alternative = alternative,
               method = res_tests$METHOD)
  class(RVAL) <- "htest"
  return(RVAL)
}

# ------------------ Helper Functions for Proportion, Odds Ratio, Risk Ratio ------------------
# (Unchanged from your original, omitted here for brevity if already sourced)
# Functions: test_prop_dif(), test_odds_ratio(), test_risk_ratio(), p_from_z(), etc.

# ------------------ Run Non-Inferiority Tests Across Time Points ------------------

# Create an empty result dataframe
NI_results <- data.frame(
  Time_Point = character(),
  Model_Resilient_diag_rate = numeric(),
  N_Model_Resilient = numeric(),
  Low_risk_diag_rate = numeric(),
  N_Low_risk = numeric(),
  SESOI = numeric(),
  OR = numeric(),
  CI_Lower = numeric(),
  CI_Upper = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)

# Define a helper function to run NI test for each follow-up
run_NI_test <- function(age_label, SESOI_value) {
  p1 <- model_summary_data$Disease_Rate[model_summary_data$Model_resilience == "1_Model_resilience" &
                                          model_summary_data$Variable == age_label]
  n1 <- model_summary_data$Total_Subjects[model_summary_data$Model_resilience == "1_Model_resilience" &
                                            model_summary_data$Variable == age_label]
  p2 <- model_summary_data$Disease_Rate[model_summary_data$Model_resilience == "0_Low_Risk" &
                                          model_summary_data$Variable == age_label]
  n2 <- model_summary_data$Total_Subjects[model_summary_data$Model_resilience == "0_Low_Risk" &
                                            model_summary_data$Variable == age_label]
  
  NI <- twoprop_test(
    p1, p2, n1, n2,
    null = SESOI_value,
    alpha = 0.025,
    alternative = "equivalence",
    effect_size = "odds.ratio"
  )
  NI <- tidy(NI)
  
  data.frame(
    Time_Point = age_label,
    Model_Resilient_diag_rate = p1,
    N_Model_Resilient = n1,
    Low_risk_diag_rate = p2,
    N_Low_risk = n2,
    SESOI = SESOI_value,
    OR = NI$estimate[1],
    CI_Lower = NI$conf.low[1],
    CI_Upper = NI$conf.high[1],
    P_Value = NI$p.value[1],
    stringsAsFactors = FALSE
  )
}

# Run tests for FU1, FU2, FU3 and append results
NI_results <- bind_rows(
  NI_results,
  run_NI_test("age 16", 2.61),
  run_NI_test("age 19", 2.68),
  run_NI_test("age 22", 2.30)
)

# Print the summarized results
print(NI_results)