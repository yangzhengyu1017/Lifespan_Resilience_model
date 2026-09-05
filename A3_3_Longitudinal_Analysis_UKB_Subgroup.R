# ==============================================================================
# Script: A3_3_Longitudinal_Analysis_UKB_Subgroup_V2.R
# Project: Lifespan Resilience Modeling (STM Submission)
# Purpose: Longitudinal analysis stratified by baseline symptom severity to assess recovery and symptom worsening trajectories across resilience groups.
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. Load required packages
# ------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(MatchIt)
library(gtsummary)
library(ggplot2)
library(ggsankey)  
library(rstatix)   
library(broom)
library(gridExtra)
library(scales)

# ------------------------------------------------------------------------------
# Data preparation
# ------------------------------------------------------------------------------
resilience_group_R <- read_csv("data/UKB_dat_for_analysis_1226.csv")[,-1]

# Calculate quantiles and group self_resilience
quantiles <- quantile(resilience_group_R$self_resilience, probs = c(1/3, 2/3), na.rm = TRUE)
resilience_group_R$self_resilience[resilience_group_R$self_resilience <= quantiles[1]] <- 0
resilience_group_R$self_resilience[resilience_group_R$self_resilience > quantiles[1] & resilience_group_R$self_resilience < quantiles[2]] <- 1
resilience_group_R$self_resilience[resilience_group_R$self_resilience >= quantiles[2]] <- 2
resilience_group_R$self_resilience <- as.factor(resilience_group_R$self_resilience)

# Recode ethnic groups
resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 2, 6, Ethnic_group))
resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 3, 2, Ethnic_group))
resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 5, 2, Ethnic_group))
resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 4, 3, Ethnic_group))
resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 6, 4, Ethnic_group))
resilience_group_R$Ethnic_group <- resilience_group_R$Ethnic_group - 1

quantiles <- quantile(resilience_group_R$self_resilience, probs = c(1/3, 2/3), na.rm = TRUE)

resilience_group_R$self_resilience[resilience_group_R$self_resilience <= quantiles[1]] <- 0
resilience_group_R$self_resilience[resilience_group_R$self_resilience > quantiles[1] & resilience_group_R$self_resilience < quantiles[2]] <- 1
resilience_group_R$self_resilience[resilience_group_R$self_resilience >= quantiles[2]] <- 2
resilience_group_R$self_resilience <- as.factor(resilience_group_R$self_resilience)

# ------------------------------------------------------------------------------
# 1. Define pairwise Chi-square test function
# ------------------------------------------------------------------------------
pairwise_chisq_test <- function(data, group_var, compare_var) {
  groups <- levels(data[[group_var]])
  p_matrix <- matrix(NA, nrow = length(groups), ncol = length(groups), dimnames = list(groups, groups))
  for (i in 1:(length(groups) - 1)) {
    for (j in (i + 1):length(groups)) {
      subset_data <- data %>% dplyr::filter(data[[group_var]] %in% c(groups[i], groups[j]))
      subset_data[[group_var]] <- base::droplevels(subset_data[[group_var]])
      table_data <- base::table(subset_data[[group_var]], subset_data[[compare_var]])
      if (all(table_data > 0)) { 
        test_result <- stats::chisq.test(table_data, correct = FALSE)
        p_matrix[i, j] <- test_result$p.value
        p_matrix[j, i] <- test_result$p.value
      } else {
        p_matrix[i, j] <- NA
        p_matrix[j, i] <- NA
      }
    }
  }
  return(p_matrix)
}

################################################################################
#
# PART 1: PHQ (Depressive Symptoms) Trajectory Analysis
#
################################################################################
cat("\n=========================================================\n")
cat("[INFO] Starting PHQ trajectory analysis\n")
cat("=========================================================\n")

# --- Data cleaning and severity stratification ---
resilience_test <- resilience_group_R %>% 
  dplyr::transmute(
    eid, Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")), Age = age_BL, 
    Ethnic = factor(Ethnic_group, levels = c("0","1","2","3"), labels = c("1_White","2_Asian","3_Black","4_Other")),  
    Education = Education_year, BMI = BMI_BL,
    selfresilience = factor(self_resilience, levels = c(0,1,2), labels = c("1_Low","2_Medium","3_High")),
    data_FU1 = Mental_onine_date_FU1, data_FU2 = Mental_onine_date_FU2, 
    PHQ_FU1 = PHQ9_Severity_FU1 - 1, PHQ_FU2 = `PHQ-9_FU2`,
    GAD_FU1 = General_Anxiety_Disorder_Severity_FU1, GAD_FU2 = General_Anxiety_Disorder_Severity_FU2
  ) %>%
  dplyr::filter(selfresilience != "2_Medium") %>% 
  dplyr::mutate(selfresilience = base::droplevels(selfresilience)) 

resilience_test <- stats::na.omit(resilience_test)

resilience_test$FU1_severity <- NA
resilience_test$FU1_severity[resilience_test$PHQ_FU1 >= 10] <- "3" # High-risk
resilience_test$FU1_severity[resilience_test$PHQ_FU1 >= 5 & resilience_test$PHQ_FU1 < 10] <- "2" # Medium-risk
resilience_test$FU1_severity[resilience_test$PHQ_FU1 < 5] <- "1" # Low-risk

resilience_test$FU2_severity <- NA
resilience_test$FU2_severity[resilience_test$PHQ_FU2 >= 10] <- "3" 
resilience_test$FU2_severity[resilience_test$PHQ_FU2 >= 5 & resilience_test$PHQ_FU2 < 10] <- "2" 
resilience_test$FU2_severity[resilience_test$PHQ_FU2 < 5] <- "1" 

# --- Propensity Score Matching (PSM) with caliper = 0.2 ---
# High-risk group
data_high <- resilience_test %>% dplyr::filter(FU1_severity == "3")
m.out1 <- MatchIt::matchit(selfresilience ~ PHQ_FU1 + GAD_FU1 + Age + Sex + Ethnic + BMI + Education, data = data_high, method = "nearest", distance = "glm", caliper = 0.001)
data_high_matched <- base::subset(MatchIt::match.data(m.out1), weights == 1)

# Medium-risk group
data_mid <- resilience_test %>% dplyr::filter(FU1_severity == "2")
m.out2 <- MatchIt::matchit(selfresilience ~ PHQ_FU1 + GAD_FU1 + Age + Sex + Ethnic + BMI + Education, data = data_mid, method = "nearest", distance = "glm", caliper = 0.001)
data_mid_matched <- base::subset(MatchIt::match.data(m.out2), weights == 1)

# Low-risk group
data_low <- resilience_test %>% dplyr::filter(FU1_severity == "1")
m.out3 <- MatchIt::matchit(selfresilience ~ PHQ_FU1 + GAD_FU1 + Age + Sex + Ethnic + BMI + Education, data = data_low, method = "nearest", distance = "glm", caliper = 0.000001)
data_low_matched <- base::subset(MatchIt::match.data(m.out3), weights == 1)

resilience_matched_data <- dplyr::bind_rows(data_high_matched, data_mid_matched, data_low_matched)

# --- Alluvial Plots ---
# Low resilience group
resilience_matched_filter_data <- resilience_matched_data %>% dplyr::filter(selfresilience == "1_Low")
df <- resilience_matched_filter_data %>% dplyr::transmute(FU1_severity, FU2_severity) %>% ggsankey::make_long(FU1_severity, FU2_severity)
p1_PHQ <- ggplot2::ggplot(df, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
  ggsankey::geom_alluvial(flow.alpha = .6, width = 0.06, space = 300) +
  ggsankey::geom_alluvial_text(size = 3, color = "black") +
  ggplot2::scale_fill_manual(values = c("#F2BC8A", "#E67548","#BE1F4E")) +
  ggsankey::theme_alluvial(base_size = 18) +
  ggplot2::labs(x = NULL, title = "PHQ - Low Resilience") +
  ggplot2::theme(legend.position = "none", plot.title = element_text(hjust = .5))

# High resilience group
resilience_matched_filter_data <- resilience_matched_data %>% dplyr::filter(selfresilience == "3_High")
df <- resilience_matched_filter_data %>% dplyr::transmute(FU1_severity, FU2_severity) %>% ggsankey::make_long(FU1_severity, FU2_severity)
p2_PHQ <- ggplot2::ggplot(df, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
  ggsankey::geom_alluvial(flow.alpha = .6, width = 0.06, space = 300) +
  ggsankey::geom_alluvial_text(size = 3, color = "black") +
  ggplot2::scale_fill_manual(values = c("#F2BC8A", "#E67548","#BE1F4E")) +
  ggsankey::theme_alluvial(base_size = 18) +
  ggplot2::labs(x = NULL, title = "PHQ - High Resilience") +
  ggplot2::theme(legend.position = "none", plot.title = element_text(hjust = .5))

# --- Define recovery and deterioration (worse) ---
resilience_compare <- resilience_matched_data %>%
  dplyr::mutate(
    recovery = dplyr::if_else(FU2_severity < FU1_severity, 1, 0),
    worse = dplyr::if_else(FU2_severity > FU1_severity, 1, 0)
  )

# --- Descriptive Statistics: Counts and Proportions ---
cat("\n--- PHQ: Recovery counts and proportions ---\n")
proportion_results_rec_PHQ <- resilience_compare %>%
  dplyr::filter(FU1_severity != "1") %>% # Exclude low-risk (cannot recover further)
  dplyr::group_by(selfresilience, FU1_severity) %>% 
  dplyr::summarize(
    total = dplyr::n(),                               
    recovered = base::sum(recovery == 1, na.rm = TRUE), 
    proportion = recovered / total,
    .groups = "drop"
  )
base::print(proportion_results_rec_PHQ)

cat("\n--- PHQ: Deterioration counts and proportions ---\n")
proportion_results_wor_PHQ <- resilience_compare %>%
  dplyr::filter(FU1_severity != "3") %>% # Exclude high-risk (cannot deteriorate further)
  dplyr::group_by(selfresilience, FU1_severity) %>% 
  dplyr::summarize(
    total = dplyr::n(),                               
    worsened = base::sum(worse == 1, na.rm = TRUE), 
    proportion = worsened / total,
    .groups = "drop"
  )
base::print(proportion_results_wor_PHQ)

# --- Logistic Regression and Odds Ratios (OR) ---
# Recovery in high-risk group
resilience_compare_high <- resilience_compare[resilience_compare$FU1_severity == 3,]
resilience_compare_high <- resilience_compare_high %>% dplyr::mutate(selfresilience = stats::relevel(factor(selfresilience), ref = "1_Low"))
model_or_high <- stats::glm(recovery ~ selfresilience, data = resilience_compare_high, family = stats::binomial(link = "logit"))
cat("\n--- PHQ: Recovery OR for High-Severity group ---\n")
print(broom::tidy(model_or_high, exponentiate = TRUE, conf.int = TRUE) %>% dplyr::select(term, OR=estimate, conf.low, conf.high, p.value))

# Recovery in medium-risk group
resilience_compare_mid <- resilience_compare[resilience_compare$FU1_severity == 2,]
resilience_compare_mid <- resilience_compare_mid %>% dplyr::mutate(selfresilience = stats::relevel(factor(selfresilience), ref = "1_Low"))
model_or_mid_rec <- stats::glm(recovery ~ selfresilience, data = resilience_compare_mid, family = stats::binomial(link = "logit"))
cat("\n--- PHQ: Recovery OR for Medium-Severity group ---\n")
print(broom::tidy(model_or_mid_rec, exponentiate = TRUE, conf.int = TRUE) %>% dplyr::select(term, OR=estimate, conf.low, conf.high, p.value))

# Deterioration in medium-risk group
model_or_mid_wor <- stats::glm(worse ~ selfresilience, data = resilience_compare_mid, family = stats::binomial(link = "logit"))
cat("\n--- PHQ: Deterioration OR for Medium-Severity group ---\n")
print(broom::tidy(model_or_mid_wor, exponentiate = TRUE, conf.int = TRUE) %>% dplyr::select(term, OR=estimate, conf.low, conf.high, p.value))

# --- Bar Plots ---
compare_dat_PHQ_rec <- resilience_compare %>% dplyr::transmute(recovery, FU1_severity, selfresilience) %>% dplyr::filter(FU1_severity != "1")
compare_dat_PHQ_rec$FU1_severity <- factor(compare_dat_PHQ_rec$FU1_severity, levels = c("3","2"), label = c("High-Severity", "Medium-Severity"))

bar_1_PHQ <- ggplot2::ggplot(data = compare_dat_PHQ_rec, aes(x=FU1_severity, y=recovery, fill=selfresilience)) +
  ggplot2::geom_bar(stat = "summary", fun="mean", position = position_dodge(), width = 0.6, color="black") +
  ggplot2::stat_summary(geom = "errorbar", position = position_dodge(width = 0.6), width = 0.15) +
  ggplot2::scale_fill_manual(values = c("#fA9E38","#4995C6"), name=NULL) +
  ggplot2::scale_x_discrete(limits = rev(levels(compare_dat_PHQ_rec$FU1_severity))) + 
  ggplot2::scale_y_continuous(expand = c(0, 0), breaks = seq(0, 1, by = 0.2), labels = scales::number_format(accuracy = 0.1)) +
  ggplot2::coord_cartesian(ylim = c(0, 1)) +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position = c(0.5, 0.95), axis.title = element_text(face = "bold"), axis.text.x = element_text(face = "bold", colour = "black", size = 10)) +
  ggplot2::labs(x = "", y = "Recovery Proportion", fill = "")

compare_dat_PHQ_wor <- resilience_compare %>% dplyr::transmute(worse, FU1_severity, selfresilience) %>% dplyr::filter(FU1_severity != "3")
compare_dat_PHQ_wor$FU1_severity <- factor(compare_dat_PHQ_wor$FU1_severity, levels = c("2","1"), label = c("Medium-Severity","Low-Severity"))

bar_2_PHQ <- ggplot2::ggplot(data = compare_dat_PHQ_wor, aes(x=FU1_severity, y=worse, fill=selfresilience)) +
  ggplot2::geom_bar(stat = "summary", fun="mean", position = position_dodge(), width = 0.6, color="black") +
  ggplot2::stat_summary(geom = "errorbar", position = position_dodge(width = 0.6), width = 0.15) +
  ggplot2::scale_fill_manual(values = c("#fA9E38","#4995C6"), name=NULL) +
  ggplot2::scale_x_discrete(limits = rev(levels(compare_dat_PHQ_wor$FU1_severity))) + 
  ggplot2::scale_y_continuous(expand = c(0, 0), breaks = seq(0, 1, by = 0.2), labels = scales::number_format(accuracy = 0.1)) +
  ggplot2::coord_cartesian(ylim = c(0, 1)) +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position = c(0.5, 0.95), axis.title = element_text(face = "bold"), axis.text.x = element_text(face = "bold", colour = "black", size = 10)) +
  ggplot2::labs(x = "", y = "Worse Proportion", fill = "")


################################################################################
#
# PART 2: GAD (Anxiety Symptoms) Trajectory Analysis
#
################################################################################
cat("\n=========================================================\n")
cat("[INFO] Starting GAD trajectory analysis\n")
cat("=========================================================\n")

# --- Data cleaning and severity stratification ---
resilience_test <- resilience_group_R %>% 
  dplyr::transmute(
    eid, Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")), Age = age_BL, 
    Ethnic = factor(Ethnic_group, levels = c("0","1","2","3"), labels = c("1_White","2_Asian","3_Black","4_Other")),  
    Education = Education_year, BMI = BMI_BL,
    selfresilience = factor(self_resilience, levels = c(0,1,2), labels = c("1_Low","2_Medium","3_High")),
    data_FU1 = Mental_onine_date_FU1, data_FU2 = Mental_onine_date_FU2, 
    PHQ_FU1 = PHQ9_Severity_FU1 - 1, PHQ_FU2 = `PHQ-9_FU2`,
    GAD_FU1 = General_Anxiety_Disorder_Severity_FU1, GAD_FU2 = General_Anxiety_Disorder_Severity_FU2
  ) %>%
  dplyr::filter(selfresilience != "2_Medium") %>% 
  dplyr::mutate(selfresilience = base::droplevels(selfresilience)) 

resilience_test <- stats::na.omit(resilience_test)

resilience_test$FU1_severity <- NA
resilience_test$FU1_severity[resilience_test$GAD_FU1 >= 10] <- "3" # High-risk
resilience_test$FU1_severity[resilience_test$GAD_FU1 >= 5 & resilience_test$GAD_FU1 < 10] <- "2" # Medium-risk
resilience_test$FU1_severity[resilience_test$GAD_FU1 < 5] <- "1" # Low-risk

resilience_test$FU2_severity <- NA
resilience_test$FU2_severity[resilience_test$GAD_FU2 >= 10] <- "3" 
resilience_test$FU2_severity[resilience_test$GAD_FU2 >= 5 & resilience_test$GAD_FU2 < 10] <- "2" 
resilience_test$FU2_severity[resilience_test$GAD_FU2 < 5] <- "1" 

# --- Propensity Score Matching (PSM) ---
# High-risk group
data_high <- resilience_test %>% dplyr::filter(FU1_severity == "3")
m.out1 <- MatchIt::matchit(selfresilience ~ PHQ_FU1 + GAD_FU1 + Age + Sex + Ethnic + BMI + Education, data = data_high, method = "nearest", distance = "glm", caliper = 0.001)
data_high_matched <- base::subset(MatchIt::match.data(m.out1), weights == 1)

# Medium-risk group
data_mid <- resilience_test %>% dplyr::filter(FU1_severity == "2")
m.out2 <- MatchIt::matchit(selfresilience ~ PHQ_FU1 + GAD_FU1 + Age + Sex + Ethnic + BMI + Education, data = data_mid, method = "nearest", distance = "glm", caliper = 0.001)
data_mid_matched <- base::subset(MatchIt::match.data(m.out2), weights == 1)

# Low-risk group
data_low <- resilience_test %>% dplyr::filter(FU1_severity == "1")
m.out3 <- MatchIt::matchit(selfresilience ~ PHQ_FU1 + GAD_FU1 + Age + Sex + Ethnic + BMI + Education, data = data_low, method = "nearest", distance = "glm", caliper = 0.000001)
data_low_matched <- base::subset(MatchIt::match.data(m.out3), weights == 1)

resilience_matched_data <- dplyr::bind_rows(data_high_matched, data_mid_matched, data_low_matched)

# --- Alluvial Plots ---
# Low resilience group
resilience_matched_filter_data <- resilience_matched_data %>% dplyr::filter(selfresilience == "1_Low")
df <- resilience_matched_filter_data %>% dplyr::transmute(FU1_severity, FU2_severity) %>% ggsankey::make_long(FU1_severity, FU2_severity)
p1_GAD <- ggplot2::ggplot(df, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
  ggsankey::geom_alluvial(flow.alpha = .6, width = 0.06, space = 300) +
  ggsankey::geom_alluvial_text(size = 3, color = "black") +
  ggplot2::scale_fill_manual(values = c("#F2BC8A", "#E67548","#BE1F4E")) +
  ggsankey::theme_alluvial(base_size = 18) +
  ggplot2::labs(x = NULL, title = "GAD - Low Resilience") +
  ggplot2::theme(legend.position = "none", plot.title = element_text(hjust = .5))

# High resilience group
resilience_matched_filter_data <- resilience_matched_data %>% dplyr::filter(selfresilience == "3_High")
df <- resilience_matched_filter_data %>% dplyr::transmute(FU1_severity, FU2_severity) %>% ggsankey::make_long(FU1_severity, FU2_severity)
p2_GAD <- ggplot2::ggplot(df, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
  ggsankey::geom_alluvial(flow.alpha = .6, width = 0.06, space = 300) +
  ggsankey::geom_alluvial_text(size = 3, color = "black") +
  ggplot2::scale_fill_manual(values = c("#F2BC8A", "#E67548","#BE1F4E")) +
  ggsankey::theme_alluvial(base_size = 18) +
  ggplot2::labs(x = NULL, title = "GAD - High Resilience") +
  ggplot2::theme(legend.position = "none", plot.title = element_text(hjust = .5))

# --- Define recovery and deterioration (worse) ---
resilience_compare <- resilience_matched_data %>%
  dplyr::mutate(
    recovery = dplyr::if_else(FU2_severity < FU1_severity, 1, 0),
    worse = dplyr::if_else(FU2_severity > FU1_severity, 1, 0)
  )

# --- Descriptive Statistics: Counts and Proportions ---
cat("\n--- GAD: Recovery counts and proportions ---\n")
proportion_results_rec_GAD <- resilience_compare %>%
  dplyr::filter(FU1_severity != "1") %>% 
  dplyr::group_by(selfresilience, FU1_severity) %>% 
  dplyr::summarize(
    total = dplyr::n(),                               
    recovered = base::sum(recovery == 1, na.rm = TRUE), 
    proportion = recovered / total,
    .groups = "drop"
  )
base::print(proportion_results_rec_GAD)

cat("\n--- GAD: Deterioration counts and proportions ---\n")
proportion_results_wor_GAD <- resilience_compare %>%
  dplyr::filter(FU1_severity != "3") %>% 
  dplyr::group_by(selfresilience, FU1_severity) %>% 
  dplyr::summarize(
    total = dplyr::n(),                               
    worsened = base::sum(worse == 1, na.rm = TRUE), 
    proportion = worsened / total,
    .groups = "drop"
  )
base::print(proportion_results_wor_GAD)

# --- Logistic Regression and Odds Ratios (OR) ---
# Recovery in high-risk group
resilience_compare_high <- resilience_compare[resilience_compare$FU1_severity == 3,]
resilience_compare_high <- resilience_compare_high %>% dplyr::mutate(selfresilience = stats::relevel(factor(selfresilience), ref = "1_Low"))
model_or_high <- stats::glm(recovery ~ selfresilience, data = resilience_compare_high, family = stats::binomial(link = "logit"))
cat("\n--- GAD: Recovery OR for High-Severity group ---\n")
print(broom::tidy(model_or_high, exponentiate = TRUE, conf.int = TRUE) %>% dplyr::select(term, OR=estimate, conf.low, conf.high, p.value))

# Recovery in medium-risk group
resilience_compare_mid <- resilience_compare[resilience_compare$FU1_severity == 2,]
resilience_compare_mid <- resilience_compare_mid %>% dplyr::mutate(selfresilience = stats::relevel(factor(selfresilience), ref = "1_Low"))
model_or_mid_rec <- stats::glm(recovery ~ selfresilience, data = resilience_compare_mid, family = stats::binomial(link = "logit"))
cat("\n--- GAD: Recovery OR for Medium-Severity group ---\n")
print(broom::tidy(model_or_mid_rec, exponentiate = TRUE, conf.int = TRUE) %>% dplyr::select(term, OR=estimate, conf.low, conf.high, p.value))

# Deterioration in medium-risk group
model_or_mid_wor <- stats::glm(worse ~ selfresilience, data = resilience_compare_mid, family = stats::binomial(link = "logit"))
cat("\n--- GAD: Deterioration OR for Medium-Severity group ---\n")
print(broom::tidy(model_or_mid_wor, exponentiate = TRUE, conf.int = TRUE) %>% dplyr::select(term, OR=estimate, conf.low, conf.high, p.value))

# --- Bar Plots ---
compare_dat_GAD_rec <- resilience_compare %>% dplyr::transmute(recovery, FU1_severity, selfresilience) %>% dplyr::filter(FU1_severity != "1")
compare_dat_GAD_rec$FU1_severity <- factor(compare_dat_GAD_rec$FU1_severity, levels = c("3","2"), label = c("High-Severity", "Medium-Severity"))

bar_1_GAD <- ggplot2::ggplot(data = compare_dat_GAD_rec, aes(x=FU1_severity, y=recovery, fill=selfresilience)) +
  ggplot2::geom_bar(stat = "summary", fun="mean", position = position_dodge(), width = 0.6, color="black") +
  ggplot2::stat_summary(geom = "errorbar", position = position_dodge(width = 0.6), width = 0.15) +
  ggplot2::scale_fill_manual(values = c("#fA9E38","#4995C6"), name=NULL) +
  ggplot2::scale_x_discrete(limits = rev(levels(compare_dat_GAD_rec$FU1_severity))) + 
  ggplot2::scale_y_continuous(expand = c(0, 0), breaks = seq(0, 1, by = 0.2), labels = scales::number_format(accuracy = 0.1)) +
  ggplot2::coord_cartesian(ylim = c(0, 1)) +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position = c(0.5, 0.95), axis.title = element_text(face = "bold"), axis.text.x = element_text(face = "bold", colour = "black", size = 10)) +
  ggplot2::labs(x = "", y = "Recovery Proportion", fill = "")

compare_dat_GAD_wor <- resilience_compare %>% dplyr::transmute(worse, FU1_severity, selfresilience) %>% dplyr::filter(FU1_severity != "3")
compare_dat_GAD_wor$FU1_severity <- factor(compare_dat_GAD_wor$FU1_severity, levels = c("2","1"), label = c("Medium-Severity","Low-Severity"))

bar_2_GAD <- ggplot2::ggplot(data = compare_dat_GAD_wor, aes(x=FU1_severity, y=worse, fill=selfresilience)) +
  ggplot2::geom_bar(stat = "summary", fun="mean", position = position_dodge(), width = 0.6, color="black") +
  ggplot2::stat_summary(geom = "errorbar", position = position_dodge(width = 0.6), width = 0.15) +
  ggplot2::scale_fill_manual(values = c("#fA9E38","#4995C6"), name=NULL) +
  ggplot2::scale_x_discrete(limits = rev(levels(compare_dat_GAD_wor$FU1_severity))) + 
  ggplot2::scale_y_continuous(expand = c(0, 0), breaks = seq(0, 1, by = 0.2), labels = scales::number_format(accuracy = 0.1)) +
  ggplot2::coord_cartesian(ylim = c(0, 1)) +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position = c(0.5, 0.95), axis.title = element_text(face = "bold"), axis.text.x = element_text(face = "bold", colour = "black", size = 10)) +
  ggplot2::labs(x = "", y = "Worse Proportion", fill = "")

# ==============================================================================
# Final: Display all combined plots
# ==============================================================================
cat("\n[INFO] All analyses completed successfully!\n")

gridExtra::grid.arrange(p1_PHQ, p2_PHQ, ncol = 2, top = "PHQ - Sankey Plot")
gridExtra::grid.arrange(bar_1_PHQ, bar_2_PHQ, ncol = 1, top = "PHQ - Recovery vs Worse")

gridExtra::grid.arrange(p1_GAD, p2_GAD, ncol = 2, top = "GAD - Sankey Plot")
gridExtra::grid.arrange(bar_1_GAD, bar_2_GAD, ncol = 1, top = "GAD - Recovery vs Worse")