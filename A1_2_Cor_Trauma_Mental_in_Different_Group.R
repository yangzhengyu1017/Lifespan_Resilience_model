# =============================================================================
# Purpose:
# This script analyzes the relationship between self-reported resilience and mental 
# health outcomes across multiple timepoints (Baseline, FU1, FU2) in a UKB cohort.
# The analysis includes data preprocessing, resilience grouping, regression-based
# correlation analyses, group comparisons, and visualization of results.
# =============================================================================

# -------------------------------
# Set up: Load required libraries
# -------------------------------
# Load all necessary R packages for data manipulation, statistical analysis, and plotting
library(data.table)
library(tidyverse)   # 包含 dplyr, readr 等
library(dplyr)
library(ggpubr)
library(ggplot2)
library(gridExtra)
library(cocor)
library(janitor)

# -------------------------------
# Data loading
# -------------------------------
# Load preprocessed resilience correlation dataset
resilience_corr_rename_dat <- read_csv("~/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/UKB_resilience_corr_rename_dat_11221.csv")
resilience_corr_rename_dat <- resilience_corr_rename_dat[, -1]  # Remove first column if unnecessary

# Load social environment, trauma, and mental health dataset
soc_env_trauma_mental_dat <- read.csv("~/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/UKB_BL_FU_trauma_mental_env_dat.csv")
soc_env_trauma_mental_dat <- soc_env_trauma_mental_dat[, -1]  # Remove first column if unnecessary

# -------------------------------
# Data preprocessing
# -------------------------------
# Merge resilience and socio-environment/trauma data
resilience_group_R <- merge(resilience_corr_rename_dat, soc_env_trauma_mental_dat[, c(1, 71, 85, 79)], by = "eid", all.x = TRUE)

# Scale self_resilience scores
resilience_group_R$self_resilience <- (resilience_group_R$self_resilience + 6) / 6

# Scale FU2 trauma scores
resilience_group_R$FU2_recent_trauma <- resilience_group_R$FU2_recent_trauma * 10

# Recode Ethnic_group variable for consistency
resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 2, 6, Ethnic_group))
resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 3, 2, Ethnic_group))
resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 5, 2, Ethnic_group))
resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 4, 3, Ethnic_group))
resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 6, 4, Ethnic_group))
resilience_group_R$Ethnic_group <- resilience_group_R$Ethnic_group - 1

# -------------------------------
# Group participants by self-resilience tertiles
# -------------------------------
# Calculate tertiles and assign Low (0), Medium (1), High (2) labels
quantiles <- quantile(resilience_group_R$self_resilience, probs = c(1/3, 2/3), na.rm = TRUE)
resilience_group_R$self_resilience[resilience_group_R$self_resilience <= quantiles[1]] <- 0
resilience_group_R$self_resilience[resilience_group_R$self_resilience > quantiles[1] & resilience_group_R$self_resilience < quantiles[2]] <- 1
resilience_group_R$self_resilience[resilience_group_R$self_resilience >= quantiles[2]] <- 2
resilience_group_R$self_resilience <- as.factor(resilience_group_R$self_resilience)

# -------------------------------
# Descriptive statistics
# -------------------------------
# Tabulate number and percentage of participants in each resilience group
des_resilience <- resilience_group_R[, c("eid", "self_resilience")]
des_resilience %>% 
  tabyl(self_resilience) %>%
  adorn_totals("row") %>%
  adorn_pct_formatting()

# Define ordered labels for resilience groups
resilience_levels <- c("1_Low", "2_Medium", "3_High")

# -------------------------------
# Functions for correlation analysis and plotting
# -------------------------------
# Function to calculate partial correlations (controlling for covariates) 
# between trauma exposure and mental health outcomes within each resilience group
calculate_correlations <- function(data, outcome_var, timepoint) {
  
  # 初始化结果表
  correlation_results <- data.frame(
    Timepoint = character(),
    Resilience_Level = character(),
    Correlation = numeric(),
    Int_low = numeric(),
    Int_high = numeric(),
    Subject_Count = integer(),
    stringsAsFactors = FALSE
  )
  
  # 遍历每个 resilience 水平
  for (res_level in resilience_levels) {
    
    resilient_group <- data[data$selfresilience == res_level, ]
    
    # 样本太少则跳过
    if (nrow(resilient_group) < 10) next
    
    # 对 outcome 和 trauma 分别控制协变量
    model_m <- lm(as.formula(paste(outcome_var, "~ Age + Sex + Ethnic + BMI + Education")), 
                  data = resilient_group)
    resilient_group$outcome_resid <- residuals(model_m)
    
    model_t <- lm(as.formula(paste("Trauma_exposure ~ Age + Sex + Ethnic + BMI + Education")), 
                  data = resilient_group)
    resilient_group$trauma_resid <- residuals(model_t)
    
    # 计算残差间的相关
    correlation_model <- cor.test(resilient_group$outcome_resid, resilient_group$trauma_resid)
    
    correlation <- correlation_model$estimate
    int_low <- correlation_model$conf.int[1]
    int_high <- correlation_model$conf.int[2]
    subject_count <- nrow(resilient_group)
    
    # 写入结果
    correlation_results <- rbind(correlation_results,
                                 data.frame(Timepoint = timepoint,
                                            Resilience_Level = res_level,
                                            Correlation = correlation,
                                            Int_low = int_low,
                                            Int_high = int_high,
                                            Subject_Count = subject_count))
  }
  
  return(correlation_results)
}

# Function to compare correlations between resilience groups using Fisher's Z
compare_correlations <- function(correlation_results) {
  comparison_results <- data.frame(Group1 = character(), Group2 = character(), 
                                   Z = numeric(), p_value = numeric(), stringsAsFactors = FALSE)
  
  for (i in 1:(length(resilience_levels) - 1)) {
    for (j in (i + 1):length(resilience_levels)) {
      group1 <- correlation_results[correlation_results$Resilience_Level == resilience_levels[i], ]
      group2 <- correlation_results[correlation_results$Resilience_Level == resilience_levels[j], ]
      
      x <- group1$Correlation
      n.x <- group1$Subject_Count
      y <- group2$Correlation
      n.y <- group2$Subject_Count
      
      comparison <- cocor.indep.groups(x, y, n.x, n.y, alternative = "two.sided")
      cocor_result <- get.cocor.results(comparison)
      Z <- cocor_result$fisher1925$statistic
      p_value <- cocor_result$fisher1925$p.value
      
      comparison_results <- rbind(comparison_results, 
                                  data.frame(Group1 = resilience_levels[i], Group2 = resilience_levels[j], 
                                             Z = Z, p_value = p_value))
    }
  }
  return(comparison_results)
}

# Function to create ggline plot for mental health outcome vs. trauma exposure
create_ggline_plot <- function(data, outcome_var) {
  max_values <- aggregate(as.formula(paste(outcome_var, "~ Trauma_exposure")), data = data, FUN = mean)
  plot <- ggline(data, x = "Trauma_exposure", y = outcome_var, add = "mean_se",
                 color = "selfresilience", palette = c("#fA9E38", "#72BD5B", "#4995C6"), size = 1) +
    stat_compare_means(aes(group = selfresilience), label = "p.signif", 
                       label.y = max_values[[outcome_var]] + 1, size = 4)
  return(plot)
}

# -------------------------------
# Baseline (BL) analysis
# -------------------------------
# Prepare dataset for BL
resilience_group_R_BL <- resilience_group_R[, c("eid", "age_BL", "gender", "BMI_BL", "Ethnic_group", "IMD",
                                                 "self_resilience", "Depressive_Symptoms_PHQ4_BL", "PHQ9_Severity_FU1", "PHQ-9_FU2",
                                                 "General_Anxiety_Disorder_Severity_FU1", "General_Anxiety_Disorder_Severity_FU2",
                                                 "trauma_num", "FU_recent_trauma", "FU2_recent_trauma", "Education_year")]
resilience_group_R_BL <- na.omit(resilience_group_R_BL)

resilience_test_BL <- resilience_group_R_BL %>% 
  transmute(eid, Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")), Age = age_BL, 
            Ethnic = factor(Ethnic_group, levels = c("0","1","2","3"), labels = c("1_White","2_Asian","3_Black","4_Other")),  
            Education = Education_year, BMI = BMI_BL,
            selfresilience = factor(self_resilience, levels = c(0,1,2), labels = c("1_Low","2_Medium","3_High")),
            PHQ_BL = Depressive_Symptoms_PHQ4_BL - 4, Trauma_exposure = trauma_num)

# Cap extreme trauma values and remove missing data
resilience_test_BL$Trauma_exposure[resilience_test_BL$Trauma_exposure >= 5] <- 4
resilience_test_BL$Trauma_exposure <- as.numeric(resilience_test_BL$Trauma_exposure)
resilience_test_BL <- na.omit(resilience_test_BL)

# Create BL plot
L1 <- create_ggline_plot(resilience_test_BL, "PHQ_BL")

# Calculate correlations and group comparisons at BL
correlation_results_BL <- calculate_correlations(resilience_test_BL, "PHQ_BL", "BL")
comparison_results_BL <- compare_correlations(correlation_results_BL)


# -------------------------------
# FU1 analysis
# -------------------------------
# Prepare FU1 dataset with demographic, resilience, and mental health variables
resilience_group_R_FU1 <- resilience_group_R[, c("eid", "age_BL", "gender", "BMI_BL", "Ethnic_group", "IMD",
                                                 "self_resilience", "Depressive_Symptoms_PHQ4_BL", "PHQ9_Severity_FU1", "PHQ-9_FU2",
                                                 "General_Anxiety_Disorder_Severity_FU1", "General_Anxiety_Disorder_Severity_FU2",
                                                 "trauma_num", "FU_recent_trauma", "FU2_recent_trauma", "Education_year")]

# Combine depressive and anxiety symptoms into a single FU1 mental symptom score (scaled 0-10)
resilience_group_R_FU1$FU1_symptoms <- (resilience_group_R_FU1$PHQ9_Severity_FU1 / 28 + 
                                          resilience_group_R_FU1$General_Anxiety_Disorder_Severity_FU1 / 21) * 10

# Recode trauma exposure to cap maximum value
resilience_group_R_FU1$FU_recent_trauma[resilience_group_R_FU1$FU_recent_trauma == 3] <- 2
resilience_group_R_FU1 <- na.omit(resilience_group_R_FU1)
# Create FU1 analysis dataset
resilience_test_FU1 <- resilience_group_R_FU1 %>% 
  transmute(
    eid,
    Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")),
    Age = age_BL, 
    Ethnic = factor(Ethnic_group, levels = c("0","1","2","3"), labels = c("1_White","2_Asian","3_Black","4_Other")),  
    Education = Education_year,
    BMI = BMI_BL,
    selfresilience = factor(self_resilience, levels = c(0,1,2), labels = c("1_Low","2_Medium","3_High")),
    Mental_symptom_scores_FU1 = FU1_symptoms,
    Trauma_exposure = FU_recent_trauma
  )

# Convert trauma exposure to numeric and remove missing values
resilience_test_FU1$Trauma_exposure <- as.numeric(resilience_test_FU1$Trauma_exposure)
resilience_test_FU1 <- na.omit(resilience_test_FU1)

# Create FU1 ggline plot (mental symptoms vs trauma exposure, colored by resilience)
L2 <- create_ggline_plot(resilience_test_FU1, "Mental_symptom_scores_FU1")

# Calculate correlations between trauma exposure and FU1 mental symptoms for each resilience group
correlation_results_FU1 <- calculate_correlations(resilience_test_FU1, "Mental_symptom_scores_FU1", "FU1")
print(correlation_results_FU1)

# Compare correlations between resilience groups at FU1
comparison_results_FU1 <- compare_correlations(correlation_results_FU1)
print(comparison_results_FU1)


# -------------------------------
# FU2 analysis
# -------------------------------
# Prepare FU2 dataset with relevant variables
resilience_group_R_FU2 <- resilience_group_R[, c("eid", "age_BL", "gender", "BMI_BL", "Ethnic_group", "IMD",
                                                 "self_resilience", "Depressive_Symptoms_PHQ4_BL", "PHQ9_Severity_FU1", "PHQ-9_FU2",
                                                 "General_Anxiety_Disorder_Severity_FU1", "General_Anxiety_Disorder_Severity_FU2",
                                                 "trauma_num", "FU_recent_trauma", "FU2_recent_trauma", "Education_year")]

# Combine depressive and anxiety symptoms into a single FU2 mental symptom score (scaled 0-10)
resilience_group_R_FU2$FU2_symptoms <- (resilience_group_R_FU2$`PHQ-9_FU2` / 28 + 
                                          resilience_group_R_FU2$General_Anxiety_Disorder_Severity_FU2 / 21) * 10

# Create FU2 analysis dataset
resilience_test_FU2 <- resilience_group_R_FU2 %>% 
  transmute(
    eid,
    Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")),
    Age = age_BL, 
    Ethnic = factor(Ethnic_group, levels = c("0","1","2","3"), labels = c("1_White","2_Asian","3_Black","4_Other")),  
    Education = Education_year,
    BMI = BMI_BL,
    selfresilience = factor(self_resilience, levels = c(0,1,2), labels = c("1_Low","2_Medium","3_High")),
    Mental_symptom_scores_FU2 = FU2_symptoms,
    Trauma_exposure = FU2_recent_trauma
  )

# Cap extreme trauma exposure values and remove missing values
resilience_test_FU2$Trauma_exposure[resilience_test_FU2$Trauma_exposure >= 5] <- 4
resilience_test_FU2$Trauma_exposure <- as.numeric(resilience_test_FU2$Trauma_exposure)
resilience_test_FU2 <- na.omit(resilience_test_FU2)

# Create FU2 ggline plot
max_values_FU2 <- aggregate(Mental_symptom_scores_FU2 ~ Trauma_exposure, data = resilience_test_FU2, FUN = mean)
L3 <- ggline(resilience_test_FU2, x = "Trauma_exposure", y = "Mental_symptom_scores_FU2", add = "mean_se",
             color = "selfresilience", palette = c("#fA9E38", "#72BD5B", "#4995C6"), size = 1) +
  stat_compare_means(aes(group = selfresilience), label = "p.signif", 
                     label.y = max_values_FU2$Mental_symptom_scores_FU2 + 3, size = 4)

# Calculate correlations between trauma exposure and FU2 mental symptoms for each resilience group
correlation_results_FU2 <- calculate_correlations(resilience_test_FU2, "Mental_symptom_scores_FU2", "FU2")
print(correlation_results_FU2)

# Compare correlations between resilience groups at FU2
comparison_results_FU2 <- compare_correlations(correlation_results_FU2)
print(comparison_results_FU2)

# -------------------------------
# Combine plots for all timepoints
# -------------------------------
combined_plot <- grid.arrange(L1, L2, L3, ncol = 3)

# -------------------------------
# Summarize correlation results across timepoints and plot
# -------------------------------
cor_result <- rbind(correlation_results_BL, correlation_results_FU1, correlation_results_FU2)

cor_plot <- ggplot(cor_result) +
  aes(x = Resilience_Level, y = Correlation, fill = Resilience_Level, colour = Resilience_Level) +
  geom_col() +
  scale_fill_manual(values = c(`1_Low` = "#fA9E38", `2_Medium` = "#72BD5B", `3_High` = "#4995C6")) +
  scale_color_manual(values = c(`1_Low` = "#fA9E38", `2_Medium` = "#72BD5B", `3_High` = "#4995C6")) +
  theme_minimal() +
  facet_wrap(vars(Timepoint))

cor_plot