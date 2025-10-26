# =============================================================================
# Purpose:
# This script analyzes the relationship between self-reported resilience and mental 
# health outcomes across multiple timepoints (Baseline, FU1, FU2) in a UKB cohort.
# The analysis includes data preprocessing, resilience grouping, regression-based
# correlation analyses, group comparisons, visualization of results, and stratification by sex.
# =============================================================================

# -------------------------------
# Set up: Load required libraries
# -------------------------------
library(data.table)
library(tidyverse)   
library(dplyr)
library(ggpubr)
library(ggplot2)
library(gridExtra)
library(cocor)
library(janitor)

# -------------------------------
# Data loading
# -------------------------------
resilience_group_R <- read_csv("~/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/UKB_dat_for_analysis_1226.csv")[,-1]
# -------------------------------
# Create resilience tertiles
# -------------------------------
quantiles <- quantile(resilience_group_R$self_resilience, probs = c(1/3, 2/3), na.rm = TRUE)
resilience_group_R$self_resilience[resilience_group_R$self_resilience <= quantiles[1]] <- 0
resilience_group_R$self_resilience[resilience_group_R$self_resilience > quantiles[1] & resilience_group_R$self_resilience < quantiles[2]] <- 1
resilience_group_R$self_resilience[resilience_group_R$self_resilience >= quantiles[2]] <- 2
resilience_group_R$self_resilience <- as.factor(resilience_group_R$self_resilience)

resilience_levels <- c("0","1","2") # Low, Medium, High coded

# -------------------------------
# Functions for stratified correlation analysis and plotting
# -------------------------------
calculate_correlations_sex <- function(data, outcome_var, timepoint) {
  
  resilience_levels <- levels(data$selfresilience)
  
  correlation_results <- data.frame(
    Timepoint = character(),
    Sex = character(),
    Resilience_Level = character(),
    Correlation = numeric(),
    Int_low = numeric(),
    Int_high = numeric(),
    Subject_Count = integer(),
    stringsAsFactors = FALSE
  )
  
  for (sex_level in c("Female", "Male")) {
    
    data_sex <- data[data$Sex == sex_level, ]
    
    for (res_level in resilience_levels) {
      
      resilient_group <- data_sex[data_sex$selfresilience == res_level, ]
      if (nrow(resilient_group) < 10) next
      
      model_m <- lm(as.formula(paste(outcome_var, "~ Age + BMI + site + Education + Ethnic")), data = resilient_group)
      resilient_group$outcome_resid <- residuals(model_m)
      
      model_t <- lm(as.formula(paste("Trauma_exposure ~ Age + BMI + site + Education + Ethnic")), data = resilient_group)
      resilient_group$trauma_resid <- residuals(model_t)
      
      correlation_model <- cor.test(resilient_group$outcome_resid, resilient_group$trauma_resid)
      
      correlation_results <- rbind(correlation_results,
                                   data.frame(Timepoint = timepoint,
                                              Sex = sex_level,
                                              Resilience_Level = factor(res_level, levels=levels(data$selfresilience)),
                                              Correlation = correlation_model$estimate,
                                              Int_low = correlation_model$conf.int[1],
                                              Int_high = correlation_model$conf.int[2],
                                              Subject_Count = nrow(resilient_group)))
    }
  }
  
  return(correlation_results)
}

compare_correlations_sex <- function(correlation_results){
  
  resilience_levels <- levels(correlation_results$Resilience_Level)
  
  comparison_results <- data.frame(
    Sex=character(), 
    Group1=character(), 
    Group2=character(),
    Z=numeric(), 
    p_value=numeric(), 
    stringsAsFactors=FALSE
  )
  
  for(sex_level in c("Female","Male")){
    data_sex <- correlation_results[correlation_results$Sex == sex_level, ]
    
    for(i in 1:(length(resilience_levels)-1)){
      for(j in (i+1):length(resilience_levels)){
        group1 <- data_sex[data_sex$Resilience_Level == resilience_levels[i], ]
        group2 <- data_sex[data_sex$Resilience_Level == resilience_levels[j], ]
        if(nrow(group1) == 0 | nrow(group2) == 0) next
        
        x <- group1$Correlation
        y <- group2$Correlation
        n.x <- group1$Subject_Count
        n.y <- group2$Subject_Count
        
        comparison <- cocor.indep.groups(x, y, n.x, n.y, alternative="two.sided")
        cocor_result <- get.cocor.results(comparison)
        
        comparison_results <- rbind(comparison_results,
                                    data.frame(Sex=sex_level, Group1=resilience_levels[i], Group2=resilience_levels[j],
                                               Z=cocor_result$fisher1925$statistic, 
                                               p_value=cocor_result$fisher1925$p.value))
      }
    }
  }
  
  return(comparison_results)
}

create_ggline_plot_sex <- function(data, outcome_var){
  plot <- ggline(data, x="Trauma_exposure", y=outcome_var, add="mean_se",
                 color="selfresilience", palette=c("#fA9E38","#72BD5B","#4995C6"), size=1) +
    facet_wrap(~Sex) +
    theme_minimal()
  return(plot)
}

# -------------------------------
# Prepare BL dataset
# -------------------------------

resilience_group_R_BL <- resilience_group_R[, c("eid", "age_BL", "gender", "BMI_BL", "Ethnic_group", "IMD",
                                                 "self_resilience", "Depressive_Symptoms_PHQ4_BL", "PHQ9_Severity_FU1", "PHQ-9_FU2",
                                                 "General_Anxiety_Disorder_Severity_FU1", "General_Anxiety_Disorder_Severity_FU2",
                                                 "trauma_num", "FU_recent_trauma", "FU2_recent_trauma", "Education_year")]
resilience_group_R_BL <- na.omit(resilience_group_R_BL)

resilience_test_BL <- resilience_group_R_BL %>%
  transmute(
    eid,
    Sex=factor(gender, levels=c(0,1), labels=c("Female","Male")),
    Age=age_BL,
    Ethnic=factor(Ethnic_group, levels=c(0,1,2,3), labels=c("1_White","2_Asian","3_Black","4_Other")),
    Education=Education_year,
    BMI=BMI_BL,
    site = factor(site),
    selfresilience=factor(self_resilience, levels=c(0,1,2), labels=c("1_Low","2_Medium","3_High")),
    PHQ_BL=Depressive_Symptoms_PHQ4_BL-4,
    Trauma_exposure=trauma_num
  )
resilience_test_BL$Trauma_exposure[resilience_test_BL$Trauma_exposure>=5]<-4
resilience_test_BL <- na.omit(resilience_test_BL)

# BL plot and correlation
L1 <- create_ggline_plot_sex(resilience_test_BL, "PHQ_BL")
correlation_results_BL <- calculate_correlations_sex(resilience_test_BL, "PHQ_BL", "BL")
comparison_results_BL <- compare_correlations_sex(correlation_results_BL)

# -------------------------------
# Prepare FU1 dataset
# -------------------------------
resilience_group_R_FU1 <- resilience_group_R[, c("eid", "age_BL", "gender", "BMI_BL", "Ethnic_group", "IMD",
                                                 "self_resilience", "Depressive_Symptoms_PHQ4_BL", "PHQ9_Severity_FU1", "PHQ-9_FU2",
                                                 "General_Anxiety_Disorder_Severity_FU1", "General_Anxiety_Disorder_Severity_FU2",
                                                 "trauma_num", "FU_recent_trauma", "FU2_recent_trauma", "Education_year")]

resilience_group_R_FU1$FU1_symptoms <- (resilience_group_R_FU1$PHQ9_Severity_FU1/28 + 
                                          resilience_group_R_FU1$General_Anxiety_Disorder_Severity_FU1/21)*10
resilience_group_R_FU1$FU_recent_trauma[resilience_group_R_FU1$FU_recent_trauma==3]<-2
resilience_group_R_FU1 <- na.omit(resilience_group_R_FU1)

resilience_test_FU1 <- resilience_group_R_FU1 %>%
  transmute(
    eid,
    Sex=factor(gender, levels=c(0,1), labels=c("Female","Male")),
    Age=age_BL,
    Ethnic=factor(Ethnic_group, levels=c(0,1,2,3), labels=c("1_White","2_Asian","3_Black","4_Other")),
    Education=Education_year,
    BMI=BMI_BL,
    site = factor(site),
    selfresilience=factor(self_resilience, levels=c(0,1,2), labels=c("1_Low","2_Medium","3_High")),
    Mental_symptom_scores_FU1=FU1_symptoms,
    Trauma_exposure=FU_recent_trauma
  )
resilience_test_FU1 <- na.omit(resilience_test_FU1)

L2 <- create_ggline_plot_sex(resilience_test_FU1, "Mental_symptom_scores_FU1")
correlation_results_FU1 <- calculate_correlations_sex(resilience_test_FU1, "Mental_symptom_scores_FU1", "FU1")
comparison_results_FU1 <- compare_correlations_sex(correlation_results_FU1)

# -------------------------------
# Prepare FU2 dataset
# -------------------------------
resilience_group_R_FU2 <- resilience_group_R[, c("eid", "age_BL", "gender", "BMI_BL", "Ethnic_group", "IMD",
                                                 "self_resilience", "Depressive_Symptoms_PHQ4_BL", "PHQ9_Severity_FU1", "PHQ-9_FU2",
                                                 "General_Anxiety_Disorder_Severity_FU1", "General_Anxiety_Disorder_Severity_FU2",
                                                 "trauma_num", "FU_recent_trauma", "FU2_recent_trauma", "Education_year")]

resilience_group_R_FU2$FU2_symptoms <- (resilience_group_R_FU2$`PHQ-9_FU2`/28 +
                                          resilience_group_R_FU2$General_Anxiety_Disorder_Severity_FU2/21)*10
resilience_group_R_FU2 <- na.omit(resilience_group_R_FU2)
resilience_test_FU2 <- resilience_group_R_FU2 %>%
  transmute(
    eid,
    Sex=factor(gender, levels=c(0,1), labels=c("Female","Male")),
    Age=age_BL,
    Ethnic=factor(Ethnic_group, levels=c(0,1,2,3), labels=c("1_White","2_Asian","3_Black","4_Other")),
    Education=Education_year,
    BMI=BMI_BL,
    site = factor(site),
    selfresilience=factor(self_resilience, levels=c(0,1,2), labels=c("1_Low","2_Medium","3_High")),
    Mental_symptom_scores_FU2=FU2_symptoms,
    Trauma_exposure=FU2_recent_trauma
  )
resilience_test_FU2$Trauma_exposure[resilience_test_FU2$Trauma_exposure>=5]<-4
resilience_test_FU2 <- na.omit(resilience_test_FU2)

L3 <- create_ggline_plot_sex(resilience_test_FU2, "Mental_symptom_scores_FU2")
correlation_results_FU2 <- calculate_correlations_sex(resilience_test_FU2, "Mental_symptom_scores_FU2", "FU2")
comparison_results_FU2 <- compare_correlations_sex(correlation_results_FU2)

# -------------------------------
# Combine plots and results
# -------------------------------
combined_plot <- grid.arrange(L1,L2,L3, ncol=3)
cor_result_sex <- rbind(correlation_results_BL, correlation_results_FU1, correlation_results_FU2)

cor_plot_sex <- ggplot(cor_result_sex) +
  aes(x=Resilience_Level, y=Correlation, fill=Resilience_Level, colour=Resilience_Level) +
  geom_col(position=position_dodge(width=0.7)) +
  scale_fill_manual(values=c(`1_Low`="#fA9E38", `2_Medium`="#72BD5B", `3_High`="#4995C6")) +
  scale_color_manual(values=c(`1_Low`="#fA9E38", `2_Medium`="#72BD5B", `3_High`="#4995C6")) +
  theme_minimal() +
  facet_grid(Timepoint ~ Sex)

cor_plot_sex