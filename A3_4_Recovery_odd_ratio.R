# ==============================================================================
# Script: A3_4_Recovery_odd_ratio.R
# Project: Lifespan Resilience Modeling (STM Submission)
# Purpose: Calculate pairwise odds ratios and 95% confidence intervals for symptom recovery versus worsening across resilience groups within symptom severity strata.
# ==============================================================================

library(dplyr)
library(epitools)
library(tidyr)

# -----------------------------
# Function to calculate pairwise odds ratios (OR), p-values, and 95% CIs
# -----------------------------
pairwise_or_test <- function(data, group_var, compare_var) {
  groups <- levels(data[[group_var]])
  # Initialize empty matrices for results
  or_matrix <- matrix(NA, nrow=length(groups), ncol=length(groups), dimnames=list(groups, groups))
  p_matrix <- matrix(NA, nrow=length(groups), ncol=length(groups), dimnames=list(groups, groups))
  ci_lower_matrix <- matrix(NA, nrow=length(groups), ncol=length(groups), dimnames=list(groups, groups))
  ci_upper_matrix <- matrix(NA, nrow=length(groups), ncol=length(groups), dimnames=list(groups, groups))
  
  for (i in 1:(length(groups)-1)) {
    for (j in (i+1):length(groups)) {
      subset_data <- data %>% filter(data[[group_var]] %in% c(groups[i], groups[j]))
      subset_data[[group_var]] <- droplevels(subset_data[[group_var]])
      tab <- table(subset_data[[group_var]], subset_data[[compare_var]])
      
      if(all(dim(tab) == c(2,2))) {
        or_result <- oddsratio(tab)
        or_val <- or_result$measure[2,1]   # OR
        ci_lower <- or_result$measure[2,2]  # 95% CI lower bound
        ci_upper <- or_result$measure[2,3]  # 95% CI upper bound
        test_result <- chisq.test(tab, correct = FALSE)
        
        or_matrix[i,j] <- or_val
        or_matrix[j,i] <- or_val
        p_matrix[i,j] <- test_result$p.value
        p_matrix[j,i] <- test_result$p.value
        ci_lower_matrix[i,j] <- ci_lower
        ci_lower_matrix[j,i] <- ci_lower
        ci_upper_matrix[i,j] <- ci_upper
        ci_upper_matrix[j,i] <- ci_upper
      } else {
        or_matrix[i,j] <- NA
        or_matrix[j,i] <- NA
        p_matrix[i,j] <- NA
        p_matrix[j,i] <- NA
        ci_lower_matrix[i,j] <- NA
        ci_lower_matrix[j,i] <- NA
        ci_upper_matrix[i,j] <- NA
        ci_upper_matrix[j,i] <- NA
      }
    }
  }
  
  # Convert matrices to long format
  or_long <- as.data.frame(as.table(or_matrix))
  colnames(or_long) <- c(group_var, "CompareGroup", "OR")
  p_long <- as.data.frame(as.table(p_matrix))
  colnames(p_long) <- c(group_var, "CompareGroup", "P")
  ci_lower_long <- as.data.frame(as.table(ci_lower_matrix))
  colnames(ci_lower_long) <- c(group_var, "CompareGroup", "CI_Lower")
  ci_upper_long <- as.data.frame(as.table(ci_upper_matrix))
  colnames(ci_upper_long) <- c(group_var, "CompareGroup", "CI_Upper")
  
  # Merge OR estimates, p-values, and confidence intervals
  long_table <- left_join(or_long, p_long, by=c(group_var, "CompareGroup")) %>%
    left_join(ci_lower_long, by=c(group_var, "CompareGroup")) %>%
    left_join(ci_upper_long, by=c(group_var, "CompareGroup"))
  long_table$Variable <- compare_var
  return(long_table)
}

# -----------------------------
# High symptom severity group
# -----------------------------
resilience_high <- resilience_compare %>% filter(FU1_severity == 3)
resilience_high$selfresilience <- factor(resilience_high$selfresilience)

variables <- c("recovery", "worse")
high_results <- lapply(variables, function(var){
  pairwise_or_test(resilience_high, "selfresilience", var)
})
high_results <- bind_rows(high_results)  # Combine into single summary table
high_results$Severity <- "High"

# -----------------------------
# Medium symptom severity group
# -----------------------------
resilience_mid <- resilience_compare %>% filter(FU1_severity == 2)
resilience_mid$selfresilience <- factor(resilience_mid$selfresilience)

variables <- c("recovery", "worse")
mid_results <- lapply(variables, function(var){
  pairwise_or_test(resilience_mid, "selfresilience", var)
})
mid_results <- bind_rows(mid_results)
mid_results$Severity <- "Medium"

# -----------------------------
# Combine all results across severity strata
# -----------------------------
all_results <- bind_rows(high_results, mid_results) %>%
  select(Severity, Variable, selfresilience, CompareGroup, OR, CI_Lower, CI_Upper, P)

# Display combined results
print(all_results)