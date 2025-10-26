# ============================================================
# Purpose:
#   This script examines the associations between blood biomarkers 
#   and depressive symptoms (PHQ), focusing on:
#     (1) the main effects of biomarkers,
#     (2) the interaction effects between biomarkers and self-resilience.
#   FDR correction is applied to identify significant effects.
# ============================================================

# ----------------------------
# Initialize result tables
# ----------------------------
interact_result_table <- data.frame()     # Store full regression results
I_results_table <- data.frame()           # Store interaction term results
main_effect_table <- data.frame()         # Store main effect results

# ----------------------------
# Define biomarker list
# ----------------------------
biomarker_list <- mediation_results_table_short$Biomarker

# ----------------------------
# Loop through each biomarker
# ----------------------------
for (b in biomarker_list) {
  
  # Prepare dataset for analysis
  resilience_test <- resilience_group_R %>%
    transmute(
      eid, 
      Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")), 
      Age = age_BL, 
      Ethnic = factor(Ethnic_group, levels = c("0", "1", "2", "3"),
                      labels = c("1_White", "2_Asian", "3_Black", "4_Other")),
      Education = Education_year,
      site = factor(site),
      BMI = BMI_BL, 
      selfresilience = self_resilience,
      PHQ = Depressive_Symptoms_PHQ4_BL,
      !!b := !!sym(b),
      !!t := !!sym(t)
    ) %>%
    filter(selfresilience != 1) %>%   # Exclude participants in the "medium" group
    na.omit()                         # Remove missing values
  
  # Linear regression model:
  # PHQ ~ biomarker + resilience + biomarker*resilience + covariates
  fit <- lm(
    as.formula(paste0(
      "PHQ ~ `", b, "` + selfresilience + `", b, "`*selfresilience + ",
      "Age + Sex + Ethnic + BMI + Education + site"
    )),
    data = resilience_test
  )
  
  # Extract regression results
  fit_result <- tidy(fit)
  
  # Store full regression output
  interact_result_table <- bind_rows(interact_result_table, fit_result)
  
  # Extract interaction term (biomarker × selfresilience)
  inter_term <- fit_result %>% 
    filter(grepl(paste0(b, ".*selfresilience"), term)) %>%
    mutate(Biomarker = b)
  I_results_table <- bind_rows(I_results_table, inter_term)
  
  # Extract main effect (biomarker)
  main_term <- fit_result %>%
    filter(term == b) %>%
    mutate(Biomarker = b)
  main_effect_table <- bind_rows(main_effect_table, main_term)
}

# ----------------------------
# Add stage labels
# ----------------------------
I_results_table$Stage <- "Blood Protein"
main_effect_table$Stage <- "Blood Protein"

# ----------------------------
# FDR correction
# ----------------------------

# Adjust p-values for interaction effects
I_results_table$adjusted_p <- p.adjust(I_results_table$p.value, method = "BH")
I_results_table_sig <- I_results_table %>% filter(adjusted_p < 0.05)

# Adjust p-values for main effects
main_effect_table$adjusted_p <- p.adjust(main_effect_table$p.value, method = "BH")
main_effect_table_sig <- main_effect_table %>% filter(adjusted_p < 0.05)

# ----------------------------
# Display results
# ----------------------------
cat("Significant interaction effects (FDR < 0.05):\n")
print(head(I_results_table_sig))

cat("\nSignificant main effects (FDR < 0.05):\n")
print(head(main_effect_table_sig))