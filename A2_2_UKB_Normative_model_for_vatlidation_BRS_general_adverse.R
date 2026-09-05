# ==============================================================================
# Script: A2_2_UKB_Normative_model_for_vatlidation_BRS_general_adverse.R
# Project: Lifespan Resilience Modeling (STM Submission)
# Purpose: Validate Brief Resilience Scale against normative outcome-based residuals adjusted comprehensively for trauma and broad adverse environmental factors.
# ==============================================================================

library(tidyverse)
library(ggpubr)
library(data.table)

# -----------------------------------------------------------------------------
# 1. Defining Comprehensive Adversity List
# -----------------------------------------------------------------------------
# Combine ecological-social, early-life risk, and environmental adversity variables
all_adversity_vars <- c(
  # Eco_Social
  "HH_Num_Vehicle_BL", "HH_Income_BL", "HH_Own_Rent_BL", "Social_Freq_Visits_BL", 
  "Social_Able_Confide_BL", "Num_People_Living_BL", "Loneliness_BL",
  # Early_risk
  "Breastfed_Baby_BL", "Comp_Body_Size_Age_10_BL", "Comp_Height_Size_Age_10_BL", "Maternal_Smoking_Birth_BL",
  "Felt_Loved_As_Child_FU1", "Phys_Abused_As_Child_FU1", "Felt_Hated_As_Child_FU1", 
  "Sex_Molested_As_Child_FU1", "Someone_Take_To_Doctor_As_Child_FU1",
  # Environment
  "NO2_Air_Pollution_2010_BL", "NOx_Air_Pollution_2010_BL", "PM10_Air_Pollution_2010_BL", 
  "PM2.5_Air_Pollution_2010_BL", "PM2.5_Absorbance_2010_BL", "PM2.5_10um_Air_Pollution_2010_BL",
  "Traffic_Intensity_Nearest_Road_BL", "Inv_Dist_Nearest_Road_BL", 
  "Traffic_Intensity_Nearest_Major_Road_BL", "Inv_Dist_Nearest_Major_Road_BL",
  "Total_Traffic_Load_Major_Roads_BL", "Close_To_Major_Road_BL", "Sum_Major_Road_Length_100m_BL",
  "NO2_Air_Pollution_2005_BL", "NO2_Air_Pollution_2006_BL", "NO2_Air_Pollution_2007_BL", 
  "PM10_Air_Pollution_2007_BL", "Avg_Daytime_Sound_Level_BL", "Avg_Evening_Sound_Level_BL", 
  "Avg_Nighttime_Sound_Level_BL", "Greenspace_Percentage_1000m_BL", "Domestic_Garden_Percentage_1000m_BL", 
  "Water_Percentage_1000m_BL", "Greenspace_Percentage_300m_BL", "Domestic_Garden_Percentage_300m_BL", 
  "Water_Percentage_300m_BL", "Natural_Env_Percentage_1000m_BL", "Natural_Env_Percentage_300m_BL", 
  "Distance_To_Coast_Euclidean_BL"
)

# -----------------------------------------------------------------------------
# 2. Data Preparation and Cleaning
# -----------------------------------------------------------------------------
# Prepare dataset and format variables
raw_df <- read_csv("data/UKB_dat_for_analysis_1226.csv")[,-1]
df_analysis <- raw_df %>%
  mutate(
    # Format factor variables
    Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")),
    Ethnic = factor(Ethnic_group),
    site = factor(site),
    Age = age_BL,
    Education = Education_year,
    BMI = BMI_BL,
    
    # Ensure continuous self-reported resilience score
    self_resilience_cont = as.numeric(self_resilience), 
    
    # Outcome variables and focal trauma exposure
    PHQ_BL = Depressive_Symptoms_PHQ4_BL,
    Trauma_BL = trauma_num,
    
    Mental_FU1 = (PHQ9_Severity_FU1/28 + General_Anxiety_Disorder_Severity_FU1/21) * 10,
    Trauma_FU1 = ifelse(FU_recent_trauma == 3, 2, FU_recent_trauma),
    
    Mental_FU2 = (`PHQ-9_FU2`/28 + General_Anxiety_Disorder_Severity_FU2/21) * 10,
    Trauma_FU2 = FU2_recent_trauma # Scaled trauma score
  )

# Standardize all adversity variables (Z-score) to ensure comparable scales
df_analysis <- df_analysis %>%
  mutate(across(all_of(all_adversity_vars), ~as.numeric(scale(.))))

# -----------------------------------------------------------------------------
# 3. Residual Extraction Function with Complete-Case Filtering
# -----------------------------------------------------------------------------
get_residuals_clean <- function(data, outcome, trauma_specific, adversity_list) {
  
  # A. Define model variables
  model_vars <- c(outcome, trauma_specific, adversity_list, 
                  "Age", "Sex", "Education", "BMI", "Ethnic", "site", 
                  "eid", "self_resilience_cont") # Retain ID and resilience score for downstream merging
  
  # B. Extract complete-case subset via listwise deletion
  # ---------------------------------------------------------
  cat(paste0("\nProcessing Model for Outcome: ", outcome, "\n"))
  cat(paste0("Initial N: ", nrow(data), "\n"))
  
  df_clean <- data %>%
    select(all_of(model_vars)) %>%
    na.omit()
  
  cat(paste0("Cleaned N (No NAs): ", nrow(df_clean), "\n"))
  cat(paste0("Dropped N: ", nrow(data) - nrow(df_clean), "\n"))
  
  # C. Construct multivariable regression formula with all adverse factors
  # Y ~ Specific Trauma + Adv1 + Adv2 + ... + AdvN + Covariates
  adv_formula <- paste(adversity_list, collapse = " + ")
  full_formula <- paste(outcome, "~", trauma_specific, "+", adv_formula, 
                        "+ Age + Sex + Education + BMI + Ethnic + site")
  
  # D. Fit multivariable regression model
  model <- lm(as.formula(full_formula), data = df_clean)
  
  # E. Extract residuals (positive: higher symptoms than expected; negative: lower symptoms)
  df_clean$Residual_Score <- residuals(model)
  
  return(df_clean %>% select(eid, self_resilience_cont, Residual_Score))
}

# -----------------------------------------------------------------------------
# 4. Compute residuals across evaluation stages
# -----------------------------------------------------------------------------

# Baseline
res_BL <- get_residuals_clean(df_analysis, "PHQ_BL", "Trauma_BL", all_adversity_vars) %>%
  mutate(Stage = "Baseline")

# FU1
res_FU1 <- get_residuals_clean(df_analysis, "Mental_FU1", "Trauma_FU1", all_adversity_vars) %>%
  mutate(Stage = "FU1")

# FU2
res_FU2 <- get_residuals_clean(df_analysis, "Mental_FU2", "Trauma_FU2", all_adversity_vars) %>%
  mutate(Stage = "FU2")

# Combine all stage results
all_residuals <- bind_rows(res_BL, res_FU1, res_FU2)
all_residuals$Stage <- factor(all_residuals$Stage, levels = c("Baseline", "FU1", "FU2"))

# -----------------------------------------------------------------------------
# 5. Correlation Analysis: Self-Reported vs. Adversity-Adjusted Residuals
# -----------------------------------------------------------------------------
cor_stats <- all_residuals %>%
  group_by(Stage) %>%
  summarise(
    Pearson_r = cor.test(self_resilience_cont, Residual_Score)$estimate,
    P_value = cor.test(self_resilience_cont, Residual_Score)$p.value,
    N = n()
  )

print("--- Correlation: Self-Reported Resilience vs. Outcome-Based Resilience (Residuals) ---")
print(cor_stats)

# -----------------------------------------------------------------------------
# 6. Visualization
# -----------------------------------------------------------------------------
resid_plot <- ggplot(all_residuals, aes(x = self_resilience_cont, y = Residual_Score)) +
  # Scatter points with alpha transparency
  geom_point(alpha = 0.2, color = "#82afda", size = 1) + 
  # Regression trendline
  geom_smooth(method = "lm", color = "#d95f02", linewidth = 1) +
  # Facet by stage
  facet_wrap(~Stage, scales = "free_y") +
  # Display correlation statistics
  stat_cor(method = "pearson", 
           label.x.npc = "left", label.y.npc = "top", 
           size = 4, color = "black") +
  # Theme styling
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 11, face = "bold"),
    panel.border = element_rect(color = "grey80", fill = NA)
  ) +
  labs(
    title = "Validation: Self-Resilience vs. Symptom Residuals",
    subtitle = "Residuals calculated after adjusting for Specific Trauma AND General Adversity",
    x = "Self-Reported Resilience Score",
    y = "Symptom Residuals (Unexplained by Adversity)"
  )

print(resid_plot)