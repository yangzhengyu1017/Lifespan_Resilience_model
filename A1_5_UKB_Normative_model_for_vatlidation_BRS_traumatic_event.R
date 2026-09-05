# ==============================================================================
# Script: A1_5_UKB_Normative_model_for_vatlidation_BRS_traumatic_event.R
# Project: Lifespan Resilience Modeling (STM Submission)
# Purpose: Validate Brief Resilience Scale (BRS) against normative outcome-based mental health symptom residuals adjusted for traumatic events and covariates.
# ==============================================================================

# 1. Prepare continuous self-reported resilience variable
# -----------------------------------------------------------------------------
# Ensure self-reported resilience is retained as a continuous numeric variable
raw_df <- read_csv("data/UKB_dat_for_analysis_1226.csv")[,-1]

# Select required columns including continuous self-reported resilience
df_resid_analysis <- raw_df %>%
  select(eid, self_resilience, 
         # Baseline Vars
         PHQ_BL = Depressive_Symptoms_PHQ4_BL,
         Trauma_BL = trauma_num,
         # FU1 Vars
         PHQ9_FU1 = PHQ9_Severity_FU1, GAD_FU1 = General_Anxiety_Disorder_Severity_FU1,
         Trauma_FU1 = FU_recent_trauma,
         # FU2 Vars
         PHQ9_FU2 = `PHQ-9_FU2`, GAD_FU2 = General_Anxiety_Disorder_Severity_FU2,
         Trauma_FU2 = FU2_recent_trauma,
         # Covariates
         Age = age_BL, Sex = gender, Education = Education_year, 
         BMI = BMI_BL, Ethnic = Ethnic_group, site = site) %>%
  # Preprocessing: construct composite scores and format factors
  mutate(
    Sex = factor(Sex), Ethnic = factor(Ethnic), site = factor(site),
    PHQ_BL = PHQ_BL - 4,
    Trauma_BL = ifelse(Trauma_BL >= 5, 4, Trauma_BL),
    
    Mental_FU1 = (PHQ9_FU1/28 + GAD_FU1/21) * 10,
    Trauma_FU1 = ifelse(Trauma_FU1 == 3, 2, Trauma_FU1),
    
    Mental_FU2 = (PHQ9_FU2/28 + GAD_FU2/21) * 10,
    Trauma_FU2 = ifelse(Trauma_FU2 >= 5, 4, Trauma_FU2)
  )

# 2. Define residual extraction function
# -----------------------------------------------------------------------------
get_residuals <- function(data, outcome, trauma) {
  # 1. Clean and subset complete cases for model
  temp_df <- data %>% 
    select(eid, all_of(c(outcome, trauma, "Age", "Sex", "Education", "BMI", "Ethnic", "site", "self_resilience"))) %>% 
    na.omit()
  
  # 2. Fit linear regression: Mental ~ Trauma + Covariates
  # Residuals represent mental health variation unexplained by trauma exposure and demographics
  model <- lm(as.formula(paste(outcome, "~", trauma, "+ Age + Sex + Education + BMI + Ethnic + site")), 
              data = temp_df)
  
  # 3. Extract model residuals
  temp_df$Residual_Score <- residuals(model)
  
  # 4. Return data frame with ID, continuous resilience, and residual score
  return(temp_df %>% select(eid, self_resilience, Residual_Score))
}

# 3. Extract residuals across the three evaluation stages
# -----------------------------------------------------------------------------
# Baseline residuals
res_BL <- get_residuals(df_resid_analysis, "PHQ_BL", "Trauma_BL") %>% 
  mutate(Stage = "Baseline")

# Follow-Up 1 residuals
res_FU1 <- get_residuals(df_resid_analysis, "Mental_FU1", "Trauma_FU1") %>% 
  mutate(Stage = "FU1")

# Follow-Up 2 residuals
res_FU2 <- get_residuals(df_resid_analysis, "Mental_FU2", "Trauma_FU2") %>% 
  mutate(Stage = "FU2")

# Combine data across stages
all_residuals <- bind_rows(res_BL, res_FU1, res_FU2)
all_residuals$Stage <- factor(all_residuals$Stage, levels = c("Baseline", "FU1", "FU2"))

# 4. Correlate self-reported resilience with residual-based resilience
# -----------------------------------------------------------------------------

cor_stats <- all_residuals %>%
  group_by(Stage) %>%
  summarise(
    Pearson_r = cor.test(self_resilience, Residual_Score)$estimate,
    P_value = cor.test(self_resilience, Residual_Score)$p.value,
    N = n()
  )

print("--- Correlation between Self-Resilience and Residual-Based Resilience ---")
print(cor_stats)

# 5. Visualization
# -----------------------------------------------------------------------------
# Scatter plot with linear regression fit
resid_plot <- ggplot(all_residuals, aes(x = self_resilience, y = Residual_Score)) +
  # Scatter points with alpha transparency
  geom_point(alpha = 0.3, color = "#82afda", size = 1) +
  # Linear trendline
  geom_smooth(method = "lm", color = "#d95f02", linewidth = 1) +
  # Facet by stage
  facet_wrap(~Stage, scales = "free") +
  # Display correlation statistics
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top", size = 4) +
  # Theme styling
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 11)
  ) +
  labs(
    title = "Validation of Self-Resilience against Outcome-Based Residuals",
    subtitle = "Negative correlation indicates consistency (Higher Self-Resilience ~ Lower Symptom Residuals)",
    x = "Self-Reported Resilience Score (Continuous)",
    y = "Mental Health Symptom Residuals"
  )

print(resid_plot)