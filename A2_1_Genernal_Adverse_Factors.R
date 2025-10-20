# Title: Resilience and Adverse Factors Analysis in UK Biobank Data
# Author: [Your Name]
# Date: October 20, 2025
# Description: 
# This script analyzes the relationship between resilience levels and various adverse factors 
# (economic/social, early risk, environmental) with mental health symptoms in the UK Biobank dataset.
# It performs data preprocessing, linear modeling, correlation analysis, group comparisons using 
# Fisher's Z-test, FDR correction, and visualization with dumbbell and bar plots for baseline (BL), 
# follow-up 1 (FU1), and follow-up 2 (FU2) timepoints. 
# The analysis focuses on low and high resilience groups, adjusting for covariates like age, sex, 
# ethnicity, BMI, and education.
# Assumptions: Required CSV files are available in the specified paths. 
# Required packages: data.table, tidyverse, rstatix, ggpubr, caret, visreg, rms, gtsummary, 
# autoReg, ggplot2, ggbeeswarm, patchwork, dplyr, emmeans, bruceR, gridExtra, cocor, ggtext, showtext.
# Output: Summary tables, statistical results, and plots for each timepoint.

# Section 1: Load Required Packages
library(data.table)     # For efficient data manipulation
library(tidyverse)      # For data wrangling and visualization
library(rstatix)        # For statistical tests
library(ggpubr)         # For publication-ready plots
library(caret)          # For machine learning utilities (used in modeling)
library(visreg)         # For visualizing regression models
library(rms)            # For regression modeling strategies
library(gtsummary)      # For summary tables
library(autoReg)        # For automated regression
library(ggplot2)        # For advanced plotting
library(ggbeeswarm)     # For beeswarm plots
library(patchwork)      # For combining plots
library(dplyr)          # For data manipulation (part of tidyverse)
library(emmeans)        # For estimated marginal means
library(bruceR)         # For additional statistical tools
library(gridExtra)      # For grid-based plot arrangements
library(cocor)          # For comparing correlations
library(ggtext)         # For enhanced text in ggplot
library(showtext)       # For custom fonts in plots
showtext_auto()         # Enable showtext for font rendering

# Section 2: Load and Preprocess Data
# Load resilience correlation data
resilience_corr_rename_dat <- read_csv("~/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/UKB_resilience_corr_rename_dat_11221.csv")
resilience_corr_rename_dat <- resilience_corr_rename_dat[, -1]  # Remove first column (likely index)

# Load resilience model data
resilience_model_dat <- read_csv("~/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/predicited_resiliece_3w_norm_dat.csv")
resilience_model_dat <- resilience_model_dat[, -1]  # Remove first column

# Load social/environmental/trauma/mental health data
soc_env_trauma_mental_dat <- read.csv("~/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/UKB_BL_FU_trauma_mental_env_dat.csv")
soc_env_trauma_mental_dat <- soc_env_trauma_mental_dat[, -1]  # Remove first column

# Load baseline trauma data
Trauma_BL_dat <- read.csv("~/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/UKB_BL_Trauma.csv")
Trauma_BL_dat <- Trauma_BL_dat[, -1]  # Remove first column

# Merge datasets for resilience grouping
resilience_group_R <- resilience_corr_rename_dat
resilience_group_R <- merge(resilience_group_R, resilience_model_dat[, c(1, 3)], by = "eid", all.x = TRUE)
resilience_group_R <- merge(resilience_group_R, soc_env_trauma_mental_dat[, c(1, 71, 85, 79)], by = "eid", all.x = TRUE)

# Normalize and adjust specific variables
resilience_group_R$self_resilience <- (resilience_group_R$self_resilience + 6) / 6
resilience_group_R$FU2_recent_trauma <- resilience_group_R$FU2_recent_trauma * 10

# Recode ethnic groups
resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 2, 6, Ethnic_group))
resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 3, 2, Ethnic_group))
resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 5, 2, Ethnic_group))
resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 4, 3, Ethnic_group))
resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 6, 4, Ethnic_group))
resilience_group_R$Ethnic_group <- resilience_group_R$Ethnic_group - 1

# Calculate quantiles and group self-resilience into tertiles
quantiles <- quantile(resilience_group_R$self_resilience, probs = c(1/3, 2/3), na.rm = TRUE)
resilience_group_R$self_resilience[resilience_group_R$self_resilience <= quantiles[1]] <- 0
resilience_group_R$self_resilience[resilience_group_R$self_resilience > quantiles[1] & resilience_group_R$self_resilience < quantiles[2]] <- 1
resilience_group_R$self_resilience[resilience_group_R$self_resilience >= quantiles[2]] <- 2
resilience_group_R$self_resilience <- as.factor(resilience_group_R$self_resilience)

# Calculate childhood trauma score
resilience_group_R$child_trauma <- resilience_group_R$Felt_Hated_As_Child_FU1 + 4 - resilience_group_R$Felt_Loved_As_Child_FU1 + 
  resilience_group_R$Sex_Molested_As_Child_FU1 + resilience_group_R$Phys_Abused_As_Child_FU1 + 
  4 - resilience_group_R$Someone_Take_To_Doctor_As_Child_FU1

# Rename specific column for consistency
colnames(resilience_group_R)[colnames(resilience_group_R) == "PM2.5-10um_Air_Pollution_2010_BL"] <- "PM2.5_10um_Air_Pollution_2010_BL"

# Negate selected variables to reverse their direction
columns_to_negate <- c("Social_Able_Confide_BL", "Felt_Loved_As_Child_FU1", "HH_Income_BL", 
                       "HH_Num_Vehicle_BL", "Someone_Take_To_Doctor_As_Child_FU1", 
                       "Natural_Env_Percentage_1000m_BL", "Greenspace_Percentage_1000m_BL",
                       "Natural_Env_Percentage_300m_BL", "Greenspace_Percentage_300m_BL",
                       "Breastfed_Baby_BL", "Num_People_Living_BL", "Comp_Body_Size_Age_10_BL",
                       "Water_Percentage_300m_BL", "Domestic_Garden_Percentage_300m_BL",
                       "Distance_To_Coast_Euclidean_BL", "Comp_Height_Size_Age_10_BL",
                       "Water_Percentage_1000m_BL")
resilience_group_R[columns_to_negate] <- -resilience_group_R[columns_to_negate]

# Define adverse factor categories (same for BL, FU1, FU2)
trauma_variables_list <- list(
  Eco_Social = c(
    "HH_Num_Vehicle_BL", "HH_Income_BL", "HH_Own_Rent_BL", "Social_Freq_Visits_BL", "Social_Able_Confide_BL",
    "Num_People_Living_BL", "Loneliness_BL"
  ),
  Early_risk = c(
    "Breastfed_Baby_BL", "Comp_Body_Size_Age_10_BL", "Comp_Height_Size_Age_10_BL",
    "Maternal_Smoking_Birth_BL", "Felt_Loved_As_Child_FU1", "Phys_Abused_As_Child_FU1",
    "Felt_Hated_As_Child_FU1", "Sex_Molested_As_Child_FU1", "Someone_Take_To_Doctor_As_Child_FU1"
  ),
  Enviroment = c(
    "NO2_Air_Pollution_2010_BL", "NOx_Air_Pollution_2010_BL", "PM10_Air_Pollution_2010_BL",
    "PM2.5_Air_Pollution_2010_BL", "PM2.5_Absorbance_2010_BL", "PM2.5_10um_Air_Pollution_2010_BL",
    "Traffic_Intensity_Nearest_Road_BL", "Inv_Dist_Nearest_Road_BL",
    "Traffic_Intensity_Nearest_Major_Road_BL", "Inv_Dist_Nearest_Major_Road_BL",
    "Total_Traffic_Load_Major_Roads_BL", "Close_To_Major_Road_BL",
    "Sum_Major_Road_Length_100m_BL", "NO2_Air_Pollution_2005_BL",
    "NO2_Air_Pollution_2006_BL", "NO2_Air_Pollution_2007_BL",
    "PM10_Air_Pollution_2007_BL", "Avg_Daytime_Sound_Level_BL",
    "Avg_Evening_Sound_Level_BL", "Avg_Nighttime_Sound_Level_BL",
    "Greenspace_Percentage_1000m_BL", "Domestic_Garden_Percentage_1000m_BL",
    "Water_Percentage_1000m_BL", "Greenspace_Percentage_300m_BL",
    "Domestic_Garden_Percentage_300m_BL", "Water_Percentage_300m_BL",
    "Natural_Env_Percentage_1000m_BL", "Natural_Env_Percentage_300m_BL",
    "Distance_To_Coast_Euclidean_BL"
  )
)

# Section 3: Analysis for Baseline (BL) Timepoint
# Initialize result tables
result_table_BL <- data.frame()
trauma_effect_table_BL <- data.frame()
correlation_results_BL_all <- data.frame(Trauma = character(), Category = character(), Timepoint = character(),
                                         Resilience_Level = character(), Correlation = numeric(), 
                                         Subject_Count = integer(), stringsAsFactors = FALSE)
comparison_results_BL_all <- data.frame(Trauma = character(), Timepoint = character(), Group1 = character(), 
                                        Group2 = character(), Z = numeric(), p_value = numeric(), 
                                        stringsAsFactors = FALSE)

# Loop over adverse factor categories and variables
for (trauma_var in names(trauma_variables_list)) {
  trauma <- trauma_variables_list[[trauma_var]]
  
  for (t in trauma) {
    # Prepare data subset
    resilience_test <- resilience_group_R %>% 
      transmute(
        eid,
        Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")),
        Age = age_BL,
        Ethnic = factor(Ethnic_group, levels = c("0", "1", "2", "3"), labels = c("1_White", "2_Asian", "3_Black", "4_Other")),
        Education = Education_year,
        BMI = BMI_BL,
        selfresilience = factor(self_resilience, levels = c(0, 1, 2), labels = c("1_Low", "2_Medium", "3_High")),
        PHQ = Depressive_Symptoms_PHQ4_BL,
        !!t := !!sym(t)
      )
    resilience_test <- na.omit(resilience_test)
    
    # Fit linear model for adverse factor effect
    lme1 <- lm(as.formula(paste("PHQ ~ ", t, "+ Age + Sex + Ethnic + BMI + Education")), data = resilience_test)
    results <- tidy(lme1)
    results$Trauma_Variable <- t
    result_table_BL <- bind_rows(result_table_BL, results)
    
    # Extract adverse factor effect
    filtered_results <- results %>% filter(term == t)
    filtered_results$Trauma_Variable <- t
    trauma_effect_table_BL <- bind_rows(trauma_effect_table_BL, filtered_results)
    
    # Correlation analysis by resilience level
    resilience_levels <- c("1_Low", "3_High")
    correlation_results_BL <- data.frame(Trauma = character(), Timepoint = character(), Resilience_Level = character(),
                                         Correlation = numeric(), Subject_Count = integer(), stringsAsFactors = FALSE)
    
    for (resilience_level in resilience_levels) {
      resilient_group <- resilience_test
      
      # Residuals for mental health after covariate adjustment
      model_m <- lm(PHQ ~ Age + Sex + Ethnic + BMI + Education, data = resilient_group)
      resilient_group$PHQ_reg <- residuals(model_m)
      
      # Residuals for adverse factor after covariate adjustment (corrected from original code)
      model_T <- lm(as.formula(paste(t, "~ Age + Sex + Ethnic + BMI + Education")), data = resilient_group)
      resilient_group$Trauma_reg <- residuals(model_T)
      
      resilient_group <- resilient_group[resilient_group$selfresilience == resilience_level, ]
      
      # Compute correlation between residuals
      correlation_model <- cor.test(resilient_group$PHQ_reg, resilient_group$Trauma_reg)
      correlation <- correlation_model$estimate
      subject_count <- nrow(resilient_group)
      
      correlation_results_BL <- rbind(correlation_results_BL, 
                                      data.frame(Trauma = t, Category = trauma_var, Timepoint = "BL",
                                                 Resilience_Level = resilience_level, Correlation = correlation, 
                                                 Subject_Count = subject_count))
    }
    
    # Compare correlations between groups using Fisher's Z-test
    comparison_results <- data.frame(Trauma = character(), Timepoint = character(), Group1 = character(), 
                                     Group2 = character(), Z = numeric(), p_value = numeric(), stringsAsFactors = FALSE)
    
    for (i in 1:(length(resilience_levels) - 1)) {
      for (j in (i + 1):length(resilience_levels)) {
        group1 <- correlation_results_BL[correlation_results_BL$Resilience_Level == resilience_levels[i], ]
        group2 <- correlation_results_BL[correlation_results_BL$Resilience_Level == resilience_levels[j], ]
        
        x <- group1$Correlation
        n.x <- group1$Subject_Count
        y <- group2$Correlation
        n.y <- group2$Subject_Count
        
        comparison <- cocor.indep.groups(x, y, n.x, n.y, alternative = "two.sided")
        cocor_result <- get.cocor.results(comparison)
        Z <- cocor_result$fisher1925$statistic
        p_value <- cocor_result$fisher1925$p.value
        
        comparison_results <- rbind(comparison_results, 
                                    data.frame(Trauma = t, Timepoint = "BL", Group1 = resilience_levels[i], 
                                               Group2 = resilience_levels[j], Z = Z, p_value = p_value))
      }
    }
    
    # Append to overall results
    correlation_results_BL_all <- rbind(correlation_results_BL_all, correlation_results_BL)
    comparison_results_BL_all <- rbind(comparison_results_BL_all, comparison_results)
  }
}

# FDR correction for adverse factor effects
p_values <- trauma_effect_table_BL$p.value
adjusted_p_values <- p.adjust(p_values, method = "BH")
trauma_effect_table_BL$trauma_adjusted_p <- adjusted_p_values
significant_count <- sum(trauma_effect_table_BL$trauma_adjusted_p < 0.05 & trauma_effect_table_BL$statistic > 0)
cat("Number of trauma effects p.value < 0.05 (BL):", significant_count, "\n")

# FDR correction for correlation comparisons
p_values <- comparison_results_BL_all$p_value
adjusted_p_values <- p.adjust(p_values, method = "BH")
comparison_results_BL_all$z_adjusted_p <- adjusted_p_values
significant_count <- sum(comparison_results_BL_all$z_adjusted_p < 0.05 & comparison_results_BL_all$Z > 0)
cat("Number of cocor p.value < 0.05 (BL):", significant_count, "\n")

# Section 4: Visualization for Baseline (BL)
# Prepare data for plotting
cor_plot_dat_BL <- merge(correlation_results_BL_all, comparison_results_BL_all[, c("Trauma", "Z")], by = "Trauma", all.x = TRUE)
z_plot_dat_BL <- merge(comparison_results_BL_all, correlation_results_BL_all[, c("Trauma", "Category")], by = "Trauma", all.x = TRUE)
z_plot_dat_BL <- unique(z_plot_dat_BL)

# Rename adverse factors for better readability
rename_map <- c(
  "HH_Num_Vehicle_BL" = "Household vehicle count (R)",
  "HH_Income_BL" = "Household income (R)",
  "HH_Own_Rent_BL" = "Rent accommodation lived in",
  "Social_Freq_Visits_BL" = "Lack of social visit",
  "Social_Able_Confide_BL" = "Lack of confiding social relationship",
  "Num_People_Living_BL" = "Number of people living in (R)",
  "Loneliness_BL" = "Loneliness",
  "Breastfed_Baby_BL" = "Not breastfed as a baby",
  "Comp_Body_Size_Age_10_BL" = "Comparative body size at age 10 (R)",
  "Comp_Height_Size_Age_10_BL" = "Comparative height at age 10 (R)",
  "Maternal_Smoking_Birth_BL" = "Maternal smoking around birth",
  "Felt_Loved_As_Child_FU1" = "Childhood emotional neglect",
  "Phys_Abused_As_Child_FU1" = "Childhood physical abuse",
  "Felt_Hated_As_Child_FU1" = "Childhood emotional abuse",
  "Sex_Molested_As_Child_FU1" = "Childhood sexual molestation",
  "Someone_Take_To_Doctor_As_Child_FU1" = "Childhood physical neglect",
  "NO2_Air_Pollution_2010_BL" = "NO2 Air Pollution in 2010",
  "NOx_Air_Pollution_2010_BL" = "NOx Air Pollution in 2010",
  "PM10_Air_Pollution_2010_BL" = "PM10 Air Pollution in 2010",
  "PM2.5_Air_Pollution_2010_BL" = "PM2.5 Air Pollution in 2010",
  "PM2.5_Absorbance_2010_BL" = "PM2.5 Absorbance in 2010",
  "PM2.5_10um_Air_Pollution_2010_BL" = "PM2.5-10um Air Pollution in 2010",
  "Traffic_Intensity_Nearest_Road_BL" = "AP: Traffic intensity on the nearest road",
  "Inv_Dist_Nearest_Road_BL" = "AP: Inverse distance to the nearest road",
  "Traffic_Intensity_Nearest_Major_Road_BL" = "AP: Traffic intensity on the nearest major road",
  "Inv_Dist_Nearest_Major_Road_BL" = "AP: Inverse distance to the nearest major road",
  "Total_Traffic_Load_Major_Roads_BL" = "AP: Traffic load on major roads",
  "Close_To_Major_Road_BL" = "AP: Close to major road",
  "Sum_Major_Road_Length_100m_BL" = "AP: Sum of road length within 100m",
  "NO2_Air_Pollution_2005_BL" = "NO2 Air Pollution in 2005",
  "NO2_Air_Pollution_2006_BL" = "NO2 Air Pollution in 2006",
  "NO2_Air_Pollution_2007_BL" = "NO2 Air Pollution in 2007",
  "PM10_Air_Pollution_2007_BL" = "PM10 Air Pollution in 2007",
  "Avg_Daytime_Sound_Level_BL" = "Average daytime sound level of noise pollution",
  "Avg_Evening_Sound_Level_BL" = "Average evening sound level of noise pollution",
  "Avg_Nighttime_Sound_Level_BL" = "Average night-time sound level of noise pollution",
  "Greenspace_Percentage_1000m_BL" = "Greenspace ratio buffer 1000m (R)",
  "Domestic_Garden_Percentage_1000m_BL" = "Domestic garden ratio buffer 1000m (R)",
  "Water_Percentage_1000m_BL" = "Water ratio buffer 1000m (R)",
  "Greenspace_Percentage_300m_BL" = "Greenspace ratio buffer 300m (R)",
  "Domestic_Garden_Percentage_300m_BL" = "Domestic garden ratio buffer 300m (R)",
  "Water_Percentage_300m_BL" = "Water ratio buffer 300m (R)",
  "Natural_Env_Percentage_1000m_BL" = "Natural Env. ratio buffer 1000m (R)",
  "Natural_Env_Percentage_300m_BL" = "Natural Env. ratio buffer 300m (R)",
  "Distance_To_Coast_Euclidean_BL" = "Distance to coast"
)

z_plot_dat_BL$Trauma <- rename_map[z_plot_dat_BL$Trauma]
cor_plot_dat_BL$Trauma <- rename_map[cor_plot_dat_BL$Trauma]

# Order data for plotting
cor_plot_dat_BL <- cor_plot_dat_BL %>%
  arrange(factor(Resilience_Level, levels = c("1_Low", "3_High")), 
          factor(Category, levels = c("Eco_Social", "Early_risk", "Enviroment")), desc(Correlation))
cor_plot_dat_BL$Trauma <- factor(cor_plot_dat_BL$Trauma, levels = unique(cor_plot_dat_BL$Trauma), ordered = TRUE)

z_trauma_order <- cor_plot_dat_BL %>% filter(Resilience_Level == "1_Low")
z_plot_dat_BL <- z_plot_dat_BL %>% arrange(match(Trauma, z_trauma_order$Trauma))
z_plot_dat_BL$Trauma <- factor(z_plot_dat_BL$Trauma, levels = unique(z_plot_dat_BL$Trauma), ordered = TRUE)

# Create wide format for correlations
wide_cor_plot_dat_BL <- correlation_results_BL_all[, -6] %>%  # Exclude Subject_Count if not needed
  pivot_wider(names_from = Resilience_Level, values_from = Correlation) %>%
  arrange(factor(Category, levels = c("Eco_Social", "Early_risk", "Enviroment")), desc(`1_Low`))
wide_cor_plot_dat_BL$Trauma <- factor(wide_cor_plot_dat_BL$Trauma, levels = unique(wide_cor_plot_dat_BL$Trauma), ordered = TRUE)

# Set category factors
cor_plot_dat_BL$Category <- factor(cor_plot_dat_BL$Category, levels = c("Eco_Social", "Early_risk", "Enviroment"), ordered = TRUE)
z_plot_dat_BL$Category <- factor(z_plot_dat_BL$Category, levels = c("Eco_Social", "Early_risk", "Enviroment"), ordered = TRUE)

# Dumbbell plot for correlations
dum_plot_BL <- ggplot(cor_plot_dat_BL, aes(x = Trauma, y = Correlation)) +
  geom_line(aes(group = Trauma), size = 1.5) + 
  geom_point(aes(color = Resilience_Level), size = 5) +
  scale_color_manual(values = c("#ff9900", "#146eb4"), labels = c("Low Resilience", "High Resilience")) +  
  labs(x = "", y = "Correlation Coefficient", title = "Baseline (BL)") +
  guides(color = FALSE) +
  theme(panel.spacing = unit(0.5, "lines"),
        panel.border = element_blank(),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_blank(),
        panel.background = element_rect(color = "white", fill = NA),
        panel.grid.major.x = element_line(color = "gray", linewidth = 0.5, 
                                          linetype = "dashed"))  # Simplified; original ifelse may not work directly

# Bar plot for Z-values
z_plot_BL <- ggplot(z_plot_dat_BL, aes(Trauma, Z)) +
  geom_col(aes(fill = factor(Z > 2.12 | Z < -2.12)), width = 0.7) +
  scale_fill_manual(values = c("#D3D3D3", "#A9A9A9"), guide = FALSE) +
  geom_hline(yintercept = 2.12, linetype = "dashed", color = "orange", size = 0.5) +
  geom_hline(yintercept = -2.12, linetype = "dashed", color = "orange", size = 0.5) +
  labs(x = "Adverse Factor", y = "Z value") + 
  guides(color = FALSE) +
  theme(panel.spacing = unit(0.5, "lines"),
        panel.border = element_blank(),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
        panel.background = element_rect(color = "white", fill = NA),
        panel.grid.major.x = element_line(color = "gray", linewidth = 0.5, linetype = "dashed"),
        panel.grid.minor.x = element_line(color = "gray", linetype = "dashed", linewidth = 0.2))

# Combine plots
combined_plot_BL <- dum_plot_BL / z_plot_BL + plot_layout(heights = c(1, 0.3))
print(combined_plot_BL)  # Export as PDF: 14in x 9in

# Section 5: Analysis for Follow-up 1 (FU1) Timepoint
# Copy data and compute FU1 symptoms
resilience_group_R_FU1 <- copy(resilience_group_R)
resilience_group_R_FU1$FU1_symptoms <- resilience_group_R_FU1$PHQ9_Severity_FU1 / 28 + 
  resilience_group_R_FU1$General_Anxiety_Disorder_Severity_FU1 / 21

# Initialize result tables for FU1
result_table_FU1 <- data.frame()
trauma_effect_table_FU1 <- data.frame()
correlation_results_FU1_all <- data.frame(Trauma = character(), Category = character(), Timepoint = character(),
                                          Resilience_Level = character(), Correlation = numeric(), 
                                          Subject_Count = integer(), stringsAsFactors = FALSE)
comparison_results_FU1_all <- data.frame(Trauma = character(), Timepoint = character(), Group1 = character(), 
                                         Group2 = character(), Z = numeric(), p_value = numeric(), 
                                         stringsAsFactors = FALSE)

# Loop similar to BL, but use FU1_symptoms
for (trauma_var in names(trauma_variables_list)) {
  trauma <- trauma_variables_list[[trauma_var]]
  
  for (t in trauma) {
    resilience_test <- resilience_group_R_FU1 %>% 
      transmute(
        eid,
        Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")),
        Age = age_BL,
        Ethnic = factor(Ethnic_group, levels = c("0", "1", "2", "3"), labels = c("1_White", "2_Asian", "3_Black", "4_Other")),
        Education = Education_year,
        BMI = BMI_BL,
        selfresilience = factor(self_resilience, levels = c(0, 1, 2), labels = c("1_Low", "2_Medium", "3_High")),
        PHQ = FU1_symptoms,  # Use FU1 symptoms
        !!t := !!sym(t)
      )
    resilience_test <- na.omit(resilience_test)
    
    # Fit linear model
    lme1 <- lm(as.formula(paste("PHQ ~ ", t, "+ Age + Sex + Ethnic + BMI + Education")), data = resilience_test)
    results <- tidy(lme1)
    results$Trauma_Variable <- t
    result_table_FU1 <- bind_rows(result_table_FU1, results)
    
    filtered_results <- results %>% filter(term == t)
    filtered_results$Trauma_Variable <- t
    trauma_effect_table_FU1 <- bind_rows(trauma_effect_table_FU1, filtered_results)
    
    # Correlation analysis
    resilience_levels <- c("1_Low", "3_High")
    correlation_results_FU1 <- data.frame(Trauma = character(), Timepoint = character(), Resilience_Level = character(),
                                          Correlation = numeric(), Subject_Count = integer(), stringsAsFactors = FALSE)
    
    for (resilience_level in resilience_levels) {
      resilient_group <- resilience_test
      
      model_m <- lm(PHQ ~ Age + Sex + Ethnic + BMI + Education, data = resilient_group)
      resilient_group$PHQ_reg <- residuals(model_m)
      
      # Add residuals for adverse factor (added for consistency)
      model_T <- lm(as.formula(paste(t, "~ Age + Sex + Ethnic + BMI + Education")), data = resilient_group)
      resilient_group$Trauma_reg <- residuals(model_T)
      
      resilient_group <- resilient_group[resilient_group$selfresilience == resilience_level, ]
      
      correlation_model <- cor.test(resilient_group$PHQ_reg, resilient_group$Trauma_reg)
      correlation <- correlation_model$estimate
      subject_count <- nrow(resilient_group)
      
      correlation_results_FU1 <- rbind(correlation_results_FU1, 
                                       data.frame(Trauma = t, Category = trauma_var, Timepoint = "FU1",
                                                  Resilience_Level = resilience_level, Correlation = correlation, 
                                                  Subject_Count = subject_count))
    }
    
    # Comparison with Fisher's Z-test
    comparison_results <- data.frame(Trauma = character(), Timepoint = character(), Group1 = character(), 
                                     Group2 = character(), Z = numeric(), p_value = numeric(), stringsAsFactors = FALSE)
    
    for (i in 1:(length(resilience_levels) - 1)) {
      for (j in (i + 1):length(resilience_levels)) {
        group1 <- correlation_results_FU1[correlation_results_FU1$Resilience_Level == resilience_levels[i], ]
        group2 <- correlation_results_FU1[correlation_results_FU1$Resilience_Level == resilience_levels[j], ]
        
        x <- group1$Correlation
        n.x <- group1$Subject_Count
        y <- group2$Correlation
        n.y <- group2$Subject_Count
        
        comparison <- cocor.indep.groups(x, y, n.x, n.y, alternative = "two.sided")
        cocor_result <- get.cocor.results(comparison)
        Z <- cocor_result$fisher1925$statistic
        p_value <- cocor_result$fisher1925$p.value
        
        comparison_results <- rbind(comparison_results, 
                                    data.frame(Trauma = t, Timepoint = "FU1", Group1 = resilience_levels[i], 
                                               Group2 = resilience_levels[j], Z = Z, p_value = p_value))
      }
    }
    
    correlation_results_FU1_all <- rbind(correlation_results_FU1_all, correlation_results_FU1)
    comparison_results_FU1_all <- rbind(comparison_results_FU1_all, comparison_results)
  }
}

# FDR corrections for FU1
p_values <- trauma_effect_table_FU1$p.value
adjusted_p_values <- p.adjust(p_values, method = "BH")
trauma_effect_table_FU1$trauma_adjusted_p <- adjusted_p_values
significant_count <- sum(trauma_effect_table_FU1$trauma_adjusted_p < 0.05 & trauma_effect_table_FU1$statistic > 0)
cat("Number of trauma effects p.value < 0.05 (FU1):", significant_count, "\n")

p_values <- comparison_results_FU1_all$p_value
adjusted_p_values <- p.adjust(p_values, method = "BH")
comparison_results_FU1_all$z_adjusted_p <- adjusted_p_values
significant_count <- sum(comparison_results_FU1_all$z_adjusted_p < 0.05 & comparison_results_FU1_all$Z > 0)
cat("Number of cocor p.value < 0.05 (FU1):", significant_count, "\n")

# Visualization for FU1 (similar to BL, adjust thresholds and title)
cor_plot_dat_FU1 <- merge(correlation_results_FU1_all, comparison_results_FU1_all[, c("Trauma", "Z")], by = "Trauma", all.x = TRUE)
z_plot_dat_FU1 <- merge(comparison_results_FU1_all, correlation_results_FU1_all[, c("Trauma", "Category")], by = "Trauma", all.x = TRUE)
z_plot_dat_FU1 <- unique(z_plot_dat_FU1)

z_plot_dat_FU1$Trauma <- rename_map[z_plot_dat_FU1$Trauma]
cor_plot_dat_FU1$Trauma <- rename_map[cor_plot_dat_FU1$Trauma]

cor_plot_dat_FU1 <- cor_plot_dat_FU1 %>%
  arrange(factor(Resilience_Level, levels = c("1_Low", "3_High")), 
          factor(Category, levels = c("Eco_Social", "Early_risk", "Enviroment")), desc(Correlation))
cor_plot_dat_FU1$Trauma <- factor(cor_plot_dat_FU1$Trauma, levels = unique(cor_plot_dat_FU1$Trauma), ordered = TRUE)

z_trauma_order <- cor_plot_dat_FU1 %>% filter(Resilience_Level == "1_Low")
z_plot_dat_FU1 <- z_plot_dat_FU1 %>% arrange(match(Trauma, z_trauma_order$Trauma))
z_plot_dat_FU1$Trauma <- factor(z_plot_dat_FU1$Trauma, levels = unique(z_plot_dat_FU1$Trauma), ordered = TRUE)

wide_cor_plot_dat_FU1 <- correlation_results_FU1_all[, -6] %>%
  pivot_wider(names_from = Resilience_Level, values_from = Correlation) %>%
  arrange(factor(Category, levels = c("Eco_Social", "Early_risk", "Enviroment")), desc(`1_Low`))
wide_cor_plot_dat_FU1$Trauma <- factor(wide_cor_plot_dat_FU1$Trauma, levels = unique(wide_cor_plot_dat_FU1$Trauma), ordered = TRUE)

cor_plot_dat_FU1$Category <- factor(cor_plot_dat_FU1$Category, levels = c("Eco_Social", "Early_risk", "Enviroment"), ordered = TRUE)
z_plot_dat_FU1$Category <- factor(z_plot_dat_FU1$Category, levels = c("Eco_Social", "Early_risk", "Enviroment"), ordered = TRUE)

dum_plot_FU1 <- ggplot(cor_plot_dat_FU1, aes(x = Trauma, y = Correlation)) +
  geom_line(aes(group = Trauma), size = 1.5) + 
  geom_point(aes(color = Resilience_Level), size = 5) +
  scale_color_manual(values = c("#ff9900", "#146eb4"), labels = c("Low Resilience", "High Resilience")) +  
  labs(x = "", y = "Correlation Coefficient", title = "Follow-up 1 (FU1)") +
  guides(color = FALSE) +
  theme(panel.spacing = unit(0.5, "lines"),
        panel.border = element_blank(),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_blank(),
        panel.background = element_rect(color = "white", fill = NA),
        panel.grid.major.x = element_line(color = "gray", linewidth = 0.5, linetype = "dashed"))

z_plot_FU1 <- ggplot(z_plot_dat_FU1, aes(Trauma, Z)) +
  geom_col(aes(fill = factor(Z > 2.28 | Z < -2.28)), width = 0.7) +
  scale_fill_manual(values = c("#D3D3D3", "#A9A9A9"), guide = FALSE) +
  geom_hline(yintercept = 2.28, linetype = "dashed", color = "orange", size = 0.5) +
  geom_hline(yintercept = -2.28, linetype = "dashed", color = "orange", size = 0.5) +
  labs(x = "Adverse Factor", y = "Z value") + 
  guides(color = FALSE) +
  theme(panel.spacing = unit(0.5, "lines"),
        panel.border = element_blank(),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
        panel.background = element_rect(color = "white", fill = NA),
        panel.grid.major.x = element_line(color = "gray", linewidth = 0.5, linetype = "dashed"),
        panel.grid.minor.x = element_line(color = "gray", linetype = "dashed", linewidth = 0.2))

combined_plot_FU1 <- dum_plot_FU1 / z_plot_FU1 + plot_layout(heights = c(1, 0.3))
print(combined_plot_FU1)  # Export as PDF: 14in x 9in

# Section 6: Analysis for Follow-up 2 (FU2) Timepoint
# Copy data and compute FU2 symptoms
resilience_group_R_FU2 <- copy(resilience_group_R)
resilience_group_R_FU2$FU2_symptoms <- resilience_group_R_FU2$`PHQ-9_FU2` / 28 + 
  resilience_group_R_FU2$General_Anxiety_Disorder_Severity_FU2 / 21

# Initialize result tables for FU2
result_table_FU2 <- data.frame()
trauma_effect_table_FU2 <- data.frame()
correlation_results_FU2_all <- data.frame(Trauma = character(), Category = character(), Timepoint = character(),
                                          Resilience_Level = character(), Correlation = numeric(), 
                                          Subject_Count = integer(), stringsAsFactors = FALSE)
comparison_results_FU2_all <- data.frame(Trauma = character(), Timepoint = character(), Group1 = character(), 
                                         Group2 = character(), Z = numeric(), p_value = numeric(), 
                                         stringsAsFactors = FALSE)

# Loop similar to BL, but use FU2_symptoms
for (trauma_var in names(trauma_variables_list)) {
  trauma <- trauma_variables_list[[trauma_var]]
  
  for (t in trauma) {
    resilience_test <- resilience_group_R_FU2 %>% 
      transmute(
        eid,
        Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")),
        Age = age_BL,
        Ethnic = factor(Ethnic_group, levels = c("0", "1", "2", "3"), labels = c("1_White", "2_Asian", "3_Black", "4_Other")),
        Education = Education_year,
        BMI = BMI_BL,
        selfresilience = factor(self_resilience, levels = c(0, 1, 2), labels = c("1_Low", "2_Medium", "3_High")),
        PHQ = FU2_symptoms,  # Use FU2 symptoms
        !!t := !!sym(t)
      )
    resilience_test <- na.omit(resilience_test)
    
    # Fit linear model
    lme1 <- lm(as.formula(paste("PHQ ~ ", t, "+ Age + Sex + Ethnic + BMI + Education")), data = resilience_test)
    results <- tidy(lme1)
    results$Trauma_Variable <- t
    result_table_FU2 <- bind_rows(result_table_FU2, results)
    
    filtered_results <- results %>% filter(term == t)
    filtered_results$Trauma_Variable <- t
    trauma_effect_table_FU2 <- bind_rows(trauma_effect_table_FU2, filtered_results)
    
    # Correlation analysis
    resilience_levels <- c("1_Low", "3_High")
    correlation_results_FU2 <- data.frame(Trauma = character(), Timepoint = character(), Resilience_Level = character(),
                                          Correlation = numeric(), Subject_Count = integer(), stringsAsFactors = FALSE)
    
    for (resilience_level in resilience_levels) {
      resilient_group <- resilience_test
      
      model_m <- lm(PHQ ~ Age + Sex + Ethnic + BMI + Education, data = resilient_group)
      resilient_group$PHQ_reg <- residuals(model_m)
      
      # Add residuals for adverse factor (added for consistency)
      model_T <- lm(as.formula(paste(t, "~ Age + Sex + Ethnic + BMI + Education")), data = resilient_group)
      resilient_group$Trauma_reg <- residuals(model_T)
      
      resilient_group <- resilient_group[resilient_group$selfresilience == resilience_level, ]
      
      correlation_model <- cor.test(resilient_group$PHQ_reg, resilient_group$Trauma_reg)
      correlation <- correlation_model$estimate
      subject_count <- nrow(resilient_group)
      
      correlation_results_FU2 <- rbind(correlation_results_FU2, 
                                       data.frame(Trauma = t, Category = trauma_var, Timepoint = "FU2",
                                                  Resilience_Level = resilience_level, Correlation = correlation, 
                                                  Subject_Count = subject_count))
    }
    
    # Comparison with Fisher's Z-test
    comparison_results <- data.frame(Trauma = character(), Timepoint = character(), Group1 = character(), 
                                     Group2 = character(), Z = numeric(), p_value = numeric(), stringsAsFactors = FALSE)
    
    for (i in 1:(length(resilience_levels) - 1)) {
      for (j in (i + 1):length(resilience_levels)) {
        group1 <- correlation_results_FU2[correlation_results_FU2$Resilience_Level == resilience_levels[i], ]
        group2 <- correlation_results_FU2[correlation_results_FU2$Resilience_Level == resilience_levels[j], ]
        
        x <- group1$Correlation
        n.x <- group1$Subject_Count
        y <- group2$Correlation
        n.y <- group2$Subject_Count
        
        comparison <- cocor.indep.groups(x, y, n.x, n.y, alternative = "two.sided")
        cocor_result <- get.cocor.results(comparison)
        Z <- cocor_result$fisher1925$statistic
        p_value <- cocor_result$fisher1925$p.value
        
        comparison_results <- rbind(comparison_results, 
                                    data.frame(Trauma = t, Timepoint = "FU2", Group1 = resilience_levels[i], 
                                               Group2 = resilience_levels[j], Z = Z, p_value = p_value))
      }
    }
    
    correlation_results_FU2_all <- rbind(correlation_results_FU2_all, correlation_results_FU2)
    comparison_results_FU2_all <- rbind(comparison_results_FU2_all, comparison_results)
  }
}

# FDR corrections for FU2
p_values <- trauma_effect_table_FU2$p.value
adjusted_p_values <- p.adjust(p_values, method = "BH")
trauma_effect_table_FU2$trauma_adjusted_p <- adjusted_p_values
significant_count <- sum(trauma_effect_table_FU2$trauma_adjusted_p < 0.05 & trauma_effect_table_FU2$statistic > 0)
cat("Number of trauma effects p.value < 0.05 (FU2):", significant_count, "\n")

p_values <- comparison_results_FU2_all$p_value
adjusted_p_values <- p.adjust(p_values, method = "BH")
comparison_results_FU2_all$z_adjusted_p <- adjusted_p_values
significant_count <- sum(comparison_results_FU2_all$z_adjusted_p < 0.05 & comparison_results_FU2_all$Z > 0)
cat("Number of cocor p.value < 0.05 (FU2):", significant_count, "\n")

# Visualization for FU2 (similar to BL, adjust thresholds and title)
cor_plot_dat_FU2 <- merge(correlation_results_FU2_all, comparison_results_FU2_all[, c("Trauma", "Z")], by = "Trauma", all.x = TRUE)
z_plot_dat_FU2 <- merge(comparison_results_FU2_all, correlation_results_FU2_all[, c("Trauma", "Category")], by = "Trauma", all.x = TRUE)
z_plot_dat_FU2 <- unique(z_plot_dat_FU2)

z_plot_dat_FU2$Trauma <- rename_map[z_plot_dat_FU2$Trauma]
cor_plot_dat_FU2$Trauma <- rename_map[cor_plot_dat_FU2$Trauma]

cor_plot_dat_FU2 <- cor_plot_dat_FU2 %>%
  arrange(factor(Resilience_Level, levels = c("1_Low", "3_High")), 
          factor(Category, levels = c("Eco_Social", "Early_risk", "Enviroment")), desc(Correlation))
cor_plot_dat_FU2$Trauma <- factor(cor_plot_dat_FU2$Trauma, levels = unique(cor_plot_dat_FU2$Trauma), ordered = TRUE)

z_trauma_order <- cor_plot_dat_FU2 %>% filter(Resilience_Level == "1_Low")
z_plot_dat_FU2 <- z_plot_dat_FU2 %>% arrange(match(Trauma, z_trauma_order$Trauma))
z_plot_dat_FU2$Trauma <- factor(z_plot_dat_FU2$Trauma, levels = unique(z_plot_dat_FU2$Trauma), ordered = TRUE)

wide_cor_plot_dat_FU2 <- correlation_results_FU2_all[, -6] %>%
  pivot_wider(names_from = Resilience_Level, values_from = Correlation) %>%
  arrange(factor(Category, levels = c("Eco_Social", "Early_risk", "Enviroment")), desc(`1_Low`))
wide_cor_plot_dat_FU2$Trauma <- factor(wide_cor_plot_dat_FU2$Trauma, levels = unique(wide_cor_plot_dat_FU2$Trauma), ordered = TRUE)

cor_plot_dat_FU2$Category <- factor(cor_plot_dat_FU2$Category, levels = c("Eco_Social", "Early_risk", "Enviroment"), ordered = TRUE)
z_plot_dat_FU2$Category <- factor(z_plot_dat_FU2$Category, levels = c("Eco_Social", "Early_risk", "Enviroment"), ordered = TRUE)

dum_plot_FU2 <- ggplot(cor_plot_dat_FU2, aes(x = Trauma, y = Correlation)) +
  geom_line(aes(group = Trauma), size = 1.5) + 
  geom_point(aes(color = Resilience_Level), size = 5) +
  scale_color_manual(values = c("#ff9900", "#146eb4"), labels = c("Low Resilience", "High Resilience")) +  
  labs(x = "", y = "Correlation Coefficient", title = "Follow-up 2 (FU2)") +
  guides(color = FALSE) +
  theme(panel.spacing = unit(0.5, "lines"),
        panel.border = element_blank(),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_blank(),
        panel.background = element_rect(color = "white", fill = NA),
        panel.grid.major.x = element_line(color = "gray", linewidth = 0.5, linetype = "dashed"))

z_plot_FU2 <- ggplot(z_plot_dat_FU2, aes(Trauma, Z)) +
  geom_col(aes(fill = factor(Z > 2.2 | Z < -2.2)), width = 0.7) +
  scale_fill_manual(values = c("#D3D3D3", "#A9A9A9"), guide = FALSE) +
  geom_hline(yintercept = 2.2, linetype = "dashed", color = "orange", size = 0.5) +
  geom_hline(yintercept = -2.2, linetype = "dashed", color = "orange", size = 0.5) +
  labs(x = "Adverse Factor", y = "Z value") + 
  guides(color = FALSE) +
  theme(panel.spacing = unit(0.5, "lines"),
        panel.border = element_blank(),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
        panel.background = element_rect(color = "white", fill = NA),
        panel.grid.major.x = element_line(color = "gray", linewidth = 0.5, linetype = "dashed"),
        panel.grid.minor.x = element_line(color = "gray", linetype = "dashed", linewidth = 0.2))

combined_plot_FU2 <- dum_plot_FU2 / z_plot_FU2 + plot_layout(heights = c(1, 0.3))
print(combined_plot_FU2)  # Export as PDF: 14in x 9in or 14in x 6in if incomplete

# End of Script
# For reproducibility, run in RStudio. Consider using renv for package versions.
# To execute: source("this_script.R")