# ==============================================================================
# Script: A2_1_Genernal_Adverse_Factors_update_V2.R
# Project: Lifespan Resilience Modeling (STM Submission)
# Purpose: Evaluate multivariable associations between broader environmental, socioeconomic, and early-life adverse factors and mental health across resilience groups.
# ==============================================================================

# ------------------------------------------------------------------------------
# Section 1: Load Required Packages
# ------------------------------------------------------------------------------
library(readr)
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(ggpubr)
library(patchwork)
library(bruceR)
library(autoReg)
library(broom)
library(cocor)
library(ggtext)
library(showtext)

showtext_auto() 

# ------------------------------------------------------------------------------
# Section 2: Global Data Import & Preprocessing (Run Once)
# ------------------------------------------------------------------------------
cat("\n[INFO] Initializing global data and performing feature engineering...\n")

# Load raw data
resilience_raw <- readr::read_csv("data/UKB_dat_for_analysis_1226.csv")[, -1]

# Variables to negate (reverse direction)
columns_to_negate <- c(
  "Felt_Loved_As_Child_FU1", "HH_Income_BL", "Social_Freq_Visits_BL", "HH_Num_Vehicle_BL", 
  "Someone_Take_To_Doctor_As_Child_FU1", "Natural_Env_Percentage_1000m_BL", 
  "Greenspace_Percentage_1000m_BL", "Natural_Env_Percentage_300m_BL", "Greenspace_Percentage_300m_BL",
  "Breastfed_Baby_BL", "Num_People_Living_BL", "Comp_Body_Size_Age_10_BL",
  "Water_Percentage_300m_BL", "Domestic_Garden_Percentage_300m_BL", "Distance_To_Coast_Euclidean_BL", 
  "Comp_Height_Size_Age_10_BL", "Water_Percentage_1000m_BL"
)

# Global preprocessing pipeline
df_global <- resilience_raw %>%
  dplyr::mutate(
    # 1. Reverse scoring for specific adverse factors
    dplyr::across(dplyr::all_of(columns_to_negate), ~ - .x),
    
    # 2. Continuous and Categorical Resilience
    self_resilience_cont = self_resilience,
    
    # 3. Standardize Covariates
    Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")),
    Age = age_BL,
    BMI = BMI_BL,
    Education = Education_year,
    site = factor(site),
    
    # 4. Safe Ethnic Group Recoding (Prevent silent NA generation)
    Ethnic_group_tmp = dplyr::case_when(
      Ethnic_group == 1 ~ 0,
      Ethnic_group == 2 ~ 3, 
      Ethnic_group == 3 ~ 1, 
      Ethnic_group == 4 ~ 2, 
      Ethnic_group == 5 ~ 1, 
      Ethnic_group == 6 ~ 3, 
      TRUE ~ Ethnic_group - 1
    ),
    Ethnic = factor(Ethnic_group_tmp, levels = c(0, 1, 2, 3), labels = c("1_White", "2_Asian", "3_Black", "4_Other")),
    
    # 5. Pre-calculate FU1 and FU2 symptoms globally
    PHQ_BL = Depressive_Symptoms_PHQ4_BL,
    FU1_symptoms = (PHQ9_Severity_FU1 - 1) / 27 + General_Anxiety_Disorder_Severity_FU1 / 21,
    FU2_symptoms = `PHQ-9_FU2` / 27 + General_Anxiety_Disorder_Severity_FU2 / 21
  )

# Calculate quantiles for Categorical Resilience
quantiles <- stats::quantile(df_global$self_resilience_cont, probs = c(1/3, 2/3), na.rm = TRUE)

df_global <- df_global %>%
  dplyr::mutate(
    selfresilience_cat = dplyr::case_when(
      self_resilience_cont <= quantiles[1] ~ "1_Low",
      self_resilience_cont > quantiles[1] & self_resilience_cont < quantiles[2] ~ "2_Medium",
      self_resilience_cont >= quantiles[2] ~ "3_High"
    ),
    selfresilience_cat = factor(selfresilience_cat, levels = c("1_Low", "2_Medium", "3_High"))
  )

# Define adverse factor categories
trauma_variables_list <- list(
  Eco_Social = c("HH_Num_Vehicle_BL", "HH_Income_BL", "HH_Own_Rent_BL", "Social_Freq_Visits_BL", "Social_Able_Confide_BL", "Num_People_Living_BL", "Loneliness_BL"),
  Early_risk = c("Breastfed_Baby_BL", "Comp_Body_Size_Age_10_BL", "Comp_Height_Size_Age_10_BL", "Maternal_Smoking_Birth_BL", "Felt_Loved_As_Child_FU1", "Phys_Abused_As_Child_FU1", "Felt_Hated_As_Child_FU1", "Sex_Molested_As_Child_FU1", "Someone_Take_To_Doctor_As_Child_FU1"),
  Enviroment = c("NO2_Air_Pollution_2010_BL", "NOx_Air_Pollution_2010_BL", "PM10_Air_Pollution_2010_BL", "PM2.5_Air_Pollution_2010_BL", "PM2.5_Absorbance_2010_BL", "PM2.5_10um_Air_Pollution_2010_BL", "Traffic_Intensity_Nearest_Road_BL", "Inv_Dist_Nearest_Road_BL", "Traffic_Intensity_Nearest_Major_Road_BL", "Inv_Dist_Nearest_Major_Road_BL", "Total_Traffic_Load_Major_Roads_BL", "Close_To_Major_Road_BL", "Sum_Major_Road_Length_100m_BL", "NO2_Air_Pollution_2005_BL", "NO2_Air_Pollution_2006_BL", "NO2_Air_Pollution_2007_BL", "PM10_Air_Pollution_2007_BL", "Avg_Daytime_Sound_Level_BL", "Avg_Evening_Sound_Level_BL", "Avg_Nighttime_Sound_Level_BL", "Greenspace_Percentage_1000m_BL", "Domestic_Garden_Percentage_1000m_BL", "Water_Percentage_1000m_BL", "Greenspace_Percentage_300m_BL", "Domestic_Garden_Percentage_300m_BL", "Water_Percentage_300m_BL", "Natural_Env_Percentage_1000m_BL", "Natural_Env_Percentage_300m_BL", "Distance_To_Coast_Euclidean_BL")
)

# Common renaming map for plots
rename_map <- c(
  "HH_Num_Vehicle_BL" = "Household vehicle count (R)", "HH_Income_BL" = "Household income (R)", "HH_Own_Rent_BL" = "Rent accommodation lived in", "Social_Freq_Visits_BL" = "Lack of social visit", "Social_Able_Confide_BL" = "Lack of confiding social relationship", "Num_People_Living_BL" = "Number of people living in (R)", "Loneliness_BL" = "Loneliness", "Breastfed_Baby_BL" = "Not breastfed as a baby", "Comp_Body_Size_Age_10_BL" = "Comparative body size at age 10 (R)", "Comp_Height_Size_Age_10_BL" = "Comparative height at age 10 (R)", "Maternal_Smoking_Birth_BL" = "Maternal smoking around birth", "Felt_Loved_As_Child_FU1" = "Childhood emotional neglect", "Phys_Abused_As_Child_FU1" = "Childhood physical abuse", "Felt_Hated_As_Child_FU1" = "Childhood emotional abuse", "Sex_Molested_As_Child_FU1" = "Childhood sexual molestation", "Someone_Take_To_Doctor_As_Child_FU1" = "Childhood physical neglect", "NO2_Air_Pollution_2010_BL" = "NO2 Air Pollution in 2010", "NOx_Air_Pollution_2010_BL" = "NOx Air Pollution in 2010", "PM10_Air_Pollution_2010_BL" = "PM10 Air Pollution in 2010", "PM2.5_Air_Pollution_2010_BL" = "PM2.5 Air Pollution in 2010", "PM2.5_Absorbance_2010_BL" = "PM2.5 Absorbance in 2010", "PM2.5_10um_Air_Pollution_2010_BL" = "PM2.5-10um Air Pollution in 2010", "Traffic_Intensity_Nearest_Road_BL" = "AP: Traffic intensity on the nearest road", "Inv_Dist_Nearest_Road_BL" = "AP: Inverse distance to the nearest road", "Traffic_Intensity_Nearest_Major_Road_BL" = "AP: Traffic intensity on the nearest major road", "Inv_Dist_Nearest_Major_Road_BL" = "AP: Inverse distance to the nearest major road", "Total_Traffic_Load_Major_Roads_BL" = "AP: Traffic load on major roads", "Close_To_Major_Road_BL" = "AP: Close to major road", "Sum_Major_Road_Length_100m_BL" = "AP: Sum of road length within 100m", "NO2_Air_Pollution_2005_BL" = "NO2 Air Pollution in 2005", "NO2_Air_Pollution_2006_BL" = "NO2 Air Pollution in 2006", "NO2_Air_Pollution_2007_BL" = "NO2 Air Pollution in 2007", "PM10_Air_Pollution_2007_BL" = "PM10 Air Pollution in 2007", "Avg_Daytime_Sound_Level_BL" = "Average daytime sound level of noise pollution", "Avg_Evening_Sound_Level_BL" = "Average evening sound level of noise pollution", "Avg_Nighttime_Sound_Level_BL" = "Average night-time sound level of noise pollution", "Greenspace_Percentage_1000m_BL" = "Greenspace ratio buffer 1000m (R)", "Domestic_Garden_Percentage_1000m_BL" = "Domestic garden ratio buffer 1000m (R)", "Water_Percentage_1000m_BL" = "Water ratio buffer 1000m (R)", "Greenspace_Percentage_300m_BL" = "Greenspace ratio buffer 300m (R)", "Domestic_Garden_Percentage_300m_BL" = "Domestic garden ratio buffer 300m (R)", "Water_Percentage_300m_BL" = "Water ratio buffer 300m (R)", "Natural_Env_Percentage_1000m_BL" = "Natural Env. ratio buffer 1000m (R)", "Natural_Env_Percentage_300m_BL" = "Natural Env. ratio buffer 300m (R)", "Distance_To_Coast_Euclidean_BL" = "Distance to coast"
)

resilience_levels_target <- c("1_Low", "3_High")


# ==============================================================================
# Section 3 & 4: Analysis and Visualization for Baseline (BL)
# ==============================================================================
cat("\n[INFO] Running Baseline (BL) Stage Analysis...\n")

trauma_effect_table_BL <- data.frame()
correlation_results_BL_all <- data.frame()
comparison_results_BL_all <- data.frame()

for (trauma_var in names(trauma_variables_list)) {
  trauma <- trauma_variables_list[[trauma_var]]
  for (t in trauma) {
    
    # 1. Prepare clean data subset
    df_wave <- df_global %>%
      dplyr::select(eid, Age, Sex, Ethnic, BMI, site, Education, selfresilience = selfresilience_cat, PHQ = PHQ_BL, !!rlang::sym(t)) %>%
      tidyr::drop_na()
    
    # 2. Main effect model
    form_main <- stats::as.formula(paste("PHQ ~", t, "+ Age + Sex + Ethnic + BMI + site + Education"))
    lme_main <- stats::lm(form_main, data = df_wave)
    results <- broom::tidy(lme_main)
    results$partial_r <- results$statistic / sqrt(results$statistic^2 + lme_main$df.residual)
    
    filtered_results <- results %>% dplyr::filter(term == t) %>% dplyr::mutate(Trauma_Variable = t)
    trauma_effect_table_BL <- dplyr::bind_rows(trauma_effect_table_BL, filtered_results)
    
    # 3. Partial correlation within resilience groups
    correlation_results_BL <- data.frame()
    for (lvl in resilience_levels_target) {
      # STATISTICAL FIX: Subset first, then extract residuals
      resilient_group <- df_wave %>% dplyr::filter(selfresilience == lvl)
      
      mod_m <- stats::lm(PHQ ~ Age + Sex + Ethnic + BMI + site + Education, data = resilient_group)
      mod_t <- stats::lm(stats::as.formula(paste(t, "~ Age + Sex + Ethnic + BMI + site + Education")), data = resilient_group)
      
      cor_model <- stats::cor.test(stats::residuals(mod_m), stats::residuals(mod_t))
      
      correlation_results_BL <- rbind(correlation_results_BL, data.frame(Trauma = t, Category = trauma_var, Timepoint = "BL", Resilience_Level = lvl, Correlation = cor_model$estimate, Subject_Count = nrow(resilient_group)))
    }
    correlation_results_BL_all <- dplyr::bind_rows(correlation_results_BL_all, correlation_results_BL)
    
    # 4. Cocor comparison
    group1 <- correlation_results_BL %>% dplyr::filter(Resilience_Level == "1_Low")
    group2 <- correlation_results_BL %>% dplyr::filter(Resilience_Level == "3_High")
    
    if(nrow(group1) > 0 & nrow(group2) > 0) {
      comparison <- cocor::cocor.indep.groups(group1$Correlation, group2$Correlation, group1$Subject_Count, group2$Subject_Count, alternative = "two.sided")
      c_res <- cocor::get.cocor.results(comparison)
      
      comparison_results_BL_all <- rbind(comparison_results_BL_all, data.frame(
        Trauma = t, Timepoint = "BL", Group1 = "1_Low", Group2 = "3_High",
        Z = c_res$fisher1925$statistic,
        Cohens_q = abs(atanh(group1$Correlation) - atanh(group2$Correlation)),
        p_value = c_res$fisher1925$p.value
      ))
    }
  }
}

# FDR Correction
trauma_effect_table_BL$trauma_adjusted_p <- stats::p.adjust(trauma_effect_table_BL$p.value, method = "BH")
comparison_results_BL_all$z_adjusted_p <- stats::p.adjust(comparison_results_BL_all$p_value, method = "BH")
# Summary statistics (Baseline)
significant_count <- sum(trauma_effect_table_BL$trauma_adjusted_p < 0.05 & trauma_effect_table_BL$statistic > 0)
cat("\nNumber of trauma effects p.value < 0.05 (BL):", significant_count, "\n")
sig_trauma_BL <- trauma_effect_table_BL %>% filter(trauma_adjusted_p < 0.05 & statistic > 0)
if(nrow(sig_trauma_BL) > 0) {
  cat("  -> Range of Partial r for significant trauma effects (BL):", 
      round(min(sig_trauma_BL$partial_r, na.rm=TRUE), 3), "to", 
      round(max(sig_trauma_BL$partial_r, na.rm=TRUE), 3), "\n")
}

sig_cocor_BL <- comparison_results_BL_all %>% filter(z_adjusted_p < 0.05 & Z > 0)
cat("Number of cocor p.value < 0.05 (BL):", nrow(sig_cocor_BL), "\n")
if(nrow(sig_cocor_BL) > 0) {
  cat("  -> Range of Cohen's q for significant differences (BL):", 
      round(min(sig_cocor_BL$Cohens_q, na.rm=TRUE), 3), "to", 
      round(max(sig_cocor_BL$Cohens_q, na.rm=TRUE), 3), "\n")
}

# --- BL Plotting ---
cor_plot_dat_BL <- merge(correlation_results_BL_all, comparison_results_BL_all[, c("Trauma", "Z")], by = "Trauma", all.x = TRUE) %>% unique()
z_plot_dat_BL <- merge(comparison_results_BL_all, correlation_results_BL_all[, c("Trauma", "Category")], by = "Trauma", all.x = TRUE) %>% unique()

cor_plot_dat_BL$Trauma <- rename_map[cor_plot_dat_BL$Trauma]
z_plot_dat_BL$Trauma <- rename_map[z_plot_dat_BL$Trauma]

cor_plot_dat_BL <- cor_plot_dat_BL %>% dplyr::arrange(factor(Resilience_Level, levels = c("1_Low", "3_High")), factor(Category, levels = c("Eco_Social", "Early_risk", "Enviroment")), desc(Correlation))
cor_plot_dat_BL$Trauma <- factor(cor_plot_dat_BL$Trauma, levels = unique(cor_plot_dat_BL$Trauma), ordered = TRUE)
z_trauma_order <- cor_plot_dat_BL %>% dplyr::filter(Resilience_Level == "1_Low")
z_plot_dat_BL <- z_plot_dat_BL %>% dplyr::arrange(match(Trauma, z_trauma_order$Trauma))
z_plot_dat_BL$Trauma <- factor(z_plot_dat_BL$Trauma, levels = unique(z_plot_dat_BL$Trauma), ordered = TRUE)

cor_plot_dat_BL$Category <- factor(cor_plot_dat_BL$Category, levels = c("Eco_Social", "Early_risk", "Enviroment"), ordered = TRUE)
z_plot_dat_BL$Category <- factor(z_plot_dat_BL$Category, levels = c("Eco_Social", "Early_risk", "Enviroment"), ordered = TRUE)

dum_plot_BL <- ggplot(cor_plot_dat_BL, aes(x = Trauma, y = Correlation)) +
  geom_line(aes(group = Trauma), linewidth = 1.5) + 
  geom_point(aes(color = Resilience_Level), size = 5) +
  scale_color_manual(values = c("#ff9900", "#146eb4"), labels = c("Low Resilience", "High Resilience")) +  
  labs(x = "", y = "Correlation Coefficient", title = "Baseline (BL)") + guides(color = "none") +
  theme(panel.spacing = unit(0.5, "lines"), panel.border = element_blank(), axis.text.y = element_text(size = 14), axis.text.x = element_blank(), panel.background = element_rect(color = "white", fill = NA), panel.grid.major.x = element_line(color = "gray", linewidth = 0.5, linetype = "dashed"))

z_plot_BL <- ggplot(z_plot_dat_BL, aes(Trauma, Z)) +
  geom_col(aes(fill = factor(Z > 2.12 | Z < -2.12)), width = 0.7) +
  scale_fill_manual(values = c("#D3D3D3", "#A9A9A9"), guide = "none") +
  geom_hline(yintercept = 2.12, linetype = "dashed", color = "orange", linewidth = 0.5) +
  geom_hline(yintercept = -2.12, linetype = "dashed", color = "orange", linewidth = 0.5) +
  labs(x = "Adverse Factor", y = "Z value") + guides(fill = "none") +
  theme(panel.spacing = unit(0.5, "lines"), panel.border = element_blank(), axis.text.y = element_text(size = 14), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14), panel.background = element_rect(color = "white", fill = NA), panel.grid.major.x = element_line(color = "gray", linewidth = 0.5, linetype = "dashed"), panel.grid.minor.x = element_line(color = "gray", linetype = "dashed", linewidth = 0.2))

combined_plot_BL <- dum_plot_BL / z_plot_BL + plot_layout(heights = c(1, 0.3))
print(combined_plot_BL)


# ==============================================================================
# Section 5: Analysis for Follow-up 1 (FU1) Timepoint
# ==============================================================================
cat("\n[INFO] Running Follow-up 1 (FU1) Stage Analysis...\n")

trauma_effect_table_FU1 <- data.frame()
correlation_results_FU1_all <- data.frame()
comparison_results_FU1_all <- data.frame()

for (trauma_var in names(trauma_variables_list)) {
  trauma <- trauma_variables_list[[trauma_var]]
  for (t in trauma) {
    
    df_wave <- df_global %>%
      dplyr::select(eid, Age, Sex, Ethnic, BMI, site, Education, selfresilience = selfresilience_cat, PHQ = FU1_symptoms, !!rlang::sym(t)) %>%
      tidyr::drop_na()
    
    form_main <- stats::as.formula(paste("PHQ ~", t, "+ Age + Sex + Ethnic + BMI + site + Education"))
    lme_main <- stats::lm(form_main, data = df_wave)
    results <- broom::tidy(lme_main)
    results$partial_r <- results$statistic / sqrt(results$statistic^2 + lme_main$df.residual)
    
    filtered_results <- results %>% dplyr::filter(term == t) %>% dplyr::mutate(Trauma_Variable = t)
    trauma_effect_table_FU1 <- dplyr::bind_rows(trauma_effect_table_FU1, filtered_results)
    
    correlation_results_FU1 <- data.frame()
    for (lvl in resilience_levels_target) {
      resilient_group <- df_wave %>% dplyr::filter(selfresilience == lvl)
      
      mod_m <- stats::lm(PHQ ~ Age + Sex + Ethnic + BMI + site + Education, data = resilient_group)
      mod_t <- stats::lm(stats::as.formula(paste(t, "~ Age + Sex + Ethnic + BMI + site + Education")), data = resilient_group)
      
      cor_model <- stats::cor.test(stats::residuals(mod_m), stats::residuals(mod_t))
      
      correlation_results_FU1 <- rbind(correlation_results_FU1, data.frame(Trauma = t, Category = trauma_var, Timepoint = "FU1", Resilience_Level = lvl, Correlation = cor_model$estimate, Subject_Count = nrow(resilient_group)))
    }
    correlation_results_FU1_all <- dplyr::bind_rows(correlation_results_FU1_all, correlation_results_FU1)
    
    group1 <- correlation_results_FU1 %>% dplyr::filter(Resilience_Level == "1_Low")
    group2 <- correlation_results_FU1 %>% dplyr::filter(Resilience_Level == "3_High")
    
    if(nrow(group1) > 0 & nrow(group2) > 0) {
      comparison <- cocor::cocor.indep.groups(group1$Correlation, group2$Correlation, group1$Subject_Count, group2$Subject_Count, alternative = "two.sided")
      c_res <- cocor::get.cocor.results(comparison)
      
      comparison_results_FU1_all <- rbind(comparison_results_FU1_all, data.frame(
        Trauma = t, Timepoint = "FU1", Group1 = "1_Low", Group2 = "3_High",
        Z = c_res$fisher1925$statistic,
        Cohens_q = abs(atanh(group1$Correlation) - atanh(group2$Correlation)),
        p_value = c_res$fisher1925$p.value
      ))
    }
  }
}

trauma_effect_table_FU1$trauma_adjusted_p <- stats::p.adjust(trauma_effect_table_FU1$p.value, method = "BH")
comparison_results_FU1_all$z_adjusted_p <- stats::p.adjust(comparison_results_FU1_all$p_value, method = "BH")

# Summary statistics (Follow-Up 1)
significant_count <- sum(trauma_effect_table_FU1$trauma_adjusted_p < 0.05 & trauma_effect_table_FU1$statistic > 0)
cat("\nNumber of trauma effects p.value < 0.05 (FU1):", significant_count, "\n")
sig_trauma_FU1 <- trauma_effect_table_FU1 %>% filter(trauma_adjusted_p < 0.05 & statistic > 0)
if(nrow(sig_trauma_FU1) > 0) {
  cat("  -> Range of Partial r for significant trauma effects (FU1):", 
      round(min(sig_trauma_FU1$partial_r, na.rm=TRUE), 3), "to", 
      round(max(sig_trauma_FU1$partial_r, na.rm=TRUE), 3), "\n")
}

sig_cocor_FU1 <- comparison_results_FU1_all %>% filter(z_adjusted_p < 0.05 & Z > 0)
cat("Number of cocor p.value < 0.05 (FU1):", nrow(sig_cocor_FU1), "\n")
if(nrow(sig_cocor_FU1) > 0) {
  cat("  -> Range of Cohen's q for significant differences (FU1):", 
      round(min(sig_cocor_FU1$Cohens_q, na.rm=TRUE), 3), "to", 
      round(max(sig_cocor_FU1$Cohens_q, na.rm=TRUE), 3), "\n")
}


# --- FU1 Plotting ---
cor_plot_dat_FU1 <- merge(correlation_results_FU1_all, comparison_results_FU1_all[, c("Trauma", "Z")], by = "Trauma", all.x = TRUE) %>% unique()
z_plot_dat_FU1 <- merge(comparison_results_FU1_all, correlation_results_FU1_all[, c("Trauma", "Category")], by = "Trauma", all.x = TRUE) %>% unique()

cor_plot_dat_FU1$Trauma <- rename_map[cor_plot_dat_FU1$Trauma]
z_plot_dat_FU1$Trauma <- rename_map[z_plot_dat_FU1$Trauma]

cor_plot_dat_FU1 <- cor_plot_dat_FU1 %>% dplyr::arrange(factor(Resilience_Level, levels = c("1_Low", "3_High")), factor(Category, levels = c("Eco_Social", "Early_risk", "Enviroment")), desc(Correlation))
cor_plot_dat_FU1$Trauma <- factor(cor_plot_dat_FU1$Trauma, levels = unique(cor_plot_dat_FU1$Trauma), ordered = TRUE)
z_trauma_order <- cor_plot_dat_FU1 %>% dplyr::filter(Resilience_Level == "1_Low")
z_plot_dat_FU1 <- z_plot_dat_FU1 %>% dplyr::arrange(match(Trauma, z_trauma_order$Trauma))
z_plot_dat_FU1$Trauma <- factor(z_plot_dat_FU1$Trauma, levels = unique(z_plot_dat_FU1$Trauma), ordered = TRUE)

dum_plot_FU1 <- ggplot(cor_plot_dat_FU1, aes(x = Trauma, y = Correlation)) +
  geom_line(aes(group = Trauma), linewidth = 1.5) + 
  geom_point(aes(color = Resilience_Level), size = 5) +
  scale_color_manual(values = c("#ff9900", "#146eb4"), labels = c("Low Resilience", "High Resilience")) +  
  labs(x = "", y = "Correlation Coefficient", title = "Follow-up 1 (FU1)") + guides(color = "none") +
  theme(panel.spacing = unit(0.5, "lines"), panel.border = element_blank(), axis.text.y = element_text(size = 14), axis.text.x = element_blank(), panel.background = element_rect(color = "white", fill = NA), panel.grid.major.x = element_line(color = "gray", linewidth = 0.5, linetype = "dashed"))

z_plot_FU1 <- ggplot(z_plot_dat_FU1, aes(Trauma, Z)) +
  geom_col(aes(fill = factor(Z > 2.28 | Z < -2.28)), width = 0.7) +
  scale_fill_manual(values = c("#D3D3D3", "#A9A9A9"), guide = "none") +
  geom_hline(yintercept = 2.28, linetype = "dashed", color = "orange", linewidth = 0.5) +
  geom_hline(yintercept = -2.28, linetype = "dashed", color = "orange", linewidth = 0.5) +
  labs(x = "Adverse Factor", y = "Z value") + guides(fill = "none") +
  theme(panel.spacing = unit(0.5, "lines"), panel.border = element_blank(), axis.text.y = element_text(size = 14), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14), panel.background = element_rect(color = "white", fill = NA), panel.grid.major.x = element_line(color = "gray", linewidth = 0.5, linetype = "dashed"), panel.grid.minor.x = element_line(color = "gray", linetype = "dashed", linewidth = 0.2))

combined_plot_FU1 <- dum_plot_FU1 / z_plot_FU1 + plot_layout(heights = c(1, 0.3))
print(combined_plot_FU1)



# ==============================================================================
# Section 6: Analysis for Follow-up 2 (FU2) Timepoint
# ==============================================================================
cat("\n[INFO] Running Follow-up 2 (FU2) Stage Analysis...\n")

trauma_effect_table_FU2 <- data.frame()
correlation_results_FU2_all <- data.frame()
comparison_results_FU2_all <- data.frame()

for (trauma_var in names(trauma_variables_list)) {
  trauma <- trauma_variables_list[[trauma_var]]
  for (t in trauma) {
    
    df_wave <- df_global %>%
      dplyr::select(eid, Age, Sex, Ethnic, BMI, site, Education, selfresilience = selfresilience_cat, PHQ = FU2_symptoms, !!rlang::sym(t)) %>%
      tidyr::drop_na()
    
    form_main <- stats::as.formula(paste("PHQ ~", t, "+ Age + Sex + Ethnic + BMI + site + Education"))
    lme_main <- stats::lm(form_main, data = df_wave)
    results <- broom::tidy(lme_main)
    results$partial_r <- results$statistic / sqrt(results$statistic^2 + lme_main$df.residual)
    
    filtered_results <- results %>% dplyr::filter(term == t) %>% dplyr::mutate(Trauma_Variable = t)
    trauma_effect_table_FU2 <- dplyr::bind_rows(trauma_effect_table_FU2, filtered_results)
    
    correlation_results_FU2 <- data.frame()
    for (lvl in resilience_levels_target) {
      resilient_group <- df_wave %>% dplyr::filter(selfresilience == lvl)
      
      mod_m <- stats::lm(PHQ ~ Age + Sex + Ethnic + BMI + site + Education, data = resilient_group)
      mod_t <- stats::lm(stats::as.formula(paste(t, "~ Age + Sex + Ethnic + BMI + site + Education")), data = resilient_group)
      
      cor_model <- stats::cor.test(stats::residuals(mod_m), stats::residuals(mod_t))
      
      correlation_results_FU2 <- rbind(correlation_results_FU2, data.frame(Trauma = t, Category = trauma_var, Timepoint = "FU2", Resilience_Level = lvl, Correlation = cor_model$estimate, Subject_Count = nrow(resilient_group)))
    }
    correlation_results_FU2_all <- dplyr::bind_rows(correlation_results_FU2_all, correlation_results_FU2)
    
    group1 <- correlation_results_FU2 %>% dplyr::filter(Resilience_Level == "1_Low")
    group2 <- correlation_results_FU2 %>% dplyr::filter(Resilience_Level == "3_High")
    
    if(nrow(group1) > 0 & nrow(group2) > 0) {
      comparison <- cocor::cocor.indep.groups(group1$Correlation, group2$Correlation, group1$Subject_Count, group2$Subject_Count, alternative = "two.sided")
      c_res <- cocor::get.cocor.results(comparison)
      
      comparison_results_FU2_all <- rbind(comparison_results_FU2_all, data.frame(
        Trauma = t, Timepoint = "FU2", Group1 = "1_Low", Group2 = "3_High",
        Z = c_res$fisher1925$statistic,
        Cohens_q = abs(atanh(group1$Correlation) - atanh(group2$Correlation)),
        p_value = c_res$fisher1925$p.value
      ))
    }
  }
}

trauma_effect_table_FU2$trauma_adjusted_p <- stats::p.adjust(trauma_effect_table_FU2$p.value, method = "BH")
comparison_results_FU2_all$z_adjusted_p <- stats::p.adjust(comparison_results_FU2_all$p_value, method = "BH")

# Summary statistics (Follow-Up 2)
significant_count <- sum(trauma_effect_table_FU2$trauma_adjusted_p < 0.05 & trauma_effect_table_FU2$statistic > 0)
cat("\nNumber of trauma effects p.value < 0.05 (FU2):", significant_count, "\n")
sig_trauma_FU2 <- trauma_effect_table_FU2 %>% filter(trauma_adjusted_p < 0.05 & statistic > 0)
if(nrow(sig_trauma_FU2) > 0) {
  cat("  -> Range of Partial r for significant trauma effects (FU2):", 
      round(min(sig_trauma_FU2$partial_r, na.rm=TRUE), 3), "to", 
      round(max(sig_trauma_FU2$partial_r, na.rm=TRUE), 3), "\n")
}

sig_cocor_FU2 <- comparison_results_FU2_all %>% filter(z_adjusted_p < 0.05 & Z > 0)
cat("Number of cocor p.value < 0.05 (FU2):", nrow(sig_cocor_FU2), "\n")
if(nrow(sig_cocor_FU2) > 0) {
  cat("  -> Range of Cohen's q for significant differences (FU2):", 
      round(min(sig_cocor_FU2$Cohens_q, na.rm=TRUE), 3), "to", 
      round(max(sig_cocor_FU2$Cohens_q, na.rm=TRUE), 3), "\n")
}

# --- FU2 Plotting ---
cor_plot_dat_FU2 <- merge(correlation_results_FU2_all, comparison_results_FU2_all[, c("Trauma", "Z")], by = "Trauma", all.x = TRUE) %>% unique()
z_plot_dat_FU2 <- merge(comparison_results_FU2_all, correlation_results_FU2_all[, c("Trauma", "Category")], by = "Trauma", all.x = TRUE) %>% unique()

cor_plot_dat_FU2$Trauma <- rename_map[cor_plot_dat_FU2$Trauma]
z_plot_dat_FU2$Trauma <- rename_map[z_plot_dat_FU2$Trauma]

cor_plot_dat_FU2 <- cor_plot_dat_FU2 %>% dplyr::arrange(factor(Resilience_Level, levels = c("1_Low", "3_High")), factor(Category, levels = c("Eco_Social", "Early_risk", "Enviroment")), desc(Correlation))
cor_plot_dat_FU2$Trauma <- factor(cor_plot_dat_FU2$Trauma, levels = unique(cor_plot_dat_FU2$Trauma), ordered = TRUE)
z_trauma_order <- cor_plot_dat_FU2 %>% dplyr::filter(Resilience_Level == "1_Low")
z_plot_dat_FU2 <- z_plot_dat_FU2 %>% dplyr::arrange(match(Trauma, z_trauma_order$Trauma))
z_plot_dat_FU2$Trauma <- factor(z_plot_dat_FU2$Trauma, levels = unique(z_plot_dat_FU2$Trauma), ordered = TRUE)

dum_plot_FU2 <- ggplot(cor_plot_dat_FU2, aes(x = Trauma, y = Correlation)) +
  geom_line(aes(group = Trauma), linewidth = 1.5) + 
  geom_point(aes(color = Resilience_Level), size = 5) +
  scale_color_manual(values = c("#ff9900", "#146eb4"), labels = c("Low Resilience", "High Resilience")) +  
  labs(x = "", y = "Correlation Coefficient", title = "Follow-up 2 (FU2)") + guides(color = "none") +
  theme(panel.spacing = unit(0.5, "lines"), panel.border = element_blank(), axis.text.y = element_text(size = 14), axis.text.x = element_blank(), panel.background = element_rect(color = "white", fill = NA), panel.grid.major.x = element_line(color = "gray", linewidth = 0.5, linetype = "dashed"))

z_plot_FU2 <- ggplot(z_plot_dat_FU2, aes(Trauma, Z)) +
  geom_col(aes(fill = factor(Z > 2.3 | Z < -2.3)), width = 0.7) +
  scale_fill_manual(values = c("#D3D3D3", "#A9A9A9"), guide = "none") +
  geom_hline(yintercept = 2.3, linetype = "dashed", color = "orange", linewidth = 0.5) +
  geom_hline(yintercept = -2.3, linetype = "dashed", color = "orange", linewidth = 0.5) +
  labs(x = "Adverse Factor", y = "Z value") + guides(fill = "none") +
  theme(panel.spacing = unit(0.5, "lines"), panel.border = element_blank(), axis.text.y = element_text(size = 14), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14), panel.background = element_rect(color = "white", fill = NA), panel.grid.major.x = element_line(color = "gray", linewidth = 0.5, linetype = "dashed"), panel.grid.minor.x = element_line(color = "gray", linetype = "dashed", linewidth = 0.2))

combined_plot_FU2 <- dum_plot_FU2 / z_plot_FU2 + plot_layout(heights = c(1, 0.3))
print(combined_plot_FU2)


# ==============================================================================
# Section 7: Final Adjusted Regression Direction Check
# ==============================================================================
cat("\n=========================================================\n")
cat("Adjusted Multivariable Regression Reports\n")
cat("=========================================================\n")

check_direction <- function(df, timepoint) {
  cat("\n--->", timepoint, "\n")
  report <- df %>%
    dplyr::select(Trauma_Variable, estimate, statistic, partial_r, p.value, trauma_adjusted_p) %>%
    dplyr::mutate(
      Status = dplyr::case_when(
        estimate > 0 & trauma_adjusted_p < 0.05 ~ "Risk factor (positive effect, p < 0.05)",
        estimate < 0 & trauma_adjusted_p < 0.05 ~ "Protective factor (negative effect, p < 0.05)",
        TRUE ~ "Non-significant independent main effect"
      )
    ) %>%
    dplyr::arrange(desc(partial_r)) %>%
    dplyr::as_tibble()
  print(report, n = Inf)
}

check_direction(trauma_effect_table_BL, "Baseline (BL)")
check_direction(trauma_effect_table_FU1, "Follow-up 1 (FU1)")
check_direction(trauma_effect_table_FU2, "Follow-up 2 (FU2)")


# ==============================================================================
# Section 8: Sensitivity Analysis (Continuous Resilience x Trauma Interaction)
# ==============================================================================
cat("\n=========================================================\n")
cat("--- Sensitivity Analysis:  Sensitivity: Continuous Resilience x Trauma Interactions\n")
cat("=========================================================\n")

interaction_table_BL <- data.frame()
interaction_table_FU1 <- data.frame()
interaction_table_FU2 <- data.frame()

for (trauma_var in names(trauma_variables_list)) {
  trauma <- trauma_variables_list[[trauma_var]]
  for (t in trauma) {
    
    # --- 8.1 BL Sensitivity ---
    sens_BL <- df_global %>%
      dplyr::select(eid, Age, Sex, Ethnic, BMI, site, Education, selfresilience = self_resilience_cont, PHQ = PHQ_BL, !!rlang::sym(t)) %>% 
      tidyr::drop_na()
    
    form_BL <- stats::as.formula(paste("PHQ ~ selfresilience *", t, "+ Age + Sex + Ethnic + BMI + site + Education"))
    lme_BL <- stats::lm(form_BL, data = sens_BL)
    
    # Extract regression coefficients with broom::tidy (estimate, std.error, statistic, p.value)
    res_BL <- broom::tidy(lme_BL) %>% 
      dplyr::filter(term == paste0("selfresilience:", t)) %>% 
      dplyr::mutate(Trauma_Variable = t)
    interaction_table_BL <- dplyr::bind_rows(interaction_table_BL, res_BL)
    
    # --- 8.2 FU1 Sensitivity ---
    sens_FU1 <- df_global %>%
      dplyr::select(eid, Age, Sex, Ethnic, BMI, site, Education, selfresilience = self_resilience_cont, PHQ = FU1_symptoms, !!rlang::sym(t)) %>% 
      tidyr::drop_na()
    
    form_FU1 <- stats::as.formula(paste("PHQ ~ selfresilience *", t, "+ Age + Sex + Ethnic + BMI + site + Education"))
    lme_FU1 <- stats::lm(form_FU1, data = sens_FU1)
    
    res_FU1 <- broom::tidy(lme_FU1) %>% 
      dplyr::filter(term == paste0("selfresilience:", t)) %>% 
      dplyr::mutate(Trauma_Variable = t)
    interaction_table_FU1 <- dplyr::bind_rows(interaction_table_FU1, res_FU1)
    
    # --- 8.3 FU2 Sensitivity ---
    sens_FU2 <- df_global %>%
      dplyr::select(eid, Age, Sex, Ethnic, BMI, site, Education, selfresilience = self_resilience_cont, PHQ = FU2_symptoms, !!rlang::sym(t)) %>% 
      tidyr::drop_na()
    
    form_FU2 <- stats::as.formula(paste("PHQ ~ selfresilience *", t, "+ Age + Sex + Ethnic + BMI + site + Education"))
    lme_FU2 <- stats::lm(form_FU2, data = sens_FU2)
    
    res_FU2 <- broom::tidy(lme_FU2) %>% 
      dplyr::filter(term == paste0("selfresilience:", t)) %>% 
      dplyr::mutate(Trauma_Variable = t)
    interaction_table_FU2 <- dplyr::bind_rows(interaction_table_FU2, res_FU2)
  }
}

# Apply FDR Correction
interaction_table_BL$interaction_adjusted_p <- stats::p.adjust(interaction_table_BL$p.value, method = "BH")
interaction_table_FU1$interaction_adjusted_p <- stats::p.adjust(interaction_table_FU1$p.value, method = "BH")
interaction_table_FU2$interaction_adjusted_p <- stats::p.adjust(interaction_table_FU2$p.value, method = "BH")

# Formatting output
format_sens_table <- function(df, timepoint) {
  df %>%
    # Model summary includes estimate, std.error, statistic, and p-value
    dplyr::select(Trauma_Variable, estimate, std.error, statistic, p.value, interaction_adjusted_p) %>%
    dplyr::mutate(
      Timepoint = timepoint,
      Interaction_Significance = dplyr::case_when(
        interaction_adjusted_p < 0.05 ~ "⭐ Significant Interaction",
        TRUE ~ "Not Significant"
      )
    ) %>%
    dplyr::arrange(interaction_adjusted_p) %>%
    dplyr::as_tibble()
}

sens_report_BL <- format_sens_table(interaction_table_BL, "Baseline (BL)")
sens_report_FU1 <- format_sens_table(interaction_table_FU1, "Follow-up 1 (FU1)")
sens_report_FU2 <- format_sens_table(interaction_table_FU2, "Follow-up 2 (FU2)")

cat("\nSignificant Resilience x Trauma interactions (BL):", sum(sens_report_BL$interaction_adjusted_p < 0.05), "\n")
cat("Significant Resilience x Trauma interactions (FU1):", sum(sens_report_FU1$interaction_adjusted_p < 0.05), "\n")
cat("Significant Resilience x Trauma interactions (FU2):", sum(sens_report_FU2$interaction_adjusted_p < 0.05), "\n")

# Use print(sens_report_BL, n = Inf) to view full tables