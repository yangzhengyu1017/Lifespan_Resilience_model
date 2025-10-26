# ==============================================
# This script performs the following steps:
# 1. Load resilience, blood count, and biochemistry data, and merge them.
# 2. Categorize participants into three resilience groups based on self_resilience quantiles.
# 3. For each blood/biochemistry variable:
#    - Fit linear models predicting baseline depressive symptoms (PHQ_BL)
#      with main effects and interactions with resilience.
#    - Compute residual correlations for low and high resilience groups.
#    - Compare correlations using Fisher z-transform (cocor package).
# 4. Adjust p-values for multiple testing using BH method.
# 5. Prepare and visualize results with a combined dumbbell + z-value plot.
# ==============================================

library(tidyverse)
library(tidyr)
library(ggtext)
library(showtext)
library(ggprism)
library(cocor)
library(patchwork)
showtext_auto()

# Load datasets
resilience_group_R <- read.csv("/public/home/yangzy/Documents/Data/UKB/resilience_group_R.csv")
Blood_count <- read.csv("/public/home/yangzy/Documents/Data/UKB/Blood_count.csv")
blood_biochemistry <- read.csv("/public/home/yangzy/Documents/Data/UKB/blood_biochemistry.csv")

# Merge datasets by participant ID
resilience_group_R <- merge(resilience_group_R, Blood_count, by = "eid", all.x = TRUE)
resilience_group_R <- merge(resilience_group_R, blood_biochemistry, by = "eid", all.x = TRUE)

# Categorize participants into three resilience groups based on self_resilience quantiles
quantiles <- quantile(resilience_group_R$self_resilience, probs = c(1/3, 2/3), na.rm = TRUE)
resilience_group_R$resilience_group[resilience_group_R$self_resilience <= quantiles[1]] <- 0
resilience_group_R$resilience_group[resilience_group_R$self_resilience > quantiles[1] & resilience_group_R$self_resilience < quantiles[2]] <- 1
resilience_group_R$resilience_group[resilience_group_R$self_resilience >= quantiles[2]] <- 2
resilience_group_R$resilience_group <- as.factor(resilience_group_R$resilience_group)

# List of blood/biochemistry variables
trauma_variables_list <- c("Albumin", "ALP", "ALT", "CRP", "Cystatin C", "GGT", "HbA1c", 
                           "HCT", "HDL_C", "HGB", "HLSR Percentage", "IGF_1", "IRF", 
                           "Lymph Percentage", "MPV", "Neut Count", "Neut Percentage", 
                           "Platelet Count", "RBC Count", "RDW", "TG", "Total Bili", 
                           "Total Protein", "Vitamin D", "WBC Count")
trauma_variables_list <- gsub(" ", "_", trauma_variables_list)

# Initialize empty dataframes for results
result_table <- data.frame()
trauma_effect_table <- data.frame()
interaction_effect_table <- data.frame()
resilience_effect_table <- data.frame()
correlation_results_BL_all <- data.frame()
comparison_results_BL_all <- data.frame()

# Loop through each variable
for (t in trauma_variables_list) {
  # Prepare data for linear modeling
  data_model <- resilience_group_R %>%
    transmute(
      eid,
      Sex = factor(gender, levels = c(0,1), labels = c("Female","Male")),
      Age = age_BL,
      Ethnic = factor(Ethnic_group, levels = c("0","1","2","3"), labels = c("1_White","2_Asian","3_Black","4_Other")),
      Education = Education_year,
      BMI = BMI_BL,
      site = factor(site),
      resilience_group = factor(resilience_group, levels = c(0,1,2), labels = c("1_Low","2_Medium","3_High")),
      PHQ_BL = Depressive_Symptoms_PHQ4_BL,
      !!t := !!sym(t)
    ) %>%
    filter(resilience_group != "2_Medium") %>%
    na.omit()
  
  # Fit linear model with main effects and interaction
  formula_lm <- as.formula(paste("PHQ_BL ~ resilience_group +", t, "+ resilience_group *", t, "+ Age + Sex + Ethnic + BMI + Education + site"))
  lm_res <- lm(formula_lm, data = data_model)
  results <- tidy(lm_res)
  results$Trauma_Variable <- t
  result_table <- bind_rows(result_table, results)
  
  # Extract trauma, resilience, and interaction effects
  trauma_effect_table <- bind_rows(trauma_effect_table, results %>% filter(term == t))
  resilience_effect_table <- bind_rows(resilience_effect_table, results %>% filter(term == "resilience_group3_High"))
  interaction_effect_table <- bind_rows(interaction_effect_table, results %>% filter(term == paste0("resilience_group3_High:", t)))
  
  # Correlation analysis by resilience group
  correlation_results_BL <- data.frame()
  for (group in c("1_Low","3_High")) {
    data_reg <- data_model
    data_reg$PHQ_BL_resid <- residuals(lm(PHQ_BL ~ Age + Sex + Ethnic + BMI + Education + site, data = data_reg))
    group_data <- subset(data_reg, resilience_group == group)
    cor_test <- cor.test(group_data$PHQ_BL_resid, group_data[[t]])
    correlation_results_BL <- rbind(correlation_results_BL, data.frame(
      Trauma = t, Timepoint = "BL", Resilience_Level = group,
      Correlation = cor_test$estimate, P_value = cor_test$p.value,
      Subject_Count = nrow(group_data)
    ))
  }
  correlation_results_BL_all <- rbind(correlation_results_BL_all, correlation_results_BL)
  
  # Compare correlations between low and high resilience groups using Fisher z
  x = group1$Correlation
  n.x = group1$Subject_Count
  y = group2$Correlation
  n.y = group2$Subject_Count
  comparison <- cocor.indep.groups(x, y, n.x, n.y, alternative = "two.sided")
  comparison_results_BL_all <- rbind(comparison_results_BL_all, data.frame(
    Trauma = t, Timepoint = "BL",
    Group1 = "1_Low", Group2 = "3_High",
    Z = get.cocor.results(comparison)$fisher1925$statistic,
    p_value = get.cocor.results(comparison)$fisher1925$p.value
  ))
}

# Adjust p-values for multiple testing
sum_result_p <- data.frame(
  Var = trauma_effect_table$Trauma_Variable,
  trauma_p = trauma_effect_table$p.value,
  resilience_p = resilience_effect_table$p.value,
  interaction_p = interaction_effect_table$p.value
)
sum_result_p$trauma_adj <- p.adjust(sum_result_p$trauma_p, method = "BH")
sum_result_p$resilience_adj <- p.adjust(sum_result_p$resilience_p, method = "BH")
sum_result_p$interaction_adj <- p.adjust(sum_result_p$interaction_p, method = "BH")

# Adjust cocor p-values
comparison_results_BL_all$z_adj_p <- p.adjust(comparison_results_BL_all$p_value, method = "BH")

# -----------------------
# Visualization
# -----------------------

# Merge with categories for plotting
blood_cate <- read.csv("~/Documents/Data/UKB/blood_parameters_category.csv")
colnames(blood_cate)[2:3] <- c("Trauma","Category")

cor_plot_dat <- merge(correlation_results_BL_all, blood_cate[,c("Trauma","Category")], by = "Trauma")
z_plot_dat <- merge(comparison_results_BL_all, blood_cate[,c("Trauma","Category")], by = "Trauma")

# Factor ordering for plotting
cor_plot_dat$Trauma <- factor(cor_plot_dat$Trauma, levels = unique(cor_plot_dat$Trauma), ordered = TRUE)
z_plot_dat$Trauma <- factor(z_plot_dat$Trauma, levels = levels(cor_plot_dat$Trauma), ordered = TRUE)

# Dumbbell plot
dum_plot <- ggplot(cor_plot_dat, aes(x = Trauma, y = Correlation)) +
  geom_line(aes(group = Trauma), size = 1.5) +
  geom_point(aes(color = Resilience_Level), size = 5) +
  scale_color_manual(values = c("#ff9900","#146eb4")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  coord_flip() +
  labs(x = "", y = "Correlation Coefficient", title = "Baseline") +
  theme(panel.background = element_rect(fill = NA, color = "white"),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size = 14))

# Z-value bar plot
z_plot <- ggplot(z_plot_dat, aes(Trauma, Z)) +
  geom_col(aes(fill = factor(Z > 2.6 | Z < -2.6)), width = 0.7) +
  scale_fill_manual(values = c("#D3D3D3","#A9A9A9"), guide = FALSE) +
  geom_hline(yintercept = 2.6, linetype = "dashed", color = "orange") +
  geom_hline(yintercept = -2.6, linetype = "dashed", color = "orange") +
  coord_flip() +
  labs(x = "Adverse Factor", y = "Z value") +
  theme(panel.background = element_rect(fill = NA, color = "white"),
        panel.grid.major.x = element_line(color = "gray", linewidth = 0.5),
        panel.grid.minor.x = element_line(color = "gray", linetype = "dashed", linewidth = 0.2),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14))

# Combine dumbbell and Z-value plot
dum_plot / z_plot + plot_layout(heights = c(1,0.3))