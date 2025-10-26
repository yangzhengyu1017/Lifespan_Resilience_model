# ===============================================================
# Project: Resilience Group Analysis and Matched Comparisons
# Purpose:
#   This script performs the following analyses on the UKB baseline data:
#   1. Categorizes individuals into low, medium, and high self-reported resilience groups.
#   2. Conducts propensity score matching between low and high resilience groups.
#   3. Summarizes matched group characteristics.
#   4. Tests group differences in financial and health satisfaction.
#   5. Visualizes satisfaction scores with adjusted p-values.
# ===============================================================

# ================================
# Load required libraries
# ================================
library(data.table)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(caret)
library(visreg)
library(rms)
library(gtsummary)
library(autoReg)
library(ggbeeswarm)
library(patchwork)
library(dplyr)
library(emmeans)
library(bruceR)
library(gridExtra)
library(MatchIt)

# ================================
# Load and preprocess data
# ================================
# Load baseline trauma and resilience datasets
Trauma_BL_dat <- read.csv("~/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/UKB_BL_Trauma.csv")
Trauma_BL_dat <- Trauma_BL_dat[,-1]  # Remove first column (index)

resilience_group_R <- read.csv("~/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/UKB_dat_for_resilience_model_1224.csv")

# Merge trauma variables into resilience dataset
resilience_group_R <- merge(resilience_group_R,
                            Trauma_BL_dat[, c(1, 3:8)],
                            by = "eid", all.x = TRUE)

# ================================
# Categorize self-reported resilience
# ================================
# Compute tertiles
quantiles <- quantile(resilience_group_R$self_resilience, probs = c(1/3, 2/3), na.rm = TRUE)

# Assign categories: 0 = Low, 1 = Medium, 2 = High
resilience_group_R$self_resilience[resilience_group_R$self_resilience <= quantiles[1]] <- 0
resilience_group_R$self_resilience[resilience_group_R$self_resilience > quantiles[1] &
                                     resilience_group_R$self_resilience < quantiles[2]] <- 1
resilience_group_R$self_resilience[resilience_group_R$self_resilience >= quantiles[2]] <- 2
resilience_group_R$self_resilience <- as.factor(resilience_group_R$self_resilience)

# ================================
# Prepare data for matching analysis
# ================================
# Keep only low and high resilience groups (exclude medium)
resilience_group_R1 <- resilience_group_R[resilience_group_R$self_resilience != 1, ]

resilience_test <- resilience_group_R1 %>% 
  transmute(
    eid,
    Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")),
    Age = age_BL,
    Ethnic = factor(Ethnic_group, levels = c("0", "1", "2", "3"), labels = c("1_White", "2_Asian", "3_Black", "4_Other")),
    Education = Education_year,
    BMI = BMI_BL,
    site = factor(site),
    selfresilience = factor(self_resilience, levels = c(0, 2), labels = c("1_Low", "3_High")),
    HH_Income_BL, HH_Num_Vehicle_BL, IMD, Financial_Difficulties_BL,
    Frailty_BL,
    Financial_Situation_Satisfaction_BL, Health_Satisfaction_BL
  )

# Remove rows with missing values
resilience_test <- na.omit(resilience_test)

# ================================
# Propensity score matching (nearest neighbor)
# ================================
m.out0 <- matchit(selfresilience ~ HH_Income_BL + HH_Num_Vehicle_BL + IMD + Financial_Difficulties_BL +
                    Frailty_BL + Age + Sex + Ethnic + BMI + Education,
                  data = resilience_test,
                  method = "nearest",
                  distance = "glm",
                  caliper = 0.001)

summary(m.out0)

# Extract matched dataset (weights = 1)
resilience_test <- match.data(m.out0)
resilience_test <- subset(resilience_test, weights == 1)

# ================================
# Summarize matched groups
# ================================
resilience_test_sum <- resilience_test %>%
  transmute(Sex, Age, Ethnic, Education, BMI, HH_Income_BL, HH_Num_Vehicle_BL, IMD,
            Financial_Difficulties_BL, Frailty_BL, selfresilience)

resilience_test_sum$Financial_Difficulties_BL <- as.factor(resilience_test_sum$Financial_Difficulties_BL)

matched_group <- resilience_test_sum %>% 
  tbl_summary(by = selfresilience,
              digits = list(Age ~ 0),
              type = list(where(is.numeric) ~ "continuous")) %>% 
  add_p() %>% add_overall()

matched_group_table <- as_tibble(matched_group)

# ================================
# Linear model: Financial Situation Satisfaction
# ================================
model <- lm(Financial_Situation_Satisfaction_BL ~ selfresilience, data = resilience_test)
summary(model)

# ================================
# Prepare data for visualization
# ================================
plot_dat <- resilience_test %>%
  transmute(selfresilience, Financial_Situation_Satisfaction_BL, Health_Satisfaction_BL)

# ================================
# Statistical testing: paired t-tests with adjusted p-values
# ================================
stat.test <- plot_dat %>% 
  pivot_longer(-selfresilience) %>%
  mutate(group = str_sub(name, start = 1, end = 4)) %>% 
  group_by(group, name) %>% 
  t_test(value ~ selfresilience) %>%
  adjust_pvalue() %>% 
  add_significance("p.adj") %>% 
  add_xy_position(x = "name", scales = "free", fun = "max") %>% 
  select(-3, -6, -7, -8, -9, -10) %>% 
  mutate(across("xmin", str_replace, "1.8", "0.8"),
         across("xmin", str_replace, "2.8", "0.8"),
         across("xmax", str_replace, "2.2", "1.2"),
         across("xmax", str_replace, "3.2", "1.2")) %>% 
  mutate(xmin = as.numeric(xmin), xmax = as.numeric(xmax))

# ================================
# Plot results: Financial and Health Satisfaction
# ================================
plot_dat %>% 
  pivot_longer(-selfresilience) %>%
  mutate(group = str_sub(name, start = 1, end = 4)) %>% 
  ggplot(aes(x = name, y = value)) +
  stat_summary(geom = "bar", position = position_dodge(width = 0.7),
               aes(fill = selfresilience), width = 0.4) +
  stat_summary(geom = "errorbar", fun.data = "mean_cl_normal",
               aes(fill = selfresilience), position = position_dodge(width = 0.7),
               width = 0.2, color = "black") +
  stat_pvalue_manual(stat.test, label = "p.adj.signif", label.size = 6,
                     hide.ns = TRUE, tip.length = 0.01,
                     position = position_dodge(width = 0.7)) +
  facet_wrap(. ~ group, scale = "free_x", nrow = 1) +
  labs(x = NULL, y = NULL) +
  scale_fill_manual(values = c("#fA9E38", "#4995C6")) +
  scale_y_continuous(limits = c(0, 6), expand = c(0, 0)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(color = "black", size = 12, margin = margin(r = 3)),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 10, margin = margin(r = 2)),
        axis.text.x = element_text(color = "black"),
        panel.background = element_rect(fill = NA, color = NA),
        panel.grid.minor = element_line(size = 0.2, color = "#e5e5e5"),
        panel.grid.major = element_line(size = 0.2, color = "#e5e5e5"),
        panel.border = element_rect(fill = NA, color = "black", size = 0.3, linetype = "solid"),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 8),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.key.width = unit(0.5, 'cm'),
        legend.key.height = unit(0.5, 'cm'),
        legend.position = c(1, 1),
        legend.justification = c(1, 1),
        legend.background = element_blank(),
        legend.box.margin = margin(0, 0, 0, 0),
        strip.text = element_text(color = "black", size = 10),
        panel.spacing.x = unit(0.3, "cm"))