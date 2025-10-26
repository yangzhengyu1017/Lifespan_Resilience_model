# ------------------------------------------------------------------
# Sensitive Analysis: Resilience as a continuous variable
# Purpose: Examine the interaction between continuous self_resilience 
#          and trauma exposure on mental health outcomes across BL, FU1, and FU2.
# ------------------------------------------------------------------

# -------------------------------
# Load necessary libraries
# -------------------------------
library(tidyverse)   # data manipulation and plotting
library(data.table)  # efficient data handling
library(ggpubr)      # for plotting functions (ggline, stat_compare_means)
library(bruceR)      # autoReg and myft for formatted regression tables
library(rstatix)     # statistical functions
library(autoReg)     # regression formatting
library(dplyr)       # data manipulation
library(broom)       # tidy regression output

# -------------------------------
# Read data
# -------------------------------
resilience_group_R <- read_csv("~/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/UKB_dat_for_analysis_1226.csv")[,-1]

# -------------------------------
# Baseline (BL) analysis
# -------------------------------
resilience_group_R_cor <- resilience_group_R[, c("eid", "age_BL", "gender", "BMI_BL", "Ethnic_group", "IMD",
                                                 "self_resilience", "Depressive_Symptoms_PHQ4_BL", "PHQ9_Severity_FU1", "PHQ-9_FU2",
                                                 "General_Anxiety_Disorder_Severity_FU1", "General_Anxiety_Disorder_Severity_FU2",
                                                 "trauma_num", "FU_recent_trauma", "FU2_recent_trauma", "Education_year")]
resilience_group_R_cor <- na.omit(resilience_group_R_cor)

resilience_test_BL <- resilience_group_R_cor %>% 
  transmute(
    eid,
    Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")),
    Age = age_BL,
    Ethnic = factor(Ethnic_group, levels = c("0","1","2","3"), labels = c("1_White","2_Asian","3_Black","4_Other")),
    Education = Education_year,
    BMI = BMI_BL,
    site = factor(site),
    self_resilience = self_resilience,                # continuous
    PHQ_BL = Depressive_Symptoms_PHQ4_BL,
    Trauma_exposure = trauma_num
  )

# Cap extreme trauma exposure values and adjust PHQ baseline
resilience_test_BL$Trauma_exposure[resilience_test_BL$Trauma_exposure >= 5] <- 4
resilience_test_BL$PHQ_BL <- resilience_test_BL$PHQ_BL - 4
resilience_test_BL <- na.omit(resilience_test_BL)

# Linear regression: interaction of continuous resilience and trauma
lme_BL <- lm(
  PHQ_BL ~ self_resilience * Trauma_exposure + Age + Sex + Ethnic + BMI + site + Education,
  data = resilience_test_BL
)
summary(lme_BL)
tidy_results_BL <- tidy(lme_BL)
tidy_results_BL$conf_int <- confint(lme_BL)
autoReg(lme_BL) %>% myft()

# -------------------------------
# FU1 analysis
# -------------------------------
resilience_group_R0 <- resilience_group_R[, c(
  "eid","age_BL","gender","BMI_BL","Ethnic_group","site","IMD",
  "self_resilience","Depressive_Symptoms_PHQ4_BL","PHQ9_Severity_FU1","PHQ-9_FU2",
  "General_Anxiety_Disorder_Severity_FU1","General_Anxiety_Disorder_Severity_FU2",
  "trauma_num","FU_recent_trauma","FU2_recent_trauma","Education_year"
)]


# Compute FU1 mental symptom score (depression + anxiety, scaled)
resilience_group_R0$FU1_symptoms <- resilience_group_R0$PHQ9_Severity_FU1/28 + 
  resilience_group_R0$General_Anxiety_Disorder_Severity_FU1/21

resilience_group_R0 <- na.omit(resilience_group_R0)

# Ensure categorical variables are factors
resilience_group_R0$gender <- as.factor(resilience_group_R0$gender)
resilience_group_R0$Ethnic_group <- as.factor(resilience_group_R0$Ethnic_group)

# Prepare FU1 dataset
resilience_test_FU1 <- resilience_group_R0 %>%
  transmute(
    eid,
    Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")),
    Age = age_BL,
    Ethnic = factor(Ethnic_group, levels = c("0","1","2","3"), labels = c("1_White","2_Asian","3_Black","4_Other")),
    Education = Education_year,
    BMI = BMI_BL,
    site = factor(site),
    self_resilience = self_resilience,                 # continuous
    Mental_symptom_scores_FU1 = FU1_symptoms,
    Trauma_exposure = FU_recent_trauma
  )
resilience_test_FU1 <- na.omit(resilience_test_FU1)

# Linear regression: interaction of continuous resilience and trauma
lme_FU1 <- lm(
  Mental_symptom_scores_FU1 ~ self_resilience * Trauma_exposure + Age + Sex + Ethnic + BMI + site + Education,
  data = resilience_test_FU1
)
summary(lme_FU1)
tidy_results_FU1 <- tidy(lme_FU1)
tidy_results_FU1$conf_int <- confint(lme_FU1)
autoReg(lme_FU1) %>% myft()

# -------------------------------
# FU2 analysis
# -------------------------------
resilience_group_R0 <- resilience_group_R0[, c(
  "eid","age_BL","gender","BMI_BL","Ethnic_group","site","IMD",
  "self_resilience","Depressive_Symptoms_PHQ4_BL","PHQ9_Severity_FU1","PHQ-9_FU2",
  "General_Anxiety_Disorder_Severity_FU1","General_Anxiety_Disorder_Severity_FU2",
  "trauma_num","FU_recent_trauma","FU2_recent_trauma","Education_year"
)]

# Compute FU2 mental symptom score
resilience_group_R0$FU2_symptoms <- resilience_group_R0$`PHQ-9_FU2`/28 + 
  resilience_group_R0$General_Anxiety_Disorder_Severity_FU2/21

resilience_group_R0 <- na.omit(resilience_group_R0)
# Prepare FU2 dataset
resilience_test_FU2 <- resilience_group_R0 %>%
  transmute(
    eid,
    Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")),
    Age = age_BL,
    Ethnic = factor(Ethnic_group, levels = c("0","1","2","3"), labels = c("1_White","2_Asian","3_Black","4_Other")),
    Education = Education_year,
    BMI = BMI_BL,
    site = factor(site),
    self_resilience = self_resilience,                 # continuous
    Mental_symptom_scores_FU2 = FU2_symptoms,
    Trauma_exposure = FU2_recent_trauma
  )

# Cap extreme trauma exposure and remove NAs
resilience_test_FU2$Trauma_exposure[resilience_test_FU2$Trauma_exposure >= 5] <- 5
resilience_test_FU2 <- na.omit(resilience_test_FU2)

# Linear regression: interaction of continuous resilience and trauma
lme_FU2 <- lm(
  Mental_symptom_scores_FU2 ~ self_resilience * Trauma_exposure + Age + Sex + Ethnic + BMI + site + Education,
  data = resilience_test_FU2
)
summary(lme_FU2)
tidy_results_FU2 <- tidy(lme_FU2)
tidy_results_FU2$conf_int <- confint(lme_FU2)
autoReg(lme_FU2) %>% myft()