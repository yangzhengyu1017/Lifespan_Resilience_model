# ============================================================
# Purpose:
#   This script investigates the associations between blood biomarkers
#   and psychological variables (Trauma Exposure, PHQ symptoms, and
#   Resilience) in the UK Biobank dataset. 
#
#   Specifically, it performs:
#     (1) Linear regressions of biomarkers on trauma exposure,
#         depressive symptoms (PHQ), and self-resilience.
#     (2) Multiple testing correction (FDR) and significance filtering.
#     (3) Mediation analysis testing whether biomarkers mediate the 
#         relationship between trauma exposure and depressive symptoms.
#     (4) Visualization of correlation patterns using annotated heatmaps.
#
#   The workflow integrates blood count, biochemistry, and 
#   psychological measures, aiming to identify biological signatures 
#   linked to resilience and affective symptoms following trauma.
# ============================================================


# ===========================
# Environment Setup
# ===========================
library(data.table)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(caret)
library(emmeans)
library(bruceR)
library(nlme)
library(lme4)
library(forestploter)
library(survminer)
library(gridExtra)
library(gtsummary)
library(autoReg)
library(writexl)
library(MatchIt)
library(lavaan)
library(mediation)
library(visreg)
library(broom)

# ===========================
# Data Import and Merge
# ===========================
resilience_group_R <- read.csv("/public/home/yangzy/Documents/Data/UKB/resilience_group_R.csv")
Blood_count <- read.csv("/public/home/yangzy/Documents/Data/UKB/Blood_count.csv")
blood_biochemistry <- read.csv("/public/home/yangzy/Documents/Data/UKB/blood_biochemistry.csv")

# Merge datasets by subject ID
resilience_group_R <- merge(resilience_group_R, Blood_count, by = "eid", all.x = TRUE)
resilience_group_R <- merge(resilience_group_R, blood_biochemistry, by = "eid", all.x = TRUE)

# ===========================
# Variable Definition
# ===========================
t <- "trauma_num"

# Biomarker list (blood count + biochemistry)
biomarker <- c("Baso Count", "Baso Percentage", "Eos Count", "Eos Percentage", "HCT", "HGB",
               "HLSR Count", "HLSR Percentage", "IRF", "Lymph Count", "Lymph Percentage", "MCH", 
               "MCHC", "MCV", "MPV", "MRV", "MSCV", "Mono Count", "Mono Percentage", "Neut Count", 
               "Neut Percentage", "NRBC Count", "NRBC Percentage", "Platelet Count", "PCT", "PDW",
               "RBC Count", "RDW", "Retic Count", "Retic Percentage", "WBC Count",
               "ALT", "Albumin", "ALP", "Apo A", "Apo B", "AST", "CRP", "Calcium", "Cholesterol",
               "Creatinine", "Cystatin C", "Direct Bili", "GGT", "Glucose", "HbA1c", "HDL_C",
               "IGF_1", "LDL_C", "Lp.a.", "Estradiol", "Phosphate", "RF", "SHBG", "Testosterone",
               "Total Bili", "Total Protein", "TG", "Uric Acid", "Urea", "Vitamin D")
biomarker <- gsub(" ", "_", biomarker)

# ===========================
# 1. Trauma × Biomarker Association
# ===========================
trauma_result_table <- data.frame()

for (b in biomarker) {
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
      PHQ = Depressive_Symptoms_PHQ4_BL,
      selfresilience = self_resilience,
      trauma_num,
      !!b := .data[[b]]
    ) %>%
    na.omit()
  
  n_subjects <- nrow(resilience_test)
  lme3 <- lm(as.formula(paste0("`", b, "` ~ trauma_num + Age + Sex + Ethnic + BMI + Education + site")),
             data = resilience_test)
  
  results <- tidy(lme3) %>% 
    mutate(Biomarker = b, Subject_Count = n_subjects)
  
  df <- n_subjects - (length(coef(lme3)) - 1) - 1
  trauma_results <- results %>%
    filter(term == "trauma_num") %>%
    mutate(correlation_r = statistic / sqrt(statistic^2 + df))
  
  trauma_result_table <- bind_rows(trauma_result_table, trauma_results)
}

trauma_result_table <- trauma_result_table %>%
  mutate(Stage = "Blood Count",
         adjusted_p = p.adjust(p.value, method = "BH"))

trauma_result_table_adjusted <- trauma_result_table %>%
  filter(adjusted_p < 0.05)

# ===========================
# 2. PHQ × Biomarker Association
# ===========================
PHQ_results_table <- data.frame()

for (b in biomarker) {
  resilience_test <- resilience_group_R %>% 
    transmute(
      eid,
      Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")),
      Age = age_BL,
      Ethnic = factor(Ethnic_group, levels = c("0", "1", "2", "3"),
                      labels = c("1_White", "2_Asian", "3_Black", "4_Other")),
      Education = Education_year,
      BMI = BMI_BL,
      selfresilience = self_resilience,
      site = factor(site),
      PHQ = Depressive_Symptoms_PHQ4_BL,
      !!b := .data[[b]]
    ) %>%
    na.omit()
  
  n_subjects <- nrow(resilience_test)
  lme3 <- lm(as.formula(paste0("`", b, "` ~ PHQ + Age + Sex + Ethnic + BMI + Education + site")),
             data = resilience_test)
  
  results <- tidy(lme3)
  df <- n_subjects - (length(coef(lme3)) - 1) - 1
  
  PHQ_results <- results %>%
    filter(term == "PHQ") %>%
    mutate(Biomarker = b,
           correlation_r = statistic / sqrt(statistic^2 + df),
           Subject_Count = n_subjects)
  
  PHQ_results_table <- bind_rows(PHQ_results_table, PHQ_results)
}

PHQ_results_table <- PHQ_results_table %>%
  mutate(Stage = "Blood Protein",
         adjusted_p = p.adjust(p.value, method = "BH"))

PHQ_results_table_adjusted <- PHQ_results_table %>%
  filter(adjusted_p < 0.05)

# ===========================
# 3. Self-Resilience × Biomarker Association
# ===========================
R_results_table <- data.frame()

for (b in biomarker) {
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
      !!b := .data[[b]]
    ) %>%
    na.omit()
  
  n_subjects <- nrow(resilience_test)
  lme3 <- lm(as.formula(paste0("`", b, "` ~ selfresilience + Age + Sex + Ethnic + BMI + Education + site")),
             data = resilience_test)
  
  results <- tidy(lme3)
  df <- n_subjects - (length(coef(lme3)) - 1) - 1
  
  R_results <- results %>%
    filter(term == "selfresilience") %>%
    mutate(Biomarker = b,
           correlation_r = statistic / sqrt(statistic^2 + df),
           Subject_Count = n_subjects)
  
  R_results_table <- bind_rows(R_results_table, R_results)
}

R_results_table <- R_results_table %>%
  mutate(Stage = "Blood Protein",
         adjusted_p = p.adjust(p.value, method = "BH"))

R_results_table_adjusted <- R_results_table %>%
  filter(adjusted_p < 0.05)

# ===========================
# 4. Mediation Analysis
# ===========================
both_results_adj <- merge(trauma_result_table, PHQ_results_table, by = "Biomarker", all.x = TRUE)
M <- both_results_adj %>%
  filter(adjusted_p.x < 0.05 & adjusted_p.y < 0.05) %>%
  pull(Biomarker)

# Prepare mediation dataset
blood_dat <- resilience_group_R %>%
  transmute(eid, 
            Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")),
            Age = age_BL, 
            Ethnic = factor(Ethnic_group, levels = c("0", "1", "2", "3"), 
                            labels = c("1_White", "2_Asian", "3_Black", "4_Other")),
            Education = Education_year,
            BMI = BMI_BL,
            selfresilience = self_resilience,
            site = factor(site),
            PHQ = Depressive_Symptoms_PHQ4_BL,
            trauma_num,
            across(all_of(M)))

# Standardize mediators
blood_dat_scale <- blood_dat
blood_dat_scale[, M] <- scale(blood_dat_scale[, M])

# Run mediation for each biomarker
mediation_results_table <- data.frame()

for (biomarker in M) {
  myData <- blood_dat_scale %>%
    transmute(eid, Sex, Age, Ethnic, Education, BMI, selfresilience, site, PHQ, trauma_num, !!biomarker := .data[[biomarker]]) %>%
    na.omit()
  
  model.M <- lm(as.formula(paste0("`", biomarker, "` ~ trauma_num + Sex + Age + Ethnic + Education + BMI + site")), myData)
  model.Y <- lm(as.formula(paste0("PHQ ~ `", biomarker, "` + trauma_num + Sex + Age + Ethnic + Education + BMI + site")), myData)
  
  results <- mediate(model.M, model.Y, treat = "trauma_num", mediator = biomarker, boot = TRUE, sims = 1000)
  
  summary_res <- summary(results)
  
  a <- coef(model.M)["trauma_num"]
  p_a <- summary(model.M)$coefficients["trauma_num", "Pr(>|t|)"]
  b <- coef(model.Y)[biomarker]
  p_b <- summary(model.Y)$coefficients[biomarker, "Pr(>|t|)"]
  c <- coef(model.Y)["trauma_num"]
  p_c <- summary(model.Y)$coefficients["trauma_num", "Pr(>|t|)"]
  
  mediation_results_table <- rbind(
    mediation_results_table,
    data.frame(
      Biomarker = biomarker,
      Estimate = summary_res$d0,
      CI_lower = summary_res$d0.ci[1],
      CI_upper = summary_res$d0.ci[2],
      P_value = summary_res$d0.p,
      a = a, p_a = p_a,
      b = b, p_b = p_b,
      c = c, p_c = p_c
    )
  )
}

# ===========================
# 5. Save Results
# ===========================
write.csv(mediation_results_table,
          "/public/home/yangzy/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/blood_mediation_data_20251023.csv",
          row.names = FALSE)


#file_path <- "/public/home/yangzy/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/blood_mediation_data_1203.csv"
#mediation_results_table <- read.csv(file_path)
mediation_results_table_short <- mediation_results_table %>% 
  filter(P_value < 0.001)
# ----------------------------
# Load required libraries
# ----------------------------
library(ComplexHeatmap)
library(RColorBrewer)
library(dplyr)
library(reshape2)
library(circlize)
library(grid)

# ============================================================
# 1. Prepare data for full correlation heatmap
# ============================================================

# Combine PHQ, trauma, and resilience results
phq_plot_dat <- na.omit(PHQ_results_table)
phq_plot_dat$term <- "Affective Disorders"

trauma_plot_dat <- na.omit(trauma_result_table)
trauma_plot_dat$term <- "Trauma Exposure"

resilience_plot_dat <- na.omit(R_results_table)
resilience_plot_dat$term <- "Resilience"

blood_plot_dat <- rbind(phq_plot_dat, trauma_plot_dat, resilience_plot_dat)

# Add biomarker categories
blood_cate <- read.csv("~/Documents/Data/UKB/blood_parameters_category.csv")
colnames(blood_cate)[2:3] <- c("Biomarker", "Category")
blood_plot_dat <- merge(blood_plot_dat, blood_cate[, c("Biomarker", "Category")], by = "Biomarker", all.x = TRUE)

# Order term levels
blood_plot_dat$term <- factor(blood_plot_dat$term,
                              levels = c("Trauma Exposure", "Affective Disorders", "Resilience"),
                              ordered = TRUE)

# Order biomarkers by category and trauma correlation
trauma_cor <- blood_plot_dat %>%
  filter(term == "Trauma Exposure") %>%
  dplyr::select(Biomarker, Category, correlation_r)

sorted_biomarkers <- blood_plot_dat %>%
  mutate(Category = factor(Category,
                           levels = c("Red blood cell", "Platelet", "White blood cell", "Immunometabolic",
                                      "Bone and joint", "Endocrine", "Liver function", "Renal function"))) %>%
  left_join(trauma_cor %>% dplyr::select(Biomarker, trauma_cor = correlation_r), by = "Biomarker") %>%
  arrange(Category, desc(trauma_cor)) %>%
  pull(Biomarker) %>%
  unique()

# Construct correlation matrix
mat <- dcast(blood_plot_dat, Biomarker ~ term, value.var = "correlation_r")
mat <- mat[match(sorted_biomarkers, mat$Biomarker), ]
rownames(mat) <- mat$Biomarker
mat <- as.matrix(mat[, -1])
colnames(mat) <- levels(blood_plot_dat$term)

# Prepare category vector
row_anno <- blood_plot_dat %>%
  dplyr::select(Biomarker, Category) %>%
  distinct()
row_anno <- row_anno[match(rownames(mat), row_anno$Biomarker), ]
category_vec <- factor(row_anno$Category,
                       levels = c("Red blood cell", "Platelet", "White blood cell", "Immunometabolic",
                                  "Bone and joint", "Endocrine", "Liver function", "Renal function"))

# Define color scales
col_fun <- colorRamp2(
  breaks = c(min(mat, na.rm = TRUE), 0, max(mat, na.rm = TRUE)),
  colors = c("blue", "white", "red")
)
category_colors <- setNames(brewer.pal(length(levels(category_vec)), "Set3"), levels(category_vec))

# Add significance marks
pvalue_mat <- dcast(blood_plot_dat, Biomarker ~ term, value.var = "adjusted_p")
pvalue_mat <- pvalue_mat[match(sorted_biomarkers, pvalue_mat$Biomarker), ]
rownames(pvalue_mat) <- pvalue_mat$Biomarker
pvalue_mat <- as.matrix(pvalue_mat[, -1])
colnames(pvalue_mat) <- levels(blood_plot_dat$term)

add_significance <- function(p) {
  ifelse(p < 0.001, "***",
         ifelse(p < 0.01, "**",
                ifelse(p < 0.05, "*", "")))
}

cell_text <- function(j, i, x, y, width, height, fill) {
  r <- round(mat[i, j], 3)
  p <- pvalue_mat[i, j]
  sig <- add_significance(p)
  grid.text(sprintf("%.3f %s", r, sig), x, y, gp = gpar(fontsize = 8))
}

# Row annotation
right_anno <- HeatmapAnnotation(
  Class = category_vec,
  which = "row",
  show_annotation_name = FALSE,
  col = list(Class = category_colors)
)

# Draw main heatmap
cor_heatmap <- Heatmap(
  mat,
  name = "Correlation",
  col = col_fun,
  cluster_rows = FALSE,
  cluster_columns = FALSE,
  row_split = category_vec,
  row_names_gp = gpar(fontsize = 8),
  column_names_gp = gpar(fontsize = 10),
  column_names_rot = 45,
  cell_fun = cell_text,
  right_annotation = right_anno
)
draw(cor_heatmap, heatmap_legend_side = "right", annotation_legend_side = "right")

# ============================================================
# 2. Mediation subset heatmap (only trauma & resilience)
# ============================================================

# Add mediation p-values
colnames(mediation_results_table_short)[5] <- "mediation_p"
merge_mediation <- mediation_results_table_short[, c("Biomarker", "mediation_p")]

phq_plot_dat <- merge(PHQ_results_table, merge_mediation, by = "Biomarker", all.x = TRUE)
trauma_plot_dat <- merge(trauma_result_table, merge_mediation, by = "Biomarker", all.x = TRUE)
resilience_plot_dat <- merge(R_results_table, merge_mediation, by = "Biomarker", all.x = TRUE)

phq_plot_dat$term <- "Affective Disorders"
trauma_plot_dat$term <- "Trauma Exposure"
resilience_plot_dat$term <- "Resilience"

blood_plot_dat <- rbind(phq_plot_dat, trauma_plot_dat, resilience_plot_dat)
blood_plot_dat <- merge(blood_plot_dat, blood_cate[, c("Biomarker", "Category")], by = "Biomarker", all.x = TRUE)
blood_plot_dat <- subset(blood_plot_dat, term != "Affective Disorders")

# Reorder factors and repeat heatmap construction steps
blood_plot_dat$term <- factor(blood_plot_dat$term,
                              levels = c("Trauma Exposure", "Resilience"),
                              ordered = TRUE)

trauma_cor <- blood_plot_dat %>%
  filter(term == "Trauma Exposure") %>%
  dplyr::select(Biomarker, Category, correlation_r)

sorted_biomarkers <- blood_plot_dat %>%
  mutate(Category = factor(Category,
                           levels = c("Red blood cell", "Platelet", "White blood cell", "Immunometabolic",
                                      "Bone and joint", "Endocrine", "Liver function", "Renal function"))) %>%
  left_join(trauma_cor %>% dplyr::select(Biomarker, trauma_cor = correlation_r), by = "Biomarker") %>%
  arrange(Category, desc(trauma_cor)) %>%
  pull(Biomarker) %>%
  unique()

mat <- dcast(blood_plot_dat, Biomarker ~ term, value.var = "correlation_r")
mat <- mat[match(sorted_biomarkers, mat$Biomarker), ]
rownames(mat) <- mat$Biomarker
mat <- as.matrix(mat[, -1])
colnames(mat) <- levels(blood_plot_dat$term)

row_anno <- blood_plot_dat %>%
  dplyr::select(Biomarker, Category) %>%
  distinct()
row_anno <- row_anno[match(rownames(mat), row_anno$Biomarker), ]
category_vec <- factor(row_anno$Category,
                       levels = c("Red blood cell", "Platelet", "White blood cell", "Immunometabolic",
                                  "Bone and joint", "Endocrine", "Liver function", "Renal function"))

pvalue_mat <- dcast(blood_plot_dat, Biomarker ~ term, value.var = "adjusted_p")
pvalue_mat <- pvalue_mat[match(sorted_biomarkers, pvalue_mat$Biomarker), ]
rownames(pvalue_mat) <- pvalue_mat$Biomarker
pvalue_mat <- as.matrix(pvalue_mat[, -1])
colnames(pvalue_mat) <- levels(blood_plot_dat$term)

cor_heatmap <- Heatmap(
  mat,
  name = "Correlation",
  col = col_fun,
  cluster_rows = FALSE,
  cluster_columns = FALSE,
  row_split = category_vec,
  row_names_gp = gpar(fontsize = 8),
  column_names_gp = gpar(fontsize = 10),
  column_names_rot = 45,
  cell_fun = cell_text,
  right_annotation = right_anno
)

# Export (7 × 6 inch)
draw(cor_heatmap, heatmap_legend_side = "right", annotation_legend_side = "right")