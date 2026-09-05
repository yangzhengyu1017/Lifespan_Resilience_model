# ==============================================================================
# Script: A5_2_Pre_Resilience_Validation_Predicted_Resiliece_BRS_Neu_Blood.R
# Project: Lifespan Resilience Modeling (STM Submission)
# Purpose: Validate predicted resilience against Brief Resilience Scale, inverted neuroticism, and peripheral blood biomarkers using Steigers Z-tests and permutation testing.
# ==============================================================================


# Ensure output directory exists
if (!dir.exists("results")) dir.create("results", recursive = TRUE)

# ============================================================
# Title: Comprehensive Master Analysis (PLSR vs True vs Reversed Neuroticism)
# Purpose:
#   1. Train PLSR & Clean Data (Strictly Test Set).
#   2. Master Summary Table (N, r, p, FDR_p for all markers + Steiger's Z).
#   3. Combined Heatmap (Trauma, Predicted, True, Reversed Neuroticism).
#   4. Permutation Tests for Global Patterns (S).
# ============================================================

# ---------- 0. Environment Setup ----------
if(!require(pacman)) install.packages("pacman")
pacman::p_load(
  data.table, tidyverse, rstatix, pls, caret, broom, purrr, cocor,
  ComplexHeatmap, RColorBrewer, reshape2, circlize, grid
)

# ---------- 1. Data Import & PLSR Modeling ----------
cat("Step 1: Loading data and training PLSR model...\n")
resilience_corr_rename_dat <- utils::read.csv("data/UKB_dat_for_resilience_model_1224.csv")
resilience_group_R <- readr::read_csv("data/UKB_dat_for_analysis_1226.csv")[,-1]
Blood_count <- utils::read.csv("data/Blood_count.csv")
blood_biochemistry <- utils::read.csv("data/blood_biochemistry.csv")

BRS_train_dat <- resilience_corr_rename_dat %>%
  dplyr::transmute(
    eid, gender, age_BL, Education_year, HH_Num_Vehicle_BL, HH_Income_BL, 
    HH_Own_Rent_BL, Able_Pay_Rent_Mortgage_FU1, Financial_Difficulties, IMD,
    Num_People_Living_BL, Been_In_Confiding_Relationship_FU1, Belittlement_Adult_FU1, 
    Marital_Separation, Live_with_partner, Live_with_children, Live_with_siblings, 
    Live_with_parents, Live_with_grandchild, Live_with_related, Live_with_unrelated, 
    Live_alone, Social_Freq_Visits_BL, Social_Able_Confide_BL, Loneliness_BL, 
    Num_social_activity, Breastfed_Baby_BL, Comp_Body_Size_Age_10_BL, 
    Comp_Height_Size_Age_10_BL, Maternal_Smoking_Birth_BL, Felt_Loved_As_Child_FU1, 
    Phys_Abused_As_Child_FU1, Felt_Hated_As_Child_FU1, Sex_Molested_As_Child_FU1,
    Someone_Take_To_Doctor_As_Child_FU1, MET_Minutes_Per_Week_Moderate_Activity_BL, 
    MET_Minutes_Per_Week_Vigorous_Activity_BL, MET_Minutes_Per_Week_Walking_BL, 
    Morning_Evening_Person_BL, TV_Time_BL, Computer_Time_BL, Big5_warmth, 
    Big5_diligence, Big5_nervousness, Big5_curiosity, Big5_sociability,
    Health_Satisfaction_BL, Family_Relationship_Satisfaction_BL, 
    Friendships_Satisfaction_BL, Financial_Situation_Satisfaction_BL,
    self_resilience
  ) %>% stats::na.omit()

train_test <- resilience_corr_rename_dat %>%
  dplyr::transmute(eid, PHQ_FU1 = PHQ9_Severity_FU1, PHQ_FU2 = `PHQ.9_FU2`,
                   GAD_FU1 = General_Anxiety_Disorder_Severity_FU1, GAD_FU2 = General_Anxiety_Disorder_Severity_FU2) %>%
  dplyr::mutate(completed = base::ifelse(base::rowSums(is.na(.)) == 0, 1, 0))

BRS_train_dat <- dplyr::left_join(BRS_train_dat, train_test[c("eid", "completed")], by = "eid")

base::set.seed(123)
selected_rows <- base::sample(base::which(BRS_train_dat$completed == 1), 7685)
BRS_train_dat$completed <- 0
BRS_train_dat$completed[selected_rows] <- 1

meats_train <- BRS_train_dat[BRS_train_dat$completed == 0, ]
meats_test  <- BRS_train_dat[BRS_train_dat$completed == 1, ]

X_cols <- base::colnames(BRS_train_dat)[2:50]
Y_col  <- "self_resilience"
all_cols <- c(X_cols, Y_col)

preProc_params <- caret::preProcess(meats_train[, all_cols], method = c("center", "scale"))
meats_train[, all_cols] <- stats::predict(preProc_params, meats_train[, all_cols])
meats_test[, all_cols]  <- stats::predict(preProc_params, meats_test[, all_cols])

my_plsr <- pls::plsr(as.matrix(meats_train[Y_col]) ~ as.matrix(meats_train[X_cols]), ncomp = 49, scale = FALSE, validation = "CV")
ncomp.opt <- pls::selectNcomp(my_plsr, method = "randomization", plot = FALSE)
best_model <- pls::plsr(as.matrix(meats_train[Y_col]) ~ as.matrix(meats_train[X_cols]), ncomp = ncomp.opt, scale = FALSE)

meats_test$predicted_resilience <- as.numeric(stats::predict(best_model, ncomp = ncomp.opt, newdata = as.matrix(meats_test[X_cols])))

# ---------- 2. Merge Biological Data ----------
cat("Step 2: Merging data for Test Set...\n")
analysis_data <- resilience_group_R %>%
  dplyr::inner_join(meats_test %>% dplyr::select(eid, predicted_resilience), by = "eid") %>%
  dplyr::left_join(Blood_count, by = "eid") %>%
  dplyr::left_join(blood_biochemistry, by = "eid") %>%
  dplyr::mutate(
    Ethnic_group = dplyr::case_when(
      Ethnic_group %in% c(3, 5) ~ 2,  
      Ethnic_group == 4 ~ 3,          
      Ethnic_group %in% c(2, 6) ~ 4,  
      TRUE ~ Ethnic_group             
    ),
    Ethnic_group = Ethnic_group - 1
  )

base::colnames(analysis_data) <- base::gsub(" ", "_", base::colnames(analysis_data))
analysis_data_clean <- analysis_data %>%
  dplyr::mutate(
    Sex = base::factor(gender, levels = c(0, 1), labels = c("Female", "Male")),
    Age = age_BL,
    Ethnic = base::factor(Ethnic_group, levels = c(0, 1, 2, 3), labels = c("1_White", "2_Asian", "3_Black", "4_Other")),
    Education = Education_year, BMI = BMI_BL, site = base::factor(site)
  )

covs <- c("Age", "Sex", "Ethnic", "BMI", "Education", "site")
covariate_str <- base::paste(covs, collapse = " + ")

# ---------- 3. Load Mediation Biomarkers & Define Cohort ----------
cat("Step 3: Creating Master Table for Mediating Biomarkers...\n")
mediation_file <- "data/blood_mediation_data_1203.csv"
M_biomarkers <- read.csv(mediation_file) %>% filter(P_value < 0.001) %>% pull(Biomarker)

# Invert Neuroticism score (multiply by -1) to align with emotional stability / resilience
compare_data <- analysis_data_clean %>%
  dplyr::filter(complete.cases(Age, Sex, Ethnic, BMI, Education, site, 
                               trauma_num, predicted_resilience, self_resilience, Big5_nervousness)) %>%
  dplyr::mutate(Inv_Nervousness = -1 * Big5_nervousness)

# ============================================================
# Part 1: Generate Master Summary Table
# ============================================================
master_results <- list()

# Pre-calculate residual partial correlations for Steiger's Z-test (using inverted neuroticism)
fit_pred_base <- lm(as.formula(paste("predicted_resilience ~", covariate_str)), data = compare_data)
fit_true_base <- lm(as.formula(paste("self_resilience ~", covariate_str)), data = compare_data)
fit_nerv_base <- lm(as.formula(paste("Inv_Nervousness ~", covariate_str)), data = compare_data)
r_kh_true <- cor(fit_pred_base$residuals, fit_true_base$residuals)
r_kh_nerv <- cor(fit_pred_base$residuals, fit_nerv_base$residuals)

for (b in M_biomarkers) {
  temp_data <- compare_data %>%
    dplyr::select(dplyr::all_of(c(b, "trauma_num", "predicted_resilience", "self_resilience", "Inv_Nervousness", covs))) %>%
    stats::na.omit()
  
  n_subj <- nrow(temp_data)
  if (n_subj < 50) next
  
  extract_stats <- function(fit, term_name) {
    tidy(fit) %>% filter(term == term_name) %>% 
      mutate(r = statistic / sqrt(statistic^2 + fit$df.residual)) %>% dplyr::select(r, p.value)
  }
  
  # Fit linear models for core indicators
  f_T <- lm(as.formula(paste0("`", b, "` ~ trauma_num + ", covariate_str)), data = temp_data)
  f_P <- lm(as.formula(paste0("`", b, "` ~ predicted_resilience + ", covariate_str)), data = temp_data)
  f_R <- lm(as.formula(paste0("`", b, "` ~ self_resilience + ", covariate_str)), data = temp_data)
  f_N <- lm(as.formula(paste0("`", b, "` ~ Inv_Nervousness + ", covariate_str)), data = temp_data)
  
  res_T <- extract_stats(f_T, "trauma_num")
  res_P <- extract_stats(f_P, "predicted_resilience")
  res_R <- extract_stats(f_R, "self_resilience")
  res_N <- extract_stats(f_N, "Inv_Nervousness")
  
  # Compute Steiger's Z-test for comparing dependent correlations
  z_true <- cocor.dep.groups.overlap(r.jk = res_P$r, r.jh = res_R$r, r.kh = r_kh_true, n = n_subj)
  z_nerv <- cocor.dep.groups.overlap(r.jk = res_P$r, r.jh = res_N$r, r.kh = r_kh_nerv, n = n_subj)
  
  master_results[[length(master_results) + 1]] <- data.frame(
    Biomarker = b, N = n_subj,
    r_Trauma = res_T$r, p_Trauma = res_T$p.value,
    r_Pred = res_P$r, p_Pred = res_P$p.value,
    r_True = res_R$r, p_True = res_R$p.value,
    r_Nerv = res_N$r, p_Nerv = res_N$p.value,
    Steiger_Z_True = z_true@steiger1980$statistic, Steiger_P_True = z_true@steiger1980$p.value,
    Steiger_Z_Nerv = z_nerv@steiger1980$statistic, Steiger_P_Nerv = z_nerv@steiger1980$p.value
  )
}

master_df <- bind_rows(master_results)

# Apply false discovery rate (FDR) adjustment across tests
master_df <- master_df %>%
  mutate(
    adjP_Trauma = p.adjust(p_Trauma, method = "BH"),
    adjP_Pred   = p.adjust(p_Pred, method = "BH"),
    adjP_True   = p.adjust(p_True, method = "BH"),
    adjP_Nerv   = p.adjust(p_Nerv, method = "BH"),
    Steiger_adjP_True = p.adjust(Steiger_P_True, method = "BH"),
    Steiger_adjP_Nerv = p.adjust(Steiger_P_Nerv, method = "BH")
  )

# Reorder columns for reporting
master_df <- master_df %>% dplyr::select(
  Biomarker, N,
  r_Trauma, p_Trauma, adjP_Trauma,
  r_Pred, p_Pred, adjP_Pred,
  r_True, p_True, adjP_True,
  r_Nerv, p_Nerv, adjP_Nerv,
  Steiger_Z_True, Steiger_P_True, Steiger_adjP_True,
  Steiger_Z_Nerv, Steiger_P_Nerv, Steiger_adjP_Nerv
)

write.csv(master_df, "results/Master_Summary_Table.csv", row.names = FALSE)
cat("=> Master Table generated: Master_Summary_Table.csv\n")

# ============================================================
# Part 2: Generate Combined Biomarker Heatmap
# ============================================================
cat("Step 4: Drawing Combined Heatmap...\n")

# Reshape wide table to long format for heatmap generation
hm_long <- master_df %>%
  dplyr::select(Biomarker, r_Trauma, r_Pred, r_True, r_Nerv, 
                p_Trauma, p_Pred, p_True, p_Nerv,
                adjP_Trauma, adjP_Pred, adjP_True, adjP_Nerv) %>%
  pivot_longer(cols = -Biomarker, 
               names_to = c(".value", "term"), 
               names_pattern = "(.*)_(.*)") %>%
  mutate(term = recode(term, 
                       "Trauma" = "Trauma Exposure", 
                       "Pred" = "Predicted Resilience", 
                       "True" = "True Resilience", 
                       "Nerv" = "Reversed Neuroticism"))

hm_long$term <- factor(hm_long$term, levels = c("Trauma Exposure", "Predicted Resilience", "True Resilience", "Reversed Neuroticism"), ordered = TRUE)

# Load biomarker category annotations
blood_cate <- read.csv("data/blood_parameters_category.csv")
colnames(blood_cate)[2:3] <- c("Biomarker", "Category")
hm_long <- merge(hm_long, blood_cate[, c("Biomarker", "Category")], by = "Biomarker", all.x = TRUE)

# Sort by category and correlation with trauma exposure
sorted_biomarkers <- hm_long %>%
  filter(term == "Trauma Exposure") %>%
  mutate(Category = factor(Category, levels = c("Red blood cell", "Platelet", "White blood cell", "Immunometabolic",
                                                "Bone and joint", "Endocrine", "Liver function", "Renal function"))) %>%
  arrange(Category, desc(r)) %>%
  pull(Biomarker)

# Construct matrices for estimates, raw p-values, and adjusted p-values
mat_r <- dcast(hm_long, Biomarker ~ term, value.var = "r")
mat_r <- as.matrix(mat_r[match(sorted_biomarkers, mat_r$Biomarker), -1])
rownames(mat_r) <- sorted_biomarkers

mat_adjP <- dcast(hm_long, Biomarker ~ term, value.var = "adjP")
mat_adjP <- as.matrix(mat_adjP[match(sorted_biomarkers, mat_adjP$Biomarker), -1])
rownames(mat_adjP) <- sorted_biomarkers

mat_rawP <- dcast(hm_long, Biomarker ~ term, value.var = "p")
mat_rawP <- as.matrix(mat_rawP[match(sorted_biomarkers, mat_rawP$Biomarker), -1])
rownames(mat_rawP) <- sorted_biomarkers

# Heatmap display configuration
row_anno <- hm_long %>% dplyr::select(Biomarker, Category) %>% distinct()
row_anno <- row_anno[match(rownames(mat_r), row_anno$Biomarker), ]
category_vec <- factor(row_anno$Category, levels = c("Red blood cell", "Platelet", "White blood cell", "Immunometabolic",
                                                     "Bone and joint", "Endocrine", "Liver function", "Renal function"))

col_fun <- colorRamp2(breaks = c(min(mat_r, na.rm = TRUE), 0, max(mat_r, na.rm = TRUE)), colors = c("blue", "white", "red"))
category_colors <- setNames(brewer.pal(length(levels(category_vec)), "Set3"), levels(category_vec))

add_significance <- function(raw_p, adj_p) {
  if (is.na(adj_p) || is.na(raw_p)) return("")
  if (adj_p < 0.01) return("**")
  if (adj_p < 0.05) return("*")
  if (raw_p < 0.05) return("+")
  return("")
}

cell_text <- function(j, i, x, y, width, height, fill) {
  val_r <- round(mat_r[i, j], 3)
  val_adj <- mat_adjP[i, j]
  val_raw <- mat_rawP[i, j]
  sig <- add_significance(val_raw, val_adj)
  grid.text(sprintf("%.3f %s", val_r, sig), x, y, gp = gpar(fontsize = 8))
}

right_anno <- HeatmapAnnotation(Class = category_vec, which = "row", show_annotation_name = FALSE, col = list(Class = category_colors))

# Render and save heatmap as high-resolution PDF
pdf("results/Combined_Biomarker_Heatmap.pdf", width = 11, height = 8) 
cor_heatmap <- Heatmap(
  mat_r, name = "Correlation", col = col_fun, cluster_rows = FALSE, cluster_columns = FALSE,
  row_split = category_vec, row_names_gp = gpar(fontsize = 8), column_names_gp = gpar(fontsize = 10),
  column_names_rot = 45, cell_fun = cell_text, right_annotation = right_anno,
  column_title = "Correlation Patterns across Models"
)
draw(cor_heatmap, heatmap_legend_side = "right", annotation_legend_side = "right")
dev.off()
cat("=> Combined Heatmap generated: Combined_Biomarker_Heatmap.pdf\n")


# ============================================================
# Part 3: Global Biomarker Pattern Permutation Test
# ============================================================
cat("\nStep 5: Starting Permutation tests for Global Patterns (S)...\n")
cat("This will run 1000 iterations and save the permutation values...\n")

# Compute observed pattern statistic (S_obs) 
S_obs_Pred <- cor(master_df$r_Trauma, master_df$r_Pred, use = "complete.obs")
S_obs_True <- cor(master_df$r_Trauma, master_df$r_True, use = "complete.obs")
S_obs_Nerv <- cor(master_df$r_Trauma, master_df$r_Nerv, use = "complete.obs")

cat(sprintf("Observed S (Pred): %.3f\n", S_obs_Pred))
cat(sprintf("Observed S (True): %.3f\n", S_obs_True))
cat(sprintf("Observed S (Reversed Nerv): %.3f\n", S_obs_Nerv))

n_perm <- 1000
S_perm_Pred <- numeric(n_perm)
S_perm_True <- numeric(n_perm)
S_perm_Nerv <- numeric(n_perm)

set.seed(789)
for (i in 1:n_perm) {
  # Permute predictor while preserving biomarker-covariate relationships 
  perm_idx <- sample(1:nrow(compare_data))
  perm_data <- compare_data
  perm_data[, c("trauma_num", "predicted_resilience", "self_resilience", "Inv_Nervousness")] <- 
    compare_data[perm_idx, c("trauma_num", "predicted_resilience", "self_resilience", "Inv_Nervousness")]
  
  r_T_p <- numeric(length(M_biomarkers))
  r_P_p <- numeric(length(M_biomarkers))
  r_R_p <- numeric(length(M_biomarkers))
  r_N_p <- numeric(length(M_biomarkers))
  
  for (j in seq_along(M_biomarkers)) {
    b <- M_biomarkers[j]
    temp <- perm_data %>% dplyr::select(all_of(c(b, "trauma_num", "predicted_resilience", "self_resilience", "Inv_Nervousness", covs))) %>% na.omit()
    if(nrow(temp) < 50) next
    
    # Fast vectorized computation of correlation coefficients 
    df_res <- nrow(temp) - length(covs) - 2
    f_T <- summary(lm(as.formula(paste0("`", b, "` ~ trauma_num + ", covariate_str)), data = temp))$coefficients["trauma_num", "t value"]
    f_P <- summary(lm(as.formula(paste0("`", b, "` ~ predicted_resilience + ", covariate_str)), data = temp))$coefficients["predicted_resilience", "t value"]
    f_R <- summary(lm(as.formula(paste0("`", b, "` ~ self_resilience + ", covariate_str)), data = temp))$coefficients["self_resilience", "t value"]
    f_N <- summary(lm(as.formula(paste0("`", b, "` ~ Inv_Nervousness + ", covariate_str)), data = temp))$coefficients["Inv_Nervousness", "t value"]
    
    r_T_p[j] <- f_T / sqrt(f_T^2 + df_res)
    r_P_p[j] <- f_P / sqrt(f_P^2 + df_res)
    r_R_p[j] <- f_R / sqrt(f_R^2 + df_res)
    r_N_p[j] <- f_N / sqrt(f_N^2 + df_res)
  }
  
  S_perm_Pred[i] <- cor(r_T_p, r_P_p, use = "complete.obs")
  S_perm_True[i] <- cor(r_T_p, r_R_p, use = "complete.obs")
  S_perm_Nerv[i] <- cor(r_T_p, r_N_p, use = "complete.obs")
}

# Calculate two-sided permutation p-values
pval_Pred <- mean(abs(S_perm_Pred) >= abs(S_obs_Pred), na.rm = TRUE)
pval_True <- mean(abs(S_perm_True) >= abs(S_obs_True), na.rm = TRUE)
pval_Nerv <- mean(abs(S_perm_Nerv) >= abs(S_obs_Nerv), na.rm = TRUE)

cat("\n================ Permutation P-Values ================\n")
cat(sprintf("Predicted Resilience: P = %.4f\n", pval_Pred))
cat(sprintf("True Resilience:      P = %.4f\n", pval_True))
cat(sprintf("Reversed Neuroticism: P = %.4f\n", pval_Nerv))
cat("========================================================\n")

# Export permutation null distribution for visualization
perm_export <- data.frame(
  Iteration = 1:n_boot, 
  S_perm_Predicted = S_perm_Pred, 
  S_perm_True = S_perm_True, 
  S_perm_Nervousness = S_perm_Nerv
)
write.csv(perm_export, "results/Permutation_S_Values_Combined.csv", row.names = FALSE)
cat("=> Permutation values saved to: Permutation_S_Values_Combined.csv\n")

# Convert permutation data to long format for faceted ggplot2 histogram
perm_long <- perm_export %>%
  pivot_longer(cols = starts_with("S_perm_"), 
               names_to = "Model", 
               names_prefix = "S_perm_", 
               values_to = "S_value") %>%
  mutate(Model = recode(Model, 
                        "Predicted" = "Predicted Resilience", 
                        "True" = "True Resilience", 
                        "Nervousness" = "Reversed Neuroticism"))

# Order model factors to place Predicted Resilience on the left
perm_long$Model <- factor(perm_long$Model, levels = c("Predicted Resilience", "True Resilience", "Reversed Neuroticism"))

# Data frame for observed statistic reference lines
obs_vals <- data.frame(
  Model = factor(c("Predicted Resilience", "True Resilience", "Reversed Neuroticism"), 
                 levels = c("Predicted Resilience", "True Resilience", "Reversed Neuroticism")),
  S_obs = c(S_obs_Pred, S_obs_True, S_obs_Nerv)
)

# Plot three-panel permutation null distribution histograms
p_hist <- ggplot(perm_long, aes(x = S_value)) +
  geom_histogram(bins = 40, fill = "skyblue", color = "white", alpha = 0.9) +
  geom_vline(data = obs_vals, aes(xintercept = S_obs), color = "red", linetype = "dashed", linewidth = 1.2) +
  facet_wrap(~ Model, scales = "free_x") +
  labs(
    title = "Permutation Distributions of Global Correlation Patterns (S)",
    x = "Permuted Correlation S (r_Trauma vs r_Model)", 
    y = "Frequency"
  ) +
  theme_bw(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "grey90", color = "black"),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Save combined histogram as PDF
ggsave("results/Combined_Permutation_Histograms.pdf", plot = p_hist, width = 12, height = 4)
cat("=> Combined Permutation Histograms generated: Combined_Permutation_Histograms.pdf\n")