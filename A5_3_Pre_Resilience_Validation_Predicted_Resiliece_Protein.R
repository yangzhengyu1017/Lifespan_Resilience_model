# ==============================================================================
# Script: A5_3_Pre_Resilience_Validation_Predicted_Resiliece_Protein.R
# Project: Lifespan Resilience Modeling (STM Submission)
# Purpose: Validate predicted resilience against plasma proteomic profiles, assessing correlation patterns with trauma and resilience via high-throughput regression and permutation.
# ==============================================================================


# Ensure output directory exists
if (!dir.exists("results")) dir.create("results", recursive = TRUE)

# ============================================================
# Title: UK Biobank - Predicted Resilience & Protein Biomarkers
# Purpose:
#   1. Train PLSR model to generate 'predicted_resilience'.
#   2. Load normalized protein data (.mat) and merge.
#   3. High-throughput linear regressions adjusting for covariates.
#   4. Heatmap visualization for Trauma and Predicted Resilience 
#      (focusing only on pre-identified significant mediator proteins).
#   5. Corrected Permutation Test for correlation similarity.
# ============================================================

# ---------- 0. Environment Setup ----------
if(!require(pacman)) install.packages("pacman")
pacman::p_load(
  R.matlab, data.table, tidyverse, rstatix, pls, caret, broom, purrr, 
  ComplexHeatmap, RColorBrewer, reshape2, circlize, grid
)

# ---------- 1. Data Import & PLSR Predicted Resilience ----------
# 1.1 Import resilience modeling data and demographic covariates
resilience_corr_rename_dat <- utils::read.csv("data/UKB_dat_for_resilience_model_1224.csv")
resilience_group_R <- readr::read_csv("data/UKB_dat_for_analysis_1226.csv")[,-1]

# 1.2 Train PLSR model on training set to generate predicted resilience scores
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
  dplyr::transmute(eid, completed = base::ifelse(base::rowSums(is.na(dplyr::select(., PHQ9_Severity_FU1, `PHQ.9_FU2`, General_Anxiety_Disorder_Severity_FU1, General_Anxiety_Disorder_Severity_FU2))) == 0, 1, 0))

BRS_train_dat <- dplyr::left_join(BRS_train_dat, train_test, by = "eid")
base::set.seed(123)
selected_rows <- base::sample(base::which(BRS_train_dat$completed == 1), 7685)
BRS_train_dat$completed <- 0
BRS_train_dat$completed[selected_rows] <- 1

meats_train <- BRS_train_dat[BRS_train_dat$completed == 0, ]
meats_test  <- BRS_train_dat[BRS_train_dat$completed == 1, ]

X_cols <- base::colnames(BRS_train_dat)[2:50]
Y_col  <- "self_resilience"

preProc_params <- caret::preProcess(meats_train[, c(X_cols, Y_col)], method = c("center", "scale"))
meats_train[, c(X_cols, Y_col)] <- stats::predict(preProc_params, meats_train[, c(X_cols, Y_col)])
meats_test[, c(X_cols, Y_col)]  <- stats::predict(preProc_params, meats_test[, c(X_cols, Y_col)])

my_plsr <- pls::plsr(as.matrix(meats_train[Y_col]) ~ as.matrix(meats_train[X_cols]), ncomp = 49, scale = FALSE, validation = "CV")
ncomp.opt <- pls::selectNcomp(my_plsr, method = "randomization", plot = FALSE)
best_model <- pls::plsr(as.matrix(meats_train[Y_col]) ~ as.matrix(meats_train[X_cols]), ncomp = ncomp.opt, scale = FALSE)
meats_test$predicted_resilience <- as.numeric(stats::predict(best_model, ncomp = ncomp.opt, newdata = as.matrix(meats_test[X_cols])))

# ---------- 2. Load and Prepare Protein Data ----------
cat("Loading protein data...\n")
mat_data <- readMat("data/UKB_Protein_norm.mat")
proteinic_dat <- as.data.frame(mat_data$ukb.protein.norm)[, -1]
colnames(proteinic_dat)[1] <- "eid"

proteinic_name <- read_csv("data/id.csv")
new_colnames <- as.character(proteinic_name[[1]])

if (length(new_colnames) == (ncol(proteinic_dat) - 1)) {
  colnames(proteinic_dat)[2:ncol(proteinic_dat)] <- new_colnames
} else {
  stop("Number of protein names does not match dataset columns.")
}
# Extract protein identifiers and sanitize names for model formulas
protein_list <- new_colnames
colnames(proteinic_dat) <- make.names(colnames(proteinic_dat), unique = TRUE)
protein_list_clean <- colnames(proteinic_dat)[-1] 

# ---------- 3. Merge Protein and Covariate Data ----------
analysis_data <- resilience_group_R %>%
  dplyr::left_join(meats_test %>% dplyr::select(eid, predicted_resilience), by = "eid") %>%
  dplyr::inner_join(proteinic_dat, by = "eid") %>%
  dplyr::mutate(
    # Recode ethnic categories into standard numerical indicators
    Ethnic_group = dplyr::case_when(
      Ethnic_group %in% c(3, 5) ~ 2,  # Asian & Chinese -> 2
      Ethnic_group == 4 ~ 3,          # Black -> 3
      Ethnic_group %in% c(2, 6) ~ 4,  # Mixed & Other -> 4
      TRUE ~ Ethnic_group             # White
    ),
    Ethnic_group = Ethnic_group - 1,  # Shift index to 0-3
    Sex = base::factor(gender, levels = c(0, 1), labels = c("Female", "Male")),
    Age = age_BL,
    Ethnic = base::factor(Ethnic_group, levels = c(0, 1, 2, 3), labels = c("1_White", "2_Asian", "3_Black", "4_Other")),
    Education = Education_year,
    BMI = BMI_BL,
    site = base::factor(site)
  )

covs <- c("Age", "Sex", "Ethnic", "BMI", "Education", "site")

# ---------- 4. High-Throughput Linear Regressions ----------
cat("Running linear models for nearly 3000 proteins. This may take a moment...\n")

# ---------- 4. High-Throughput Linear Regressions ----------
cat("Running linear models for nearly 3000 proteins. This may take a moment...\n")

run_batch_lm <- function(data, biomarkers, main_predictor, paired_predictor, covariates, stage_name) {
  covariate_str <- base::paste(covariates, collapse = " + ")
  
  purrr::map_dfr(biomarkers, function(b) {
    formula_str <- base::paste0("`", b, "` ~ ", main_predictor, " + ", covariate_str)
    
    # Enforce pairwise complete cases across predictor pairs and covariates
    model_data <- data %>% 
      dplyr::select(eid, dplyr::all_of(c(b, main_predictor, paired_predictor, covariates))) %>% 
      stats::na.omit()
    
    n_subj <- base::nrow(model_data)
    if(n_subj < 50) return(NULL)
    
    fit <- stats::lm(stats::as.formula(formula_str), data = model_data)
    broom::tidy(fit) %>%
      dplyr::filter(term == main_predictor) %>%
      dplyr::mutate(Biomarker = b, Subject_Count = n_subj,
                    correlation_r = statistic / base::sqrt(statistic^2 + fit$df.residual))
  }) %>% dplyr::mutate(Stage = stage_name, adjusted_p = stats::p.adjust(p.value, method = "BH"))
}

# 4.1 Trauma Exposure regressions
trauma_results <- run_batch_lm(analysis_data, protein_list_clean, "trauma_num", "predicted_resilience", covs, "Trauma_Effect")

# 4.2 Predicted Resilience regressions
resilience_results <- run_batch_lm(analysis_data, protein_list_clean, "predicted_resilience", "trauma_num", covs, "Predicted_Resilience_Effect")

# ---------- 5. Filter for Significant Mediator Proteins ----------
mediation_file <- "data/protein_mediation_data.csv"
mediation_df <- read.csv(mediation_file)

# Standardize biomarker names in mediation table for consistent merging
mediation_df$Biomarker <- make.names(mediation_df$Biomarker, unique = TRUE)
M_biomarkers <- mediation_df %>% dplyr::filter(P_value < 0.001) %>% dplyr::pull(Biomarker)

cat(sprintf("Found %d significant mediator proteins (P < 0.001).\n", length(M_biomarkers)))

# ---------- 6. Heatmap Visualization (Proteins) ----------
cat("Preparing Heatmap with Dual P-value Significance Markers...\n")

trauma_hm_dat <- trauma_results %>% filter(Biomarker %in% M_biomarkers) %>% mutate(term = "Trauma Exposure")
resil_hm_dat <- resilience_results %>% filter(Biomarker %in% M_biomarkers) %>% mutate(term = "Predicted Resilience")
blood_plot_dat <- bind_rows(trauma_hm_dat, resil_hm_dat)
blood_plot_dat$term <- factor(blood_plot_dat$term, levels = c("Trauma Exposure", "Predicted Resilience"), ordered = TRUE)

# Order biomarkers by correlation with trauma exposure (descending)
sorted_biomarkers <- blood_plot_dat %>%
  filter(term == "Trauma Exposure") %>%
  arrange(desc(correlation_r)) %>%
  pull(Biomarker)

# 1. Correlation coefficient matrix (r)
mat <- dcast(blood_plot_dat, Biomarker ~ term, value.var = "correlation_r")
mat <- mat[match(sorted_biomarkers, mat$Biomarker), ] # Align by sorted biomarker order
rownames(mat) <- mat$Biomarker
mat <- as.matrix(mat[, -1])

# 2. FDR-adjusted p-value matrix
pvalue_mat <- dcast(blood_plot_dat, Biomarker ~ term, value.var = "adjusted_p")
pvalue_mat <- pvalue_mat[match(sorted_biomarkers, pvalue_mat$Biomarker), ]
rownames(pvalue_mat) <- pvalue_mat$Biomarker
pvalue_mat <- as.matrix(pvalue_mat[, -1])

# 3. Unadjusted p-value matrix
raw_pvalue_mat <- dcast(blood_plot_dat, Biomarker ~ term, value.var = "p.value")
raw_pvalue_mat <- raw_pvalue_mat[match(sorted_biomarkers, raw_pvalue_mat$Biomarker), ]
rownames(raw_pvalue_mat) <- raw_pvalue_mat$Biomarker
raw_pvalue_mat <- as.matrix(raw_pvalue_mat[, -1])

# Color palette configuration
col_fun <- colorRamp2(breaks = c(min(mat, na.rm = TRUE), 0, max(mat, na.rm = TRUE)), colors = c("blue", "white", "red"))

# Significance annotation with dual p-value thresholds
add_significance <- function(raw_p, adj_p) {
  if (is.na(adj_p) || is.na(raw_p)) return("")
  if (adj_p < 0.01) return("**")        # FDR < 0.01
  if (adj_p < 0.05) return("*")         # FDR < 0.05
  if (raw_p < 0.05) return("+")         # Unadjusted p < 0.05
  return("")
}

# Cell text rendering
cell_text <- function(j, i, x, y, width, height, fill) {
  r <- round(mat[i, j], 3)
  adj_p <- pvalue_mat[i, j]
  raw_p <- raw_pvalue_mat[i, j]
  sig <- add_significance(raw_p, adj_p)
  grid.text(sprintf("%.3f %s", r, sig), x, y, gp = gpar(fontsize = 8))
}

# Render ComplexHeatmap for protein biomarkers
cor_heatmap <- Heatmap(
  mat, 
  name = "Correlation", 
  col = col_fun, 
  cluster_rows = FALSE,   # Maintain specified sorting order
  cluster_columns = FALSE, 
  row_names_gp = gpar(fontsize = 8), 
  column_names_gp = gpar(fontsize = 10),
  column_names_rot = 45, 
  cell_fun = cell_text
)

pdf("results/protein_heatmap_dual_p.pdf", width = 8, height = 10)
draw(cor_heatmap, heatmap_legend_side = "right")
dev.off()
cat("=> Protein Heatmap with Dual P-value markers generated!\n")

# ---------- 7. Corrected Permutation Test (Optimized Pairwise & Fast) ----------
cat("Starting Corrected Permutation Test (N = 1000)...\n")

# Pre-filter complete cases for covariates and focal psychological measures
blood_permut_data <- analysis_data %>%
  dplyr::filter(complete.cases(Age, Sex, Ethnic, BMI, Education, site, trauma_num, predicted_resilience))

# Define fast linear regression function with pairwise deletion
calc_stat_fast <- function(data, blood_cols, trauma_col, resil_col) {
  r_T <- numeric(length(blood_cols))
  r_R <- numeric(length(blood_cols))
  
  for (i in seq_along(blood_cols)) {
    b <- blood_cols[i]
    
    # Subset complete cases for focal protein
    temp_data <- data %>%
      dplyr::select(all_of(c(b, trauma_col, resil_col, "Age", "Sex", "Ethnic", "BMI", "Education", "site"))) %>%
      na.omit()
    
    if (nrow(temp_data) < 50) {
      r_T[i] <- NA; r_R[i] <- NA
      next
    }
    
    # Fast regression extracting t-statistic
    fit_T <- lm(as.formula(paste0("`", b, "` ~ ", trauma_col, " + Age + Sex + Ethnic + BMI + Education + site")), data = temp_data)
    df_res <- fit_T$df.residual
    t_T <- summary(fit_T)$coefficients[trauma_col, "t value"]
    
    fit_R <- lm(as.formula(paste0("`", b, "` ~ ", resil_col, " + Age + Sex + Ethnic + BMI + Education + site")), data = temp_data)
    t_R <- summary(fit_R)$coefficients[resil_col, "t value"]
    
    # Convert t-statistic to correlation coefficient
    r_T[i] <- t_T / sqrt(t_T^2 + df_res)
    r_R[i] <- t_R / sqrt(t_R^2 + df_res)
  }
  
  S <- cor(r_T, r_R, use = "complete.obs")
  return(list(S = S, r_T = r_T, r_R = r_R))
}

# Compute observed pattern similarity statistic S_obs
cat("Calculating observed S value...\n")
results_obs <- calc_stat_fast(blood_permut_data, M_biomarkers, "trauma_num", "predicted_resilience")
S_obs <- results_obs$S
cat("Observed S (corr(r_T, r_R)):", S_obs, "\n")

# Permutation testing
n_perm <- 1000
S_perm <- numeric(n_perm)

set.seed(123)
for (i in 1:n_perm) {
  perm_idx <- sample(nrow(blood_permut_data))
  perm_data <- blood_permut_data
  
  # Permute psychological predictor while preserving biological covariance with covariates
  perm_data[, c("trauma_num", "predicted_resilience")] <- 
    blood_permut_data[perm_idx, c("trauma_num", "predicted_resilience")]
  
  # Inline fast calculation of permutation statistics
  r_T_p <- numeric(length(M_biomarkers))
  r_R_p <- numeric(length(M_biomarkers))
  
  for (j in seq_along(M_biomarkers)) {
    b <- M_biomarkers[j]
    temp <- perm_data %>% 
      dplyr::select(all_of(c(b, "trauma_num", "predicted_resilience", "Age", "Sex", "Ethnic", "BMI", "Education", "site"))) %>% 
      na.omit()
    
    if(nrow(temp) < 50) next
    
    fit_T <- lm(as.formula(paste0("`", b, "` ~ trauma_num + Age + Sex + Ethnic + BMI + Education + site")), data = temp)
    df_res <- fit_T$df.residual
    
    t_T <- summary(fit_T)$coefficients["trauma_num", "t value"]
    t_R <- summary(lm(as.formula(paste0("`", b, "` ~ predicted_resilience + Age + Sex + Ethnic + BMI + Education + site")), data = temp))$coefficients["predicted_resilience", "t value"]
    
    r_T_p[j] <- t_T / sqrt(t_T^2 + df_res)
    r_R_p[j] <- t_R / sqrt(t_R^2 + df_res)
  }
  
  S_perm[i] <- cor(r_T_p, r_R_p, use = "complete.obs")
}

# Calculate two-sided permutation p-value
p_value <- mean(abs(S_perm) >= abs(S_obs), na.rm = TRUE)
cat("Corrected Permutation p-value (Two-tailed):", p_value, "\n")

# ---------- 8. Save Plot & Summary Results ----------
cat("Step 8: Generating Master Summary Table and Saving Outputs...\n")

# 8.1 Assemble Master Summary Table
# Extract trauma regression results
trauma_summary <- trauma_results %>%
  dplyr::filter(Biomarker %in% M_biomarkers) %>%
  dplyr::select(
    Biomarker, 
    N = Subject_Count, 
    r_Trauma = correlation_r, 
    p_Trauma = p.value, 
    adjP_Trauma = adjusted_p
  )

# Extract predicted resilience regression results
resil_summary <- resilience_results %>%
  dplyr::filter(Biomarker %in% M_biomarkers) %>%
  dplyr::select(
    Biomarker, 
    r_PredResil = correlation_r, 
    p_PredResil = p.value, 
    adjP_PredResil = adjusted_p
  )

# Combine into master table
master_summary_table <- dplyr::left_join(trauma_summary, resil_summary, by = "Biomarker")

# Save master summary table
write.csv(master_summary_table, "results/protein_master_summary_table.csv", row.names = FALSE)
cat("=> Master summary table saved: protein_master_summary_table.csv\n")


# 8.2 Save permutation distribution plot
perm_df <- data.frame(S_perm = S_perm)
ggplot(perm_df, aes(x = S_perm)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "white") +
  geom_vline(xintercept = S_obs, color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Permutation Distribution of S (Proteomics & Predicted Resilience)", x = "S (corr(r_T, r_R))", y = "Frequency") +
  theme_minimal()

ggsave("results/protein_permutation_plot_predicted.png", width = 8, height = 6)


# 8.3 Save permutation distribution data
write.csv(S_perm, "results/protein_permutation_predicted_1000.csv", row.names = FALSE)

cat("All operations completed successfully!\n")