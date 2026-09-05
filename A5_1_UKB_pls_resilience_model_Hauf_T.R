# ==============================================================================
# Script: A5_1_UKB_pls_resilience_model_Hauf_T_V2.R
# Project: Lifespan Resilience Modeling (STM Submission)
# Purpose: Construct multivariate psychological resilience model in UK Biobank using Partial Least Squares Regression (PLSR) with Haufe transformation and bootstrap stability.
# ==============================================================================

# ---------- 1. Load Packages ----------
library(data.table)
library(tidyverse)
library(rstatix)
library(pls)
library(ggsci)

# Ensure output directory exists
if (!dir.exists("results")) dir.create("results", recursive = TRUE)

library(caret)     # Load caret for training-set-based preprocessing
library(ggplot2)

# ---------- 2. Load and Prepare Data ----------
resilience_corr_rename_dat <- utils::read.csv("data/UKB_dat_for_resilience_model_1224.csv")

# Select and rename predictors
BRS_train_dat <- resilience_corr_rename_dat %>%
  dplyr::transmute(
    eid,
    gender, age_BL, Education_year,
    HH_Num_Vehicle_BL, HH_Income_BL, HH_Own_Rent_BL, Able_Pay_Rent_Mortgage_FU1, Financial_Difficulties, IMD,
    Num_People_Living_BL, Been_In_Confiding_Relationship_FU1, Belittlement_Adult_FU1, Marital_Separation,
    Live_with_partner, Live_with_children, Live_with_siblings, Live_with_parents, Live_with_grandchild,
    Live_with_related, Live_with_unrelated, Live_alone,
    Social_Freq_Visits_BL, Social_Able_Confide_BL, Loneliness_BL, Num_social_activity,
    Breastfed_Baby_BL, Comp_Body_Size_Age_10_BL, Comp_Height_Size_Age_10_BL, Maternal_Smoking_Birth_BL,
    Felt_Loved_As_Child_FU1, Phys_Abused_As_Child_FU1, Felt_Hated_As_Child_FU1, Sex_Molested_As_Child_FU1,
    Someone_Take_To_Doctor_As_Child_FU1,
    MET_Minutes_Per_Week_Moderate_Activity_BL, MET_Minutes_Per_Week_Vigorous_Activity_BL,
    MET_Minutes_Per_Week_Walking_BL, Morning_Evening_Person_BL, TV_Time_BL, Computer_Time_BL,
    Big5_warmth, Big5_diligence, Big5_nervousness, Big5_curiosity, Big5_sociability,
    Health_Satisfaction_BL, Family_Relationship_Satisfaction_BL, Friendships_Satisfaction_BL,
    Financial_Situation_Satisfaction_BL,
    self_resilience
  ) %>%
  stats::na.omit()

# Preprocessing parameters estimated strictly on training set to prevent data leakage
# BRS_train_dat[, 2:51] <- scale(BRS_train_dat[, 2:51])

# ---------- 3. Create Train/Test Split ----------
train_test <- resilience_corr_rename_dat %>%
  dplyr::transmute(
    eid, 
    PHQ_FU1 = PHQ9_Severity_FU1,
    PHQ_FU2 = `PHQ.9_FU2`,
    GAD_FU1 = General_Anxiety_Disorder_Severity_FU1,
    GAD_FU2 = General_Anxiety_Disorder_Severity_FU2
  ) %>%
  dplyr::mutate(completed = base::ifelse(base::rowSums(is.na(.)) == 0, 1, 0))

BRS_train_dat <- base::merge(BRS_train_dat, train_test[c("eid", "completed")], by = "eid", all.x = TRUE)

# Randomly select test set from completed participants
base::set.seed(123)
completed_rows <- base::which(BRS_train_dat$completed == 1)
selected_rows <- base::sample(completed_rows, 7685)
BRS_train_dat$completed <- 0
BRS_train_dat$completed[selected_rows] <- 1


# ---------- 4. Safe Preprocessing & Fit Initial PLSR Model ----------
meats_train <- BRS_train_dat[BRS_train_dat$completed == 0, ]
meats_test  <- BRS_train_dat[BRS_train_dat$completed == 1, ]

X_cols <- base::colnames(BRS_train_dat)[2:50]
Y_col  <- "self_resilience"

# Combine predictors (X) and response (Y) for unified preprocessing
all_cols <- c(X_cols, Y_col)

# Compute centering and scaling parameters from training set
preProc_params <- caret::preProcess(meats_train[, all_cols], method = c("center", "scale"))

# Apply training-derived parameters to training and test sets
meats_train[, all_cols] <- stats::predict(preProc_params, meats_train[, all_cols])
meats_test[, all_cols]  <- stats::predict(preProc_params, meats_test[, all_cols])

# Extract standardized predictor and response matrices

# Convert to matrix format for PLSR
X_train <- base::as.matrix(meats_train[X_cols])
Y_train <- base::as.matrix(meats_train[Y_col])
X_test  <- base::as.matrix(meats_test[X_cols])
Y_test  <- base::as.matrix(meats_test[Y_col])

# Fit model with cross-validation 
# Specify scale = FALSE as features were pre-standardized using training statistics
my_plsr <- pls::plsr(Y_train ~ X_train, ncomp = 49, scale = FALSE, validation = "CV")

# Select optimal number of components
ncomp.opt <- pls::selectNcomp(my_plsr, method = "randomization", plot = TRUE)

# Evaluate on test data (scale = FALSE)
best_model <- pls::plsr(Y_train ~ X_train, ncomp = ncomp.opt, scale = FALSE, validation = "CV")
test_pred  <- base::as.matrix(stats::predict(best_model, ncomp = ncomp.opt, newdata = X_test))
mean_r2    <- base::mean(base::diag(stats::cor(test_pred, Y_test))^2)
base::print(mean_r2)


# ---------- 5. Extract and Visualize Coefficients (Haufe-transformed) ----------
# Extract raw PLSR weights (W)
raw_W <- base::as.numeric(stats::coef(best_model, ncomp = ncomp.opt))

# Compute covariance matrix of predictors (Sigma_X)
Sigma_X_train <- stats::cov(X_train)

# Compute Haufe-transformed activation pattern: A = Sigma_X %*% W
haufe_A <- Sigma_X_train %*% raw_W

# Assemble data frame for visualization
coef_df <- base::data.frame(Mean = base::as.numeric(haufe_A))
base::rownames(coef_df) <- X_cols
X_var <- base::rownames(coef_df)

Category <- c(rep("Demographic", 3), rep("Economics", 6),
              rep("Family-support", 12), rep("Social-support", 4),
              rep("Early-risk", 9), rep("Lifestyle", 6),
              rep("Personality", 5), rep("Satisfaction", 4))

plot_data <- base::data.frame(Variable = X_var, Category = Category, Mean = coef_df$Mean)
plot_data <- plot_data %>%
  dplyr::arrange(Category, dplyr::desc(abs(Mean))) %>%
  dplyr::mutate(Category = base::factor(Category, levels = base::unique(Category)))

ggplot2::ggplot(plot_data, aes(x = Variable, y = Mean, fill = Category)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsci::scale_fill_lancet() +
  ggplot2::labs(title = "Haufe-transformed Activation Patterns in the Resilience Model",
                x = "Factors", y = "Activation (Haufe Weight)")


# ---------- 6. Domain-wise R² Analysis ----------
factor_name <- base::data.frame(var_names = X_cols, Category = Category)
categories <- base::unique(factor_name$Category)
result_df <- base::data.frame(Category = character(), best_ncomp = numeric(), train_r2 = numeric(), test_r2 = numeric())

for (cat in categories) {
  vars <- factor_name$var_names[factor_name$Category == cat]
  X_train_c <- base::as.matrix(meats_train[vars])
  X_test_c  <- base::as.matrix(meats_test[vars])
  
  # Fit domain-specific PLSR models with scale = FALSE
  sep_plsr <- pls::plsr(Y_train ~ X_train_c, ncomp = length(vars), scale = FALSE, validation = "CV")
  ncomp.c  <- pls::selectNcomp(sep_plsr, method = "randomization", plot = FALSE)
  
  best_c_model <- pls::plsr(Y_train ~ X_train_c, ncomp = ncomp.c, scale = FALSE, validation = "CV")
  
  train_pred <- stats::predict(best_c_model, ncomp = ncomp.c, newdata = X_train_c)
  test_pred  <- stats::predict(best_c_model, ncomp = ncomp.c, newdata = X_test_c)
  
  train_r2 <- base::mean(base::diag(stats::cor(train_pred, Y_train))^2)
  test_r2  <- base::mean(base::diag(stats::cor(test_pred, Y_test))^2)
  
  result_df <- base::rbind(result_df, base::data.frame(Category = cat, best_ncomp = ncomp.c,
                                                       train_r2 = train_r2, test_r2 = test_r2))
}

ggplot2::ggplot(result_df, aes(x = stats::reorder(Category, -test_r2), y = test_r2, fill = Category)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::coord_flip() +
  ggplot2::theme_minimal() +
  ggsci::scale_fill_lancet() +
  ggplot2::labs(title = "Explained Variance (R²) by Category", x = NULL, y = "R²")


# ---------- 7. Bootstrap Stability Analysis (Haufe-transformed) ----------
bootstrap_function_haufe <- function(data) {
  # 1. Resample observations with replacement (bootstrap sample)
  boot_sample <- data[base::sample(base::nrow(data), replace = TRUE), ]
  X <- base::as.matrix(boot_sample[X_cols])
  Y <- base::as.matrix(boot_sample[Y_col])
  
  # 2. Fit PLSR model on bootstrap sample 
  # Maintain scale = FALSE to prevent scale mismatches across bootstrap iterations
  model <- pls::plsr(Y ~ X, ncomp = ncomp.opt, scale = FALSE)
  
  # 3. Extract weights W corresponding to optimal number of components
  W <- base::as.numeric(stats::coef(model, ncomp = ncomp.opt))
  
  # 4. Compute covariance matrix of bootstrap sample predictors
  Sigma_X <- stats::cov(X)
  
  # 5. Execute Haufe transformation: A = Sigma_X %*% W
  A <- Sigma_X %*% W
  
  return(base::as.numeric(A))
}

base::set.seed(123)
n_iter <- 1000

# Perform bootstrap stability analysis strictly on training data
boot_coef <- base::replicate(n_iter, bootstrap_function_haufe(meats_train))

mean_values <- base::rowMeans(boot_coef)
sd_values   <- base::apply(boot_coef, 1, stats::sd)

boot_plot_data <- base::data.frame(
  Variable = X_cols,
  Mean = mean_values,
  SD = sd_values,
  Category = Category
)

# 1. Define category display order
custom_category_order <- c("Personality", "Satisfaction", "Social-support",
                           "Early-risk", "Family-support", "Demographic",
                           "Economics", "Lifestyle")

# Create variable rename map for publication-quality plots
rename_map <- c(
  "gender" = "Gender",
  "age_BL" = "Age",
  "Education_year" = "Years of education",
  "HH_Num_Vehicle_BL" = "Household vehicle count",
  "HH_Income_BL" = "Household income",
  "HH_Own_Rent_BL" = "Housing tenure",
  "Able_Pay_Rent_Mortgage_FU1" = "Housing affordability",
  "Financial_Difficulties" = "Financial difficulties",
  "IMD" = "Index of Multiple Deprivation",
  "Num_People_Living_BL" = "Household size",
  "Been_In_Confiding_Relationship_FU1" = "Confiding partner relationship",
  "Belittlement_Adult_FU1" = "Belittling adult relationship",
  "Live_with_partner" = "Living with partner",
  "Live_with_children" = "Living with children",
  "Live_with_siblings" = "Living with siblings",
  "Live_with_parents" = "Living with parents",
  "Live_with_grandchild" = "Living with grandchild",
  "Live_with_related" = "Living with other relatives",
  "Live_with_unrelated" = "Living with unrelated individuals",
  "Live_alone" = "Living alone",
  "Marital_Separation" = "Marital separation",
  "Social_Freq_Visits_BL" = "Frequency of social visits",
  "Social_Able_Confide_BL" = "Confiding social relationship",
  "Loneliness_BL" = "Loneliness",
  "Num_social_activity" = "Number of social activities",
  "Breastfed_Baby_BL" = "Breastfed as baby",
  "Comp_Body_Size_Age_10_BL" = "Comparative body size at age 10",
  "Comp_Height_Size_Age_10_BL" = "Comparative height at age 10",
  "Maternal_Smoking_Birth_BL" = "Maternal smoking around birth",
  "Felt_Loved_As_Child_FU1" = "Childhood emotional neglect (R)",
  "Phys_Abused_As_Child_FU1" = "Childhood physical abuse",
  "Felt_Hated_As_Child_FU1" = "Childhood emotional abuse",
  "Sex_Molested_As_Child_FU1" = "Childhood sexual molestation",
  "Someone_Take_To_Doctor_As_Child_FU1" = "Childhood physical neglect (R)",
  "MET_Minutes_Per_Week_Moderate_Activity_BL" = "Moderate physical activity",
  "MET_Minutes_Per_Week_Vigorous_Activity_BL" = "Vigorous physical activity",
  "MET_Minutes_Per_Week_Walking_BL" = "Walking time",
  "Morning_Evening_Person_BL" = "Chronotype",
  "TV_Time_BL" = "Television viewing time",
  "Computer_Time_BL" = "Computer use time",
  "Big5_warmth" = "Big 5: Agreeableness",
  "Big5_diligence" = "Big 5: Conscientiousness",
  "Big5_nervousness" = "Big 5: Neuroticism",
  "Big5_sociability" = "Big 5: Extraversion",
  "Big5_curiosity" = "Big 5: Openness",
  "Health_Satisfaction_BL" = "Health satisfaction",
  "Family_Relationship_Satisfaction_BL" = "Family satisfaction",
  "Friendships_Satisfaction_BL" = "Friendship satisfaction",
  "Financial_Situation_Satisfaction_BL" = "Financial satisfaction"
)

# Map variable names to publication labels
# Prepare data and sort by activation magnitude
boot_plot_data <- boot_plot_data %>%
  dplyr::mutate(Variable = rename_map[base::as.character(Variable)]) %>%
  dplyr::mutate(Category = base::factor(Category, levels = custom_category_order)) %>%
  dplyr::arrange(Category, dplyr::desc(Mean)) %>% # Sort by descending activation magnitude
  dplyr::mutate(Variable = base::factor(Variable, levels = base::unique(Variable)))

# 3. Generate visualization
ggplot2::ggplot(boot_plot_data, aes(x = Variable, y = Mean, ymin = Mean - SD, ymax = Mean + SD, fill = Category)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::geom_errorbar(width = 0.2) +
  ggplot2::theme_minimal() +
  ggsci::scale_fill_lancet() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, size = 11)
    # Retain vertical gridlines for visual alignment
  ) +
  ggplot2::labs(
    title = "Bootstrap-estimated Haufe Patterns (1000 iterations)",
    x = "Variables",
    y = "Activation (Mean ± SD)"
  )

# 3. Generate visualization
ggplot2::ggplot(boot_plot_data, ggplot2::aes(x = Variable, y = Mean, ymin = Mean - SD, ymax = Mean + SD, fill = Category)) +
  # Adjust bar width (0.6) for optimal spacing
  ggplot2::geom_bar(stat = "identity", width = 0.8) +
  # Set error bar width proportional to bar width
  ggplot2::geom_errorbar(width = 0.15) +
  ggplot2::theme_minimal() +
  ggsci::scale_fill_lancet() +
  ggplot2::theme(
    # Rotate x-axis labels 90 degrees with center alignment
    axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, size = 11)
    # Retain vertical gridlines to guide visual alignment
  ) +
  ggplot2::labs(
    title = "Bootstrap-estimated Haufe Patterns (1000 iterations)",
    x = "Variables",
    y = "Activation (Mean ± SD)"
  )

# Export results (Optional)
# utils::write.csv(boot_plot_data, "results/PLS_bootstrap_haufe_results.csv", row.names = FALSE)