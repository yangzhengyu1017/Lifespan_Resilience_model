# ============================================================
# Title: Resilience Model Construction using PLSR
# Author: Zhengyu Yang
# Purpose:
#   1. Load and preprocess resilience-related variables from UK Biobank data.
#   2. Train a PLSR model to predict resilience based on multi-domain features.
#   3. Examine variable weights and their stability using bootstrapping.
#   4. Visualize domain-level explained variance (R²) and feature importance.
# ============================================================


# ---------- 1. Load Packages ----------
library(data.table)
library(tidyverse)
library(rstatix)
library(pls)
library(ggsci)


# ---------- 2. Load and Prepare Data ----------
resilience_corr_rename_dat <- read.csv("~/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/UKB_dat_for_resilience_model_1224.csv")

# Select and rename predictors
BRS_train_dat <- resilience_corr_rename_dat %>%
  transmute(
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
  na.omit()

# Standardize predictors
BRS_train_dat[, 2:51] <- scale(BRS_train_dat[, 2:51])

# ---------- 3. Create Train/Test Split ----------
train_test <- resilience_corr_rename_dat %>%
  transmute(eid, PHQ_FU1 = PHQ9_Severity_FU1,
            PHQ_FU2 = `PHQ.9_FU2`,
            GAD_FU1 = General_Anxiety_Disorder_Severity_FU1,
            GAD_FU2 = General_Anxiety_Disorder_Severity_FU2) %>%
  mutate(completed = ifelse(rowSums(is.na(.)) == 0, 1, 0))

BRS_train_dat <- merge(BRS_train_dat, train_test[c("eid", "completed")], by = "eid", all.x = TRUE)

# Randomly select test set from completed participants
set.seed(123)
completed_rows <- which(BRS_train_dat$completed == 1)
selected_rows <- sample(completed_rows, 7685)
BRS_train_dat$completed <- 0
BRS_train_dat$completed[selected_rows] <- 1


# ---------- 4. Fit Initial PLSR Model ----------
meats_train <- BRS_train_dat[BRS_train_dat$completed == 0, ]
meats_test <- BRS_train_dat[BRS_train_dat$completed == 1, ]

X_cols <- colnames(BRS_train_dat)[2:50]
Y_col <- "self_resilience"

X_train <- as.matrix(meats_train[X_cols])
Y_train <- as.matrix(meats_train[Y_col])
X_test <- as.matrix(meats_test[X_cols])
Y_test <- as.matrix(meats_test[Y_col])

# Fit model with cross-validation
my_plsr <- plsr(Y_train ~ X_train, ncomp = 49, scale = TRUE, validation = "CV")

# Select optimal number of components
ncomp.opt <- selectNcomp(my_plsr, method = "randomization", plot = TRUE)

# Evaluate on test data
best_model <- plsr(Y_train ~ X_train, ncomp = ncomp.opt, scale = TRUE, validation = "CV")
test_pred <- as.matrix(predict(best_model, ncomp = ncomp.opt, X_test))
mean_r2 <- mean(diag(cor(test_pred, Y_test))^2)
print(mean_r2)


# ---------- 5. Extract and Visualize Coefficients ----------
coef_df <- data.frame(coef(best_model, ncomp = ncomp.opt))
X_var <- rownames(coef_df)

Category <- c(rep("Demographic", 3), rep("Economics", 6),
              rep("Family-support", 12), rep("Social-support", 4),
              rep("Early-risk", 9), rep("Lifestyle", 6),
              rep("Personality", 5), rep("Satisfaction", 4))

plot_data <- data.frame(Variable = X_var, Category = Category, Mean = coef_df$X_train_matrix)
plot_data <- plot_data %>%
  arrange(Category, desc(abs(Mean))) %>%
  mutate(Category = factor(Category, levels = unique(Category)))

ggplot(plot_data, aes(x = Variable, y = Mean, fill = Category)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_lancet() +
  labs(title = "Weights of Factors in the Resilience Model",
       x = "Factors", y = "Weight")


# ---------- 6. Domain-wise R² Analysis ----------
factor_name <- data.frame(var_names = X_cols, Category = Category)
categories <- unique(factor_name$Category)
result_df <- data.frame(Category = character(), best_ncomp = numeric(), train_r2 = numeric(), test_r2 = numeric())

for (cat in categories) {
  vars <- factor_name$var_names[factor_name$Category == cat]
  X_train_c <- as.matrix(meats_train[vars])
  X_test_c <- as.matrix(meats_test[vars])
  
  sep_plsr <- plsr(Y_train ~ X_train_c, ncomp = length(vars), scale = TRUE, validation = "CV")
  ncomp.c <- selectNcomp(sep_plsr, method = "randomization", plot = FALSE)
  
  best_c_model <- plsr(Y_train ~ X_train_c, ncomp = ncomp.c, scale = TRUE, validation = "CV")
  
  train_pred <- predict(best_c_model, ncomp = ncomp.c, X_train_c)
  test_pred <- predict(best_c_model, ncomp = ncomp.c, X_test_c)
  
  train_r2 <- mean(diag(cor(train_pred, Y_train))^2)
  test_r2 <- mean(diag(cor(test_pred, Y_test))^2)
  
  result_df <- rbind(result_df, data.frame(Category = cat, best_ncomp = ncomp.c,
                                           train_r2 = train_r2, test_r2 = test_r2))
}

ggplot(result_df, aes(x = reorder(Category, -test_r2), y = test_r2, fill = Category)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  scale_fill_lancet() +
  labs(title = "Explained Variance (R²) by Category", x = NULL, y = "R²")


# ---------- 7. Bootstrap Stability Analysis ----------
bootstrap_function <- function(data) {
  boot_sample <- data[sample(nrow(data), replace = TRUE), ]
  X <- as.matrix(boot_sample[X_cols])
  Y <- as.matrix(boot_sample[Y_col])
  model <- plsr(Y ~ X, ncomp = 2, scale = TRUE)
  return(as.numeric(coef(model, ncomp = 2)))
}

set.seed(123)
n_iter <- 1000
boot_coef <- replicate(n_iter, bootstrap_function(BRS_train_dat))
mean_values <- rowMeans(boot_coef)
sd_values <- apply(boot_coef, 1, sd)

boot_plot_data <- data.frame(
  Variable = X_cols,
  Mean = mean_values,
  SD = sd_values,
  Category = Category
)

ggplot(boot_plot_data, aes(x = Variable, y = Mean, ymin = Mean - SD, ymax = Mean + SD, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_errorbar(width = 0.2) +
  theme_minimal() +
  scale_fill_lancet() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 13)) +
  labs(title = "Bootstrap-estimated Weights (1000 iterations)",
       x = "Variables", y = "Coefficient (Mean ± SD)")

# Export results
write.csv(boot_plot_data, "~/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/PLS_bootstrap_results_1224
          .csv", row.names = FALSE)
