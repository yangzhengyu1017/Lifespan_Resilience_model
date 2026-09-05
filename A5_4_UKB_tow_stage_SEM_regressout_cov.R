# ==============================================================================
# Script: A5_4_UKB_tow_stage_SEM_regressout_cov.R
# Project: Lifespan Resilience Modeling (STM Submission)
# Purpose: Two-stage structural equation modeling (SEM) investigating direct and mediated pathways linking objective environment, personality, subjective satisfaction, and resilience with covariate adjustment.
# ==============================================================================

library(tidyverse)
library(data.table)
library(lavaan)
library(patchwork)
library(knitr)


# Ensure output directory exists
if (!dir.exists("results")) dir.create("results", recursive = TRUE)

set.seed(2026) 

# ==============================================================================
# 1. Data Import and Cleaning
# ==============================================================================
file_path <- "data/UKB_dat_for_resilience_model_1224.csv"
if (!file.exists(file_path)) stop("Error: File not found: ", file_path)

raw_dat <- fread(file_path)

Analysis_Data <- raw_dat %>%
  dplyr::select(
    eid, gender, age_BL, Ethnic_group, Education_year, site,
    HH_Income_BL, HH_Num_Vehicle_BL, Financial_Difficulties, IMD,
    Social_Freq_Visits_BL, Social_Able_Confide_BL, Num_social_activity, Frailty_BL,
    Financial_Situation_Satisfaction_BL, Health_Satisfaction_BL, Friendships_Satisfaction_BL,
    Big5_sociability, Big5_nervousness, Big5_warmth, Big5_curiosity, Big5_diligence,
    self_resilience
  ) %>% na.omit()

# ==============================================================================
# 2. Composite Measure Construction and Covariate Residualization
# ==============================================================================
cat("Constructing composite measures...\n")
SEM_Data_Full <- Analysis_Data %>%
  mutate(
    # Factorize demographic covariates
    Sex = factor(gender),
    Age = age_BL,
    Ethnic = factor(Ethnic_group),
    Education = Education_year,
    Site = factor(site),
    
    # Standardize individual indicators
    z_Income      = scale(HH_Income_BL),
    z_Vehicle     = scale(HH_Num_Vehicle_BL),
    z_FinDiff_R   = scale(-Financial_Difficulties),
    z_IMD         = scale(-IMD),
    z_Health_R    = scale(-Frailty_BL),
    z_SocVisit    = scale(Social_Freq_Visits_BL),
    z_SocConfide  = scale(-Social_Able_Confide_BL),
    z_SocActiv    = scale(Num_social_activity),
    Financial_Sat_z   = scale(Financial_Situation_Satisfaction_BL),
    Health_Sat_z      = scale(Health_Satisfaction_BL),
    Friendships_Sat_z = scale(Friendships_Satisfaction_BL),
    
    # Construct composite indices and target variables
    Overall_Objective = as.numeric(scale(rowMeans(cbind(z_Income, z_Vehicle, z_FinDiff_R, z_IMD, z_Health_R, z_SocVisit, z_SocConfide, z_SocActiv)))),
    Overall_Satisfaction = as.numeric(scale(rowMeans(cbind(Financial_Sat_z, Health_Sat_z, Friendships_Sat_z)))),
    Resilience  = as.numeric(scale(self_resilience)),
    Sociability = as.numeric(scale(Big5_sociability)),
    Nervousness = as.numeric(scale(Big5_nervousness)),
    Warmth      = as.numeric(scale(Big5_warmth)),
    Curiosity   = as.numeric(scale(Big5_curiosity)),
    Diligence   = as.numeric(scale(Big5_diligence))
  ) %>%
  dplyr::select(Overall_Objective, Overall_Satisfaction, Resilience, Sociability, Nervousness, Warmth, Curiosity, Diligence, Sex, Age, Ethnic, Education, Site) %>%
  na.omit()

cat("Regressing out covariate effects from measures...\n")
main_vars <- c("Overall_Objective", "Overall_Satisfaction", "Resilience", "Sociability", "Nervousness", "Warmth", "Curiosity", "Diligence")

SEM_Data_Clean <- SEM_Data_Full
for (var in main_vars) {
  # Regression formula: focal variable ~ covariates
  formula_str <- paste0(var, " ~ Sex + Age + Ethnic + Education + Site")
  model_lm <- lm(as.formula(formula_str), data = SEM_Data_Full)
  # Extract residuals and re-standardize to ensure unit variance
  SEM_Data_Clean[[var]] <- as.numeric(scale(resid(model_lm)))
}

# Clean residualized dataset for structural equation modeling
SEM_Data <- SEM_Data_Clean %>% dplyr::select(all_of(main_vars))

# ==============================================================================
# 3. Specify SEM Model Syntax
# ==============================================================================
model1_syntax <- '
  Sociability ~ a1 * Overall_Objective
  Nervousness ~ a2 * Overall_Objective
  Warmth      ~ a3 * Overall_Objective
  Curiosity   ~ a4 * Overall_Objective
  Diligence   ~ a5 * Overall_Objective
  Overall_Satisfaction ~ cp * Overall_Objective + b1 * Sociability + b2 * Nervousness + b3 * Warmth + b4 * Curiosity + b5 * Diligence
  
  ind_sociability := a1 * b1
  ind_nervousness := a2 * b2
  ind_warmth      := a3 * b3
  ind_curiosity   := a4 * b4
  ind_diligence   := a5 * b5
  total_ind := ind_sociability + ind_nervousness + ind_warmth + ind_curiosity + ind_diligence
  total     := cp + total_ind
  
  prop_sociability := ind_sociability / total
  prop_nervousness := ind_nervousness / total
  prop_warmth      := ind_warmth / total
  prop_curiosity   := ind_curiosity / total
  prop_diligence   := ind_diligence / total
  prop_total_all   := total_ind / total
'

model2_syntax <- '
  Sociability ~ a1 * Overall_Satisfaction
  Nervousness ~ a2 * Overall_Satisfaction
  Warmth      ~ a3 * Overall_Satisfaction
  Curiosity   ~ a4 * Overall_Satisfaction
  Diligence   ~ a5 * Overall_Satisfaction
  Resilience ~ cp * Overall_Satisfaction + b1 * Sociability + b2 * Nervousness + b3 * Warmth + b4 * Curiosity + b5 * Diligence
  
  ind_sociability := a1 * b1
  ind_nervousness := a2 * b2
  ind_warmth      := a3 * b3
  ind_curiosity   := a4 * b4
  ind_diligence   := a5 * b5
  total_ind := ind_sociability + ind_nervousness + ind_warmth + ind_curiosity + ind_diligence
  total     := cp + total_ind
  
  prop_sociability := ind_sociability / total
  prop_nervousness := ind_nervousness / total
  prop_warmth      := ind_warmth / total
  prop_curiosity   := ind_curiosity / total
  prop_diligence   := ind_diligence / total
  prop_total_all   := total_ind / total
'
# Include residual covariances among mediators in syntax
mediator_covs <- '
  Sociability ~~ Nervousness
  Sociability ~~ Warmth
  Sociability ~~ Curiosity
  Sociability ~~ Diligence
  Nervousness ~~ Warmth
  Nervousness ~~ Curiosity
  Nervousness ~~ Diligence
  Warmth      ~~ Curiosity
  Warmth      ~~ Diligence
  Curiosity   ~~ Diligence
'

model1_syntax <- paste0(model1_syntax, mediator_covs)
model2_syntax <- paste0(model2_syntax, mediator_covs)
# ==============================================================================
# 4. Model Estimation and Parameter Extraction
# ==============================================================================
n_boot <- 5000 
cat("--- Fitting Model 1 (Objective Environment -> Personality -> Satisfaction) ---\n")
fit1 <- sem(model1_syntax, data = SEM_Data,  se = "bootstrap", bootstrap = n_boot)

cat("--- Fitting Model 2 (Satisfaction -> Personality -> Resilience) ---\n")
fit2 <- sem(model2_syntax, data = SEM_Data, se = "bootstrap", bootstrap = n_boot)

sig_star <- function(p) case_when(p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", TRUE ~ "")

extract_all_results <- function(fit, model_name) {
  params <- parameterEstimates(fit, boot.ci.type = "bca.simple", standardized = TRUE) %>% dplyr::filter(op == ":=")
  macro_res <- params %>% dplyr::filter(lhs %in% c("total_ind", "total", "prop_total_all")) %>%
    dplyr::select(lhs, est, pvalue, ci.lower, ci.upper) %>% tidyr::pivot_wider(names_from = lhs, values_from = c(est, pvalue, ci.lower, ci.upper)) %>%
    dplyr::transmute(
      Model = model_name, Trait = "Total Big5", Type = "Macro (Overall)",
      Ind_Est = est_total_ind, Ind_Sig = paste0(round(est_total_ind, 3), sig_star(pvalue_total_ind)),
      Ind_CI  = paste0("[", round(ci.lower_total_ind, 3), ", ", round(ci.upper_total_ind, 3), "]"),
      Prop_Value = est_prop_total_all, Prop_Pct = paste0(round(est_prop_total_all * 100, 1), "%", sig_star(pvalue_prop_total_all))
    )
  ind_micro <- params %>% dplyr::filter(stringr::str_detect(lhs, "^ind_"))
  prop_micro <- params %>% dplyr::filter(stringr::str_detect(lhs, "^prop_") & lhs != "prop_total_all")
  micro_res <- data.frame(
    Model = model_name, Trait = stringr::str_to_title(stringr::str_replace(ind_micro$lhs, "ind_", "")), Type = "Micro (Specific)",
    Ind_Est = ind_micro$est, Ind_Sig = paste0(round(ind_micro$est, 3), sig_star(ind_micro$pvalue)),
    Ind_CI  = paste0("[", round(ind_micro$ci.lower, 3), ", ", round(ind_micro$ci.upper, 3), "]"),
    Prop_Value = prop_micro$est, Prop_Pct = paste0(round(prop_micro$est * 100, 1), "%", sig_star(prop_micro$pvalue))
  )
  dplyr::bind_rows(macro_res, micro_res)
}

final_results <- dplyr::bind_rows(extract_all_results(fit1, "M1: Objective -> Big5 -> Satisfaction"), extract_all_results(fit2, "M2: Satisfaction -> Big5 -> Resilience"))

cat("=== Complete Mediation Effects and Proportions (Covariate-Adjusted) ===\n")
knitr::kable(final_results %>% dplyr::select(Model, Type, Trait, Ind_Sig, Ind_CI, Prop_Pct), align = "c")

# ==============================================================================
# 4.5 Extract All Path Coefficients (Beta, CI, P-value)
# ==============================================================================

extract_all_paths <- function(fit, model_name, n_boot) {
  
  # Extract all parameter estimates with bootstrap confidence intervals
  params <- parameterEstimates(
    fit,
    boot.ci.type = "bca.simple",
    standardized  = TRUE,   # Include standardized coefficients
    ci            = TRUE,
    level         = 0.95
  )
  
  # ---- A. Regression paths (op == "~") ----
  reg_paths <- params %>%
    dplyr::filter(op == "~") %>%
    dplyr::transmute(
      Model    = model_name,
      Type     = "Regression",
      Path     = paste(lhs, "~", rhs),
      Label    = label,
      Beta     = round(est, 4),
      SE       = round(se, 4),
      Z        = round(z, 3),
      P_value  = pvalue,
      CI_lower = round(ci.lower, 4),
      CI_upper = round(ci.upper, 4),
      Std_all  = round(std.all, 4),
      Sig      = sig_star(pvalue),
      Summary  = paste0(round(est, 3), sig_star(pvalue),
                        " [", round(ci.lower, 3), ", ", round(ci.upper, 3), "]")
    )
  
  # ---- B. Defined parameters: Indirect effects & proportions (op == ":=") ----
  def_paths <- params %>%
    dplyr::filter(op == ":=") %>%
    dplyr::transmute(
      Model    = model_name,
      Type     = dplyr::case_when(
        grepl("^ind_",  lhs) ~ "Specific Indirect",
        grepl("^total", lhs) ~ "Total / Total Indirect",
        grepl("^prop_", lhs) ~ "Proportion Mediated",
        TRUE                 ~ "Defined"
      ),
      Path     = lhs,
      Label    = label,
      Beta     = round(est, 4),
      SE       = round(se, 4),
      Z        = round(z, 3),
      P_value  = pvalue,
      CI_lower = round(ci.lower, 4),
      CI_upper = round(ci.upper, 4),
      Std_all  = round(std.all, 4),
      Sig      = sig_star(pvalue),
      Summary  = paste0(round(est, 3), sig_star(pvalue),
                        " [", round(ci.lower, 3), ", ", round(ci.upper, 3), "]")
    )
  
  # ---- C. Residual covariances (op == "~~", excluding variances) ----
  cov_paths <- params %>%
    dplyr::filter(op == "~~", lhs != rhs) %>%
    dplyr::transmute(
      Model    = model_name,
      Type     = "Residual Covariance",
      Path     = paste(lhs, "~~", rhs),
      Label    = label,
      Beta     = round(est, 4),
      SE       = round(se, 4),
      Z        = round(z, 3),
      P_value  = pvalue,
      CI_lower = round(ci.lower, 4),
      CI_upper = round(ci.upper, 4),
      Std_all  = round(std.all, 4),
      Sig      = sig_star(pvalue),
      Summary  = paste0(round(est, 3), sig_star(pvalue),
                        " [", round(ci.lower, 3), ", ", round(ci.upper, 3), "]")
    )
  
  # ---- D. Residual variances (op == "~~", lhs == rhs) ----
  var_paths <- params %>%
    dplyr::filter(op == "~~", lhs == rhs) %>%
    dplyr::transmute(
      Model    = model_name,
      Type     = "Residual Variance",
      Path     = paste0("Var(", lhs, ")"),
      Label    = label,
      Beta     = round(est, 4),
      SE       = round(se, 4),
      Z        = round(z, 3),
      P_value  = pvalue,
      CI_lower = round(ci.lower, 4),
      CI_upper = round(ci.upper, 4),
      Std_all  = round(std.all, 4),
      Sig      = sig_star(pvalue),
      Summary  = paste0(round(est, 3), sig_star(pvalue),
                        " [", round(ci.lower, 3), ", ", round(ci.upper, 3), "]")
    )
  
  dplyr::bind_rows(reg_paths, def_paths, cov_paths, var_paths)
}

# ---------- Execute parameter extraction ----------
all_paths_m1 <- extract_all_paths(fit1, "M1: Objective -> Big5 -> Satisfaction", n_boot)
all_paths_m2 <- extract_all_paths(fit2, "M2: Satisfaction -> Big5 -> Resilience", n_boot)

all_paths <- dplyr::bind_rows(all_paths_m1, all_paths_m2)

# ==============================================================================
# 4.6 Print summary tables by path type
# ==============================================================================

cat("\n\n=== Complete Summary of SEM Path Coefficients ===\n\n")

# --- Regression Paths ---
cat("--- Regression Paths ---\n")
print(knitr::kable(
  all_paths %>% dplyr::filter(Type == "Regression") %>%
    dplyr::select(Model, Path, Label, Beta, SE, CI_lower, CI_upper, P_value, Sig),
  digits = 4, align = "c",
  caption = "All Regression Paths (a-paths, b-paths, c'-path)"
))

# --- Indirect Effects ---
cat("\n--- Indirect Effects ---\n")
print(knitr::kable(
  all_paths %>% dplyr::filter(Type %in% c("Specific Indirect", "Total / Total Indirect")) %>%
    dplyr::select(Model, Path, Beta, SE, CI_lower, CI_upper, P_value, Sig),
  digits = 4, align = "c",
  caption = "Indirect Effects (Specific + Total)"
))

# --- Proportion Mediated ---
cat("\n--- Proportion Mediated ---\n")
print(knitr::kable(
  all_paths %>% dplyr::filter(Type == "Proportion Mediated") %>%
    dplyr::select(Model, Path, Beta, CI_lower, CI_upper, P_value, Sig) %>%
    dplyr::mutate(Pct = paste0(round(Beta * 100, 1), "%")),
  digits = 4, align = "c",
  caption = "Proportion Mediated (% of Total Effect)"
))

# --- Residual Covariances ---
cat("\n--- Residual Covariances ---\n")
print(knitr::kable(
  all_paths %>% dplyr::filter(Type == "Residual Covariance") %>%
    dplyr::select(Model, Path, Beta, SE, CI_lower, CI_upper, P_value, Sig),
  digits = 4, align = "c",
  caption = "Residual Covariances among Mediators"
))

# ==============================================================================
# 4.7 Export results to CSV
# ==============================================================================

write.csv(all_paths, file = "results/Two_stage_SEM_all_paths_full_results.csv", row.names = FALSE)

# ==============================================================================
# 4.8 Formatted summary table: Regression paths and indirect effects
# ==============================================================================

academic_table <- all_paths %>%
  dplyr::filter(Type %in% c("Regression", "Specific Indirect",
                            "Total / Total Indirect", "Proportion Mediated")) %>%
  dplyr::transmute(
    Model,
    Type,
    Path,
    `β`          = Beta,
    `95% CI`     = paste0("[", CI_lower, ", ", CI_upper, "]"),
    `P`          = ifelse(P_value < 0.001, "< .001", round(P_value, 3)),
    `Sig`        = Sig
  )

cat("\n=== Academic Summary Table ===\n")
print(knitr::kable(academic_table, align = "c",
                   caption = "Complete SEM Path Coefficients with Bootstrap 95% CIs"))

# ==============================================================================
# 5. Academic publication visualization: Horizontal bar plots
# ==============================================================================

plot_df <- final_results %>%
  dplyr::filter(Type == "Micro (Specific)") %>%
  mutate(
    Model_short = ifelse(grepl("M1", Model), "Model 1: Obj Env \u2192 Sat", "Model 2: Sat \u2192 Resilience"),
    Trait = factor(Trait, levels = rev(c("Sociability", "Nervousness", "Warmth", "Curiosity", "Diligence"))),
    Prop_Numeric = Prop_Value * 100
  )

tot_prop_m1 <- round(final_results$Prop_Value[final_results$Model == "M1: Objective -> Big5 -> Satisfaction" & final_results$Type == "Macro (Overall)"] * 100, 1)
tot_prop_m2 <- round(final_results$Prop_Value[final_results$Model == "M2: Satisfaction -> Big5 -> Resilience" & final_results$Type == "Macro (Overall)"] * 100, 1)

# ---- Panel A: Indirect effects (horizontal bars) ----
p1 <- ggplot(plot_df, aes(x = Trait, y = Ind_Est, fill = Model_short)) +
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6, 
           color = "black", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "solid", color = "grey30") +
  geom_text(
    aes(
      label = Ind_Sig,
      y = ifelse(Ind_Est >= 0,
                 Ind_Est + max(abs(plot_df$Ind_Est)) * 0.12,
                 Ind_Est - max(abs(plot_df$Ind_Est)) * 0.12)
    ),
    position = position_dodge(0.7), size = 3.2, fontface = "bold"
  ) +
  scale_fill_manual(values = c(
    "Model 1: Obj Env \u2192 Sat"   = "#4A90E2",
    "Model 2: Sat \u2192 Resilience" = "#D0021B"
  )) +
  coord_flip() +
  labs(
    title = "A. Specific Indirect Effects (a \u00D7 b)",
    x = NULL,
    y = "Indirect Effect (\u03B2)",
    fill = "Pathway"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title     = element_text(face = "bold", size = 13),
    legend.position = "bottom",
    legend.title    = element_text(size = 10),
    legend.text     = element_text(size = 9),
    axis.text.y     = element_text(face = "bold", size = 11),
    axis.text.x     = element_text(size = 10)
  )

# ---- Panel B: Proportion mediated (horizontal bars) ----
p2 <- ggplot(plot_df, aes(x = Trait, y = Prop_Numeric, fill = Model_short)) +
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6, 
           color = "black", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "solid", color = "grey30") +
  geom_text(
    aes(
      label = paste0(round(Prop_Numeric, 1), "%"),
      y = ifelse(Prop_Numeric >= 0,
                 Prop_Numeric + max(abs(plot_df$Prop_Numeric)) * 0.12,
                 Prop_Numeric - max(abs(plot_df$Prop_Numeric)) * 0.12)
    ),
    position = position_dodge(0.7), size = 3.2
  ) +
  scale_fill_manual(values = c(
    "Model 1: Obj Env \u2192 Sat"   = "#50E3C2",
    "Model 2: Sat \u2192 Resilience" = "#F5A623"
  )) +
  coord_flip() +
  labs(
    title    = "B. Proportion Mediated (%)",
    subtitle = sprintf("Total Big5: M1 = %s%% | M2 = %s%%", tot_prop_m1, tot_prop_m2),
    x = NULL,
    y = "Proportion Mediated (%)",
    fill = "Pathway"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title     = element_text(face = "bold", size = 13),
    plot.subtitle  = element_text(face = "italic", color = "grey40", size = 10),
    legend.position = "bottom",
    legend.title    = element_text(size = 10),
    legend.text     = element_text(size = 9),
    #axis.text.y     = element_blank(),   # Omit redundant trait labels on right panel
    axis.ticks.y    = element_blank(),
    axis.text.x     = element_text(size = 10)
  )

# ---- Horizontal composition with patchwork ----
final_plot <- p1 | p2

#final_plot <- p1 + p2 + plot_layout(ncol = 2, widths = c(1.15, 1))

print(final_plot)

# Optional figure export
# ggsave("SEM_mediation_horizontal.pdf", final_plot, width = 14, height = 6, dpi = 300)
# ggsave("SEM_mediation_horizontal.png", final_plot, width = 14, height = 6, dpi = 300)
# ==============================================================================
# 7. Model evaluation and fit statistics
# ==============================================================================

# ---- 7.1 Model fit indices ----
extract_fit <- function(fit, model_name) {
    fi <- fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "tli", 
                           "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", 
                           "srmr"))
    df_fi <- as.data.frame(as.list(fi))
  df_fi <- cbind(Model = model_name, df_fi)
  return(df_fi)
}

fit_table <- dplyr::bind_rows(
  extract_fit(fit1, "Model 1: Obj -> Sat"),
  extract_fit(fit2, "Model 2: Sat -> Res")
)

cat("\n================================================================\n")
cat("=== Standard Model Fit Indices ===\n")
print(knitr::kable(fit_table, digits = 3))

# ---- 7.2 Variance explained (R-squared) ----
cat("\n================================================================\n")
cat("=== Variance Explained (R-squared) for Endogenous Variables ===\n")
cat("\n[Model 1: R-squared for Satisfaction and Personality Traits]\n")
print(lavInspect(fit1, "r2"))

cat("\n[Model 2: R-squared for Resilience and Personality Traits]\n")
print(lavInspect(fit2, "r2"))

# ---- 7.3 Sample attrition analysis ----
cat("\n================================================================\n")
cat("=== Sample Attrition Report ===\n")
cat(sprintf(
  "Initial cohort sample size (Raw)   : %d\nFinal analyzed sample size (Final) : %d\nExcluded due to missing values: %d (%.1f%%)\n",
  nrow(raw_dat), nrow(Analysis_Data), 
  nrow(raw_dat) - nrow(Analysis_Data),
  (nrow(raw_dat) - nrow(Analysis_Data)) / nrow(raw_dat) * 100
))
cat("================================================================\n")