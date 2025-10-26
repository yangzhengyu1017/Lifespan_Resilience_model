# ============================================================
# Longitudinal Resilience Analysis with Matched Samples
# - Processes both PHQ and GAD measures
# - Classifies baseline/follow-up severity
# - Conducts propensity score matching
# - Generates summary tables, Sankey plots, and OR estimates
# - computes pairwise odds ratios (OR), confidence
#   intervals (CI), and p-values between resilience subgroups
#   for both recovery and worsening outcomes, across high- and
#   medium-severity groups.
# ============================================================

# ------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------
library(ggalluvial)
library(cowplot)
library(ggsankey)
library(rstatix)
library(dplyr)
library(gtsummary)
library(epitools)
library(MatchIt)
library(readr)
library(gridExtra)
library(tidyr)

# ------------------------------------------------------------
# Import dataset
# ------------------------------------------------------------
resilience_group_R <- read_csv(
  "~/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/UKB_dat_for_analysis_1226.csv"
)[, -1]
# -------------------------------
# Group participants by self-resilience tertiles
# -------------------------------
# Calculate tertiles and assign Low (0), Medium (1), High (2) labels
quantiles <- quantile(resilience_group_R$self_resilience, probs = c(1/3, 2/3), na.rm = TRUE)
resilience_group_R$self_resilience[resilience_group_R$self_resilience <= quantiles[1]] <- 0
resilience_group_R$self_resilience[resilience_group_R$self_resilience > quantiles[1] & resilience_group_R$self_resilience < quantiles[2]] <- 1
resilience_group_R$self_resilience[resilience_group_R$self_resilience >= quantiles[2]] <- 2
resilience_group_R$self_resilience <- as.factor(resilience_group_R$self_resilience)

# ------------------------------------------------------------
# Define parameters
# ------------------------------------------------------------
severity_vars    <- c("PHQ", "GAD")             # Depression / Anxiety
severity_levels  <- c("1", "2", "3")            # Low, Medium, High severity
calipers         <- c(0.000001, 0.001, 0.001)   # Matching calipers per level
resilience_levels <- c("1_Low", "3_High")       # Only low and high resilience used
severity_labels  <- c("High-Severity", "Medium-Severity", "Low-Severity")

# ============================================================
# Main analysis loop for PHQ and GAD
# ============================================================
for (severity_var in severity_vars) {
  cat("\n=== Processing", severity_var, "Analysis ===\n")
  
  # ----------------------------------------------------------
  # Data preparation
  # ----------------------------------------------------------
  resilience_test <- resilience_group_R %>%
    transmute(
      eid,
      Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")),
      Age = age_BL,
      Ethnic = factor(Ethnic_group, levels = c("0", "1", "2", "3"),
                      labels = c("1_White", "2_Asian", "3_Black", "4_Other")),
      Education = Education_year,
      BMI = BMI_BL,
      site = factor(site),
      selfresilience = factor(self_resilience, levels = c(0, 1, 2),
                              labels = c("1_Low", "2_Medium", "3_High")),
      data_FU1 = Mental_onine_date_FU1,
      data_FU2 = Mental_onine_date_FU2,
      PHQ_FU1 = PHQ9_Severity_FU1,
      PHQ_FU2 = `PHQ-9_FU2`,
      GAD_FU1 = General_Anxiety_Disorder_Severity_FU1,
      GAD_FU2 = General_Anxiety_Disorder_Severity_FU2
    ) %>%
    filter(selfresilience != "2_Medium") %>%
    mutate(selfresilience = droplevels(selfresilience)) %>%
    na.omit()
  
  # ----------------------------------------------------------
  # Define severity categories at FU1 and FU2
  # ----------------------------------------------------------
  for (tp in c("FU1", "FU2")) {
    var <- paste0(severity_var, "_", tp)
    resilience_test[[paste0(tp, "_severity")]] <- case_when(
      resilience_test[[var]] >= 10 ~ "3",
      resilience_test[[var]] >= 5  ~ "2",
      TRUE                         ~ "1"
    )
  }
  
  # ----------------------------------------------------------
  # Matching and group comparison
  # ----------------------------------------------------------
  matched_data_list <- list()
  summary_tables <- list()
  
  for (i in seq_along(severity_levels)) {
    level <- severity_levels[i]
    caliper <- calipers[i]
    
    cat("\nMatching for", severity_var, "Severity Level:", level, "\n")
    temp_data <- resilience_test %>% filter(FU1_severity == level)
    sample_sizes <- table(temp_data$selfresilience)
    
    # Skip small samples
    if (any(sample_sizes < 10)) {
      warning("Insufficient sample size for severity level ", level)
      next
    }
    
    # Propensity score matching
    m_out <- matchit(
      selfresilience ~ PHQ_FU1 + GAD_FU1 + Age + Sex + Ethnic + BMI + Education,
      data = temp_data, method = "nearest", distance = "glm", caliper = caliper
    )
    print(summary(m_out))
    
    matched_data <- match.data(m_out) %>%
      filter(weights == 1) %>%
      transmute(eid, Sex, Age, Ethnic, Education, BMI, selfresilience,
                PHQ_FU1, GAD_FU1, FU1_severity, FU2_severity)
    matched_data_list[[level]] <- matched_data
    
    # Summary statistics by group
    summary_table <- matched_data %>%
      tbl_summary(
        by = selfresilience,
        statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~ "{n} ({p}%)"),
        type = list(Age ~ "continuous", Education ~ "continuous",
                    PHQ_FU1 ~ "continuous", GAD_FU1 ~ "continuous")
      ) %>%
      add_p() %>%
      add_overall()
    print(summary_table)
    summary_tables[[level]] <- as_tibble(summary_table)
    
    # Check group difference
    model_check <- lm(as.formula(paste0(severity_var, "_FU1 ~ selfresilience")), data = matched_data)
    print(summary(model_check))
  }
  
  resilience_matched_data <- bind_rows(matched_data_list)
  
  # ----------------------------------------------------------
  # Sankey plots showing severity transitions
  # ----------------------------------------------------------
  for (resilience in resilience_levels) {
    cat("\nGenerating Sankey Plot for", severity_var, resilience, "\n")
    df_sub <- resilience_matched_data %>% filter(selfresilience == resilience)
    
    if (all(c("FU1_severity", "FU2_severity") %in% names(df_sub))) {
      print(df_sub %>% count(FU1_severity, name = "Count"))
      print(df_sub %>% count(FU2_severity, name = "Count"))
      
      df_long <- df_sub %>%
        transmute(FU1_severity, FU2_severity) %>%
        make_long(FU1_severity, FU2_severity)
      
      p <- ggplot(df_long, aes(x = x, next_x = next_x, node = node, next_node = next_node,
                               fill = factor(node), label = node)) +
        geom_alluvial(flow.alpha = 0.6, width = 0.06, space = 300) +
        geom_alluvial_text(size = 3, color = "black") +
        scale_fill_manual(values = c("#F2BC8A", "#E67548", "#BE1F4E")) +
        theme_alluvial(base_size = 18) +
        labs(x = NULL, title = paste(severity_var, resilience)) +
        theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
      print(p)
    } else {
      warning("Missing FU1 or FU2 severity info for ", resilience)
    }
  }
  
  # ----------------------------------------------------------
  # Compute odds ratios for recovery and worsening
  # ----------------------------------------------------------
  resilience_compare <- resilience_matched_data %>%
    mutate(
      recovery = if_else(FU2_severity < FU1_severity, 1, 0),
      worse = if_else(FU2_severity > FU1_severity, 1, 0)
    )
  
  all_results <- list()
  
  for (level in c("3", "2")) {
    for (var in c("recovery", "worse")) {
      subset_data <- resilience_compare %>%
        filter(FU1_severity == level) %>%
        mutate(selfresilience = factor(selfresilience))
      
      groups <- levels(subset_data$selfresilience)
      or_matrix <- matrix(NA, nrow = length(groups), ncol = length(groups),
                          dimnames = list(groups, groups))
      p_matrix <- ci_lower_matrix <- ci_upper_matrix <- or_matrix
      
      for (i in 1:(length(groups) - 1)) {
        for (j in (i + 1):length(groups)) {
          temp_data <- subset_data %>%
            filter(selfresilience %in% c(groups[i], groups[j])) %>%
            mutate(selfresilience = droplevels(selfresilience))
          tab <- table(temp_data$selfresilience, temp_data[[var]])
          
          if (all(dim(tab) == c(2, 2))) {
            or_result <- oddsratio(tab)
            or_matrix[i, j] <- or_matrix[j, i] <- or_result$measure[2, 1]
            ci_lower_matrix[i, j] <- ci_lower_matrix[j, i] <- or_result$measure[2, 2]
            ci_upper_matrix[i, j] <- ci_upper_matrix[j, i] <- or_result$measure[2, 3]
            p_matrix[i, j] <- p_matrix[j, i] <- chisq.test(tab, correct = FALSE)$p.value
          }
        }
      }
      
      long_table <- left_join(
        as.data.frame(as.table(or_matrix)) %>% setNames(c("selfresilience", "CompareGroup", "OR")),
        as.data.frame(as.table(p_matrix)) %>% setNames(c("selfresilience", "CompareGroup", "P")),
        by = c("selfresilience", "CompareGroup")
      ) %>%
        left_join(as.data.frame(as.table(ci_lower_matrix)) %>% setNames(c("selfresilience", "CompareGroup", "CI_Lower")),
                  by = c("selfresilience", "CompareGroup")) %>%
        left_join(as.data.frame(as.table(ci_upper_matrix)) %>% setNames(c("selfresilience", "CompareGroup", "CI_Upper")),
                  by = c("selfresilience", "CompareGroup")) %>%
        mutate(Variable = var, Severity = if_else(level == "3", "High", "Medium"))
      
      all_results <- bind_rows(all_results, long_table)
    }
  }
  
  all_results <- all_results %>%
    dplyr::select(Severity, Variable, selfresilience, CompareGroup, OR, CI_Lower, CI_Upper, P)
  print(all_results)
  
  # ----------------------------------------------------------
  # Plot recovery and worsening proportions
  # ----------------------------------------------------------
  # Recovery
  compare_dat <- resilience_compare %>%
    transmute(recovery, FU1_severity, selfresilience) %>%
    filter(FU1_severity != "1") %>%
    mutate(FU1_severity = factor(FU1_severity, levels = c("3", "2"),
                                 labels = severity_labels[1:2]))
  
  bar_1 <- ggplot(compare_dat, aes(x = FU1_severity, y = recovery, fill = selfresilience)) +
    geom_bar(stat = "summary", fun = "mean", position = position_dodge(), width = 0.6, color = "black") +
    stat_summary(geom = "errorbar", position = position_dodge(width = 0.6), width = 0.15) +
    scale_fill_manual(values = c("#FA9E38", "#4995C6"), name = NULL) +
    theme_classic() +
    labs(x = "", y = "", title = paste(severity_var, "Recovery")) +
    guides(fill = guide_legend(nrow = 1)) +
    theme(legend.position = c(0.5, 0.95))
  
  # Worsening
  compare_dat <- resilience_compare %>%
    transmute(worse, FU1_severity, selfresilience) %>%
    filter(FU1_severity != "3") %>%
    mutate(FU1_severity = factor(FU1_severity, levels = c("2", "1"),
                                 labels = severity_labels[2:3]))
  
  bar_2 <- ggplot(compare_dat, aes(x = FU1_severity, y = worse, fill = selfresilience)) +
    geom_bar(stat = "summary", fun = "mean", position = position_dodge(), width = 0.6, color = "black") +
    stat_summary(geom = "errorbar", position = position_dodge(width = 0.6), width = 0.15) +
    scale_fill_manual(values = c("#FA9E38", "#4995C6"), name = NULL) +
    theme_classic() +
    labs(x = "", y = "", title = paste(severity_var, "Worsening")) +
    guides(fill = guide_legend(nrow = 1)) +
    theme(legend.position = c(0.5, 0.95))
  
  grid.arrange(bar_1, bar_2, ncol = 1)
}


# ============================================================
# Function: pairwise_or_test
# Description:
#   Performs pairwise comparisons (2x2 contingency tables)
#   between levels of a grouping variable for a binary outcome.
#   Returns OR, 95% CI, and p-values in long format.
# ============================================================
pairwise_or_test <- function(data, group_var, compare_var) {
  groups <- levels(data[[group_var]])
  
  # Initialize empty matrices for results
  or_matrix <- matrix(NA, nrow = length(groups), ncol = length(groups),
                      dimnames = list(groups, groups))
  p_matrix <- ci_lower_matrix <- ci_upper_matrix <- or_matrix
  
  # Loop through all unique pairs of groups
  for (i in 1:(length(groups) - 1)) {
    for (j in (i + 1):length(groups)) {
      subset_data <- data %>%
        filter(.data[[group_var]] %in% c(groups[i], groups[j])) %>%
        mutate(!!group_var := droplevels(.data[[group_var]]))
      
      tab <- table(subset_data[[group_var]], subset_data[[compare_var]])
      
      # Only proceed for valid 2x2 tables
      if (all(dim(tab) == c(2, 2))) {
        or_result <- oddsratio(tab)
        test_result <- chisq.test(tab, correct = FALSE)
        
        or_matrix[i, j] <- or_matrix[j, i] <- or_result$measure[2, 1]
        ci_lower_matrix[i, j] <- ci_lower_matrix[j, i] <- or_result$measure[2, 2]
        ci_upper_matrix[i, j] <- ci_upper_matrix[j, i] <- or_result$measure[2, 3]
        p_matrix[i, j] <- p_matrix[j, i] <- test_result$p.value
      }
    }
  }
  
  # Convert matrices to long format
  or_long <- as.data.frame(as.table(or_matrix)) %>%
    setNames(c(group_var, "CompareGroup", "OR"))
  p_long <- as.data.frame(as.table(p_matrix)) %>%
    setNames(c(group_var, "CompareGroup", "P"))
  ci_lower_long <- as.data.frame(as.table(ci_lower_matrix)) %>%
    setNames(c(group_var, "CompareGroup", "CI_Lower"))
  ci_upper_long <- as.data.frame(as.table(ci_upper_matrix)) %>%
    setNames(c(group_var, "CompareGroup", "CI_Upper"))
  
  # Merge results into a single table
  long_table <- or_long %>%
    left_join(p_long, by = c(group_var, "CompareGroup")) %>%
    left_join(ci_lower_long, by = c(group_var, "CompareGroup")) %>%
    left_join(ci_upper_long, by = c(group_var, "CompareGroup")) %>%
    mutate(Variable = compare_var)
  
  return(long_table)
}

# ============================================================
# Analysis for high- and medium-severity subgroups
# ============================================================

# Define variables of interest
variables <- c("recovery", "worse")

# --- High-severity group ---
resilience_high <- resilience_compare %>%
  filter(FU1_severity == 3) %>%
  mutate(selfresilience = factor(selfresilience))

high_results <- lapply(variables, function(var) {
  pairwise_or_test(resilience_high, "selfresilience", var)
}) %>%
  bind_rows() %>%
  mutate(Severity = "High")

# --- Medium-severity group ---
resilience_mid <- resilience_compare %>%
  filter(FU1_severity == 2) %>%
  mutate(selfresilience = factor(selfresilience))

mid_results <- lapply(variables, function(var) {
  pairwise_or_test(resilience_mid, "selfresilience", var)
}) %>%
  bind_rows() %>%
  mutate(Severity = "Medium")

# ============================================================
# Combine and display results
# ============================================================
all_results <- bind_rows(high_results, mid_results) %>%
  dplyr::select(Severity, Variable, selfresilience, CompareGroup, OR, CI_Lower, CI_Upper, P)

print(all_results)
