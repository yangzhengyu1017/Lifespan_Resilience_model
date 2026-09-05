# Lifespan_Resilience_model
A lifespan resilience model to understand resilience psychosocial mechanisms 

File Overview

### Phase A1: Trauma–Mental Health Correlations & Resilience Grouping

- **`A1_1_Cor_Trauma_Mental_all_Group.R`**
  - **Summary**: Quantifies the baseline and longitudinal associations between trauma exposure and mental health outcomes across the full UK Biobank cohort.
  - **Details**: Uses multivariable linear regression and partial correlation analysis (`ppcor`) adjusting for age, sex, ethnic background, BMI, education, and assessment centre across Baseline (BL), Follow-Up 1 (FU1), and Follow-Up 2 (FU2).

- **`A1_2_Cor_Trauma_Mental_in_Different_Group_V2.R`**
  - **Summary**: Compares the strength of trauma–mental health associations across stratified resilience groups (Low, Medium, and High).
  - **Details**: Performs group-stratified regression models and applies Fisher's z-transformation (`cocor`) to statistically test correlation attenuation in individuals with high resilience compared to medium and low resilience groups across BL, FU1, and FU2.

- **`A1_3_Cor_Trauma_Mental_in_Different_Group_Sensitive_Analysis_V2.R`**
  - **Summary**: Assesses continuous moderation effects of self-reported resilience on the association between trauma exposure and mental health symptoms.
  - **Details**: Conducts sensitivity analyses specifying full interaction models (`Mental ~ Trauma × Continuous_Resilience + Covariates`) to verify that resilience buffering is robust and not an artifact of categorical grouping.

- **`A1_4_Cor_Trauma_Mental_in_Different_Group_Stratified_by_Sex_V2.R`**
  - **Summary**: Evaluates sex-specific differences in the relationship between trauma exposure, resilience, and mental health outcomes.
  - **Details**: Stratifies analyses by sex (Female vs. Male) to evaluate whether resilience buffers trauma-related psychological distress equally across sexes across all three longitudinal waves.

- **`A1_5_UKB_Normative_model_for_vatlidation_BRS_traumatic_event.R`**
  - **Summary**: Validates self-reported Brief Resilience Scale (BRS) scores against normative outcome-based mental health symptom residuals adjusted for trauma.
  - **Details**: Extracts residuals from linear models regressing mental health symptoms on trauma exposure and demographic covariates, correlating these objective symptom residuals with subjective BRS scores to establish construct validity.

---

### Phase A2: Broader Adverse Factors

- **`A2_1_Genernal_Adverse_Factors_update_V2.R`**
  - **Summary**: Investigates the relationship between broad environmental, socioeconomic, and early-life adverse factors and mental health outcomes across resilience tiers.
  - **Details**: Evaluates a high-dimensional battery of adverse factors (air pollution, traffic noise, greenspace, socioeconomic deprivation, childhood neglect/abuse) across BL, FU1, and FU2, controlling for focal trauma and correcting for multiple testing using the Benjamini–Hochberg False Discovery Rate (FDR).

- **`A2_2_UKB_Normative_model_for_vatlidation_BRS_general_adverse.R`**
  - **Summary**: Evaluates residual-based resilience against an "all-in" comprehensive adversity framework spanning environmental, social, and early developmental risks.
  - **Details**: Fits comprehensive multivariable models incorporating the full panel of 45+ environmental, social, and developmental adversity indicators simultaneously, demonstrating that subjective resilience correlates with symptom residuals even under multivariable ecological adversity.

---

### Phase A3: Longitudinal Mental Health Trajectories

- **`A3_1_Logitudinal_Analysis_UKB_V2.R`**
  - **Summary**: Tracks longitudinal trajectories of depressive (PHQ) and anxiety (GAD) symptoms over follow-up periods across resilience groups.
  - **Details**: Evaluates symptom progression across longitudinal waves using linear mixed-effects frameworks to assess whether high resilience confers sustained protection over time in the unmatched cohort.

- **`A3_2_Logitudinal_Analysis_UKB_matched_V2.R`**
  - **Summary**: Analyzes longitudinal symptom trajectories in propensity-score matched cohorts to minimize baseline confounding.
  - **Details**: Employs Propensity Score Matching (PSM) across demographic and baseline clinical covariates to compare mental health trajectories between high-resilience and low/medium-resilience individuals under balanced baseline conditions.

- **`A3_3_Longitudinal_Analysis_UKB_Subgroup_V2.R`**
  - **Summary**: Evaluates longitudinal symptom remission and recovery across resilience groups stratified by baseline symptom severity.
  - **Details**: Stratifies participants by initial depressive symptom severity (mild, moderate, severe) to examine whether resilience facilitates symptomatic recovery or prevents symptom worsening over follow-up.

- **`A3_4_Recovery_odd_ratio.R`**
  - **Summary**: Computes pairwise odds ratios and 95% confidence intervals for symptom recovery versus worsening across resilience groups.
  - **Details**: Utilizes contingency table analyses and `epitools::oddsratio` with chi-squared tests to calculate odds ratios for clinical recovery among individuals presenting with high and medium initial symptom burdens.

---

### Phase A4: Blood and Proteomic Biomarker Signatures

- **`A4_1_Blood_relation_mediation.R`**
  - **Summary**: Analyzes multivariable associations and formal mediation pathways connecting peripheral blood biomarkers with trauma and resilience.
  - **Details**: Evaluates clinical hematology panels (complete blood counts) and serum biochemistry markers, identifying systemic physiological mediators that transmit or buffer the biological impact of psychological trauma.

- **`A4_2_Blood_cor_pattern_permutation.R`**
  - **Summary**: Tests the global pattern similarity of blood biomarker associations between trauma exposure and psychological resilience via permutation testing.
  - **Details**: Computes the pattern-similarity correlation statistic ($S_{	ext{obs}}$) across all blood markers and benchmarks it against an empirical null distribution generated from 1,000 predictor-permuted iterations preserving biomarker–covariate structures.

- **`A4_5_Proteinic_relation_mediation.R`**
  - **Summary**: Identifies circulating plasma proteomic markers (Olink panel) associated with trauma, depression, and resilience, performing high-throughput mediation modeling.
  - **Details**: Integrates normalized protein expression matrices (`.mat`) with phenotypic data, testing linear regressions with FDR adjustment and identifying specific inflammatory, metabolic, and neurotrophic proteins acting as mediators.

- **`A4_6_Proteinic_cor_pattern_permutation.R`**
  - **Summary**: Evaluates global proteomic pattern consistency between trauma exposure and resilience using high-throughput permutation testing.
  - **Details**: Executes 1,000 permutations of psychological predictors while holding covariate–protein covariance structures fixed, demonstrating that the global proteomic signature of resilience systematically mirrors and opposes trauma-induced biological alterations.

- **`A4_7_Proteinic_Enichment_analysis.R`**
  - **Summary**: Performs functional biological enrichment on resilience-associated proteomic biomarkers to identify involved molecular pathways.
  - **Details**: Maps significant mediator proteins to Entrez Gene IDs and performs Gene Ontology (GO: Biological Process, Cellular Component, Molecular Function), KEGG pathway, and Reactome pathway enrichment analyses.

---

### Phases A5–A6: Resilience Model Construction, Biological Validation & Cross-Cohort Generalization

- **`A5_1_UKB_pls_resilience_model_Hauf_T_V2.R`**
  - **Summary**: Constructs a multivariate psychological resilience prediction model in UK Biobank using Partial Least Squares Regression (PLSR) with Haufe transformation and bootstrap stability.
  - **Details**: Splits the cohort into training and testing sets, standardizes predictors strictly within training folds to avoid data leakage, optimizes the number of latent components via cross-validation, applies the Haufe transformation to recover interpretable activation patterns, and benchmarks feature importance across 1,000 bootstrap iterations.

- **`A5_2_Pre_Resilience_Validation_Predicted_Resiliece_BRS_Neu_Blood.R`**
  - **Summary**: Benchmarks predicted resilience against self-reported BRS, inverted neuroticism, and peripheral blood biomarker profiles.
  - **Details**: Evaluates model-derived resilience against both subjective resilience and inverted neuroticism, generating master summary tables, comparative heatmaps, Steiger's Z-tests for dependent correlations, and three-panel permutation null distributions.

- **`A5_3_Pre_Resilience_Validation_Predicted_Resiliece_Protein.R`**
  - **Summary**: Validates the predicted resilience score against high-dimensional circulating plasma proteomic profiles.
  - **Details**: Evaluates associations between model-predicted resilience and proteomic markers, generating dual-threshold significance heatmaps (raw $p < 0.05$ and FDR-adjusted $p < 0.05$) and verifying global proteomic alignment via permutation testing.

- **`A5_4_UKB_tow_stage_SEM_regressout_cov.R`**
  - **Summary**: Implements two-stage structural equation modeling (SEM) to delineate direct and indirect pathways linking objective environment, Big Five personality traits, subjective satisfaction, and resilience.
  - **Details**: Residualizes all indicators against demographic covariates prior to model estimation, fits dual structural models with 1,000 BCa bootstrap resamples, calculates standardized indirect effects and mediation proportions, and produces publication-ready horizontal summary plots.

- **`A6_1_IMAGEN_resilience_model.R`**
  - **Summary**: Translates and externally validates the UK Biobank-derived resilience model in the longitudinal adolescent IMAGEN cohort across ages 14 to 22.
  - **Details**: Maps UKB PLSR model weights onto adolescent IMAGEN participants, identifies model-resilient individuals, visualizes 8-year longitudinal trajectories of internalizing disorder risks, and conducts equivalence/non-inferiority testing against low-risk controls.

---
