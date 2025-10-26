# Lifespan_Resilience_model
A lifespan resilience model to understand resilience psychosocial mechanisms 

File Overview

A1: Trauma–Mental Health Correlations
	•	A1_1_Cor_Trauma_Mental_all_Group.R：Computes correlations between trauma exposure and mental health outcomes in the entire UKB sample.
	•	A1_2_Cor_Trauma_Mental_in_Different_Group.R：Examines differences in trauma–mental health correlations across resilience groups.
	•	A1_3_Cor_Trauma_Mental_in_Different_Group_Sensitive_Analysis.R：Performs sensitivity analysis using resilience as a continuous variable.
	•	A1_4_Cor_Trauma_Mental_in_Different_Group_Stratified_by_Sex.R：Conducts subgroup analyses stratified by sex to assess potential gender-specific effects.

⸻

A2: Broader Adverse Factors
	•	A2_1_Genernal_Adverse_Factors.R
Examines the associations between general adverse factors and mental health across different resilience groups.

⸻

A3: Longitudinal Analyses
	•	A3_1_Logitudinal_Analysis_UKB.R
Analyzes longitudinal changes in mental health across resilience groups.
	•	A3_2_Logitudinal_Analysis_UKB_matched.R
Repeats the longitudinal analysis using matched samples to control for confounders.
	•	A3_3_Longitudinal_Analysis_UKB_Subgroup.R
Conducts longitudinal analyses stratified by symptom severity subgroups.

⸻

A4: Blood and Proteomic Biomarkers
	•	A4_1_Blood_relation_mediation.R
Tests associations between blood biomarkers, trauma exposure, and resilience.
	•	A4_2_Blood_cor_pattern_permutation.R
Performs permutation analysis to examine the pattern similarity between biomarker–trauma and biomarker–resilience associations.
	•	A4_3_Mediation_between_Blood_and_depression_interaction.R
Tests whether resilience moderates the mediating effects of blood biomarkers on the trauma–mental health relationship.
	•	A4_4_Mediation_between_Blood_and_depression_interaction_grouping.R
Replicates the above moderation-mediation analysis using resilience-based subgrouping.
	•	A4_5_Proteinic_relation_mediation.R
Examines associations between proteomic markers, trauma exposure, and resilience.
	•	A4_6_Proteinic_cor_pattern_permutation.R
Performs permutation tests for proteomic correlation patterns.
	•	A4_7_Mediation_between_Proteinic_and_depression_interaction.R
Tests resilience moderation in the mediation between trauma, proteomic markers, and mental health.
	•	A4_8_Proteinic_Enichment_analysis.R
Conducts enrichment analysis for identified protein biomarkers.

⸻

A5–A6: Resilience Model Construction and Validation
	•	A5_1_UKB_pls_resilience_model.R
Builds a resilience prediction model using Partial Least Squares (PLS) regression in the UK Biobank cohort.
	•	A6_1_IMAGEN_resilience_model.R
Applies and validates the UKB-derived resilience model in the IMAGEN adolescent cohort.

⸻

A7: Subjective Satisfaction Analysis
	•	A7_1_Subjective_satisfication_analysis.R
Compares subjective life satisfaction across different resilience groups.
