library(data.table)
library(tidyverse)
library(rstatix)
library(ggpubr)## 导入基础数据--------
library(data.table)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(caret)
library(dplyr)
library(emmeans)
library(bruceR)
library(nlme)                    # Fit Gaussian linear and nonlinear mixed-effects models
library(lme4)                    # Fit linear and generalized linear mixed-effects models
library(forestploter)
library(survminer)
library(gridExtra)
library(dplyr)
library(gtsummary)
library(autoReg)
library(tidyverse)
library(writexl)
library(MatchIt)
library(lavaan)

# 整理FU的mental health的数据，以每个量表为单位，进行标准化，然后相加。 ------------------------------
#设置文件路径和读取数据：
ukb_path <- c('/public/mig_old_storage/home2/UKB_Tabular_merged_10/')

#获取UKB的代码库
item_id <- as_tibble(fread(str_c(ukb_path,'UKB_FieldID_Subset.csv')))

#查询UKB代码所属子集
df_check <- item_id[which(str_detect(item_id$Field_ID,'23474')),]

#把对应存放数据的csv先缓存进来
#scale_dat <- as_tibble(fread(str_c('/public/home3/UKB_bulks/dataset_4075724/ukb675773.csv')))
#建议考虑其他的一些提数据的方法：
scale_dat <- read_csv(str_c(ukb_path,'UKB_subset_8.csv'))


#提取Blood count--------------------
Blood_count <- scale_dat[, c("eid","30160-0.0", "30220-0.0", "30150-0.0", "30210-0.0", "30030-0.0", "30020-0.0", 
                             "30300-0.0", "30290-0.0", "30280-0.0", "30120-0.0", "30180-0.0", "30050-0.0", 
                             "30060-0.0", "30040-0.0", "30100-0.0", "30260-0.0", "30270-0.0", "30130-0.0",
                             "30190-0.0", "30140-0.0", "30200-0.0", "30170-0.0", "30230-0.0", "30080-0.0", 
                             "30090-0.0", "30110-0.0", "30010-0.0", "30070-0.0", "30250-0.0", "30240-0.0", 
                             "30000-0.0")]


column_abbreviations <- c("eid", "Baso Count", "Baso Percentage", "Eos Count", 
                          "Eos Percentage", "HCT", "HGB", 
                          "HLSR Count", "HLSR Percentage", 
                          "IRF", "Lymph Count", "Lymph Percentage", "MCH", 
                          "MCHC", "MCV", "MPV", 
                          "MRV", "MSCV", "Mono Count", 
                          "Mono Percentage", "Neut Count", "Neut Percentage", "NRBC Count", 
                          "NRBC Percentage", "Platelet Count", 
                          "PCT", "PDW", "RBC Count", 
                          "RDW", "Retic Count", "Retic Percentage", 
                          "WBC Count")
# 将空格替换为下划线
column_abbreviations <- gsub(" ", "_", column_abbreviations)
# 重命名列
names(Blood_count) <- column_abbreviations


#提取Blood biochemistry-----------------------------------
# 提取新的列
blood_biochemistry <- scale_dat[, c("eid","30620-0.0", "30600-0.0", "30610-0.0", "30630-0.0", "30640-0.0", "30650-0.0",
                                    "30710-0.0", "30680-0.0", "30690-0.0", "30700-0.0", "30720-0.0", "30660-0.0",
                                    "30730-0.0", "30740-0.0", "30750-0.0", "30760-0.0", "30770-0.0", "30780-0.0",
                                    "30790-0.0", "30800-0.0", "30810-0.0", "30820-0.0", "30830-0.0", "30850-0.0",
                                    "30840-0.0", "30860-0.0", "30870-0.0", "30880-0.0", "30670-0.0", "30890-0.0")]


column_abbreviations_new <- c("eid", "ALT", "Albumin", "ALP", "Apo A", "Apo B", "AST",
                              "CRP", "Calcium", "Cholesterol", "Creatinine", "Cystatin C",
                              "Direct Bili", "GGT", "Glucose", "HbA1c", "HDL_C",
                              "IGF_1", "LDL_C", "Lp(a)", "Estradiol", "Phosphate",
                              "RF", "SHBG", "Testosterone", "Total Bili",
                              "Total Protein", "TG", "Uric Acid", "Urea", "Vitamin D")
column_abbreviations_new <- gsub(" ", "_", column_abbreviations_new)
# 重命名列
names(blood_biochemistry) <- column_abbreviations_new


#提取第二次采集的血液数据-------------------------
Blood_2nd_1 <- scale_dat[, c("eid","30160-1.0", "30220-1.0", "30150-1.0", "30210-1.0", "30030-1.0", "30020-1.0", 
                             "30300-1.0", "30290-1.0", "30280-1.0", "30120-1.0", "30180-1.0", "30050-1.0", 
                             "30060-1.0", "30040-1.0", "30100-1.0", "30260-1.0", "30270-1.0", "30130-1.0",
                             "30190-1.0", "30140-1.0", "30200-1.0", "30170-1.0", "30230-1.0", "30080-1.0", 
                             "30090-1.0", "30110-1.0", "30010-1.0", "30070-1.0", "30250-1.0", "30240-1.0", 
                             "30000-1.0",
                             "30620-1.0", "30600-1.0", "30610-1.0", "30630-1.0", "30640-1.0", "30650-1.0",
                             "30710-1.0", "30680-1.0", "30690-1.0", "30700-1.0", "30720-1.0", "30660-1.0",
                             "30730-1.0", "30740-1.0", "30750-1.0", "30760-1.0", "30770-1.0", "30780-1.0",
                             "30790-1.0", "30800-1.0", "30810-1.0", "30820-1.0", "30830-1.0", "30850-1.0",
                             "30840-1.0", "30860-1.0", "30870-1.0", "30880-1.0", "30670-1.0", "30890-1.0")]


column_abbreviations_2nd <- c("eid", "Baso Count", "Baso Percentage", "Eos Count", 
                              "Eos Percentage", "HCT", "HGB", 
                              "HLSR Count", "HLSR Percentage", 
                              "IRF", "Lymph Count", "Lymph Percentage", "MCH", 
                              "MCHC", "MCV", "MPV", 
                              "MRV", "MSCV", "Mono Count", 
                              "Mono Percentage", "Neut Count", "Neut Percentage", "NRBC Count", 
                              "NRBC Percentage", "Platelet Count", 
                              "PCT", "PDW", "RBC Count", 
                              "RDW", "Retic Count", "Retic Percentage", 
                              "WBC Count",
                              "ALT", "Albumin", "ALP", "Apo A", "Apo B", "AST",
                              "CRP", "Calcium", "Cholesterol", "Creatinine", "Cystatin C",
                              "Direct Bili", "GGT", "Glucose", "HbA1c", "HDL_C",
                              "IGF_1", "LDL_C", "Lp(a)", "Estradiol", "Phosphate",
                              "RF", "SHBG", "Testosterone", "Total Bili",
                              "Total Protein", "TG", "Uric Acid", "Urea", "Vitamin D")
column_abbreviations_2nd <- gsub(" ", "_", column_abbreviations_2nd)
names(Blood_2nd_1) <- column_abbreviations_2nd


#导入BRS数据————————————————————————————————————————————————
resilience_corr_rename_dat <- read_csv("~/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/UKB_resilience_corr_rename_dat_11221.csv")
resilience_corr_rename_dat <- resilience_corr_rename_dat[,-1]
#resilience_model_dat <- read_csv("~/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/predicited_resiliece_3w_norm_dat.csv")
#resilience_model_dat <- resilience_model_dat[,-1]
soc_env_trauma_mental_dat <- read.csv("~/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/UKB_BL_FU_trauma_mental_env_dat.csv")
soc_env_trauma_mental_dat <- soc_env_trauma_mental_dat[,-1]


# 进行trauma 验证分析 -----------------------------------------------------
library(dplyr)
library(emmeans)
library(bruceR)
library(visreg)

#对数据进行整理，保证真实的resilience和模型计算的resilien能够兼容
resilience_group_R <- resilience_corr_rename_dat
#resilience_group_R <- merge(resilience_group_R,resilience_model_dat, by = "eid", all.x = TRUE)
resilience_group_R <- merge(resilience_group_R,soc_env_trauma_mental_dat[,c(1,71,85,79)], by = "eid", all.x = TRUE)
resilience_group_R <- merge(resilience_group_R,Blood_count, by = "eid", all.x = TRUE)
resilience_group_R <- merge(resilience_group_R,blood_biochemistry, by = "eid", all.x = TRUE)

resilience_group_R$self_resilience <- (resilience_group_R$self_resilience + 6 )/6
resilience_group_R$FU2_recent_trauma <- resilience_group_R$FU2_recent_trauma * 10
resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 2, 6, Ethnic_group))
resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 3, 2, Ethnic_group))
resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 5, 2, Ethnic_group))
resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 4, 3, Ethnic_group))
resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 6, 4, Ethnic_group))

resilience_group_R$Ethnic_group <- resilience_group_R$Ethnic_group - 1
check <-resilience_group_R$Felt_Hated_As_Child_FU1
# 计算分位数
#quantiles <- quantile(resilience_group_R$self_resilience, probs = c(1/3, 2/3),na.rm = TRUE)

# 替换值
#resilience_group_R$self_resilience[resilience_group_R$self_resilience <= quantiles[1]] <- 0
#resilience_group_R$self_resilience[resilience_group_R$self_resilience > quantiles[1] & resilience_group_R$self_resilience < quantiles[2]] <- 1
#resilience_group_R$self_resilience[resilience_group_R$self_resilience >= quantiles[2]] <- 2
#resilience_group_R$self_resilience <- as.factor(resilience_group_R$self_resilience)

resilience_group_R$child_trauma <- resilience_group_R$Felt_Hated_As_Child_FU1 + 4 - resilience_group_R$Felt_Loved_As_Child_FU1 +
  resilience_group_R$Phys_Abused_As_Child_FU1 + resilience_group_R$Sex_Molested_As_Child_FU1 + 4 - resilience_group_R$Someone_Take_To_Doctor_As_Child_FU1
#整理permutation使用的表格-------------
#导入中介后的结果

file_path <- "/public/home/yangzy/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/blood_mediation_data_1203.csv"

# 导入数据
mediation_results_table <- read.csv(file_path)
mediation_results_table_short <- mediation_results_table[mediation_results_table$P_value < 0.001,]
t <- c("trauma_num")

# 加载必要的包
library(dplyr)
library(ggplot2)
library(broom)

# 整理 blood_permut_data
M_biomarker <- mediation_results_table_short$Biomarker
num_bio <- length(M_biomarker)

selected_columns <- select(resilience_group_R, all_of(M_biomarker))

blood_permut_data <- resilience_group_R %>%
  transmute(eid, 
            Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")),
            Age = age_BL, 
            Ethnic = factor(Ethnic_group, levels = c("0", "1", "2", "3"), 
                            labels = c("1_White", "2_Asian", "3_Black", "4_Other")),  
            Education = Education_year,
            BMI = BMI_BL, 
            selfresilience = self_resilience,
            PHQ = Depressive_Symptoms_PHQ4_BL,
            site = factor(site),
            trauma_num) %>%
  bind_cols(selected_columns) %>%
  filter(complete.cases(select(., eid, Sex, Age, Ethnic, Education, BMI, selfresilience, site, trauma_num, PHQ)))

# 定义血液指标列名
blood_list <- M_biomarker

# 定义计算统计量的函数，并返回 r_T 和 r_R
calc_stat <- function(data, blood_cols, trauma_col, resil_col) {
  r_T <- numeric(length(blood_cols))
  r_R <- numeric(length(blood_cols))
  
  for (i in seq_along(blood_cols)) {
    b <- blood_cols[i]
    temp_data_trauma <- data %>% 
      select(all_of(b), all_of(trauma_col), Age, Sex, Ethnic, BMI, Education, site) %>%
      na.omit()
    lme_trauma <- lm(
      as.formula(paste0("`", b, "` ~ ", trauma_col, " + Age + Sex + Ethnic + BMI + Education + site")),
      data = temp_data_trauma)
    results_trauma <- tidy(lme_trauma)
    n <- nrow(temp_data_trauma)
    k <- length(coef(lme_trauma)) - 1
    df <- n - k - 1
    r_T[i] <- results_trauma %>% 
      filter(term == trauma_col) %>% 
      mutate(correlation_r = statistic / sqrt(statistic^2 + df)) %>% 
      pull(correlation_r)
    
    temp_data_resil <- data %>% 
      select(all_of(b), all_of(resil_col), Age, Sex, Ethnic, BMI, Education, site) %>%
      na.omit()
    lme_resil <- lm(
      as.formula(paste0("`", b, "` ~ ", resil_col, " + Age + Sex + Ethnic + BMI + Education + site")),
      data = temp_data_resil)
    results_resil <- tidy(lme_resil)
    n <- nrow(temp_data_resil)
    k <- length(coef(lme_resil)) - 1
    df <- n - k - 1
    r_R[i] <- results_resil %>% 
      filter(term == resil_col) %>% 
      mutate(correlation_r = statistic / sqrt(statistic^2 + df)) %>% 
      pull(correlation_r)
  }
  
  S <- cor(r_T, r_R, use = "complete.obs")
  return(list(S = S, r_T = r_T, r_R = r_R))  # 返回 S 和相关系数向量
}

# 计算真实的统计量并提取 r_T 和 r_R
results_obs <- calc_stat(blood_permut_data, blood_list, "trauma_num", "selfresilience")
S_obs <- results_obs$S
r_T_obs <- results_obs$r_T
r_R_obs <- results_obs$r_R

# 报告真实的相关系数
cat("Observed S (corr(r_T, r_R)):", S_obs, "\n\n")

cat("Correlation with trauma_num (r_T):\n")
r_T_table <- data.frame(Biomarker = blood_list, r_T = r_T_obs)
print(r_T_table, row.names = FALSE)
cat("\n")

cat("Correlation with selfresilience (r_R):\n")
r_R_table <- data.frame(Biomarker = blood_list, r_R = r_R_obs)
print(r_R_table, row.names = FALSE)
cat("\n")

# 置换检验
n_perm <- 1000
S_perm <- numeric(n_perm)

set.seed(123)
for (i in 1:n_perm) {
  perm_idx <- sample(nrow(blood_permut_data))
  perm_data <- blood_permut_data
  perm_data[, c("trauma_num", "selfresilience", "Sex", "Age", "Ethnic", "Education", "BMI", "site")] <- 
    blood_permut_data[perm_idx, c("trauma_num", "selfresilience", "Sex", "Age", "Ethnic", "Education", "BMI", "site")]
  
  S_perm[i] <- calc_stat(perm_data, blood_list, "trauma_num", "selfresilience")$S
}

# 计算p值
p_value <- mean(S_perm <= S_obs, na.rm = TRUE)
cat("p-value:", p_value, "\n")

# 可视化
perm_df <- data.frame(S_perm = S_perm)
ggplot(perm_df, aes(x = S_perm)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "skyblue") +
  geom_vline(xintercept = S_obs, color = "red", linetype = "dashed", size = 1) +
  labs(title = "Permutation Distribution of S", x = "S (corr(r_T, r_R))", y = "Frequency") +
  theme_minimal()   # 去掉网格线

ggsave("permutation_plot.png", width = 8, height = 6)

# 可选：保存相关系数到CSV文件
write.csv(r_T_table, "correlation_trauma.csv", row.names = FALSE)
write.csv(r_R_table, "correlation_resilience.csv", row.names = FALSE)
write.csv(S_perm, "/public/home/yangzy/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/blood_permutation_trauma_resilience_1000.csv", row.names = FALSE)
