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
#分析Blood count和创伤的相关-------------
#定义trauma
t <- c("trauma_num")
#biomarker <- c("BC30160", "PBP30220", "CE30150", "PEP30210", "LC30120", "LPC30180", "MC30130", 
#               "MPC30190", "NC30140", "NPC30200", "WBC30000",
#               "CRP30710")
biomarker <- c("Baso Count", "Baso Percentage", "Eos Count", 
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
biomarker <- gsub(" ", "_", biomarker)
# 创建一个空的列表来存储生成的图表
# 加载所需包
library(ggplot2)
library(dplyr)
library(broom)  # 用于 tidy()

# 定义 trauma 和 biomarker
t <- c("trauma_num")
#biomarker <- c("BC30160", "PBP30220", "CE30150", "PEP30210", "LC30120", "LPC30180", "MC30130", 
#               "MPC30190", "NC30140", "NPC30200", "WBC30000", "CRP30710")

# 创建空的列表和数据框
trauma_lines <- list()
trauma_reg <- list()
result_table <- data.frame()
trauma_result_table <- data.frame()
PHQ_results_table <- data.frame()

for (b in biomarker) {
  # 创建新数据集和新变量
  resilience_test <- resilience_group_R %>% 
    transmute(eid, 
              Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")), 
              Age = age_BL, 
              Ethnic = factor(Ethnic_group, levels = c("0", "1", "2", "3"), 
                              labels = c("1_White", "2_Asian", "3_Black", "4_Other")),  
              Education = Education_year,
              site = factor(site),
              BMI = BMI_BL,
              PHQ = Depressive_Symptoms_PHQ4_BL,
              selfresilience = self_resilience,
              !!paste(t) := !!sym(t),
              !!paste(b) := .data[[b]])
  
  # 移除缺失值并记录样本量
  resilience_test <- na.omit(resilience_test)
  n_subjects <- nrow(resilience_test)  # 当前biomarker的有效样本量
  
  # 线性回归模型
  lme3 <- lm(
    as.formula(paste0("`", b, "` ~ ", t, " + Age + Sex + Ethnic + BMI + Education + site")),
    data = resilience_test)
  
  # 提取回归结果
  results <- tidy(lme3)
  
  # 添加 Biomarker 和样本量列
  results$Biomarker <- b
  results$Subject_Count <- n_subjects  # 添加样本量
  
  # 合并到 result_table
  result_table <- bind_rows(result_table, results)
  
  # 筛选 trauma 的结果并添加样本量
  trauma_results <- results %>% 
    filter(term %in% t) %>% 
    mutate(
      Biomarker = b,
      Subject_Count = n_subjects  # 确保样本量被保留
    )
  
  trauma_result_table <- bind_rows(trauma_result_table, trauma_results)
  
  # 计算自由度 (df = n - k - 1)
  n <- n_subjects  # 使用已计算的样本量
  k <- length(coef(lme3)) - 1  # 自变量数量（不含截距）
  df <- n - k - 1  # 剩余自由度
  
  # 计算相关系数 r
  trauma_result_table <- trauma_result_table %>%
    mutate(correlation_r = statistic / sqrt(statistic^2 + df))
}

# 添加 Stage 列
trauma_result_table$Stage <- "Blood Count"

# FDR 校正
trauma_result_table <- trauma_result_table %>%
  mutate(adjusted_p = p.adjust(p.value, method = "BH"))

# 筛选显著结果
trauma_result_table_adjusted <- trauma_result_table %>% 
  filter(adjusted_p < 0.05)

# 查看结果（包含样本量）
print(head(trauma_result_table))

# 可选：检查每个biomarker的样本量分布
table(trauma_result_table$Biomarker, trauma_result_table$Subject_Count)



##分析和心理的相关---------------
# 定义 biomarker
#biomarker <- c("BC30160", "PBP30220", "CE30150", "PEP30210", "LC30120", "LPC30180", "MC30130", 
#               "MPC30190", "NC30140", "NPC30200", "WBC30000", "CRP30710")

# 初始化结果表（假设 result_table 已定义）
result_table <- data.frame()
PHQ_results_table <- data.frame()

for (b in biomarker) {
  # 创建新数据集和新变量
  resilience_test <- resilience_group_R %>% 
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
              !!paste(b) := .data[[b]],
              !!paste(t) := !!sym(t))  # 注意：t 未定义，可能需要修正
  
  # 移除缺失值
  resilience_test <- na.omit(resilience_test)
  n_subjects <- nrow(resilience_test)  # 当前biomarker的有效样本量
  # 线性回归模型
  lme3 <- lm(
    as.formula(paste0("`", b, "` ~ PHQ + Age + Sex + Ethnic + BMI + Education + site")),
    data = resilience_test)
  
  # 提取回归结果
  results <- tidy(lme3)
  
  # 添加 Biomarker 列
  results$Biomarker <- b
  results$Subject_Count <- n_subjects  # 添加样本量
  # 合并到 result_table
  result_table <- bind_rows(result_table, results)
  
  # 筛选 PHQ 的结果
  PHQ_results <- results %>% filter(term %in% c("PHQ"))
  PHQ_results$Biomarker <- b
  
  # 计算自由度 (df = n - k - 1)
  n <- n_subjects  # 使用已计算的样本量
  k <- length(coef(lme3)) - 1  # 自变量数量（不含截距）
  df <- n - k - 1  # 剩余自由度
  
  # 添加到 PHQ_results_table 并计算相关系数 r
  PHQ_results <- PHQ_results %>%
    mutate(correlation_r = statistic / sqrt(statistic^2 + df))
  PHQ_results_table <- bind_rows(PHQ_results_table, PHQ_results)
}

# 添加 Stage 列
PHQ_results_table$Stage <- "Blood Protein"

# FDR 校正
p_values <- PHQ_results_table$p.value
adjusted_p_values <- p.adjust(p_values, method = "BH")
PHQ_results_table$adjusted_p <- adjusted_p_values

# 筛选显著结果
PHQ_results_table_adjusted <- PHQ_results_table[PHQ_results_table$adjusted_p < 0.05, ]

# 查看结果
print(head(PHQ_results_table))

both_results <- merge(trauma_result_table, PHQ_results_table, by = "Biomarker",all.x = T)
both_results_adj <- both_results[both_results$adjusted_p.x <0.05 & both_results$adjusted_p.y <0.05,]

##分析和Resilience的相关---------------
# 初始化结果表
R_results_table <- data.frame()
I_results_table <- data.frame()
interact_result_table <- data.frame()
result_table <- data.frame()  # 假设 result_table 已定义


# 定义 t（假设与之前一致）
t <- "trauma_num"  # 根据上下文定义，确保与之前一致

for (b in biomarker) {
  # 创建新数据集和新变量
  resilience_test <- resilience_group_R %>% 
    transmute(eid, 
              Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")), 
              Age = age_BL, 
              Ethnic = factor(Ethnic_group, levels = c("0", "1", "2", "3"), 
                              labels = c("1_White", "2_Asian", "3_Black", "4_Other")),  
              Education = Education_year,
              site = factor(site),
              BMI = BMI_BL, 
              selfresilience = self_resilience,
              PHQ = Depressive_Symptoms_PHQ4_BL,
              !!paste(b) := .data[[b]],
              !!paste(t) := !!sym(t))
  
  # 移除缺失值
  resilience_test <- na.omit(resilience_test)
  n_subjects <- nrow(resilience_test)  # 当前biomarker的有效样本量
  # 模型 1：仅 selfresilience
  lme3 <- lm(
    as.formula(paste0("`", b, "` ~ selfresilience + Age + Sex + Ethnic + BMI + Education + site")),
    data = resilience_test)
  
  # 提取结果
  results <- tidy(lme3)
  
  
  # 添加 Biomarker 列
  results$Biomarker <- b
  results$Subject_Count <- n_subjects  # 添加样本量
  # 合并到结果表
  result_table <- bind_rows(result_table, results)
  
  # 计算自由度 (df = n - k - 1)
  n <- n_subjects  # 使用已计算的样本量
  k_lme3 <- length(coef(lme3)) - 1  # lme3 自变量数量
  df_lme3 <- n - k_lme3 - 1  # lme3 剩余自由度
  
  
  # 筛选 selfresilience 结果并计算 r
  R_results <- results %>% 
    filter(term %in% c("selfresilience")) %>%
    mutate(correlation_r = statistic / sqrt(statistic^2 + df_lme3))
  R_results$Biomarker <- b
  R_results_table <- bind_rows(R_results_table, R_results)
  
  
}

# 添加 Stage 列
R_results_table$Stage <- "Blood Protein"

# FDR 校正
p_values <- R_results_table$p.value
adjusted_p_values <- p.adjust(p_values, method = "BH")
R_results_table$adjusted_p <- adjusted_p_values

# 筛选显著结果
R_results_table_adjusted <- R_results_table[R_results_table$adjusted_p < 0.05, ]

# 查看结果
print(head(R_results_table))


three_results <- merge(both_results, R_results_table, by = "Biomarker",all.x = T)
three_results <- merge(both_results_shorted, R_results_table, by = "Biomarker",all.x = T)



#合并Blood的数据----------------------
M_biomarker <- both_results_adj$Biomarker
num_bio <- length(M_biomarker)

selected_columns <- select(resilience_group_R, all_of(M_biomarker))

blood_dat <- resilience_group_R %>%
  transmute(eid, 
            Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")),
            Age = age_BL, 
            Ethnic = factor(Ethnic_group, levels = c("0", "1", "2", "3"), labels = c("1_White", "2_Asian", "3_Black", "4_Other")),  
            Education = Education_year,
            BMI = BMI_BL, 
            selfresilience = self_resilience,
            site = factor(site),
            PHQ = Depressive_Symptoms_PHQ4_BL,
            trauma_num)

blood_dat <- cbind(blood_dat,selected_columns)


blood_dat_scale <- blood_dat
blood_dat_scale[, c(11:(10+num_bio))] <- scale(blood_dat_scale[, c(11:(10+num_bio))])

#批量中介分析(加入协变量）----------------------------
library(lavaan)
library(mediation)

M <- both_results_adj$Biomarker

mediation_results_table <- data.frame(Biomarker = character(),
                                      Estimate = numeric(),
                                      CI_lower = numeric(),
                                      CI_upper = numeric(),
                                      P_value = numeric(),
                                      a = numeric(),
                                      p_a = numeric(),
                                      b = numeric(),
                                      p_b = numeric(),
                                      c = numeric(),
                                      p_c = numeric(),
                                      stringsAsFactors = FALSE)

# 循环遍历M向量中的每个变量
for (i in 1:length(M)) {
  biomarker <- M[i]
  
  myData <- blood_dat_scale %>% 
    transmute(eid, 
              Sex = Sex, 
              Age = Age, 
              Ethnic = Ethnic,  
              Education = Education,
              BMI = BMI, 
              selfresilience = selfresilience,
              site = factor(site),
              PHQ = PHQ,
              !!paste(biomarker) := .data[[biomarker]],
              !!paste(t) := !!sym(t))  # 注意：t 未定义，可能需要修正
  
  myData <- na.omit(myData)
  
  # Step1，自变量和因变量是否有显著关系
  model.0 <- lm(PHQ ~ trauma_num + Sex + Age + Ethnic +Education + BMI + site, myData)
  
  # Step2，自变量和中介变量是否有显著关系
  model.M <- lm(as.formula(paste0("`", biomarker, "` ~ trauma_num + Sex + Age + Ethnic +Education + BMI + site")), myData)
  
  # Step3，自变量和中介变量的关系考虑进去之后，中介变量和因变量是否有显著关系
  model.Y <- lm(as.formula(paste0("PHQ ~ `", biomarker, "`+ trauma_num + Sex + Age + Ethnic +Education + BMI + site")), myData)
  
  # 用bootstrapping，看中介效应是否显著
  results <- mediate(model.M, model.Y, treat = 'trauma_num', mediator = biomarker, boot = TRUE, sims = 1000)
  
  # 提取中介效应的估计值和置信区间
  estimate <- summary(results)$d0
  ci_lower <- summary(results)$d0.ci[1]
  ci_upper <- summary(results)$d0.ci[2]
  coefficients <- coef(model.M)
  a <- coefficients["trauma_num"]
  summary_model.m <- summary(model.M)
  coefficients_table <- summary_model.m$coefficients
  p_a <- coefficients_table["trauma_num", "Pr(>|t|)"]
  
  coefficients <- coef(model.Y)
  #  b <- coefficients[paste0("`",biomarker,"`")]
  # 提取 b 值（根据行名是否需要反引号）
  if (paste0("`", biomarker, "`") %in% names(coefficients)) {
    b <- coefficients[paste0("`",biomarker,"`")]
  } else {
    b <- coefficients[biomarker]
  }
  summary_model.Y <- summary(model.Y)
  coefficients_table <- summary_model.Y$coefficients
  # 提取 p 值（根据行名是否需要反引号）
  if (paste0("`", biomarker, "`") %in% rownames(coefficients_table)) {
    p_b <- coefficients_table[paste0("`", biomarker, "`"), "Pr(>|t|)"]
  } else {
    p_b <- coefficients_table[biomarker, "Pr(>|t|)"]
  }
  c <- coefficients["trauma_num"]
  p_c <- coefficients_table["trauma_num", "Pr(>|t|)"]
  
  
  # 提取中介效应的P值
  p_value <- summary(results)$d0.p
  
  # 直接将结果添加到数据框中
  mediation_results_table[i, ] <- c(biomarker, estimate, ci_lower, ci_upper, p_value, a, p_a, b, p_b, c, p_c)
}


# 打印结果表格
print(mediation_results_table)

# 定义保存路径
file_path <- "/public/home/yangzy/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/blood_mediation_data_1203.csv"

# 将数据保存为 CSV 文件
write.csv(mediation_results_table, file = file_path, row.names = FALSE)
# 导入数据
mediation_results_table <- read.csv(file_path)

mediation_results_table_short <- mediation_results_table[mediation_results_table$P_value < 0.001,]



#血液指标到PHQ--------------
# 初始化数据框
result_table <- data.frame()
I_results_table <- data.frame()
interact_result_table <- data.frame()
main_effect_table <- data.frame()  # 新增：存储主效应结果

# 血液指标列表
biomarker <- mediation_results_table_short$Biomarker

# 循环处理每个 Biomarker
for (b in biomarker) {
  # 创建新数据集
  resilience_test <- resilience_group_R %>%
    transmute(eid, 
              Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")), 
              Age = age_BL, 
              Ethnic = factor(Ethnic_group, levels = c("0", "1", "2", "3"), labels = c("1_White", "2_Asian", "3_Black", "4_Other")),  
              Education = Education_year,
              site = factor(site),
              BMI = BMI_BL, 
              selfresilience = self_resilience,
              PHQ = Depressive_Symptoms_PHQ4_BL,
              !!paste(b) := !!sym(b),
              !!paste(t) := !!sym(t)) 
  
  # 删除 selfresilience == 1 的行
  resilience_test <- resilience_test[resilience_test$selfresilience != 1, ]
  
  # 删除 NA 值
  resilience_test <- na.omit(resilience_test)
  
  # 线性回归模型
  lme4 <- lm(
    as.formula(paste0("PHQ ~ `", b, "` + selfresilience + `", b, "`*selfresilience + Age + Sex + Ethnic + BMI + Education + site")),
    data = resilience_test)
  
  # 提取回归结果
  interact_result <- tidy(lme4)
  
  # 存储所有回归结果
  interact_result_table <- bind_rows(interact_result_table, interact_result)
  
  # 提取交互项 (b:selfresilience)
  I_results <- interact_result %>% 
    filter(grepl(paste0(b, ".*selfresilience"), term))
  I_results$Biomarker <- b
  I_results_table <- bind_rows(I_results_table, I_results)
  
  # 提取主效应 (b 的主效应项)
  main_effect <- interact_result %>% 
    filter(term == paste0(b))  # 匹配 b 的主效应项
  main_effect$Biomarker <- b
  main_effect_table <- bind_rows(main_effect_table, main_effect)
}

# 添加 Stage 列
I_results_table$Stage <- "Blood Protein"
main_effect_table$Stage <- "Blood Protein"  # 新增：为主效应表添加 Stage

# FDR 校正交互项的 p 值
p_values <- I_results_table$p.value
adjusted_p_values <- p.adjust(p_values, method = "BH")
I_results_table$adjusted_p <- adjusted_p_values

# 筛选显著的交互项
I_results_table_adjusted <- I_results_table[I_results_table$adjusted_p < 0.05, ]

# FDR 校正主效应的 p 值（可选）
p_values_main <- main_effect_table$p.value
adjusted_p_values_main <- p.adjust(p_values_main, method = "BH")
main_effect_table$adjusted_p <- adjusted_p_values_main

# 筛选显著的主效应（可选）
main_effect_table_adjusted <- main_effect_table[main_effect_table$adjusted_p < 0.05, ]

# 查看结果
print("交互项结果：")
print(head(I_results_table))
print("主效应结果：")
print(head(main_effect_table))



#血液指标对trauma到PHQ的调节------
#然后做蛋白调节trauma到phq
result_table <- data.frame()
I_results_table <- data.frame()
interact_result_table <- data.frame()

biomarker <- R_results_table_adjusted$Biomarker
for (b in biomarker) {
  resilience_test <- resilience_group_R %>% # 创建新数据集新变量  
    transmute(eid, Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")), Age = age_BL, 
              Ethnic = factor(Ethnic_group, levels = c("0","1","2","3"), labels = c("1_White","2_Asian","3_Black","4_Other")),  
              Education = Education_year,
              site =factor(site),
              BMI = BMI_BL, 
              #              selfresilience = self_resilience,
              PHQ = Depressive_Symptoms_PHQ4_BL,
              !!paste(b) := !!sym(b),
              !!paste(t) := !!sym(t))
  
  
  resilience_test <- na.omit(resilience_test)
  
  lme4 <- lm(
    as.formula(paste0("`PHQ` ~ `", b, "`+ trauma_num  +",b,"*trauma_num + Age + Sex + Ethnic + BMI + Education + site")),
    data = resilience_test)
  interact_result <- tidy(lme4)
  # Add trauma variable as a column to identify results from each iteration
  interact_result$Biomarker <- b
  # Append the results to the result_table
  interact_result_table <- bind_rows(interact_result_table, interact_result)
  
  I_results <- interact_result %>% filter(term == paste0(b, ":trauma_num"))
  I_results$Biomarker <- b
  I_results_table <- bind_rows(I_results_table, I_results)
  
}


I_results_table$Stage <-"Blood Protein"

p_values <- I_results_table$p.value
# Apply Benjamini-Hochberg FDR correction
adjusted_p_values <- p.adjust(p_values, method = "BH")
I_results_table$adjusted_p <- adjusted_p_values

I_results_table_adjusted <- I_results_table[I_results_table$adjusted_p < 0.05,]

#对FU1的调节----------
resilience_group_R$FU1_symptoms <- resilience_group_R$PHQ9_Severity_FU1/28 + resilience_group_R$General_Anxiety_Disorder_Severity_FU1/21
result_table <- data.frame()
I_FU1_results_table <- data.frame()
interact_result_table <- data.frame()
#biomarker <- both_results_adj$Biomarker
#biomarker <- I_results_table_adjusted$Biomarker
for (b in biomarker) {
  resilience_test <- resilience_group_R %>% # 创建新数据集新变量  
    transmute(eid, Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")), Age = age_BL, 
              Ethnic = factor(Ethnic_group, levels = c("0","1","2","3"), labels = c("1_White","2_Asian","3_Black","4_Other")),  
              Education = Education_year,
              site =factor(site),
              BMI = BMI_BL, 
              #              selfresilience = self_resilience,
              PHQ = FU1_symptoms,
              !!paste(b) := !!sym(b),
              trauma_num = FU_recent_trauma)
  
  
  resilience_test <- na.omit(resilience_test)
  
  lme4 <- lm(
    as.formula(paste0("`PHQ` ~ `", b, "`+ trauma_num  +",b,"*trauma_num + Age + Sex + Ethnic + BMI + Education + site")),
    data = resilience_test)
  interact_result <- tidy(lme4)
  # Add trauma variable as a column to identify results from each iteration
  interact_result$Biomarker <- b
  # Append the results to the result_table
  interact_result_table <- bind_rows(interact_result_table, interact_result)
  
  I_results <- interact_result %>% filter(term == paste0(b, ":trauma_num"))
  I_results$Biomarker <- b
  I_FU1_results_table <- bind_rows(I_FU1_results_table, I_results)
  
}


I_FU1_results_table$Stage <-"Blood Protein"

p_values <- I_FU1_results_table$p.value
# Apply Benjamini-Hochberg FDR correction
adjusted_p_values <- p.adjust(p_values, method = "BH")
I_FU1_results_table$adjusted_p <- adjusted_p_values

I_FU1_results_table_adjusted <- I_FU1_results_table[I_FU1_results_table$adjusted_p < 0.05,]


#对FU2的调节----------
resilience_group_R$FU2_symptoms <- (resilience_group_R$`PHQ-9_FU2`/28 + resilience_group_R$General_Anxiety_Disorder_Severity_FU2/21) * 10
result_table <- data.frame()
I_FU2_results_table <- data.frame()
interact_result_table <- data.frame()
#biomarker <- both_results_shorted$Biomarker
#biomarker <- I_results_table_adjusted$Biomarker
for (b in biomarker) {
  resilience_test <- resilience_group_R %>% # 创建新数据集新变量  
    transmute(eid, Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")), Age = age_BL, 
              Ethnic = factor(Ethnic_group, levels = c("0","1","2","3"), labels = c("1_White","2_Asian","3_Black","4_Other")),  
              Education = Education_year,
              site =factor(site),
              BMI = BMI_BL, 
              #              selfresilience = self_resilience,
              PHQ = FU2_symptoms,
              !!paste(b) := !!sym(b),
              trauma_num = FU2_recent_trauma)
  
  
  resilience_test <- na.omit(resilience_test)
  
  lme4 <- lm(
    as.formula(paste0("`PHQ` ~ `", b, "`+ trauma_num  +",b,"*trauma_num + Age + Sex + Ethnic + BMI + Education + site")),
    data = resilience_test)
  interact_result <- tidy(lme4)
  # Add trauma variable as a column to identify results from each iteration
  interact_result$Biomarker <- b
  # Append the results to the result_table
  interact_result_table <- bind_rows(interact_result_table, interact_result)
  
  I_results <- interact_result %>% filter(term == paste0(b, ":trauma_num"))
  I_results$Biomarker <- b
  I_FU2_results_table <- bind_rows(I_FU2_results_table, I_results)
  
}


I_FU2_results_table$Stage <-"Blood Protein"

p_values <- I_FU2_results_table$p.value
# Apply Benjamini-Hochberg FDR correction
adjusted_p_values <- p.adjust(p_values, method = "BH")
I_FU2_results_table$adjusted_p <- adjusted_p_values

I_FU2_results_table_adjusted <- I_FU2_results_table[I_FU2_results_table$adjusted_p < 0.05,]


#分组散点图-------
# 计算分位数
quantiles <- quantile(resilience_group_R$self_resilience, probs = c(1/3, 2/3),na.rm = TRUE)

# 替换值
resilience_group_R$self_resilience[resilience_group_R$self_resilience <= quantiles[1]] <- 0
resilience_group_R$self_resilience[resilience_group_R$self_resilience > quantiles[1] & resilience_group_R$self_resilience < quantiles[2]] <- 1
resilience_group_R$self_resilience[resilience_group_R$self_resilience >= quantiles[2]] <- 2
resilience_group_R$self_resilience <- as.factor(resilience_group_R$self_resilience)

library(ggplot2)
library(dplyr)
library(ggpubr)  # 用于 stat_cor
library(patchwork)  # 用于拼接图表
library(ggExtra)  # 用于添加边缘分布图

# 定义 biomarker（假设包含 5 个值）
biomarker <- mediation_results_table_short$Biomarker
biomarker <- I_results_table_adjusted$Biomarker
biomarker <- c("CRP30710","WBC30000","NC30140")
plot_list <- list()

for (b in biomarker) {
  cor_data <- resilience_group_R %>% 
    transmute(eid, 
              Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")), 
              Age = age_BL, 
              Ethnic = factor(Ethnic_group, levels = c("0", "1", "2", "3"), 
                              labels = c("1_White", "2_Asian", "3_Black", "4_Other")),  
              Education = Education_year,
              site = factor(site),
              BMI = BMI_BL, 
              selfresilience = self_resilience,
              PHQ = Depressive_Symptoms_PHQ4_BL,
              trauma_num = trauma_num,
              !!paste(b) := !!sym(b)) %>%
    filter(abs(!!sym(b) - mean(!!sym(b), na.rm = TRUE)) <= 3 * sd(!!sym(b), na.rm = TRUE))
  
  cor_data$selfresilience <- as.factor(cor_data$selfresilience)
  cor_data <- na.omit(cor_data)
  
  model_m <- lm(as.formula(paste("PHQ ~  Age + Sex + Ethnic + BMI + Education +site")), 
                data = cor_data)
  cor_data$PHQ_BL_reg <- residuals(model_m)
  
  base_plot <- ggplot(cor_data, aes(x = .data[[b]], y = PHQ_BL_reg))
  
  p <- base_plot + 
    geom_point(aes(color = selfresilience, shape = selfresilience), 
               alpha = 0.2, size = 0.4, 
               position = position_jitter(width = 0, height = 0.4)) +
    geom_smooth(aes(color = selfresilience, fill = selfresilience), method = "lm") +
    scale_color_manual(values = c("#ffbe7a", "#9bbf8a", "#82afda")) +
    scale_fill_manual(values = c("#ffbe7a", "#9bbf8a", "#82afda")) +
    ggpubr::stat_cor(aes(color = selfresilience), 
                     label.x = max(cor_data[[b]], na.rm = TRUE) * 0.90, 
                     size = 3, hjust = 1) +
    labs(x = b, y = "PHQ", color = "", shape = "", fill = "") +
    theme(
      axis.line = element_line(color = "black"),  # 添加 x 和 y 轴线
      panel.grid.major = element_blank(),         # 移除主网格线
      panel.grid.minor = element_blank(),         # 移除次网格线
      panel.background = element_blank(),          # 移除灰色背景
      legend.position = "none"                    # 移除图例
    )
  
  p_with_margins <- ggMarginal(p, type = "density", groupColour = TRUE, groupFill = TRUE)
  plot_list[[b]] <- p_with_margins
}

combined_plot <- wrap_plots(plot_list, ncol = 3, guides = "collect")
print(combined_plot) #导出12inch*4inch pdf 

#画热图------------
library(ComplexHeatmap)
library(RColorBrewer)
colnames(mediation_results_table_short)[5] <- "mediation_p"
phq_plot_dat <- merge(PHQ_results_table, mediation_results_table_short[,c(1,5)], by = "Biomarker", all.x = T)
#phq_plot_dat <- PHQ_results_table
phq_plot_dat <- na.omit(phq_plot_dat)
phq_plot_dat$term <- "Affective Disorders"

trauma_plot_dat <- merge(trauma_result_table, mediation_results_table_short[,c(1,5)], by = "Biomarker", all.x = T)
#trauma_plot_dat <- trauma_result_table
trauma_plot_dat <- na.omit(trauma_plot_dat)
trauma_plot_dat$term <- "Trauma Exposure"


resilience_plot_dat <- merge(R_results_table, mediation_results_table_short[,c(1,5)], by = "Biomarker", all.x = T)
#resilience_plot_dat <- R_results_table
resilience_plot_dat <- na.omit(resilience_plot_dat)
resilience_plot_dat$term <- "Resilience"
#resilience_plot_dat$statistic <- -resilience_plot_dat$statistic

blood_plot_dat <- rbind(phq_plot_dat,trauma_plot_dat)
blood_plot_dat <- rbind(blood_plot_dat,resilience_plot_dat)

blood_cate <- read.csv("~/Documents/Data/UKB/blood_parameters_category.csv")
colnames(blood_cate)[2] <- "Biomarker"
colnames(blood_cate)[3] <- "Category"
blood_plot_dat <- merge(blood_plot_dat, blood_cate[,c(2,3)], by = "Biomarker", all.x = T)

#blood_plot_dat <- blood_plot_dat %>%
#  arrange(factor(term, levels = c("Trauma Exposure", "Affective Disorders", "Resilience")), desc(statistic))

# 对 term 和 Category 分别进行因子化并排序
blood_plot_dat <- blood_plot_dat %>%
  arrange(
    factor(
      term, 
      levels = c("Trauma Exposure", "Affective Disorders", "Resilience")
    ),
    factor(
      Category, 
      levels = rev(c("Red blood cell", "Platelet", "White blood cell", "Immunometabolic", "Bone and joint", 
                     "Endocrine", "Liver function", "Renal function"))
    ),
    desc(statistic)
  )

# 将 Category 的因子水平顺序颠倒
blood_plot_dat$Category <- factor(
  blood_plot_dat$Category,
  levels = rev(c("Red blood cell", "Platelet", "White blood cell", "Immunometabolic", "Bone and joint", 
                 "Endocrine", "Liver function", "Renal function")),
  ordered = TRUE
)

blood_plot_dat$Biomarker <- factor(blood_plot_dat$Biomarker,
                                   levels = unique(blood_plot_dat$Biomarker),
                                   ordered = T)

#热图
library(ggplot2)
# 定义一个函数来生成显著性标记
add_significance <- function(p) {
  ifelse(p < 0.001, "***",
         ifelse(p < 0.01, "**",
                ifelse(p < 0.05, "*", "")))
}

# 创建热图并添加显著性标记
cor_plot <- ggplot(blood_plot_dat, aes(x = term, y = Biomarker, fill = correlation_r)) +
  geom_tile() +  # 绘制热图
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +  # 双向颜色梯度
  geom_text(aes(label = paste(round(correlation_r, 3), add_significance(adjusted_p))), size = 3) +  # 添加数值和显著性标记
  theme_minimal() +
  labs(x = "", y = "Biomarkers", fill = "Correlation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(cor_plot)


#带类别的热图-------------
library(ComplexHeatmap)
library(RColorBrewer)
library(dplyr)
library(reshape2)
phq_plot_dat <- PHQ_results_table
phq_plot_dat <- na.omit(phq_plot_dat)
phq_plot_dat$term <- "Affective Disorders"

trauma_plot_dat <- trauma_result_table
trauma_plot_dat <- na.omit(trauma_plot_dat)
trauma_plot_dat$term <- "Trauma Exposure"


resilience_plot_dat <- R_results_table
resilience_plot_dat <- na.omit(resilience_plot_dat)
resilience_plot_dat$term <- "Resilience"
#resilience_plot_dat$statistic <- -resilience_plot_dat$statistic

blood_plot_dat <- rbind(phq_plot_dat,trauma_plot_dat)
blood_plot_dat <- rbind(blood_plot_dat,resilience_plot_dat)

blood_cate <- read.csv("~/Documents/Data/UKB/blood_parameters_category.csv")
colnames(blood_cate)[2] <- "Biomarker"
colnames(blood_cate)[3] <- "Category"
blood_plot_dat <- merge(blood_plot_dat, blood_cate[,c(2,3)], by = "Biomarker", all.x = T)



# 1. 调整 term 的顺序
blood_plot_dat$term <- factor(blood_plot_dat$term,
                              levels = c("Trauma Exposure", "Affective Disorders", "Resilience"),
                              ordered = TRUE)

# 2. 按 Category 和 Trauma Exposure 的相关性倒序排序 Biomarker
# 提取 term == "Trauma Exposure" 的相关性
trauma_cor <- blood_plot_dat %>%
  filter(term == "Trauma Exposure") %>%
  select(Biomarker, Category, correlation_r)

# 按 Category 分组，并按 correlation_r 倒序排序
sorted_biomarkers <- blood_plot_dat %>%
  # 先确保 Category 的顺序
  mutate(Category = factor(Category,
                           levels = c("Red blood cell", "Platelet", "White blood cell", "Immunometabolic",
                                      "Bone and joint", "Endocrine", "Liver function", "Renal function"))) %>%
  # 加入 Trauma Exposure 的 correlation_r
  left_join(trauma_cor %>% select(Biomarker, trauma_cor = correlation_r),
            by = "Biomarker") %>%
  # 按 Category 和 trauma_cor 倒序排序
  arrange(Category, desc(trauma_cor)) %>%
  # 提取排序后的 Biomarker
  pull(Biomarker) %>%
  unique()  # 确保唯一性

# 3. 重新构造数据矩阵，确保 Biomarker 和 term 的顺序
mat <- reshape2::dcast(blood_plot_dat, Biomarker ~ term, value.var = "correlation_r")
# 设置行顺序为 sorted_biomarkers
mat <- mat[match(sorted_biomarkers, mat$Biomarker), ]
rownames(mat) <- mat$Biomarker
mat <- as.matrix(mat[, -1])  # 去掉 Biomarker 列，转换为矩阵

# 确保列顺序按 term 的因子水平
colnames(mat) <- levels(blood_plot_dat$term)

# 4. 准备 Category 分组
row_anno <- blood_plot_dat %>%
  select(Biomarker, Category) %>%
  distinct()
row_anno <- row_anno[match(rownames(mat), row_anno$Biomarker), ]
category_vec <- factor(row_anno$Category,
                       levels = c("Red blood cell", "Platelet", "White blood cell", "Immunometabolic",
                                  "Bone and joint", "Endocrine", "Liver function", "Renal function"))

# 5. 定义颜色
col_fun <- circlize::colorRamp2(
  breaks = c(min(mat, na.rm = TRUE), 0, max(mat, na.rm = TRUE)),
  colors = c("blue", "white", "red")
)
category_colors <- structure(
  RColorBrewer::brewer.pal(length(levels(category_vec)), "Set3"),
  names = levels(category_vec)
)

# 6. 准备显著性标记
pvalue_mat <- reshape2::dcast(blood_plot_dat, Biomarker ~ term, value.var = "adjusted_p")
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

# 7. 右侧颜色条带
right_anno <- HeatmapAnnotation(
  Class = category_vec,
  which = "row",
  show_annotation_name = FALSE,
  col = list(Class = category_colors)
)

# 8. 绘制热图
cor_heatmap <- Heatmap(
  mat,
  name = "Correlation",
  col = col_fun,
  cluster_rows = FALSE,  # 不对行聚类，保持排序
  cluster_columns = FALSE,  # 不对列聚类，保持 term 顺序
  row_split = category_vec,  # 按 Category 分组
  row_names_gp = gpar(fontsize = 8),
  column_names_gp = gpar(fontsize = 10),
  row_title_gp = gpar(fontsize = 10),
  column_title_gp = gpar(fontsize = 10),
  column_names_rot = 45,
  cell_fun = cell_text,
  right_annotation = right_anno
)

# 打印热图
draw(cor_heatmap, heatmap_legend_side = "right", annotation_legend_side = "right")

#具有中介效应的trauma和resilience热图-------------
library(ComplexHeatmap)
library(RColorBrewer)
library(dplyr)
library(reshape2)
colnames(mediation_results_table_short)[5] <- "mediation_p"
phq_plot_dat <- merge(PHQ_results_table, mediation_results_table_short[,c(1,5)], by = "Biomarker", all.x = T)
#phq_plot_dat <- PHQ_results_table
phq_plot_dat <- na.omit(phq_plot_dat)
phq_plot_dat$term <- "Affective Disorders"

trauma_plot_dat <- merge(trauma_result_table, mediation_results_table_short[,c(1,5)], by = "Biomarker", all.x = T)
#trauma_plot_dat <- trauma_result_table
trauma_plot_dat <- na.omit(trauma_plot_dat)
trauma_plot_dat$term <- "Trauma Exposure"


resilience_plot_dat <- merge(R_results_table, mediation_results_table_short[,c(1,5)], by = "Biomarker", all.x = T)
#resilience_plot_dat <- R_results_table
resilience_plot_dat <- na.omit(resilience_plot_dat)
resilience_plot_dat$term <- "Resilience"
#resilience_plot_dat$statistic <- -resilience_plot_dat$statistic

blood_plot_dat <- rbind(phq_plot_dat,trauma_plot_dat)
blood_plot_dat <- rbind(blood_plot_dat,resilience_plot_dat)

blood_cate <- read.csv("~/Documents/Data/UKB/blood_parameters_category.csv")
colnames(blood_cate)[2] <- "Biomarker"
colnames(blood_cate)[3] <- "Category"
blood_plot_dat <- merge(blood_plot_dat, blood_cate[,c(2,3)], by = "Biomarker", all.x = T)

blood_plot_dat <- blood_plot_dat[blood_plot_dat$term != "Affective Disorders", ]


# 1. 调整 term 的顺序
blood_plot_dat$term <- factor(blood_plot_dat$term,
                              levels = c("Trauma Exposure", "Resilience"),
                              ordered = TRUE)

# 2. 按 Category 和 Trauma Exposure 的相关性倒序排序 Biomarker
# 提取 term == "Trauma Exposure" 的相关性
trauma_cor <- blood_plot_dat %>%
  filter(term == "Trauma Exposure") %>%
  select(Biomarker, Category, correlation_r)

# 按 Category 分组，并按 correlation_r 倒序排序
sorted_biomarkers <- blood_plot_dat %>%
  # 先确保 Category 的顺序
  mutate(Category = factor(Category,
                           levels = c("Red blood cell", "Platelet", "White blood cell", "Immunometabolic",
                                      "Bone and joint", "Endocrine", "Liver function", "Renal function"))) %>%
  # 加入 Trauma Exposure 的 correlation_r
  left_join(trauma_cor %>% select(Biomarker, trauma_cor = correlation_r),
            by = "Biomarker") %>%
  # 按 Category 和 trauma_cor 倒序排序
  arrange(Category, desc(trauma_cor)) %>%
  # 提取排序后的 Biomarker
  pull(Biomarker) %>%
  unique()  # 确保唯一性

# 3. 重新构造数据矩阵，确保 Biomarker 和 term 的顺序
mat <- reshape2::dcast(blood_plot_dat, Biomarker ~ term, value.var = "correlation_r")
# 设置行顺序为 sorted_biomarkers
mat <- mat[match(sorted_biomarkers, mat$Biomarker), ]
rownames(mat) <- mat$Biomarker
mat <- as.matrix(mat[, -1])  # 去掉 Biomarker 列，转换为矩阵

# 确保列顺序按 term 的因子水平
colnames(mat) <- levels(blood_plot_dat$term)

# 4. 准备 Category 分组
row_anno <- blood_plot_dat %>%
  select(Biomarker, Category) %>%
  distinct()
row_anno <- row_anno[match(rownames(mat), row_anno$Biomarker), ]
category_vec <- factor(row_anno$Category,
                       levels = c("Red blood cell", "Platelet", "White blood cell", "Immunometabolic",
                                  "Bone and joint", "Endocrine", "Liver function", "Renal function"))

# 5. 定义颜色
col_fun <- circlize::colorRamp2(
  breaks = c(min(mat, na.rm = TRUE), 0, max(mat, na.rm = TRUE)),
  colors = c("blue", "white", "red")
)
category_colors <- structure(
  RColorBrewer::brewer.pal(length(levels(category_vec)), "Set3"),
  names = levels(category_vec)
)

# 6. 准备显著性标记
pvalue_mat <- reshape2::dcast(blood_plot_dat, Biomarker ~ term, value.var = "adjusted_p")
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

# 7. 右侧颜色条带
right_anno <- HeatmapAnnotation(
  Class = category_vec,
  which = "row",
  show_annotation_name = FALSE,
  col = list(Class = category_colors)
)

# 8. 绘制热图
cor_heatmap <- Heatmap(
  mat,
  name = "Correlation",
  col = col_fun,
  cluster_rows = FALSE,  # 不对行聚类，保持排序
  cluster_columns = FALSE,  # 不对列聚类，保持 term 顺序
  row_split = category_vec,  # 按 Category 分组
  row_names_gp = gpar(fontsize = 8),
  column_names_gp = gpar(fontsize = 10),
  row_title_gp = gpar(fontsize = 10),
  column_title_gp = gpar(fontsize = 10),
  column_names_rot = 45,
  cell_fun = cell_text,
  right_annotation = right_anno
)

# 打印热图（导出7inch*6inch）
draw(cor_heatmap, heatmap_legend_side = "right", annotation_legend_side = "right")



#进行分组比较画哑铃图--------
# 计算分位数
quantiles <- quantile(resilience_group_R$self_resilience, probs = c(1/3, 2/3),na.rm = TRUE)

# 替换值
resilience_group_R$resilience_group[resilience_group_R$self_resilience <= quantiles[1]] <- 0
resilience_group_R$resilience_group[resilience_group_R$self_resilience > quantiles[1] & resilience_group_R$self_resilience < quantiles[2]] <- 1
resilience_group_R$resilience_group[resilience_group_R$self_resilience >= quantiles[2]] <- 2
resilience_group_R$resilience_group <- as.factor(resilience_group_R$resilience_group)

trauma_variables_list <- c("Albumin", "ALP", "ALT", "CRP", "Cystatin C", "GGT", "HbA1c", 
                           "HCT", "HDL_C", "HGB", "HLSR Percentage", "IGF_1", "IRF", 
                           "Lymph Percentage", "MPV", "Neut Count", "Neut Percentage", 
                           "Platelet Count", "RBC Count", "RDW", "TG", "Total Bili", 
                           "Total Protein", "Vitamin D", "WBC Count")
trauma_variables_list <- gsub(" ", "_", trauma_variables_list)

trauma_var <- "blood"
trauma_lines <- list()
trauma_reg <- list()
result_table <- data.frame()
trauma_effect_table <- data.frame()
interaction_effct_table <- data.frame()
resilience_effect_table <- data.frame()

# 初始化一个空的数据框，用于存储分组相关比较结果
correlation_results_BL_all <- data.frame(Trauma = character(),Category = character(), Timepoint = character(),Resilience_Level = character(), Correlation = numeric(), P_value = numeric(), Subject_Count = integer(), stringsAsFactors = FALSE)
# 初始化一个空的数据框，用于存储两两比较的结果
comparison_results_BL_all <- data.frame(Trauma = character(), Timepoint = character(),Group1 = character(), Group2 = character(), Coef_diff = numeric(), p_value = numeric(), stringsAsFactors = FALSE)


for (t in trauma_variables_list) {
  resilience_test <- resilience_group_R %>% 
    transmute(
      eid,
      Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")),
      Age = age_BL,
      Ethnic = factor(Ethnic_group, levels = c("0", "1", "2", "3"), labels = c("1_White", "2_Asian", "3_Black", "4_Other")),
      Education = Education_year,
      BMI = BMI_BL,
      site = factor(site),
      resilience_group = factor(resilience_group, levels = c(0, 1, 2), labels = c("1_Low", "2_Medium", "3_High")),
      PHQ_BL = Depressive_Symptoms_PHQ4_BL,
      !!t := !!sym(t)
    ) #%>%
  #filter(abs(!!sym(t) - mean(!!sym(t), na.rm = TRUE)) <= 3 * sd(!!sym(t), na.rm = TRUE))
  resilience_test <- subset(resilience_test, resilience_group != "2_Medium")
  resilience_test <- na.omit(resilience_test)
  
  lme2 <- lm(
    as.formula(paste("PHQ_BL ~ resilience_group +", t, "+ resilience_group *", t, "+ Age + Sex + Ethnic + BMI + Education + site")),
    data = resilience_test
  )
  
  results <- tidy(lme2)
  
  # Add trauma variable as a column to identify results from each iteration
  results$Trauma_Variable <- t
  
  # Append the results to the result_table
  result_table <- bind_rows(result_table, results)
  
  #filtered_results <- results %>% filter(term %in% c("resilience_group2_Medium", "resilience_group3_High", t, paste0("resilience_group2_Medium:", t), paste0("resilience_group3_High:", t)))
  filtered_results <- results %>% filter(term %in% c(t))
  filtered_results$Trauma_Variable <- t
  trauma_effect_table <- bind_rows(trauma_effect_table, filtered_results)
  
  resilience_results <- results %>% filter(term %in% c("resilience_group3_High"))
  resilience_results$Trauma_Variable <- t
  resilience_effect_table <- bind_rows(resilience_effect_table, resilience_results)
  
  filtered_high_results <- results %>% filter(term %in% c(paste0("resilience_group3_High:", t)))
  filtered_high_results$Trauma_Variable <- t
  filtered_high_results$Stage <- trauma_var
  interaction_effct_table <- bind_rows(interaction_effct_table, filtered_high_results)
  
  #进行相关分析的比较
  # 比较不同组的创伤相关情况---------
  resilience <- c("1_Low", "3_High")
  # 初始化一个空的数据框，用于存储分组相关比较结果
  correlation_results_BL <- data.frame(
    Trauma = character(), 
    Timepoint = character(),
    Resilience_Level = character(), 
    Correlation = numeric(), 
    P_value = numeric(),  # 新增：存储 p 值
    Subject_Count = integer(), 
    stringsAsFactors = FALSE
  )
  
  # 循环遍历每个 resilience 水平
  for (resilience_level in resilience) {
    resilient_group <- resilience_test
    # 在当前 resilience 水平下，计算线性模型
    model_m <- lm(as.formula(paste("PHQ_BL ~ Age + Sex + Ethnic + BMI + Education + site")), 
                  data = resilient_group)
    resilient_group$PHQ_BL_reg <- residuals(model_m)
    
    resilient_group <- resilient_group[resilient_group$resilience_group == resilience_level, ]
    
    # 计算相关性和 p 值
    correlation_model <- cor.test(resilient_group$PHQ_BL_reg, resilient_group[[t]])
    correlation <- correlation_model$estimate
    p_value <- correlation_model$p.value  # 提取 p 值
    
    # 获取当前 resilience 水平的被试量
    subject_count <- nrow(resilient_group)
    
    # 将结果添加到数据框中
    correlation_results_BL <- rbind(correlation_results_BL, 
                                    data.frame(Trauma = t,
                                               Category = trauma_var,
                                               Timepoint = "BL",
                                               Resilience_Level = resilience_level, 
                                               Correlation = correlation, 
                                               P_value = p_value,  # 添加 p 值
                                               Subject_Count = subject_count))
  }
  
  # 打印结果
  print(correlation_results_BL)
  
  library(cocor)
  # 初始化一个空的数据框，用于存储两两比较的结果
  comparison_results <- data.frame(Trauma = character(), Timepoint = character(), Group1 = character(), Group2 = character(), Coef_diff = numeric(), p_value = numeric(), stringsAsFactors = FALSE)
  
  
  # 嵌套循环比较每两组之间的相关性
  for (i in 1:(length(resilience) - 1)) {
    for (j in (i + 1):length(resilience)) {
      # 选择要比较的两组数据
      group1 <- correlation_results_BL[correlation_results_BL$Resilience_Level == resilience[i], ]
      group2 <- correlation_results_BL[correlation_results_BL$Resilience_Level == resilience[j], ]
      
      # 使用 cocor.test 函数比较两组数据的相关性
      x = group1$Correlation
      n.x = group1$Subject_Count
      y = group2$Correlation
      n.y = group2$Subject_Count
      comparison <- cocor.indep.groups(x, y, n.x,  n.y, 
                                       alternative = "two.sided")
      
      # 提取比较结果中的系数差异和 p 值
      cocor_result <- get.cocor.results(comparison)
      Z <- cocor_result$fisher1925$statistic
      p_value <- cocor_result$fisher1925$p.value
      
      # 将结果添加到数据框中
      comparison_results <- rbind(comparison_results, 
                                  data.frame(Trauma = t,
                                             Timepoint = "BL",
                                             Group1 = resilience[i], 
                                             Group2 = resilience[j], 
                                             Z = Z, 
                                             p_value = p_value))
    }
  }
  
  # 打印比较结果
  #print(comparison_results)
  
  # 初始化一个空的数据框，用于存储两两比较的结果
  correlation_results_BL_all <- rbind(correlation_results_BL_all, correlation_results_BL)
  comparison_results_BL_all <- rbind(comparison_results_BL_all, comparison_results)
  
}



sum_result_p <- data.frame(Var = trauma_effect_table$Trauma_Variable,
                           trauma_p = trauma_effect_table$p.value,
                           resilience_p = resilience_effect_table$p.value,
                           interaction_p = interaction_effct_table$p.value)

p_values <- sum_result_p$resilience_p
adjusted_p_values <- p.adjust(p_values, method = "BH")
sum_result_p$resilience_adjusted_p <- adjusted_p_values

p_values <- sum_result_p$trauma_p
adjusted_p_values <- p.adjust(p_values, method = "BH")
sum_result_p$trauma_adjusted_p <- adjusted_p_values

p_values <- sum_result_p$interaction_p
adjusted_p_values <- p.adjust(p_values, method = "BH")
sum_result_p$interaction_adjusted_p <- adjusted_p_values

# 统计 p.value 值小于 0.05 的个数
#significant_count <- sum(sum_result_p$resilience_adjusted_p < 0.05)
#cat("Number of resilience main effects p.value < 0.05:", significant_count, "\n")


#significant_count <- sum(sum_result_p$trauma_adjusted_p < 0.05)
#cat("Number of trauma effects p.value < 0.05:", significant_count, "\n")

significant_count <- sum(sum_result_p$interaction_adjusted_p < 0.05)
cat("Number of interaction effects p.value < 0.05:", significant_count, "\n")

#相关比较的矫正
p_values <- comparison_results_BL_all$p_value
adjusted_p_values <- p.adjust(p_values, method = "BH")
comparison_results_BL_all$z_adjusted_p <- adjusted_p_values
significant_count <- sum(comparison_results_BL_all$z_adjusted_p < 0.05)
cat("Number of cocor p.value < 0.05:", significant_count, "\n")


#BL阶段作图-----------------------------------
library(tidyverse)
library(ggtext)
library(showtext)
showtext_auto()

blood_cate <- read.csv("~/Documents/Data/UKB/blood_parameters_category.csv")
colnames(blood_cate)[2] <- "Trauma"
colnames(blood_cate)[3] <- "Category"

#整理数据
cor_plot_dat <- merge(correlation_results_BL_all, comparison_results_BL_all[,c(1,5)], by = "Trauma", all.x = T)
z_plot_dat <- merge(comparison_results_BL_all,correlation_results_BL_all[,c(1,2)], by = "Trauma", all.x = T)
z_plot_dat <- unique(z_plot_dat)
#colnames(both_result_adj)[colnames(both_result_adj) == "Biomarker"] <- "Trauma"
#cor_plot_dat <- merge(cor_plot_dat, both_result_adj[,c(1,5)], by = "Trauma", all.x = T)

cor_plot_dat <- cor_plot_dat[, !names(cor_plot_dat) %in% "Category"]
z_plot_dat <- merge(z_plot_dat, blood_cate[,c(2,3)], by = "Trauma", all.x = T)

cor_plot_dat <- merge(cor_plot_dat, blood_cate[,c(2,3)], by = "Trauma", all.x = T)

cor_plot_dat <- cor_plot_dat %>%
  arrange(factor(Resilience_Level, levels = c("1_Low","3_High")), 
          factor(Category, levels = c("Red blood cell", "Platelet", "White blood cell", "Immunometabolic",
                                      "Bone and joint", "Endocrine", "Liver function", "Renal function")), desc(Correlation))%>%
  arrange(desc(row_number()))  # 反转整个排序

cor_plot_dat$Trauma <- factor(cor_plot_dat$Trauma,
                              levels = unique(cor_plot_dat$Trauma),
                              ordered = T)
z_trauma_order <- cor_plot_dat %>%
  filter(Resilience_Level == "1_Low")

z_plot_dat <- z_plot_dat %>%
  arrange(match(Trauma, z_trauma_order$Trauma))

z_plot_dat$Trauma <- factor(z_plot_dat$Trauma,
                            levels = unique(z_plot_dat$Trauma),
                            ordered = T)

cor_plot_dat <- cor_plot_dat %>%
  arrange(match(Trauma, z_trauma_order$Trauma))

cor_plot_dat$Trauma <- factor(cor_plot_dat$Trauma,
                              levels = unique(cor_plot_dat$Trauma),
                              ordered = T)
library(tidyr)
library(ggprism)

#拼接画法（哑铃+z）--------------
cor_plot_dat$Category <- factor(cor_plot_dat$Category, 
                                levels = c("Red blood cell", "Platelet", "White blood cell", "Immunometabolic",
                                           "Bone and joint", "Endocrine", "Liver function", "Renal function"),
                                ordered = TRUE)

#z_plot_dat$Category <- factor(z_plot_dat$Category, 
#                              levels = c("Red blood cell", "Platelet", "White blood cell", "Immunometabolic",
#                                         "Bone and joint", "Endocrine", "Liver function", "Renal function"),
#                              ordered = TRUE)

# 拼接画法（哑铃+z）
dum_plot <- ggplot(cor_plot_dat, aes(x = Trauma, y = Correlation)) +
  geom_line(aes(group = Trauma), size = 1.5) + 
  geom_point(aes(color = Resilience_Level), size = 5) +
  scale_color_manual(values = c("#ff9900", "#146eb4"),
                     labels = c("Resilience Level 1", "Resilience Level 2")) +  
  labs(x = "", y = "Correlation Coefficient", title = "Baseline") +
  guides(color = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +  # 添加 x=0 参考线
#  facet_grid(Category ~ ., scales = "free_x", space = "free_x") +  # 按 Category 分面
  coord_flip() +  # 旋转坐标系 90 度
  theme(panel.spacing=unit(0.5,"lines"),
        panel.border = element_blank(),
        axis.text.y = element_text(size = 14),
        #axis.text.x = element_blank(),
        
        panel.background = element_rect(color = "white", fill = NA),
        #panel.grid.major.x = element_line(color = "gray", linewidth = 0.5, linetype = "dashed"),
        #panel.grid.major.x = element_line(color = "gray", linewidth = 0.5, 
        #                                  linetype = ifelse(cor_plot_dat$Z > 2.2, "solid", "dashed")),
        panel.grid.minor.x = element_blank()
  )

dum_plot

z <- ggplot(z_plot_dat, aes(Trauma, Z)) +
  geom_col(aes(fill = factor(Z > 2.6 | Z < -2.6)), width = 0.7) +
  scale_fill_manual(values = c("#D3D3D3", "#A9A9A9"), guide = FALSE) +
  geom_hline(yintercept = 2.6, linetype = "dashed", color = "orange", size = 0.5) +
  geom_hline(yintercept = -2.6, linetype = "dashed", color = "orange", size = 0.5) +
  scale_alpha_manual(values = c(1, 0.6)) +
  labs(x = "Adverse Factor", y = "Z value") + 
  guides(color = FALSE) +
  coord_flip() +  # 旋转坐标系 90 度
  #facet_grid(.~Category, scales = "free_x",space = 'free_x')+
  theme(panel.spacing=unit(0.5,"lines"),
        panel.border = element_blank(),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
        panel.background = element_rect(color = "white", fill = NA),
        panel.grid.major.x = element_line(color = "gray", linewidth = 0.5, 
                                          linetype = ifelse(z_plot_dat$Z > 2.2, "solid", "dashed")),
        panel.grid.minor.x = element_line(color = "gray", linetype = "dashed", linewidth = 0.2))

z


library(patchwork)
dum_plot/z+plot_layout(heights = c(1, 0.3))

