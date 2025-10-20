library(R.matlab)
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

# 整理蛋白的数据 ------------------------------
# 指定文件路径
file_path <- "~/Documents/Data/UKB/UKB_Protein_norm.mat"
# 读取.mat文件
#proteinic_dat <- read.csv(file_path)
mat_data <- readMat(file_path)
proteinic_name <- read_csv("~/Documents/Data/UKB/id.csv")
proteinic_dat <- mat_data$ukb.protein.norm
proteinic_dat <- as.data.frame(proteinic_dat)
proteinic_dat <- proteinic_dat[,-1]
colnames(proteinic_dat)[1] <- "eid"
# 确保 proteinic_name 是正确的向量格式
new_colnames <- as.character(proteinic_name[[1]])  
# 检查列数是否匹配
if (length(new_colnames) == (2921 - 2 + 1)) {
  colnames(proteinic_dat)[2:2921] <- new_colnames
} else {
  stop("列名数量与目标列数不匹配，请检查数据")
}

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
resilience_group_R <- merge(resilience_group_R,proteinic_dat, by = "eid", all.x = TRUE)

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
file_path <- "/public/home/yangzy/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/protein_mediation_data.csv"
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

#ggsave("permutation_plot.png", width = 8, height = 6)

# 可选：保存相关系数到CSV文件
#write.csv(r_T_table, "correlation_trauma.csv", row.names = FALSE)
#write.csv(r_R_table, "correlation_resilience.csv", row.names = FALSE)
write.csv(S_perm, "/public/home/yangzy/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/protein_permutation_trauma_resilience_1000.csv", row.names = FALSE)
