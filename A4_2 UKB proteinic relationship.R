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
#分析Blood count和创伤的相关-------------
#定义trauma
t <- c("trauma_num")
biomarker <- colnames(resilience_group_R[,c(558:3477)])
# 创建一个空的列表来存储生成的图表
trauma_lines <- list()
trauma_reg <- list()
result_table <- data.frame()
trauma_result_table <- data.frame()
PHQ_results_table <- data.frame()

# 存储相关结果
correlation_results_trauma <- data.frame(Timepoint = character(),Blood = character(), Correlation = numeric(), 
                                         Int_low = numeric(), Int_high = numeric(),p_value = numeric(), 
                                         Subject_Count = integer(), stringsAsFactors = FALSE)

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
              !!paste(t) := !!sym(t),
              !!paste(b) := !!sym(b))
  
  # 移除缺失值
  resilience_test <- na.omit(resilience_test)
  n_subjects <- nrow(resilience_test)  # 当前biomarker的有效样本量
  # 线性回归模型
  lme3 <- lm(
    as.formula(paste0("`", b, "` ~ ", t, " + Age + Sex + Ethnic + BMI + Education + site")),
    data = resilience_test)
  
  # 提取回归结果
  results <- tidy(lme3)
  
  # 添加 Biomarker 列
  results$Biomarker <- b
  results$Subject_Count <- n_subjects  # 添加样本量
  # 合并到 result_table
  result_table <- bind_rows(result_table, results)
  
  # 筛选 trauma 的结果
  trauma_results <- results %>% filter(term %in% t)
  trauma_results$Biomarker <- b
  # 计算自由度 (df = n - k - 1)
  n <- nrow(resilience_test)  # 样本大小
  k <- length(coef(lme3)) - 1  # 自变量数量（不含截距）
  df <- n - k - 1  # 剩余自由度
  
  # 计算相关系数 r
  trauma_results <- trauma_results %>%
    mutate(correlation_r = statistic / sqrt(statistic^2 + df))
  
  trauma_result_table <- bind_rows(trauma_result_table, trauma_results)

}

trauma_result_table$Stage <-"Protein"

# Assuming partial_cor_results is your data frame with p-values
p_values <- trauma_result_table$p.value
# Apply Benjamini-Hochberg FDR correction
adjusted_p_values <- p.adjust(p_values, method = "BH")
trauma_result_table$adjusted_p <- adjusted_p_values

# 定义保存路径
file_path <- "/public/home/yangzy/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/trauma_result_table.csv"
# 将数据保存为 CSV 文件
write.csv(trauma_result_table, file = file_path, row.names = FALSE)
trauma_result_table <-  read_csv(file_path)

trauma_result_table_adjusted <- trauma_result_table[trauma_result_table$adjusted_p < 0.05,]
trauma_noresult_table <- trauma_result_table[trauma_result_table$p.value > 0.05,]


##分析和心理的相关---------------
biomarker <- colnames(resilience_group_R[,c(558:3477)])

for (b in biomarker) {
  resilience_test <- resilience_group_R %>% # 创建新数据集新变量  
    transmute(eid, Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")), Age = age_BL, 
              Ethnic = factor(Ethnic_group, levels = c("0","1","2","3"), labels = c("1_White","2_Asian","3_Black","4_Other")),  
              Education = Education_year,
              BMI = BMI_BL, 
              selfresilience = self_resilience,
              site = factor(site),
              PHQ = Depressive_Symptoms_PHQ4_BL,
              !!paste(b) := !!sym(b),
              !!paste(t) := !!sym(t))
  
  
  resilience_test <- na.omit(resilience_test)
  
  lme3 <- lm(
    as.formula(paste0("`", b, "`~ + PHQ + Age + Sex + Ethnic + BMI + Education + site")),
    data = resilience_test)
  
  results <- tidy(lme3)
  
  # Add trauma variable as a column to identify results from each iteration
  results$Biomarker <- b
  
  # Append the results to the result_table
  result_table <- bind_rows(result_table, results)
  
  
  PHQ_results <- results %>% filter(term %in% c("PHQ"))
  PHQ_results$Biomarker <- b
  # 计算自由度 (df = n - k - 1)
  n <- nrow(resilience_test)  # 样本大小
  k <- length(coef(lme3)) - 1  # 自变量数量（不含截距）
  df <- n - k - 1  # 剩余自由度
  
  # 计算相关系数 r
  PHQ_results <- PHQ_results %>%
    mutate(correlation_r = statistic / sqrt(statistic^2 + df))
  
  PHQ_results_table <- bind_rows(PHQ_results_table, PHQ_results)
  
}



PHQ_results_table$Stage <-"Protein"

p_values <- PHQ_results_table$p.value
# Apply Benjamini-Hochberg FDR correction
adjusted_p_values <- p.adjust(p_values, method = "BH")
PHQ_results_table$adjusted_p <- adjusted_p_values

# 定义保存路径
file_path <- "/public/home/yangzy/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/blood_protein_PHQ_results_table.csv"
# 将数据保存为 CSV 文件
write.csv(PHQ_results_table, file = file_path, row.names = FALSE)
PHQ_results_table <-  read_csv(file_path)

PHQ_results_table_adjusted <- PHQ_results_table[PHQ_results_table$adjusted_p < 0.05,]





both_results <- merge(trauma_result_table, PHQ_results_table, by = "Biomarker",all.x = T)
both_results_adj <- both_results[both_results$adjusted_p.x <0.05 & both_results$adjusted_p.y <0.05,]
# 删除第一行
#both_results_adj <- both_results_adj[-1, ]
# 定义保存路径
file_path <- "/public/home/yangzy/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/blood_protein_both_result_175.csv"
# 将数据保存为 CSV 文件
write.csv(both_results_adj, file = file_path, row.names = FALSE)
both_results_adj <-  read_csv("/public/home/yangzy/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/blood_protein_both_result_175.csv")


##分析和Resilience的相关---------------
R_results_table <- data.frame()
#I_results_table <- data.frame()
#interact_result_table <- data.frame()
#biomarker <- both_results_adj$Biomarker
#biomarker <- mediation_results_table_short$Biomarker
for (b in biomarker) {
  resilience_test <- resilience_group_R %>% # 创建新数据集新变量  
    transmute(eid, Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")), Age = age_BL, 
              Ethnic = factor(Ethnic_group, levels = c("0","1","2","3"), labels = c("1_White","2_Asian","3_Black","4_Other")),  
              Education = Education_year,
              site =factor(site),
              BMI = BMI_BL, 
              selfresilience = self_resilience,
              PHQ = Depressive_Symptoms_PHQ4_BL,
              !!paste(b) := !!sym(b),
              !!paste(t) := !!sym(t))
  
  
  resilience_test <- na.omit(resilience_test)
  
  lme3 <- lm(
    as.formula(paste0("`", b, "`~ selfresilience + Age + Sex + Ethnic + BMI + Education + site")),
    data = resilience_test)
  
  results <- tidy(lme3)
  
  
  # Add trauma variable as a column to identify results from each iteration
  results$Biomarker <- b
  
  # Append the results to the result_table
  result_table <- bind_rows(result_table, results)
  
  
  R_results <- results %>% filter(term %in% c("selfresilience"))
  R_results$Biomarker <- b
  
  # 计算自由度 (df = n - k - 1)
  n <- nrow(resilience_test)  # 样本大小
  k <- length(coef(lme3)) - 1  # 自变量数量（不含截距）
  df <- n - k - 1  # 剩余自由度
  
  # 计算相关系数 r
  R_results <- R_results %>%
    mutate(correlation_r = statistic / sqrt(statistic^2 + df))
  
  R_results_table <- bind_rows(R_results_table, R_results)
}



R_results_table$Stage <-"Blood Protein"


p_values <- R_results_table$p.value
# Apply Benjamini-Hochberg FDR correction
adjusted_p_values <- p.adjust(p_values, method = "BH")
R_results_table$adjusted_p <- adjusted_p_values

# 定义保存路径
file_path <- "/public/home/yangzy/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/R_results_table_protein.csv"
# 将数据保存为 CSV 文件
write.csv(R_results_table, file = file_path, row.names = FALSE)
R_results_table <- read.csv(file_path)

R_results_table_adjusted <- R_results_table[R_results_table$adjusted_p < 0.05,]


three_results <- merge(both_results, R_results_table, by = "Biomarker",all.x = T)
three_results <- merge(both_results_adj, R_results_table, by = "Biomarker",all.x = T)
# 定义保存路径
file_path <- "/public/home/yangzy/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/three_results_table_protein.csv"
# 将数据保存为 CSV 文件
write.csv(three_results, file = file_path, row.names = FALSE)
three_results <- read.csv(file_path)


#合并Blood的数据----------------------
M_biomarker <- both_results_adj$Biomarker
num_bio <- length(M_biomarker)

#selected_columns <- select(resilience_group_R, all_of(M_biomarker))
selected_columns <- subset(resilience_group_R, select = M_biomarker)
library(dplyr)
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

#blood_dat <- na.omit(blood_dat)
#weights <- PHQ_results_table_adjusted$weight
#weightblood <- data.frame(county = both_results_shorted$Biomarker, weights = both_results_shorted$weight)

# 获取权重向量
#weights <- weightblood[, 2]

#blood_dat[, 10:(9+num_bio)] <- sweep(blood_dat[, 10:(9+num_bio)], 2, weights, FUN = "*")

#blood_dat$wtmean <- as.matrix(blood_dat[,c(10:(9+num_bio))]) %*% as.matrix(weightblood[, 2])
#resilience_test$wtmean <- rowMeans(resilience_test[,c(9:39)])
#colnames(blood_dat)[10+num_bio] <- "Blood_merged"
#blood_dat <- na.omit(blood_dat)


# 将 M_biomarker 转换为字符串

#biomarker_formula <- paste(M_biomarker, collapse = " + ")
#Blood_formula <- as.formula(paste("~", biomarker_formula))
#Blood_formula

blood_dat_scale <- blood_dat
blood_dat_scale[, c(11:(10+num_bio))] <- scale(blood_dat_scale[, c(11:(10+num_bio))])

#批量中介分析(加入协变量）----------------------------
library(lavaan)
library(mediation)

M <- both_results_adj$Biomarker
#M <- "MC30130"
myData <- blood_dat_scale

# 创建一个空的数据框来存储结果
mediation_results_table <- data.frame(Biomarker = character(),
                                      Estimate = numeric(),
                                      CI_lower = numeric(),
                                      CI_upper = numeric(),
                                      P_value = numeric(),
                                      Prop_Mediated = numeric(),
                                      Prop_Mediated_ci_upper = numeric(),
                                      Prop_Mediated_ci_upper = numeric(),
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
  
  sub_mydata <- resilience_group_R %>%
    transmute(eid, 
              Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")),
              Age = age_BL, 
              Ethnic = factor(Ethnic_group, levels = c("0", "1", "2", "3"), labels = c("1_White", "2_Asian", "3_Black", "4_Other")),  
              Education = Education_year,
              BMI = BMI_BL, 
              selfresilience = self_resilience,
              site = factor(site),
              PHQ = Depressive_Symptoms_PHQ4_BL,
              trauma_num,
              !!paste(biomarker) := !!sym(biomarker))

  sub_mydata <- na.omit(sub_mydata)
  # Step1，自变量和因变量是否有显著关系
  model.0 <- lm(PHQ ~ trauma_num + Sex + Age + Ethnic +Education + BMI, sub_mydata)
  
  # Step2，自变量和中介变量是否有显著关系
  model.M <- lm(as.formula(paste(biomarker, "~ trauma_num + Sex + Age + Ethnic +Education + BMI")), sub_mydata)
  
  # Step3，自变量和中介变量的关系考虑进去之后，中介变量和因变量是否有显著关系
  model.Y <- lm(as.formula(paste("PHQ ~ ", biomarker, "+ trauma_num + Sex + Age + Ethnic +Education + BMI")), sub_mydata)
  
  # 用bootstrapping，看中介效应是否显著
  results <- mediate(model.M, model.Y, treat = 'trauma_num', mediator = biomarker, boot = TRUE, sims = 1000)
  
  # 提取中介效应的估计值和置信区间
  estimate <- summary(results)$d0
  ci_lower <- summary(results)$d0.ci[1]
  ci_upper <- summary(results)$d0.ci[2]
  Prop_Mediated <- summary(results)$n0
  Prop_Mediated_ci_lower <- summary(results)$n0.ci[1]
  Prop_Mediated_ci_upper <- summary(results)$n0.ci[2]
  coefficients <- coef(model.M)
  a <- coefficients["trauma_num"]
  summary_model.m <- summary(model.M)
  coefficients_table <- summary_model.m$coefficients
  p_a <- coefficients_table["trauma_num", "Pr(>|t|)"]
  
  coefficients <- coef(model.Y)
  b <- coefficients[biomarker]
  summary_model.Y <- summary(model.Y)
  coefficients_table <- summary_model.Y$coefficients
  p_b <- coefficients_table[biomarker, "Pr(>|t|)"]
  c <- coefficients["trauma_num"]
  p_c <- coefficients_table["trauma_num", "Pr(>|t|)"]
  
  
  # 提取中介效应的P值
  p_value <- summary(results)$d0.p
  
  # 直接将结果添加到数据框中
  mediation_results_table[i, ] <- c(biomarker, estimate, ci_lower, ci_upper, p_value, Prop_Mediated, Prop_Mediated_ci_lower, Prop_Mediated_ci_upper, a, p_a, b, p_b, c, p_c)
}


# 打印结果表格
print(mediation_results_table)


# 定义保存路径
file_path <- "/public/home/yangzy/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/protein_mediation_data.csv"

# 将数据保存为 CSV 文件
write.csv(mediation_results_table, file = file_path, row.names = FALSE)
# 导入数据
mediation_results_table <- read.csv(file_path)
mediation_results_table_short <- mediation_results_table[mediation_results_table$P_value < 0.001,]
mediation_results_table_short <- mediation_results_table[c(1:5),]
mediation_results_table$P_value_FDR <- p.adjust(mediation_results_table$P_value, method = "fdr")



#作图---------------------
colnames(mediation_results_table_short)[5] <- "mediation_p"
phq_plot_dat <- merge(PHQ_results_table_adjusted, mediation_results_table_short[,c(1,5)], by = "Biomarker", all.x = T)
phq_plot_dat <- na.omit(phq_plot_dat)
phq_plot_dat$term <- "Affective Disorders"

trauma_plot_dat <- merge(trauma_result_table_adjusted, mediation_results_table_short[,c(1,5)], by = "Biomarker", all.x = T)
trauma_plot_dat <- na.omit(trauma_plot_dat)
trauma_plot_dat$term <- "Trauma Exposure"

resilience_plot_dat <- merge(R_results_table, mediation_results_table_short[,c(1,5)], by = "Biomarker", all.x = T)
resilience_plot_dat$term <- "Resilience"
#resilience_plot_dat$statistic <- -resilience_plot_dat$statistic

blood_plot_dat <- rbind(phq_plot_dat,trauma_plot_dat)
blood_plot_dat <- rbind(blood_plot_dat,resilience_plot_dat)

blood_plot_dat <- blood_plot_dat %>%
  arrange(factor(term, levels = c("Trauma Exposure", "Affective Disorders", "Resilience")), desc(statistic))

blood_plot_dat$term <- factor(blood_plot_dat$term,
                              levels = c("Trauma Exposure", "Affective Disorders", "Resilience"),
                              ordered = T)

blood_plot_dat$Biomarker <- factor(blood_plot_dat$Biomarker,
                                   levels = unique(blood_plot_dat$Biomarker),
                                   ordered = T)

cor_plot <- ggplot(blood_plot_dat) +
  aes(
    x = Biomarker,
    y = statistic,
    fill = term,
    colour = term
  ) +
  geom_col(position = "dodge", width = 0.7) +
  #geom_errorbar(aes(ymin = Int_low, ymax = Int_high), width = 0.3, color = "#6F6F6F", linewidth = 0.7) +  # 添加误差线
  scale_fill_manual(
    values = c(`Trauma Exposure` = "#D57B70",
               `Affective Disorders` = "#94B5D8",
               `Resilience` = "#ACD2C7")
  ) +
  scale_color_manual(
    values = c(`Trauma Exposure` = "#D57B70",
               `Affective Disorders` = "#94B5D8",
               `Resilience` = "#ACD2C7")
  ) +
  theme_minimal() +
  #facet_wrap(vars(Biomarker), scales = free_x)+
  geom_hline(yintercept = -1.9, linetype = "dashed", color = "gray", linewidth = 0.5) +
  theme(panel.spacing=unit(1.5,"lines"),
        panel.border = element_blank(),
        #panel.border = element_rect(color = "white", linewidth = 2, fill = NA),
        axis.text.y = element_text(size = 9),
        
        axis.text.x = element_text(size = 9),
        panel.background = element_rect(color = "white", fill = NA),
        panel.grid.major.y = element_line(color = "gray", linewidth = 0.5, 
                                          linetype = "solid"),
        panel.grid.major.x = element_blank())

cor_plot

geom_hline(yintercept = 2.2, linetype = "dashed", color = "orange", size = 0.5) +
  panel.grid.major.x = element_line(color = "gray", linewidth = 0.5, 
                                    linetype = ifelse(z_plot_dat$Z > 2.2, "solid", "dashed"))


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



#先找和resilience的相关，在做调节-----------
##分析和Resilience的相关---------------
R_results_table <- data.frame()
result_table <-data.frame()
interact_result_table <- data.frame()
biomarker <- colnames(resilience_group_R[,c(558:3477)])
for (b in biomarker) {
  resilience_test <- resilience_group_R %>% # 创建新数据集新变量  
    transmute(eid, Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")), Age = age_BL, 
              Ethnic = factor(Ethnic_group, levels = c("0","1","2","3"), labels = c("1_White","2_Asian","3_Black","4_Other")),  
              Education = Education_year,
              site =factor(site),
              BMI = BMI_BL, 
              selfresilience = self_resilience,
 #             PHQ = Depressive_Symptoms_PHQ4_BL,
              !!paste(b) := !!sym(b),
              !!paste(t) := !!sym(t))
  
  
  resilience_test <- na.omit(resilience_test)
  
  lme3 <- lm(
    as.formula(paste0("`", b, "`~ + selfresilience + trauma_num + Age + Sex + Ethnic + BMI + Education + site")),
    data = resilience_test)
  
  results <- tidy(lme3)

  # Add trauma variable as a column to identify results from each iteration
  results$Biomarker <- b
   
  R_results <- results %>% filter(term %in% c("selfresilience"))
  R_results$Biomarker <- b
  
  # 计算自由度 (df = n - k - 1)
  n <- nrow(resilience_test)  # 样本大小
  k <- length(coef(lme3)) - 1  # 自变量数量（不含截距）
  df <- n - k - 1  # 剩余自由度
  
  # 计算相关系数 r
  R_results <- R_results %>%
    mutate(correlation_r = statistic / sqrt(statistic^2 + df))
  
  R_results_table <- bind_rows(R_results_table, R_results)
}



R_results_table$Stage <-"Blood Protein"

p_values <- R_results_table$p.value
# Apply Benjamini-Hochberg FDR correction
adjusted_p_values <- p.adjust(p_values, method = "BH")
R_results_table$adjusted_p <- adjusted_p_values

# 定义保存路径
file_path <- "/public/home/yangzy/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/R_results_table_adj_trauma.csv"
# 将数据保存为 CSV 文件
write.csv(R_results_table, file = file_path, row.names = FALSE)
R_results_table <- read.csv(file_path)
R_results_table_adjusted <- R_results_table[R_results_table$adjusted_p < 0.05,]
R_results_table_adjusted <- R_results_table[R_results_table$adjusted_p < 0.05 & R_results_table$statistic < 0,]

#然后做蛋白调节trauma到phq
result_table <- data.frame()
I_results_table <- data.frame()
interact_result_table <- data.frame()
biomarker <- mediation_results_table_short$Biomarker
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
biomarker <- I_results_table_adjusted$Biomarker
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
biomarker <- I_results_table_adjusted$Biomarker
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




#进行GO分析---------
library(clusterProfiler)
library(org.Hs.eg.db)
library(ggplot2)
upProtein <- I_results_table_adjusted$Biomarker
Protein <- bitr(rownames(upProtein),
                fromType = "SYMBOL",
                toType = c("UNIPROT"),
                OrgDb = org.Hs.eg.db)

GO <- enrichGO(gene = Protein$ENTREZID)


#分析resilience蛋白对trauma到蛋白的调节作用--------
interact_result_table <- tibble()
I_results_table <- tibble()
both_results_adj <- read_csv("/public/home/yangzy/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/blood_protein_both_result_175.csv")
biomarker <- both_results_adj$Biomarker
#resilience_protein <- missing_biomarkers
common_biomarkers <- intersect(R_results_table_adjusted$Biomarker, trauma_noresult_table$Biomarker)
common_biomarkers_df <- data.frame(Biomarker = common_biomarkers)
resilience_protein <- common_biomarkers_df$Biomarker
#resilience_protein <- c("ACSL1", "AGER", "ALCAM", "ANGPTL7", "ATP1B4", "BAG3", "C4BPB", "CCER2", "CD8A", "CEACAM16", "CRNN", 
                        "CSF1R", "CTSL", "CXCL14", "DCBLD2", "FOLR1", "FRMD7", "GGH", "IDUA", "IL17A", "IL1RL2", "IL6ST", 
                        "KLK6", "LTBP3", "MERTK", "PALM2", "PENK", "PLA2G10", "PON1", "PRL", "PRND", "PRRT3", "PTH", 
                        "ROR1", "SLITRK1", "SORCS2", "SPRR3")
#resilience_protein <- c("self_resilience")
for (b in biomarker) {
  for (rb in resilience_protein) {
    resilience_test <- resilience_group_R %>% # 创建新数据集新变量  
      transmute(eid, 
                Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")), 
                Age = age_BL, 
                Ethnic = factor(Ethnic_group, levels = c("0","1","2","3"), 
                                labels = c("1_White","2_Asian","3_Black","4_Other")),  
                Education = Education_year,
                site = factor(site),
                BMI = BMI_BL, 
                !!rb := !!sym(rb), # 确保正确引用 resilience_protein
                !!b := !!sym(b),  # 直接使用 b 作为列名
                !!t := !!sym(t))  # 直接使用 t 作为列名
    
    resilience_test <- na.omit(resilience_test)
    
    lme4 <- lm(
      as.formula(paste0("`", b, "` ~ trauma_num + `", rb, "` + `", rb, "` * trauma_num + Age + Sex + Ethnic + BMI + Education + site")),
      data = resilience_test)
    
    interact_result <- tidy(lme4)
    interact_result$Biomarker <- b
    interact_result$resilience_protein <- rb
    
    # Append to result tables
    interact_result_table <- bind_rows(interact_result_table, interact_result)
    
    I_results <- interact_result %>% filter(term == paste0("trauma_num:", rb))
    I_results$Biomarker <- b
    I_results$resilience_protein <- rb
    I_results_table <- bind_rows(I_results_table, I_results)
  }
}

I_results_table$Stage <-"Blood Protein"
# 定义保存路径
file_path <- "/public/home/yangzy/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/interaction_resilience protein and trauma protein.csv"
# 将数据保存为 CSV 文件
write.csv(I_results_table, file = file_path, row.names = FALSE)

p_values <- I_results_table$p.value
# Apply Benjamini-Hochberg FDR correction
adjusted_p_values <- p.adjust(p_values, method = "BH")
I_results_table$adjusted_p <- adjusted_p_values

I_results_table_adjusted <- I_results_table[I_results_table$adjusted_p < 0.05,]

#提取会加剧反应的resilience protein
I_results_table_worse <- I_results_table_adjusted[I_results_table_adjusted$statistic > 0,]
resilience_protein_worse <- I_results_table_worse %>% 
  distinct(resilience_protein)


#整理蛋白的计算数据--------------------------------

trauma_result_table <-  read_csv("/public/home/yangzy/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/trauma_result_table.csv")
trauma_result_table_adjusted <- trauma_result_table[trauma_result_table$adjusted_p < 0.05,]

PHQ_results_table <-  read_csv("/public/home/yangzy/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/blood_protein_PHQ_results_table.csv")
PHQ_results_table_adjusted <- PHQ_results_table[PHQ_results_table$adjusted_p < 0.05,]

both_results_adj <-  read_csv("/public/home/yangzy/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/blood_protein_both_result_175.csv")

mediation_results_table <- read.csv("/public/home/yangzy/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/protein_mediation_data.csv")
mediation_results_table_short <- mediation_results_table[mediation_results_table$P_value < 0.001,]

R_results_table <- read.csv("/public/home/yangzy/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/R_results_table_adj_trauma.csv")
R_results_table_adjusted <- R_results_table[R_results_table$adjusted_p < 0.05,]

#富集分析-----
#install.packages(clusterProfiler)
library(org.Hs.eg.db)

#BiocManager::install("org.Hs.eg.db")
#BiocManager::install("clusterProfiler",force = TRUE)
#BiocManager::install("ReactomePA")
#BiocManager::install("reactome.db")
library("clusterProfiler")
library("org.Hs.eg.db")
library("ReactomePA")

#setwd('/Users/xiechao/XIC/Other_work-MAC/fanwei')
# 读取蛋白列表
#protein_list <- read.table("protein_list_All_dep_pp.txt", stringsAsFactors = FALSE)$V1
protein_list <- mediation_results_table_short$Biomarker
protein_list <- R_results_table_adjusted$Biomarker
# 将基因符号转换为 ENTREZ ID
entrez_ids <- bitr(protein_list, fromType = "SYMBOL", toType = "ENTREZID", OrgDb = org.Hs.eg.db)

go_enrich<-clusterProfiler::enrichGO(gene=entrez_ids$ENTREZID,
                                     ont='all',#可选'BP','CC','MF'or'all'
                                     keyType="ENTREZID",
                                     OrgDb=org.Hs.eg.db,
                                     pAdjustMethod="BH",#p值矫正方法
                                     pvalueCutoff=0.05,
                                     qvalueCutoff=0.05)
#将RNTREZ转换为Symbol
go_enrich<-DOSE::setReadable(go_enrich,
                             OrgDb=org.Hs.eg.db,
                             keyType='ENTREZID')


#去除冗余的GO term
go_geo<- simplify(go_enrich, cutoff=0.7, by="p.adjust",
                  select_fun=min)
#提取goG富集结果表格
go_result<-go_geo@result
go_result

dotplot(go_geo,
        x = "GeneRatio",
        color = "p.adjust",
        showCategory=10,
        split='ONTOLOGY',
        label_format = Inf)+#不换行
  #分面
  facet_grid(ONTOLOGY~.,
             space = 'free_y',#面板大小根据y轴自行调整
             scale='free_y'#子图坐标轴根据y轴自行调整
  )

# KEGG 富集分析
kegg_result <- enrichKEGG(gene = entrez_ids$ENTREZID, organism = 'hsa', pAdjustMethod = "BH")
head(kegg_result)

# Reactome 富集分析
reactome_result <- enrichPathway(gene = entrez_ids$ENTREZID, organism = "human", pAdjustMethod = "BH")
head(reactome_result)
# 提取结果为表格
reactome_table <- as.data.frame(reactome_result@result)

# 绘制结果
barplot(kegg_result, showCategory = 10)
barplot(reactome_result, showCategory = 10)


#开始画图-----
#整理数据
trauma_result_table$Stage <- "1_Trauma"
PHQ_results_table$Stage <- "2_PHQ"
R_results_table$Stage <- "3_Resilience"

trauma_plot <- trauma_result_table
PHQ_plot <- PHQ_results_table
resilience_plot <- R_results_table
resilience_plot$log <- -log10(resilience_plot$p.value)

trauma_plot <- trauma_plot %>%
  mutate(
    top10 = ifelse(rank(adjusted_p) <= 15, "yes", "no")  # 标记前 10 个最显著的行
  )

PHQ_plot <- PHQ_plot %>%
  mutate(
    top10 = ifelse(rank(adjusted_p) <= 15, "yes", "no")  # 标记前 10 个最显著的行
  )

resilience_plot <- resilience_plot %>%
  mutate(
    top10 = ifelse(rank(adjusted_p) <= 15, "yes", "no")  # 标记前 10 个最显著的行
  )

# 拼接两个数据框
combined_table <- rbind(trauma_plot, PHQ_plot)
combined_table <- rbind(combined_table, resilience_plot)

combined_table <- merge(combined_table, mediation_results_table_short[,c(1,5)], by = "Biomarker", all.x = T)
combined_table <- combined_table %>%
  mutate(
    mediation_effect = ifelse(P_value == 0, "yes", "no")  # 选出有中介的
  )

# 基于CRAN安装R包，检测没有则安装
p_list = c("ggplot2", "dplyr", "readr", "ggrepel", "patchwork","CMplot")
for(p in p_list){if (!requireNamespace(p)){install.packages(p)} 
  library(p, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)}# 加载R包 Load the package
suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(readr)))
suppressWarnings(suppressMessages(library(ggrepel)))
suppressWarnings(suppressMessages(library(patchwork)))
suppressWarnings(suppressMessages(library(CMplot)))

# 读取数据
# Read data
data <- combined_table
# 根据p值和logFC进行分类
# Classification based on p-value and logFC
data <- data %>%  
  mutate(change = case_when(
    adjusted_p < 0.05 ~ ifelse(statistic < 0, "depleted", "enriched"), 
    TRUE ~ "nosig"
  ))
# 将p值取负对数
# Take the negative logarithm of the p-value
data <- data %>%
  mutate(P.Value = -log10(p.value))

data$labels <- ifelse(data$top10 == "yes",   data$Biomarker,"")
# 绘制图形
# Plot
p1 <- ggplot(data, aes(x = Stage, y = P.Value)) + 
  geom_point(aes(color = Stage, shape = change, size = change,alpha = change), position = position_jitter(width = 0.4)) + 
  geom_text_repel( aes(label=labels),size=3,box.padding = 0.4,color= "black", show.legend = F,max.overlaps = 100)+
  scale_y_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, 20)) + 
  #scale_size_continuous(range = c(1, 3.5)) + 
  scale_shape_manual(values = c(6,17,16)) + 
  scale_size_manual(values = c(3,3,2)) + 
  scale_alpha_manual(values = c(0.7,0.7,0.3)) + 
  #scale_color_brewer(palette = "Set2") + 
  scale_color_manual(values = c("#eaaa60","#e68b81","#84c3b7")) + # 添加这行来定义颜色
  geom_hline(yintercept = -log10(0.05), linetype = "dotted", color = "darkred") + 
  labs(x = "Stage", y = expression(-log[10](P)), 
       #y = "-log10(P-Value)", 
       title = "Manhattan Plot") + 
  theme_light() + 
  theme(legend.position = "top",
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 10), 
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10), 
        plot.title = element_text(size = 14, face = "bold"))+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p1


#分组散点图-----------
library(ggplot2)
library(dplyr)
library(ggpubr)
library(patchwork)
library(ggExtra)  # 用于添加边缘分布图

# 定义 biomarker
biomarker <- c("GUSB", "PCBD1", "PIGR")

biomarker <- c("GUSB","PCBD1","PIGR")
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
               alpha = 0.2, size = 1, 
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
colnames(mediation_results_table_short)[5] <- "mediation_p"
phq_plot_dat <- merge(PHQ_results_table_adjusted, mediation_results_table_short[,c(1,5)], by = "Biomarker", all.x = T)
phq_plot_dat <- na.omit(phq_plot_dat)
phq_plot_dat$term <- "Affective Disorders"

trauma_plot_dat <- merge(trauma_result_table_adjusted, mediation_results_table_short[,c(1,5)], by = "Biomarker", all.x = T)
trauma_plot_dat <- na.omit(trauma_plot_dat)
trauma_plot_dat$term <- "Trauma Exposure"


resilience_plot_dat <- merge(R_results_table, mediation_results_table_short[,c(1,5)], by = "Biomarker", all.x = T)
resilience_plot_dat <- na.omit(resilience_plot_dat)
resilience_plot_dat$term <- "Resilience"
#resilience_plot_dat$statistic <- -resilience_plot_dat$statistic

blood_plot_dat <- rbind(phq_plot_dat,trauma_plot_dat)
blood_plot_dat <- rbind(blood_plot_dat,resilience_plot_dat)

blood_plot_dat <- blood_plot_dat %>%
  arrange(factor(term, levels = c("Trauma Exposure", "Affective Disorders", "Resilience")), desc(statistic))

blood_plot_dat$term <- factor(blood_plot_dat$term,
                              levels = c("Trauma Exposure", "Affective Disorders", "Resilience"),
                              ordered = T)

blood_plot_dat$Biomarker <- factor(blood_plot_dat$Biomarker,
                                   levels = unique(blood_plot_dat$Biomarker),
                                   ordered = T)

#热图
library(ggplot2)
cor_plot <- ggplot(blood_plot_dat, aes(x = term, y = Biomarker, fill = correlation_r)) +
  geom_tile() +  # 绘制热图
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +  # 双向颜色梯度
  geom_text(aes(label = round(correlation_r, 3)), size = 3) +  # 添加数值标签
  theme_minimal() +
  labs(x = "", y = "Biomarkers", fill = "Correlation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(cor_plot)

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
biomarker <- both_results_adj$Biomarker
