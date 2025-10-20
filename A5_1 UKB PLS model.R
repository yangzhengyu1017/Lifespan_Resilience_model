## delet work satisfication for too much people were unemployed.

##导入基础数据--------
library(data.table)
library(tidyverse)
library(rstatix)
library(modeldata)  
library(pls)
resilience_corr_rename_dat <- read_csv("~/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/UKB_resilience_corr_rename_dat_11221.csv")
resilience_corr_rename_dat <- resilience_corr_rename_dat[,-1]

#设置文件路径和读取数据：
ukb_path <- c('/public/mig_old_storage/home2/UKB_Tabular_merged_10/')

#获取UKB的代码库
item_id <- as_tibble(fread(str_c(ukb_path,'UKB_FieldID_Subset.csv')))

#查询UKB代码所属子集
df_check <- item_id[which(str_detect(item_id$Field_ID,'6141')),]


#提取BL创伤----------------------
scale_dat <- as_tibble(fread(str_c(ukb_path,'UKB_subset_3.csv')))
stressor_bl <- scale_dat[,c(1,which(str_detect(colnames(scale_dat),'6145-0.0')))]



# 删除里面不想回答的人
has_minus_3 <- grep("-3", stressor_bl$`6145-0.0`)
stressor_bl$`6145-0.0`[has_minus_3] <- NA


#stressor_bl$`6145-0.0` <- ifelse(stressor_bl$`6145-0.0` == "", NA, stressor_bl$`6145-0.0`)

stressor_bl$Self_Illness <- ifelse(is.na(stressor_bl$`6145-0.0`), NA, str_count(stressor_bl$`6145-0.0`, "1"))
stressor_bl$Relative_Illness <- ifelse(is.na(stressor_bl$`6145-0.0`), NA, str_count(stressor_bl$`6145-0.0`, "2"))
stressor_bl$Relative_Death <- ifelse(is.na(stressor_bl$`6145-0.0`), NA, str_count(stressor_bl$`6145-0.0`, "3"))
stressor_bl$Spouse_Death <- ifelse(is.na(stressor_bl$`6145-0.0`), NA, str_count(stressor_bl$`6145-0.0`, "4"))
stressor_bl$Marital_Separation <- ifelse(is.na(stressor_bl$`6145-0.0`), NA, str_count(stressor_bl$`6145-0.0`, "5"))
stressor_bl$Financial_Difficulties <- ifelse(is.na(stressor_bl$`6145-0.0`), NA, str_count(stressor_bl$`6145-0.0`, "6"))

#休闲时间
leisure_dat <- scale_dat[,c(1,which(str_detect(colnames(scale_dat),'6160-0.0')))]
has_minus_3 <- grep("-3", leisure_dat$`6160-0.0`)
leisure_dat$`6160-0.0`[has_minus_3] <- NA


#leisure_dat$`6160-0.0` <- ifelse(leisure_dat$`6160-0.0` == "", NA, leisure_dat$`6160-0.0`)

leisure_dat$sport <- ifelse(is.na(leisure_dat$`6160-0.0`), NA, str_count(leisure_dat$`6160-0.0`, "1"))
leisure_dat$social_club <- ifelse(is.na(leisure_dat$`6160-0.0`), NA, str_count(leisure_dat$`6160-0.0`, "2"))
leisure_dat$resligious_group <- ifelse(is.na(leisure_dat$`6160-0.0`), NA, str_count(leisure_dat$`6160-0.0`, "3"))
leisure_dat$education_class <- ifelse(is.na(leisure_dat$`6160-0.0`), NA, str_count(leisure_dat$`6160-0.0`, "4"))
leisure_dat$other_group <- ifelse(is.na(leisure_dat$`6160-0.0`), NA, str_count(leisure_dat$`6160-0.0`, "5"))
leisure_dat$Num_social_activity <- leisure_dat$sport + leisure_dat$social_club + leisure_dat$resligious_group+
  leisure_dat$education_class + leisure_dat$other_group
leisure_dat$no_activity <- ifelse(is.na(leisure_dat$`6160-0.0`), NA, str_count(leisure_dat$`6160-0.0`, "7"))
leisure_dat <- leisure_dat %>%
  mutate(Num_social_activity = ifelse(no_activity == 1, 0, Num_social_activity))
leisure_dat <- leisure_dat %>%
  mutate(Num_social_activity = ifelse(is.na(`6160-0.0`), NA, Num_social_activity))

#提取家庭居住数据-----------------------
family_menber_bl <- scale_dat[,c(1,which(str_detect(colnames(scale_dat),'6141-0.0')))]
has_minus_3 <- grep("-3", family_menber_bl$`6141-0.0`)
family_menber_bl$`6141-0.0`[has_minus_3] <- NA


#family_menber_bl$`6141-0.0` <- ifelse(family_menber_bl$`6141-0.0` == "", NA, family_menber_bl$`6141-0.0`)

family_menber_bl$Live_with_partner <- ifelse(is.na(family_menber_bl$`6141-0.0`), NA, str_count(family_menber_bl$`6141-0.0`, "1"))
family_menber_bl$Live_with_children <- ifelse(is.na(family_menber_bl$`6141-0.0`), NA, str_count(family_menber_bl$`6141-0.0`, "2"))
family_menber_bl$Live_with_siblings <- ifelse(is.na(family_menber_bl$`6141-0.0`), NA, str_count(family_menber_bl$`6141-0.0`, "3"))
family_menber_bl$Live_with_parents <- ifelse(is.na(family_menber_bl$`6141-0.0`), NA, str_count(family_menber_bl$`6141-0.0`, "4"))
family_menber_bl$Live_with_grandchild <- ifelse(is.na(family_menber_bl$`6141-0.0`), NA, str_count(family_menber_bl$`6141-0.0`, "6"))
family_menber_bl$Live_with_related <- ifelse(is.na(family_menber_bl$`6141-0.0`), NA, str_count(family_menber_bl$`6141-0.0`, "7"))
family_menber_bl$Live_with_unrelated <- ifelse(is.na(family_menber_bl$`6141-0.0`), NA, str_count(family_menber_bl$`6141-0.0`, "8"))

family_menber_bl <- merge(family_menber_bl, resilience_corr_rename_dat[,c(1,27)], by = "eid", all.x = TRUE)
family_menber_bl$Live_alone <- 0
family_menber_bl$Live_alone <- ifelse(is.na(family_menber_bl$`6141-0.0`), NA, 0)
family_menber_bl$Live_alone <- ifelse(is.na(family_menber_bl$Num_People_Living_BL), NA, 0)
family_menber_bl <- family_menber_bl %>%
  mutate(Live_alone = ifelse(Num_People_Living_BL == 1, 1, Live_alone))
family_menber_bl <- family_menber_bl %>%
  mutate(Live_alone = ifelse(Num_People_Living_BL > 1, 0, Live_alone))

#合并相关的数据------------------------------------
resilience_corr_rename_dat <- merge(resilience_corr_rename_dat,stressor_bl[,c(1,3:8)], by="eid", all.x = TRUE)
resilience_corr_rename_dat <- merge(resilience_corr_rename_dat,leisure_dat[,c(1,3:8)], by="eid", all.x = TRUE)
resilience_corr_rename_dat <- merge(resilience_corr_rename_dat,family_menber_bl[,c(1,3:9,11)], by="eid", all.x = TRUE)

#把满意度转过来
resilience_corr_rename_dat$Financial_Situation_Satisfaction_BL <- 6 - resilience_corr_rename_dat$Financial_Situation_Satisfaction_BL
resilience_corr_rename_dat$Health_Satisfaction_BL <- 6 - resilience_corr_rename_dat$Health_Satisfaction_BL
resilience_corr_rename_dat$Work_Job_Satisfaction_BL <- 6 - resilience_corr_rename_dat$Work_Job_Satisfaction_BL
resilience_corr_rename_dat$Family_Relationship_Satisfaction_BL <- 6 - resilience_corr_rename_dat$Family_Relationship_Satisfaction_BL
resilience_corr_rename_dat$Friendships_Satisfaction_BL <- 6 - resilience_corr_rename_dat$Friendships_Satisfaction_BL
resilience_corr_rename_dat$Social_Freq_Visits_BL <- 8 - resilience_corr_rename_dat$Social_Freq_Visits_BL
resilience_corr_rename_dat$Social_Able_Confide_BL <- 5 - resilience_corr_rename_dat$Social_Able_Confide_BL


#BRS_train_dat <-resilience_corr_rename_dat[,c(1,19,22:24,38:47,239:243)]
BRS_train_dat <- resilience_corr_rename_dat %>% # 创建新数据集新变量  
  transmute(eid, 
            gender, age_BL, Education_year, 
            HH_Num_Vehicle_BL, HH_Income_BL, HH_Own_Rent_BL,Able_Pay_Rent_Mortgage_FU1, Financial_Difficulties, IMD,
            Num_People_Living_BL, Been_In_Confiding_Relationship_FU1,Belittlement_Adult_FU1, Marital_Separation,
            Live_with_partner, Live_with_children,Live_with_siblings, Live_with_parents,Live_with_grandchild,Live_with_related,Live_with_unrelated,Live_alone,
            Social_Freq_Visits_BL,Social_Able_Confide_BL, Loneliness_BL,Num_social_activity,
            Breastfed_Baby_BL, Comp_Body_Size_Age_10_BL, Comp_Height_Size_Age_10_BL, Maternal_Smoking_Birth_BL,
            Felt_Loved_As_Child_FU1,Phys_Abused_As_Child_FU1, Felt_Hated_As_Child_FU1, Sex_Molested_As_Child_FU1,Someone_Take_To_Doctor_As_Child_FU1,
            MET_Minutes_Per_Week_Moderate_Activity_BL,MET_Minutes_Per_Week_Vigorous_Activity_BL,MET_Minutes_Per_Week_Walking_BL,Morning_Evening_Person_BL,TV_Time_BL,Computer_Time_BL,
            Big5_warmth, Big5_diligence, Big5_nervousness, Big5_curiosity, Big5_sociability,
            Health_Satisfaction_BL, Family_Relationship_Satisfaction_BL, Friendships_Satisfaction_BL,Financial_Situation_Satisfaction_BL,
            self_resilience
  )



BRS_train_dat <- na.omit(BRS_train_dat)
#set unemployed people work satisfication as NA
#BRS_train_dat$Work_Job_Satisfaction_BL[BRS_train_dat$Work_Job_Satisfaction_BL == -1] <- NA
BRS_train_dat[,c(2:51)] <- scale(BRS_train_dat[,c(2:51)])
#BRS_train_dat$gender <- factor(BRS_train_dat$gender)
glimpse(BRS_train_dat)
#BRS_train_dat$Adopted_Child_BL[is.na(BRS_train_dat$Adopted_Child_BL)] <- 0

##检查所有训练集的数量--
train_test <- resilience_corr_rename_dat %>% # 创建新数据集新变量  
  transmute(eid, PHQ_FU1 = PHQ9_Severity_FU1,
            PHQ_FU2 = `PHQ-9_FU2`, GAD_FU1 = General_Anxiety_Disorder_Severity_FU1,
            GAD_FU2 = General_Anxiety_Disorder_Severity_FU2
  )

train_test <- train_test %>%
  mutate(completed = ifelse(rowSums(is.na(.)) == 0, 1, 0))
BRS_train_dat <- merge(BRS_train_dat, train_test[c(1,6)], by = "eid", all.x = TRUE)

# Set seed for reproducibility
set.seed(123)
# Identify rows with completed equal to 1
completed_rows <- which(BRS_train_dat$completed == 1)
# Randomly select 30000 rows
selected_rows <- sample(completed_rows, 7685)
# Create a new column to indicate the selected rows
BRS_train_dat$completed <- 0
# Mark the selected rows as 1
BRS_train_dat$completed[selected_rows] <- 1

lm_dat <- BRS_train_dat[,c(2:51)]
model <- lm(self_resilience ~ . , data = lm_dat)

summary(model)


library(data.table)
library(tidyverse)
library(lightgbm)

#分拆训练集和验证集-------
# 读取数据
#train_x_dat_nobrain <- BRS_train_dat[BRS_train_dat$completed == 0,-c(1,32,33)]
#train_y_dat_nobrain <- BRS_train_dat[BRS_train_dat$completed == 0,c(32)]
#predic_x_dat <- BRS_train_dat[BRS_train_dat$completed == 1,-c(1,32,33)]
#predic_y_dat <- BRS_train_dat[BRS_train_dat$completed == 1,c(32)]


# 数据集转换为矩阵
#train_x_dat_nobrain <- data.matrix(train_x_dat_nobrain)
#train_y_dat_nobrain <- data.matrix(train_y_dat_nobrain)
#predic_x_dat <- data.matrix(predic_x_dat)
#predic_y_dat <- data.matrix(predic_y_dat)

#使用随机抽样的方法将数据进行分类----------------------
# Split data in train, val test
str(BRS_train_dat)
#BRS_train_dat <- as.numeric(BRS_train_dat)
meats_train <- BRS_train_dat[BRS_train_dat$completed == 0,]
#meats_train <- train_val_dat[1:25000,]
#meats_val <- train_val_dat[25001:35726,]
meats_test <- BRS_train_dat[BRS_train_dat$completed == 1,]
# Split the column names in X and Y
X_colnames <- colnames(BRS_train_dat)[c(2:50)]
Y_colnames <- colnames(BRS_train_dat)[51]
# Split each train, val, test into two matrices
X_train_matrix <- as.matrix(meats_train[X_colnames])
Y_train_matrix <- as.matrix(meats_train[Y_colnames])
#X_val_matrix <- as.matrix(meats_val[X_colnames])
#Y_val_matrix <- as.matrix(meats_val[Y_colnames])
X_test_matrix <- as.matrix(meats_test[X_colnames])
Y_test_matrix <- as.matrix(meats_test[Y_colnames])

# fit the full model on train data
my_plsr <- plsr(Y_train_matrix ~ X_train_matrix, ncomp=49, scale = TRUE, validation='CV')  

#通过RMSEP功能分析模型使用不同个数的组分--------------------
plot(RMSEP(my_plsr))  

#每个变量的回归系数(每个变量对应一个波长)----------
plot(my_plsr, plottype = "coef", ncomp=c(1:5), legendpos = "bottomleft")  
plot(my_plsr, plottype = "coef", ncomp=c(1:5,11), legendpos = "bottomleft")  
plot(my_plsr, plottype = "coef", ncomp=c(7,29), legendpos = "bottomleft")  
#遍历1-50个组分组合的情况，并且使用validation data 去帮助筛选最优的组分数目--------------------
# Loop through possible values for n_comp to optimize R2 on validation data
ncomp.onesigma <- selectNcomp(my_plsr, method = "onesigma", plot = TRUE)
ncomp.permute <- selectNcomp(my_plsr, method = "randomization", plot = TRUE)



#验证我们是否也在测试数据集上是否也能得到较好的结果----------------------
# Predict on test for having a final R2 estimate
best_ncomp <- ncomp.permute
best_model <- plsr(Y_train_matrix ~ X_train_matrix, ncomp=best_ncomp, scale = TRUE, validation='CV')  
test_predictions <- as.matrix(data.frame(predict(best_model, ncomp=best_ncomp, X_test_matrix)))  
mean_r2 <- mean(diag(cor(test_predictions, Y_test_matrix))**2)  
print(mean_r2)  

summary(best_model)

coefficients <- coef(best_model, ncomp = ncomp.permute)
print(coefficients)
coefficients <- data.frame(coefficients)


PR_score_dat <- BRS_train_dat[BRS_train_dat$completed == 1,c(1,51)]
PR_score_dat <- cbind(PR_score_dat,test_predictions)
colnames(PR_score_dat) <- c("eid","BRS","PRS")
#write.csv(PR_score_dat,"~/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/PRS_PLSR_0323.csv")


#画条形图---------------
X_colnames <- rownames(coefficients)
Category <- c(rep("Demographic",3), rep("Economics", 6), rep("Family-support", 12),rep("Social-support", 4),
              rep("Early-risk", 9), rep("Lifestyle", 6),rep("Personality", 5), rep("Satisfaction", 4))

info_df <- data.frame(X_colnames, Category)

# 合并数据框
plot_data <- cbind(info_df, coefficients)

colnames(plot_data) <- c("Variable", "Category", "Mean")
plot_data$Category <- factor(plot_data$Category,
                             levels = unique(plot_data$Category),
                             ordered = T)
# 对 merged_df 按照 Category 和系数的绝对值进行排序
plot_data <- plot_data[order(plot_data$Category, -plot_data$Mean), ]

# 使用 ggplot2 创建条形图

plot_data$Variable <- factor(plot_data$Variable,
                             levels = unique(plot_data$Variable),
                             ordered = T)

library(ggsci)
# 绘制柱状图并添加误差线
p <- ggplot(plot_data, aes(x = Variable, y = Mean, fill = Category)) +
  geom_bar(stat = "identity",position = position_dodge(width = 0.4),width = 0.6) +
  labs(title = "Weights of factors in the resilience model", x = "Factors", y = "Weight") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_lancet()# 旋转x轴标签以免重叠

# 显示绘图
print(p)



#不同成分的解释量--------------------
# 创建一个空列表，用于存储各个类别的变量名向量
var_names <- colnames(BRS_train_dat)[c(2:50)]
Category <- c(rep("Demographic",3), rep("Economics", 6), rep("Family-support", 12),rep("Social-support", 4),
              rep("Early-risk", 9), rep("Lifestyle", 6),rep("Personality", 5), rep("Satisfaction", 4))
Domnine <- c(rep("Individual_trait",3), rep("Postive_resource", 6), rep("Postive_resource", 12),rep("Postive_resource", 4),
             rep("Past_experience", 9), rep("Individual_trait", 6),rep("Individual_trait", 5), rep("Individual_trait", 4))


factor_name <- cbind(var_names,Category,Domnine)
factor_name <- data.frame(factor_name)
category_vars_list <- list()

# 获取所有不同的类别值
categories <- unique(factor_name$Category)

# 创建空的数据框用于存储结果
result_df <- data.frame(Category = character(), best_ncomp = numeric(), train_mean_r2 = numeric(), pred_mean_r2 = numeric())

# 循环遍历part中的每个部分
for (category in categories) {
  
  
  # 从数据中选择当前类别的变量名，并存储到向量中
  selected_vars <- factor_name$var_names[factor_name$Category == category]
  # 从meats_train中提取对应的X和Y变量
  X_train_matrix <- as.matrix(meats_train[selected_vars])
  Y_train_matrix <- as.matrix(meats_train[Y_colnames])
  X_test_matrix <- as.matrix(meats_test[selected_vars])
  Y_test_matrix <- as.matrix(meats_test[Y_colnames])
  
  # 拟合PLSR模型
  sep_plsr <- plsr(Y_train_matrix ~ X_train_matrix, ncomp = length(selected_vars), scale = TRUE, validation = 'CV')
  ncomp.permute <- selectNcomp(sep_plsr, method = "randomization", plot = FALSE)
  best_ncomp <- ncomp.permute
  
  # 使用最佳组件数拟合最终模型
  best_model <- plsr(Y_train_matrix ~ X_train_matrix, ncomp = best_ncomp, scale = TRUE, validation = 'CV')
  
  # 计算训练集R2值
  train_predictions <- as.matrix(data.frame(predict(best_model, ncomp = best_ncomp, X_train_matrix)))
  train_mean_r2 <- mean(diag(cor(train_predictions, Y_train_matrix)) ** 2)
  
  # 计算测试集R2值
  test_predictions <- as.matrix(data.frame(predict(best_model, ncomp = best_ncomp, X_test_matrix)))
  pred_mean_r2 <- mean(diag(cor(test_predictions, Y_test_matrix)) ** 2)
  
  # 将结果添加到结果数据框中
  result_df <- rbind(result_df, data.frame(Category = category, best_ncomp = best_ncomp, train_mean_r2 = train_mean_r2, pred_mean_r2 = pred_mean_r2))
}

# 显示结果
print(result_df)

plotR_data <- result_df[,c(1,4)]
# 对 merged_df 按照 Category 和系数的绝对值进行排序
plotR_data <- plotR_data[order(-plotR_data$pred_mean_r2), ]

# 使用 ggplot2 创建条形图
plotR_data$Category <- factor(plotR_data$Category,
                              levels = unique(plotR_data$Category),
                              ordered = T)

library(ggsci)

# 绘制柱状图并添加误差线，并翻转为横向
p <- ggplot(plotR_data, aes(x = Category, y = pred_mean_r2, fill = Category)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.4), width = 0.6) +
  labs(title = "R2 by Category", x = NULL, y = "Category") +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) + # 设置y轴标签的方向
  coord_flip() + # 翻转坐标轴方向
  scale_fill_lancet() + # 设置填充色
  xlim(rev(levels(plotR_data$Category))) # 反转x轴顺序

# 显示绘图
print(p)


#循环估计权重------------------------
#重复采样获得对参数的估计----------------------------

#bootstrap_sample <- BRS_train_dat[sample(nrow(BRS_train_dat), replace = TRUE), ]

# 定义一个函数，用于执行一次自助法采样、拟合模型并计算R方值
bootstrap_function <- function(data) {
  # 从原始数据中随机有放回地抽取相同数量的样本
  bootstrap_sample <- data[sample(nrow(data), replace = TRUE), ]
  
  #定义训练集
  # Split the column names in X and Y
  X_colnames <- colnames(bootstrap_sample)[c(2:50)]
  Y_colnames <- colnames(bootstrap_sample)[51]
  # Split each train, val, test into two matrices
  X_train_matrix <- as.matrix(bootstrap_sample[X_colnames])
  Y_train_matrix <- as.matrix(bootstrap_sample[Y_colnames])
  
  best_ncomp <- 2
  best_model <- plsr(Y_train_matrix ~ X_train_matrix, ncomp=best_ncomp, scale = TRUE, validation='CV')  
  
  coefficients <- coef(best_model, ncomp = best_ncomp)
  #coefficients <- data.frame(coefficients)
  return(coefficients)
}

# 重复执行bootstrap_function函数1000次，并保存R方值
n_iterations <- 1000
coefficients_values <- replicate(n_iterations, bootstrap_function(BRS_train_dat))
coefficients_values <-data.frame(coefficients_values)
view(coefficients_values)
#write_csv(coefficients_values,"~/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/PLSR_bootstrap_1000_cor_1204.csv")
#coefficients_values <- read_csv("~/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/PLSR_bootstrap_1000_cor_0316.csv")


# 计算每个变量的平均值和标准差
mean_values <- apply(coefficients_values, 1, mean)  # 计算每行（变量）的平均值
sd_values <- apply(coefficients_values, 1, sd)  # 计算每行（变量）的标准差

var_names <- colnames(BRS_train_dat)[c(2:50)]
Category <- c(rep("Demographic",3), rep("Economics", 6), rep("Family-support", 12),rep("Social-support", 4),
              rep("Early-risk", 9), rep("Lifestyle", 6),rep("Personality", 5), rep("Satisfaction", 4))
factor_name <- cbind(var_names,Category)

# 创建数据框以供绘图使用
plot_data <- data.frame(
  Variable = var_names,  # 变量名
  Mean = mean_values,  # 平均值
  SD = sd_values,  # 标准差
  Category <- Category
)

plot_data$Category <- factor(plot_data$Category,
                             levels = unique(plotR_data$Category),
                             ordered = T)
# 对 merged_df 按照 Category 和系数的绝对值进行排序
plot_data <- plot_data[order(plot_data$Category, -plot_data$Mean), ]

# 使用 ggplot2 创建条形图
plot_data$Variable <- factor(plot_data$Variable,
                             levels = unique(plot_data$Variable),
                             ordered = T)

#把重复采样得到的weight数据存储
write.csv(plot_data, "~/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/PLS_boost_7cate_0501.csv")
plot_data <- read_csv("~/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/PLS_boost_7cate_0501.csv")
plot_data <- plot_data[,-1]

# 创建重命名映射
rename_map <- c(
  "gender" = "Gender",
  "age_BL" = "Age",
  "Education_year" = "Education year",
  "HH_Num_Vehicle_BL" = "Household vehicle count",
  "HH_Income_BL" = "Household income",
  "HH_Own_Rent_BL" = "Rent accommodation lived in",
  "Able_Pay_Rent_Mortgage_FU1" = "Housing Affordability",
  "Financial_Difficulties" = "Financial difficulties",
  "IMD" = "IMD",
  "Num_People_Living_BL" = "Number of people living in",
  "Been_In_Confiding_Relationship_FU1" = "Confiding partner relationship",
  "Belittlement_Adult_FU1" = "Belittling paternal relationship",
  "Live_with_partner" = "Live with partner",
  "Live_with_children" = "Live with children",
  "Live_with_siblings" = "Live with siblings",
  "Live_with_parents" = "Live with parents",
  "Live_with_grandchild" = "Live with grandchild",
  "Live_with_related" = "Live with related",
  "Live_with_unrelated" = "Live with unrelated",
  "Live_alone" = "Live alone",
  "Marital_Separation" = "Marital separation",
  "Social_Freq_Visits_BL" = "Frequence of social visits",
  "Social_Able_Confide_BL" = "Confiding social relationship",
  "Loneliness_BL" = "Loneliness",
  "Num_social_activity" = "Number of social activities",
  "Breastfed_Baby_BL" = "Breastfed as a baby",
  "Comp_Body_Size_Age_10_BL" = "Comparative body size at age 10",
  "Comp_Height_Size_Age_10_BL" = "Comparative height at age 10",
  "Maternal_Smoking_Birth_BL" = "Maternal smoking around birth",
  "Felt_Loved_As_Child_FU1" = "Childhood emotional neglect",
  "Phys_Abused_As_Child_FU1" = "Childhood physical abuse",
  "Felt_Hated_As_Child_FU1" = "Childhood emotional abuse",
  "Sex_Molested_As_Child_FU1" = "Childhood sexual molestation",
  "Someone_Take_To_Doctor_As_Child_FU1" = "Childhood physical neglect",
  "MET_Minutes_Per_Week_Moderate_Activity_BL" = "Moderate exercise time per week",
  "MET_Minutes_Per_Week_Vigorous_Activity_BL" = "Vigorous exercise time per week",
  "MET_Minutes_Per_Week_Walking_BL" = "Walking time per week",
  "Morning_Evening_Person_BL" = "Chronotype: evening or morning person",
  "TV_Time_BL" = "TV time",
  "Computer_Time_BL" = "Computer time",
  "Big5_warmth" = "Big5-Agreeableness",
  "Big5_diligence" = "Big5-Conscientiousness",
  "Big5_nervousness" = "Big5-Neuroticism",
  "Big5_sociability" = "Big5-Extraversion",
  "Big5_curiosity" = "Big5-Openness",
  #"Work_Job_Satisfaction_BL" = "Work satisfaction",
  "Health_Satisfaction_BL" = "Health satisfaction",
  "Family_Relationship_Satisfaction_BL" = "Family satisfaction",
  "Friendships_Satisfaction_BL" = "Friendships satisfaction",
  "Financial_Situation_Satisfaction_BL" = "Financial satisfaction"
)

# 使用rename_map重命名Variable列
plot_data$Variable <- rename_map[plot_data$Variable]


plot_data$Category <- factor(plot_data$Category,
                             levels = unique(plotR_data$Category),
                             ordered = T)
# 对 merged_df 按照 Category 和系数的绝对值进行排序
plot_data <- plot_data[order(plot_data$Category, -plot_data$Mean), ]

# 使用 ggplot2 创建条形图
plot_data$Variable <- factor(plot_data$Variable,
                             levels = unique(plot_data$Variable),
                             ordered = T)



library(ggsci)
# 绘制柱状图并添加误差线
p <- ggplot(plot_data, aes(x = Variable, y = Mean, ymin = Mean - SD, ymax = Mean + SD, fill = Category)) +
  geom_bar(stat = "identity",position = position_dodge(width = 0.4),width = 0.6) +
  geom_errorbar(width = 0.2, position = position_dodge(width = 0.9)) +  # 添加误差线
  labs(title = "Weights of factors in the resilience model", x = "Factors", y = "Weight") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust = 0.5, size = 13)) + scale_fill_lancet()# 旋转x轴标签以免重叠

# 显示绘图(导出1000*700分辨率)
print(p)


#相关检查----------------------
# 计算两个变量之间的相关系数和相关性检验
correlation_result <- cor.test(resilience_corr_rename_dat$Num_social_activity , resilience_corr_rename_dat$self_resilience)

# 打印相关性系数
cat("Correlation coefficient:", correlation_result$estimate, "\n")

# 打印相关性检验结果
print(correlation_result)


#验证（创伤）---------------------
library(data.table)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(caret)
library(visreg)
library(rms)
library(gtsummary)
library(autoReg)
library(ggpubr)
library(ggplot2)
library(ggbeeswarm)
library(patchwork)
library(gridExtra)
library(skimr)
library(janitor)
library(dplyr)
resilience_corr_rename_dat <- read_csv("~/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/UKB_resilience_corr_rename_dat_11221.csv")
resilience_corr_rename_dat <- resilience_corr_rename_dat[,-1]
#resilience_model_dat <- read_csv("~/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/predicited_resiliece_3w_norm_dat.csv")
#resilience_model_dat <- resilience_model_dat[,-1]
soc_env_trauma_mental_dat <- read.csv("~/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/UKB_BL_FU_trauma_mental_env_dat.csv")
soc_env_trauma_mental_dat <- soc_env_trauma_mental_dat[,-1]


#trauma和心理健康的相关分析-------------------------------------
library(dplyr)
library(emmeans)
library(bruceR)
library(ppcor)
library(QuantPsyc)

# 进行trauma 验证分析 -----------------------------------------------------
#对数据进行整理，保证真实的resilience和模型计算的resilien能够兼容
resilience_group_R <- resilience_corr_rename_dat
#resilience_group_R <- merge(resilience_group_R,resilience_model_dat, by = "eid", all.x = TRUE)
resilience_group_R <- merge(resilience_group_R,soc_env_trauma_mental_dat[,c(1,71,85,79)], by = "eid", all.x = TRUE)

resilience_group_R$self_resilience <- (resilience_group_R$self_resilience + 6 )/6
resilience_group_R$FU2_recent_trauma <- resilience_group_R$FU2_recent_trauma * 10
resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 2, 6, Ethnic_group))
resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 3, 2, Ethnic_group))
resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 5, 2, Ethnic_group))
resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 4, 3, Ethnic_group))
resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 6, 4, Ethnic_group))

resilience_group_R$Ethnic_group <- resilience_group_R$Ethnic_group - 1

#合并模型预估分数，并进行替换
resilience_group_R <- merge(resilience_group_R, PR_score_dat, by = 'eid', all.x = T)
resilience_group_R <- resilience_group_R %>%
  dplyr::select(-self_resilience) %>%          # 删除 self_resilience 列
  dplyr::select(-BRS) %>%
  rename(self_resilience = PRS)         # 将 PRS 列重命名为 self_resilience
# 计算分位数
quantiles <- quantile(resilience_group_R$self_resilience, probs = c(1/3, 2/3),na.rm = TRUE)

# 替换值
resilience_group_R$self_resilience[resilience_group_R$self_resilience <= quantiles[1]] <- -1
resilience_group_R$self_resilience[resilience_group_R$self_resilience > quantiles[1] & resilience_group_R$self_resilience < quantiles[2]] <- 0
resilience_group_R$self_resilience[resilience_group_R$self_resilience >= quantiles[2]] <- 1
resilience_group_R$self_resilience <- resilience_group_R$self_resilience + 1
resilience_group_R$self_resilience <- as.factor(resilience_group_R$self_resilience)

des_resilience <- resilience_group_R[,c("eid","self_resilience")]
des_resilience %>% 
  tabyl(self_resilience) %>%
  adorn_totals("row") %>%
  adorn_pct_formatting()

## BL阶段--------

resilience_test <- resilience_group_R %>% # 创建新数据集新变量  
  transmute(eid, Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")), Age = age_BL, 
            Ethnic = factor(Ethnic_group, levels = c("0","1","2","3"), labels = c("1_White","2_Asian","3_Black","4_Other")),  
            Education = Education_year,
            BMI = BMI_BL,
            selfresilience = factor(self_resilience, levels = c(0,1,2), labels = c("1_Low","2_Medium","3_High")),
            PHQ_BL = Depressive_Symptoms_PHQ4_BL, Trauma_exposure = trauma_num)
resilience_test$Trauma_exposure[resilience_test$Trauma_exposure >= 5] <- 4
resilience_test <- na.omit(resilience_test)
#resilience_test <- resilience_test[!is.na(resilience_test$selfresilience), ]

resilience_test$Trauma_exposure <- as.numeric(resilience_test$Trauma_exposure)
resilience_test <- na.omit(resilience_test)

des_dat <- resilience_test[,c("eid","Trauma_exposure")]
des_dat <- na.omit(des_dat)
des_dat$Trauma_exposure <- as.factor(des_dat$Trauma_exposure)

bl_characteric <- resilience_test %>% tbl_summary(by = selfresilience,
                                                  type = list(Age ~ "continuous", Age ~ "continuous")  # 合并 type 选项
) %>%
  add_p() %>% 
  add_overall()
# 将 gtsummary 对象转换为 tibble 或 data frame
bl_characteric_table <- as_tibble(bl_characteric)

# 如果需要，可以使用 as.data.frame
bl_characteric_table <- as.data.frame(bl_characteric)

des_dat %>% 
  tabyl(Trauma_exposure) %>%
  adorn_totals("row") %>%
  adorn_pct_formatting()

lme2 <- lm(
  PHQ_BL ~  selfresilience + Trauma_exposure + selfresilience*Trauma_exposure + Age + Sex + Ethnic + BMI + Education,
  data = resilience_test)
summary(lme2)
library(rockchalk)
ps  <- plotSlopes(lme2, plotx="Trauma_exposure", modx="selfresilience", xlab = "X", ylab = "Y")#, modxVals = "std.dev")

tidy_results_BL <- tidy(lme2)
tidy_results_BL$conf_int <- confint(lme2)
T1 <- autoReg(lme2) %>% 
  myft()
T1

library(ggplot2)
compare_means(PHQ_BL ~  selfresilience,
              data = resilience_test,
              group.by = "Trauma_exposure")
library(ggpubr)

# 计算每条折线的最大值
max_values <- aggregate(as.formula(paste("PHQ_BL ~Trauma_exposure")),
                        data = resilience_test, FUN = mean)
# 创建 ggline 图表
L1 <- ggline(resilience_test, x = "Trauma_exposure", y = "PHQ_BL", add = "mean_se",
             color = "selfresilience", palette = c("#fA9E38", "#72BD5B", "#4995C6"), size = 1) +
  # 设置线条粗细
  stat_compare_means(aes(group = selfresilience), label = "p.signif", 
                     label.y = max_values$PHQ_BL + 1, size = 4)  # 设置点的大小
L1

#比较不同组的创伤相关情况---------
resilience <- c("1_Low","2_Medium","3_High")

# 初始化一个空的数据框，用于存储结果
correlation_results_BL <- data.frame(Timepoint = character(),Resilience_Level = character(), Correlation = numeric(), Subject_Count = integer(), stringsAsFactors = FALSE)

# 循环遍历每个 resilience 水平
for (resilience_level in resilience) {
  # 从数据中筛选出当前 resilience 水平的观测值
  resilient_group <- resilience_test[resilience_test$selfresilience == resilience_level, ]
  
  # 在当前 resilience 水平下，计算线性模型
  model <- lm(PHQ_BL ~ Trauma_exposure + Education + BMI + Sex + Age, data = resilient_group)
  
  # 获取线性模型的系数
  betas <- coef(model)
  
  # 计算 Trauma_exposure 和 PHQ_BL 的相关系数
  correlation <- betas["Trauma_exposure"] * sd(resilient_group$Trauma_exposure) / sd(resilient_group$PHQ_BL)
  
  # 获取当前 resilience 水平的被试量
  subject_count <- nrow(resilient_group)
  
  # 将结果添加到数据框中
  correlation_results_BL <- rbind(correlation_results_BL, 
                                  data.frame(Timepoint = "BL",
                                             Resilience_Level = resilience_level, 
                                             Correlation = correlation, 
                                             Subject_Count = subject_count))
}

# 打印结果
print(correlation_results_BL)

library(cocor)

# 初始化一个空的数据框，用于存储两两比较的结果
comparison_results <- data.frame(Group1 = character(), Group2 = character(), Coef_diff = numeric(), p_value = numeric(), stringsAsFactors = FALSE)

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
                                data.frame(Group1 = resilience[i], 
                                           Group2 = resilience[j], 
                                           Z = Z, 
                                           p_value = p_value))
  }
}

# 打印比较结果
print(comparison_results)


## FU1阶段--------
# mental_symptoms_FU1
resilience_group_R0 <- copy(resilience_group_R)
resilience_group_R0 <- resilience_group_R0[,c("eid","age_BL","gender","BMI_BL",
                                              "Ethnic_group","site","IMD",
                                              "self_resilience","Depressive_Symptoms_PHQ4_BL",
                                              "PHQ9_Severity_FU1","PHQ-9_FU2",
                                              "General_Anxiety_Disorder_Severity_FU1","General_Anxiety_Disorder_Severity_FU2",
                                              "AUDIT_Score_FU1_FU1","AUDIT_Score_FU2_FU2","trauma_num","FU_recent_trauma","FU2_recent_trauma","Education_year")]
resilience_group_R0$FU1_symptoms <- (resilience_group_R0$PHQ9_Severity_FU1/28 + resilience_group_R0$General_Anxiety_Disorder_Severity_FU1/21 + resilience_group_R0$AUDIT_Score_FU1_FU1/40 ) * 10
#resilience_group_R0$FU1_symptoms <- rescale(resilience_group_R0$FU1_symptoms)

resilience_group_R0$FU_recent_trauma[resilience_group_R0$FU_recent_trauma == 3] <- 2

#resilience_group_R0 <- na.omit(resilience_group_R0)

resilience_group_R0$self_resilience <-as.factor(resilience_group_R0$self_resilience)
resilience_group_R0$gender <-as.factor(resilience_group_R0$gender)
resilience_group_R0$Ethnic_group <-as.factor(resilience_group_R0$Ethnic_group)
#resilience_group_R0$FU_recent_trauma <-as.factor(resilience_group_R0$FU_recent_trauma)

resilience_test <- resilience_group_R0 %>% # 创建新数据集新变量  
  transmute(eid, Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")), Age = age_BL, 
            Ethnic = factor(Ethnic_group, levels = c("0","1","2","3"), labels = c("1_White","2_Asian","3_Black","4_Other")),  
            Education = Education_year,
            BMI = BMI_BL,
            selfresilience = factor(self_resilience, levels = c(0,1,2), labels = c("1_Low","2_Medium","3_High")),
            Mental_symptom_scores_FU1 = FU1_symptoms, Trauma_exposure = FU_recent_trauma)
#resilience_test %>% tbl_summary(by = selfresilience, digits = list(Age ~ 0)) %>% add_p() %>%   add_overall()
resilience_test$Trauma_exposure <- as.numeric(resilience_test$Trauma_exposure)
resilience_test <- na.omit(resilience_test)

des_dat <- resilience_test[,c("eid","Trauma_exposure")]
des_dat <- na.omit(des_dat)
des_dat$Trauma_exposure <- as.factor(des_dat$Trauma_exposure)

des_dat %>% 
  tabyl(Trauma_exposure) %>%
  adorn_totals("row") %>%
  adorn_pct_formatting()



str(resilience_test) # 查看数据集结构
lme2 <- lm(
  Mental_symptom_scores_FU1 ~  selfresilience + Trauma_exposure + selfresilience*Trauma_exposure + Age + Sex + Ethnic + BMI + Education,
  data = resilience_test)
summary(lme2)
tidy_results_FU1 <- tidy(lme2)
tidy_results_FU1$conf_int <- confint(lme2)
autoReg(lme2) %>% 
  myft()

library(ggplot2)
compare_means(Mental_symptom_scores_FU1 ~  selfresilience,
              data = resilience_test,
              group.by = "Trauma_exposure")
library(ggpubr)

# 计算每条折线的最大值
max_values <- aggregate(as.formula(paste("Mental_symptom_scores_FU1 ~Trauma_exposure")),
                        data = resilience_test, FUN = mean)
# 创建 ggline 图表
L2 <- ggline(resilience_test, x = "Trauma_exposure", y = "Mental_symptom_scores_FU1", add = "mean_se",
             color = "selfresilience", palette = c("#fA9E38", "#72BD5B", "#4995C6"), size = 1) +
  # 设置线条粗细
  stat_compare_means(aes(group = selfresilience), label = "p.signif", 
                     label.y = max_values$Mental_symptom_scores_FU1 + 1, size = 4)  # 设置点的大小
L2
#比较不同组的创伤相关情况---------
resilience <- c("1_Low","2_Medium","3_High")

# 初始化一个空的数据框，用于存储结果
correlation_results_FU1 <- data.frame(Timepoint = character(),Resilience_Level = character(), Correlation = numeric(), Subject_Count = integer(), stringsAsFactors = FALSE)

# 循环遍历每个 resilience 水平
for (resilience_level in resilience) {
  # 从数据中筛选出当前 resilience 水平的观测值
  resilient_group <- resilience_test[resilience_test$selfresilience == resilience_level, ]
  
  # 在当前 resilience 水平下，计算线性模型
  model <- lm(Mental_symptom_scores_FU1 ~ Trauma_exposure + Education + BMI + Sex + Age, data = resilient_group)
  
  # 获取线性模型的系数
  betas <- coef(model)
  
  # 计算 Trauma_exposure 和 PHQ_BL 的相关系数
  correlation <- betas["Trauma_exposure"] * sd(resilient_group$Trauma_exposure) / sd(resilient_group$Mental_symptom_scores_FU1)
  
  # 获取当前 resilience 水平的被试量
  subject_count <- nrow(resilient_group)
  
  # 将结果添加到数据框中
  correlation_results_FU1 <- rbind(correlation_results_FU1, 
                                   data.frame(Timepoint = "FU1",
                                              Resilience_Level = resilience_level, 
                                              Correlation = correlation, 
                                              Subject_Count = subject_count))
}

# 打印结果
print(correlation_results_FU1)

library(cocor)

# 初始化一个空的数据框，用于存储两两比较的结果
comparison_results <- data.frame(Group1 = character(), Group2 = character(), Coef_diff = numeric(), p_value = numeric(), stringsAsFactors = FALSE)

# 嵌套循环比较每两组之间的相关性
for (i in 1:(length(resilience) - 1)) {
  for (j in (i + 1):length(resilience)) {
    # 选择要比较的两组数据
    group1 <- correlation_results_FU1[correlation_results_FU1$Resilience_Level == resilience[i], ]
    group2 <- correlation_results_FU1[correlation_results_FU1$Resilience_Level == resilience[j], ]
    
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
                                data.frame(Group1 = resilience[i], 
                                           Group2 = resilience[j], 
                                           Z = Z, 
                                           p_value = p_value))
  }
}

# 打印比较结果
print(comparison_results)


## FU2阶段--------
# mental_symptoms_FU2
resilience_group_R0 <- copy(resilience_group_R)
resilience_group_R0 <- resilience_group_R0[,c("eid","age_BL","gender","BMI_BL",
                                              "Ethnic_group","site","IMD",
                                              "self_resilience","Depressive_Symptoms_PHQ4_BL",
                                              "PHQ9_Severity_FU1","PHQ-9_FU2",
                                              "General_Anxiety_Disorder_Severity_FU1","General_Anxiety_Disorder_Severity_FU2",
                                              "AUDIT_Score_FU1_FU1","AUDIT_Score_FU2_FU2","trauma_num","FU_recent_trauma","FU2_recent_trauma","Education_year")]
resilience_group_R0$FU2_symptoms <- (resilience_group_R0$`PHQ-9_FU2`/28 + resilience_group_R0$General_Anxiety_Disorder_Severity_FU2/21 + resilience_group_R0$AUDIT_Score_FU2_FU2/40 ) * 10
#resilience_group_R0$FU2_symptoms <- rescale(resilience_group_R0$FU2_symptoms)

resilience_group_R0$gender <-as.factor(resilience_group_R0$gender)
resilience_group_R0$Ethnic_group <-as.factor(resilience_group_R0$Ethnic_group)
#resilience_group_R0$FU2_recent_trauma <- as.factor(resilience_group_R0$FU2_recent_trauma)

resilience_test <- resilience_group_R0 %>% # 创建新数据集新变量  
  transmute(eid, Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")), Age = age_BL, 
            Ethnic = factor(Ethnic_group, levels = c("0","1","2","3"), labels = c("1_White","2_Asian","3_Black","4_Other")),  
            Education = Education_year,
            BMI = BMI_BL,
            selfresilience = factor(self_resilience, levels = c(0,1,2), labels = c("1_Low","2_Medium","3_High")),
            Mental_symptom_scores_FU2 = FU2_symptoms, Trauma_exposure = FU2_recent_trauma)

resilience_test$Trauma_exposure[resilience_test$Trauma_exposure >= 5] <- 4
#resilience_test %>% tbl_summary(by = selfresilience, digits = list(Age ~ 0)) %>% add_p() %>%   add_overall()
resilience_test$Trauma_exposure <- as.numeric(resilience_test$Trauma_exposure)

resilience_test <- na.omit(resilience_test)

str(resilience_test) # 查看数据集结构

des_dat <- resilience_test[,c("eid","Trauma_exposure")]
des_dat <- na.omit(des_dat)
des_dat$Trauma_exposure <- as.factor(des_dat$Trauma_exposure)

des_dat %>% 
  tabyl(Trauma_exposure) %>%
  adorn_totals("row") %>%
  adorn_pct_formatting()




lme2 <- lm(
  Mental_symptom_scores_FU2 ~  selfresilience + Trauma_exposure + selfresilience*Trauma_exposure + Age + Sex + Ethnic + BMI + Education,
  data = resilience_test)
summary(lme2)
tidy_results_FU2 <- tidy(lme2)
tidy_results_FU2$conf_int <- confint(lme2)
autoReg(lme2) %>% 
  myft()


library(ggplot2)
compare_means(Mental_symptom_scores_FU2 ~  selfresilience,
              data = resilience_test,
              group.by = "Trauma_exposure")
library(ggpubr)

# 计算每条折线的最大值
max_values <- aggregate(as.formula(paste("Mental_symptom_scores_FU2 ~Trauma_exposure")),
                        data = resilience_test, FUN = mean)
# 创建 ggline 图表
L3 <- ggline(resilience_test, x = "Trauma_exposure", y = "Mental_symptom_scores_FU2", add = "mean_se",
             color = "selfresilience", palette = c("#fA9E38", "#72BD5B", "#4995C6"), size = 1) +
  # 设置线条粗细
  stat_compare_means(aes(group = selfresilience), label = "p.signif", 
                     label.y = max_values$Mental_symptom_scores_FU2 + 3, size = 4)  # 设置点的大小

L3
#比较不同组的创伤相关情况---------
resilience <- c("1_Low","2_Medium","3_High")

# 初始化一个空的数据框，用于存储结果
correlation_results_FU2 <- data.frame(Timepoint = character(),Resilience_Level = character(), Correlation = numeric(), Subject_Count = integer(), stringsAsFactors = FALSE)

# 循环遍历每个 resilience 水平
for (resilience_level in resilience) {
  # 从数据中筛选出当前 resilience 水平的观测值
  resilient_group <- resilience_test[resilience_test$selfresilience == resilience_level, ]
  
  # 在当前 resilience 水平下，计算线性模型
  model <- lm(Mental_symptom_scores_FU2 ~ Trauma_exposure + Education + BMI + Sex + Age, data = resilient_group)
  
  # 获取线性模型的系数
  betas <- coef(model)
  
  # 计算 Trauma_exposure 和 PHQ_BL 的相关系数
  correlation <- betas["Trauma_exposure"] * sd(resilient_group$Trauma_exposure) / sd(resilient_group$Mental_symptom_scores_FU2)
  
  # 获取当前 resilience 水平的被试量
  subject_count <- nrow(resilient_group)
  
  # 将结果添加到数据框中
  correlation_results_FU2 <- rbind(correlation_results_FU2, 
                                   data.frame(Timepoint = "FU2",
                                              Resilience_Level = resilience_level, 
                                              Correlation = correlation, 
                                              Subject_Count = subject_count))
}

# 打印结果
print(correlation_results_FU2)

library(cocor)

# 初始化一个空的数据框，用于存储两两比较的结果
comparison_results <- data.frame(Group1 = character(), Group2 = character(), Coef_diff = numeric(), p_value = numeric(), stringsAsFactors = FALSE)

# 嵌套循环比较每两组之间的相关性
for (i in 1:(length(resilience) - 1)) {
  for (j in (i + 1):length(resilience)) {
    # 选择要比较的两组数据
    group1 <- correlation_results_FU2[correlation_results_FU2$Resilience_Level == resilience[i], ]
    group2 <- correlation_results_FU2[correlation_results_FU2$Resilience_Level == resilience[j], ]
    
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
                                data.frame(Group1 = resilience[i], 
                                           Group2 = resilience[j], 
                                           Z = Z, 
                                           p_value = p_value))
  }
}

# 打印比较结果
print(comparison_results)





##汇总6个图-------
combined_plot <- grid.arrange(L1, L2, L3, ncol = 3)

#汇总相关的数据进行画图-----------------------
cor_result <- rbind(correlation_results_BL, correlation_results_FU1)
cor_result <- rbind(cor_result, correlation_results_FU2)

cor_plot <- ggplot(cor_result) +
  aes(
    x = Resilience_Level,
    y = Correlation,
    fill = Resilience_Level,
    colour = Resilience_Level
  ) +
  geom_col() +
  scale_fill_manual(
    values = c(`1_Low` = "#fA9E38",
               `2_Medium` = "#72BD5B",
               `3_High` = "#4995C6")
  ) +
  scale_color_manual(
    values = c(`1_Low` = "#fA9E38",
               `2_Medium` = "#72BD5B",
               `3_High` = "#4995C6")
  ) +
  theme_minimal() +
  facet_wrap(vars(Timepoint))

cor_plot

#验证（longitudinal）-------
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
library(tidyr) 

##不匹配后FU1FU2longitudinal分析--------
##PHQ----
resilience_test <- resilience_group_R %>% # 创建新数据集新变量  
  transmute(eid, Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")), Age = age_BL, 
            Ethnic = factor(Ethnic_group, levels = c("0","1","2","3"), labels = c("1_White","2_Asian","3_Black","4_Other")),  
            Education = Education_year,
            BMI = BMI_BL,
            selfresilience = factor(self_resilience, levels = c(0,1,2), labels = c("1_Low","2_Medium","3_High")),
            data_FU1 = Mental_onine_date_FU1, data_FU2 = Mental_onine_date_FU2, PHQ_FU1 = PHQ9_Severity_FU1,
            PHQ_FU2 = `PHQ-9_FU2`,
            GAD_FU1 = General_Anxiety_Disorder_Severity_FU1,
            GAD_FU2 = General_Anxiety_Disorder_Severity_FU2)
resilience_test <- na.omit(resilience_test)

str(resilience_test) # 查看数据集结构

#resilience_test %>% tbl_summary(by = selfresilience, digits = list(Age ~ 0)) %>% add_p() %>%   add_overall()
resilience_test <- na.omit(resilience_test)

resilience_test$PHQ_Change <- resilience_test$PHQ_FU2 - resilience_test$PHQ_FU1



lme2 <- lm(
  PHQ_FU1 ~  selfresilience + Age + Sex + Ethnic + BMI + Education,
  data = resilience_test)
summary(lme2)
autoReg(lme2) %>% 
  myft()

T1 <- tidy(lme2)
T1$confint <- confint(lme2)
T1 <- T1 %>%
  mutate(beta = paste0(round(estimate, 2), " (", round(confint[, 1], 2), ", ", round(confint[, 2], 2), ")"))
T1 <- T1[,c(1,7,4,5)]
T1 <- T1 %>%
  rename_with(~ paste0("PHQ_FU1_", .), -term)
print(T1)

#modelPlot(lme2,change.pointsize = T)

lme2 <- lm(
  PHQ_Change ~ selfresilience + Age + Sex + Ethnic + BMI + Education,
  data = resilience_test)
summary(lme2)
autoReg(lme2) %>% 
  myft()

T2 <- tidy(lme2)
T2$confint <- confint(lme2)
T2 <- T2 %>%
  mutate(beta = paste0(round(estimate, 2), " (", round(confint[, 1], 2), ", ", round(confint[, 2], 2), ")"))
T2 <- T2[,c(7,4,5)]
T2 <- T2 %>%
  rename_with(~ paste0("PHQ_Change_", .), )
print(T2)

lme2 <- lm(
  PHQ_Change ~ PHQ_FU1 + selfresilience + Age + Sex + Ethnic + BMI + Education,
  data = resilience_test)
summary(lme2)
autoReg(lme2) %>% 
  myft()

T3 <- tidy(lme2)
T3$confint <- confint(lme2)
T3 <- T3 %>%
  mutate(beta = paste0(round(estimate, 2), " (", round(confint[, 1], 2), ", ", round(confint[, 2], 2), ")"))
T3 <- T3[,c(7,4,5)]
T3 <- T3 %>%
  rename_with(~ paste0("PHQ_Change_adjusted_", .),)
T3 <- T3[-2,]
print(T3)

#转成长表格
tb_long <- resilience_test %>% pivot_longer(
  cols = c("PHQ_FU1","PHQ_FU2"),
  names_to = c(".value","FU"), # 列名中其他部分要用的
  names_sep = "_", # 列名中的分隔符
  values_drop_na = T
)
glimpse(tb_long)

p_PHQ <- ggline(tb_long, x = "FU", y = "PHQ", add = "mean_se",
                color = "selfresilience", palette = c("#E2C098","#A6CAA8","#85C3DC"), size = 1) +  # 设置线条粗细
  stat_compare_means(aes(group = selfresilience), label = "p.signif", 
                     label.y = c(5.1, 4.6), size = 4)  # 设置点的大小



##GAD--------
resilience_test <- resilience_group_R %>% # 创建新数据集新变量  
  transmute(eid, Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")), Age = age_BL, 
            Ethnic = factor(Ethnic_group, levels = c("0","1","2","3"), labels = c("1_White","2_Asian","3_Black","4_Other")),  
            Education = Education_year,
            BMI = BMI_BL,
            selfresilience = factor(self_resilience, levels = c(0,1,2), labels = c("1_Low","2_Medium","3_High")),
            data_FU1 = Mental_onine_date_FU1, data_FU2 = Mental_onine_date_FU2, PHQ_FU1 = PHQ9_Severity_FU1,
            PHQ_FU2 = `PHQ-9_FU2`,
            GAD_FU1 = General_Anxiety_Disorder_Severity_FU1,
            GAD_FU2 = General_Anxiety_Disorder_Severity_FU2)
resilience_test <- na.omit(resilience_test)

str(resilience_test) # 查看数据集结构

#resilience_test %>% tbl_summary(by = selfresilience, digits = list(Age ~ 0)) %>% add_p() %>%   add_overall()
resilience_test <- na.omit(resilience_test)

resilience_test$GAD_Change <- resilience_test$GAD_FU2 - resilience_test$GAD_FU1


lme2 <- lm(
  GAD_FU1 ~  selfresilience + Age + Sex + Ethnic + BMI + Education,
  data = resilience_test)
summary(lme2)
autoReg(lme2) %>% 
  myft()

T4 <- tidy(lme2)
T4$confint <- confint(lme2)
T4 <- T4 %>%
  mutate(beta = paste0(round(estimate, 2), " (", round(confint[, 1], 2), ", ", round(confint[, 2], 2), ")"))
T4 <- T4[,c(7,4,5)]
T4 <- T4 %>%
  rename_with(~ paste0("GAD_FU1_", .),)
print(T4)

lme2 <- lm(
  GAD_Change ~ selfresilience + Age + Sex + Ethnic + BMI + Education,
  data = resilience_test)
summary(lme2)
autoReg(lme2) %>% 
  myft()

T5 <- tidy(lme2)
T5$confint <- confint(lme2)
T5 <- T5 %>%
  mutate(beta = paste0(round(estimate, 2), " (", round(confint[, 1], 2), ", ", round(confint[, 2], 2), ")"))
T5 <- T5[,c(7,4,5)]
T5 <- T5 %>%
  rename_with(~ paste0("GAD_Change_", .),)
print(T5)

lme2 <- lm(
  GAD_Change ~ GAD_FU1 + selfresilience + Age + Sex + Ethnic + BMI + Education,
  data = resilience_test)
summary(lme2)
autoReg(lme2) %>% 
  myft()

T6 <- tidy(lme2)
T6$confint <- confint(lme2)
T6 <- T6 %>%
  mutate(beta = paste0(round(estimate, 2), " (", round(confint[, 1], 2), ", ", round(confint[, 2], 2), ")"))
T6 <- T6[,c(7,4,5)]
T6 <- T6 %>%
  rename_with(~ paste0("GAD_Change_adjusted_", .),)
T6 <- T6[-2,]
print(T6)


#转成长表格
tb_long <- resilience_test %>% pivot_longer(
  cols = c("GAD_FU1","GAD_FU2"),
  names_to = c(".value","FU"), # 列名中其他部分要用的
  names_sep = "_", # 列名中的分隔符
  values_drop_na = T
)
glimpse(tb_long)

p_GAD <- ggline(tb_long, x = "FU", y = "GAD", add = "mean_se",
                color = "selfresilience", palette = c("#E2C098","#A6CAA8","#85C3DC"), size = 1) +  # 设置线条粗细
  stat_compare_means(aes(group = selfresilience), label = "p.signif", 
                     label.y = c(3.5, 4), size = 4)  # 设置点的大小


#整理不匹配的结果-------
data_frames <- mget(ls(pattern = "^T\\d+"))

# 使用 bind_rows 进行拼接
Stastic_result <- bind_cols(data_frames)

# 打印结果
print(Stastic_result)

##做匹配的分析 ----------
##PHQ----
resilience_test <- resilience_group_R %>% # 创建新数据集新变量  
  transmute(eid, Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")), Age = age_BL, 
            Ethnic = factor(Ethnic_group, levels = c("0","1","2","3"), labels = c("1_White","2_Asian","3_Black","4_Other")),  
            Education = Education_year,
            BMI = BMI_BL,
            selfresilience = factor(self_resilience, levels = c(0,1,2), labels = c("1_Low","2_Medium","3_High")),
            data_FU1 = Mental_onine_date_FU1, data_FU2 = Mental_onine_date_FU2, PHQ_FU1 = PHQ9_Severity_FU1,
            PHQ_FU2 = `PHQ-9_FU2`,
            GAD_FU1 = General_Anxiety_Disorder_Severity_FU1,
            GAD_FU2 = General_Anxiety_Disorder_Severity_FU2) %>%
  filter(selfresilience != "2_Medium") %>% # 删除 selfresilience 为 "2_Medium" 的行
  mutate(selfresilience = droplevels(selfresilience))  # 重置因子水平

#resilience_test$selfresilience[resilience_test$selfresilience == "2_Medium"] <- NA
resilience_test <- na.omit(resilience_test)

unmatch_group <- resilience_test %>% tbl_summary(by = selfresilience,
                                                 type = list(Age ~ "continuous", Age ~ "continuous")  # 合并 type 选项
) %>%
  add_p() %>% 
  add_overall()
unmatch_group

# 将 gtsummary 对象转换为 tibble 或 data frame
unmatch_group_table <- as_tibble(unmatch_group)


str(resilience_test) # 查看数据集结构
#resilience_test %>% tbl_summary(by = selfresilience, digits = list(Age ~ 0)) %>% add_p() %>%   add_overall()
resilience_test <- na.omit(resilience_test)

resilience_test$PHQ_Change <- resilience_test$PHQ_FU2 - resilience_test$PHQ_FU1


m.out0 <- matchit(selfresilience ~ PHQ_FU1 + GAD_FU1 + Age + Sex + Ethnic + BMI + Education, data = resilience_test,
                  method = "nearest", distance = "glm", caliper = 0.0001)
summary(m.out0)

resilience_test <- match.data(m.out0)
resilience_test <- subset(resilience_test, weights == 1)

matched_data <- resilience_test %>%
  transmute(eid, Sex, Age, 
            Ethnic,  
            Education,
            BMI,
            selfresilience,PHQ_FU1,
            GAD_FU1)
matched_group <- matched_data %>% tbl_summary(by = selfresilience,
                                              type = list(Age ~ "continuous", Age ~ "continuous")  # 合并 type 选项
) %>%
  add_p() %>% 
  add_overall()
matched_group

# 将 gtsummary 对象转换为 tibble 或 data frame
matched_group_table <- as_tibble(matched_group)


lme2 <- lm(
  PHQ_FU1 ~  selfresilience + Age + Sex + Ethnic + BMI + Education,
  data = resilience_test)
summary(lme2)
autoReg(lme2) %>% 
  myft()

M1 <- tidy(lme2)
M1$confint <- confint(lme2)
M1 <- M1 %>%
  mutate(beta = paste0(round(estimate, 2), " (", round(confint[, 1], 2), ", ", round(confint[, 2], 2), ")"))
M1 <- M1[,c(1,7,4,5)]
M1 <- M1 %>%
  rename_with(~ paste0("PHQ_FU1_Matched_", .), -term)
print(M1)
#modelPlot(lme2,change.pointsize = T)

lme2 <- lm(
  PHQ_Change ~ selfresilience + Age + Sex + Ethnic + BMI + Education,
  data = resilience_test)
summary(lme2)
autoReg(lme2) %>% 
  myft()

M2 <- tidy(lme2)
M2$confint <- confint(lme2)
M2 <- M2 %>%
  mutate(beta = paste0(round(estimate, 2), " (", round(confint[, 1], 2), ", ", round(confint[, 2], 2), ")"))
M2 <- M2[,c(7,4,5)]
M2 <- M2 %>%
  rename_with(~ paste0("PHQ_Change_Matched_", .), )
print(M2)

lme2 <- lm(
  PHQ_Change ~ PHQ_FU1 + selfresilience + Age + Sex + Ethnic + BMI + Education,
  data = resilience_test)
summary(lme2)
autoReg(lme2) %>% 
  myft()

M3 <- tidy(lme2)
M3$confint <- confint(lme2)
M3 <- M3 %>%
  mutate(beta = paste0(round(estimate, 2), " (", round(confint[, 1], 2), ", ", round(confint[, 2], 2), ")"))
M3 <- M3[,c(7,4,5)]
M3 <- M3 %>%
  rename_with(~ paste0("PHQ_Change_Matched_adjusted_", .),)
M3 <- M3[-2,]
print(M3)

#转成长表格
tb_long <- resilience_test %>% pivot_longer(
  cols = c("PHQ_FU1","PHQ_FU2"),
  names_to = c(".value","FU"), # 列名中其他部分要用的
  names_sep = "_", # 列名中的分隔符
  values_drop_na = T
)
glimpse(tb_long)

p_PHQ2 <- ggline(tb_long, x = "FU", y = "PHQ", add = "mean_se",
                 color = "selfresilience", palette = c("#E2C098","#85C3DC"), size = 1) +  # 设置线条粗细
  stat_compare_means(aes(group = selfresilience), label = "p.signif", 
                     label.y = c(2.5, 2.6), size = 4)  # 设置点的大小


##GAD--------
resilience_test$GAD_Change <- resilience_test$GAD_FU2 - resilience_test$GAD_FU1

lme2 <- lm(
  GAD_FU1 ~  selfresilience + Age + Sex + Ethnic + BMI + Education,
  data = resilience_test)
summary(lme2)
autoReg(lme2) %>% 
  myft()

M4 <- tidy(lme2)
M4$confint <- confint(lme2)
M4 <- M4 %>%
  mutate(beta = paste0(round(estimate, 2), " (", round(confint[, 1], 2), ", ", round(confint[, 2], 2), ")"))
M4 <- M4[,c(7,4,5)]
M4 <- M4 %>%
  rename_with(~ paste0("GAD_FU1_Matched_", .),)
print(M4)

lme2 <- lm(
  GAD_Change ~ selfresilience + Age + Sex + Ethnic + BMI + Education,
  
  data = resilience_test)
summary(lme2)
autoReg(lme2) %>% 
  myft()

M5 <- tidy(lme2)
M5$confint <- confint(lme2)
M5 <- M5 %>%
  mutate(beta = paste0(round(estimate, 2), " (", round(confint[, 1], 2), ", ", round(confint[, 2], 2), ")"))
M5 <- M5[,c(7,4,5)]
M5 <- M5 %>%
  rename_with(~ paste0("GAD_Change_Matched_", .),)
print(M5)

lme2 <- lm(
  GAD_Change ~ GAD_FU1 + selfresilience + Age + Sex + Ethnic + BMI + Education,
  data = resilience_test)
summary(lme2)
autoReg(lme2) %>% 
  myft()

M6 <- tidy(lme2)
M6$confint <- confint(lme2)
M6 <- M6 %>%
  mutate(beta = paste0(round(estimate, 2), " (", round(confint[, 1], 2), ", ", round(confint[, 2], 2), ")"))
M6 <- M6[,c(7,4,5)]
M6 <- M6 %>%
  rename_with(~ paste0("GAD_Change_Matched_adjusted_", .),)
M6 <- M6[-2,]
print(M6)

#转成长表格
tb_long <- resilience_test %>% pivot_longer(
  cols = c("GAD_FU1","GAD_FU2"),
  names_to = c(".value","FU"), # 列名中其他部分要用的
  names_sep = "_", # 列名中的分隔符
  values_drop_na = T
)
glimpse(tb_long)

p_GAD2 <- ggline(tb_long, x = "FU", y = "GAD", add = "mean_se",
                 color = "selfresilience", palette = c("#E2C098","#85C3DC"), size = 1) +  # 设置线条粗细
  stat_compare_means(aes(group = selfresilience), label = "p.signif", 
                     label.y = c(1.1, 2.3), size = 4)  # 设置点的大小



#整理匹配后的结果-------
Match_frames <- mget(ls(pattern = "^M\\d+"))

# 使用 bind_rows 进行拼接
match_Stastic_result <- bind_cols(Match_frames)

# 打印结果
print(match_Stastic_result)

##合并6个图----

# 合并四个图
combined_plot <- grid.arrange(p_PHQ, p_GAD, p_PHQ2, p_GAD2, ncol = 2)

# 显示合并后的图
print(combined_plot)