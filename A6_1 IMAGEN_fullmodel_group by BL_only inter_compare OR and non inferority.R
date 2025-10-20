#deleted work satisfication

library(readr)
library(data.table)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(R.matlab)
library(nlme)                    # Fit Gaussian linear and nonlinear mixed-effects models
library(lme4)    
library(gridExtra)
library(grid)
library(gptstudio)
library(gtsummary)
library(sjPlot) 
library(sjmisc) 
library(janitor)
library(rio)
#UKB预测（完整的包括除人格外其他变量的模型）--------------------
#整理IMAGEN数据----

IMAGEN_BL_dat <- read_csv("/public/mig_old_storage/home1/ISTBI_data/IMAGEN_New_Preprocessed/Behaviours/BL/IMAGEN_DAWBA_BL.csv")
IMAGEN_BL_dat <- IMAGEN_BL_dat[,c("PSC2","age","sconduct","shyper","semotion","speer")]
colnames(IMAGEN_BL_dat) <- c("eid","BL_age","CD_BL","HK_BL","ED_BL","PP_BL")
IMAGEN_BL_dat$External_BL <- IMAGEN_BL_dat$CD_BL + IMAGEN_BL_dat$HK_BL
IMAGEN_BL_dat$Internal_BL <- IMAGEN_BL_dat$ED_BL + IMAGEN_BL_dat$PP_BL


IMAGEN_FU1_dat <- read_csv("/public/home/yangzy/Documents/File/R_script/BSR_analysis/IMAGEN_dawba_FU1.csv")
IMAGEN_FU1_dat <- IMAGEN_FU1_dat[,c("PSC2","sconduct","shyper","semotion","speer")]
colnames(IMAGEN_FU1_dat) <- c("eid","CD_FU1","HK_FU1","ED_FU1","PP_FU1")
IMAGEN_FU1_dat$External_FU1 <- IMAGEN_FU1_dat$CD_FU1 + IMAGEN_FU1_dat$HK_FU1
IMAGEN_FU1_dat$Internal_FU1 <- IMAGEN_FU1_dat$ED_FU1 + IMAGEN_FU1_dat$PP_FU1


IMAGEN_FU2_dat <- read_csv("/public/mig_old_storage/home1/ISTBI_data/IMAGEN_New_Preprocessed/First_Level/EFT_Decoder/Behaviour/FU2_Behaviour/IMAGEN_FU2_SDQ.csv")
IMAGEN_FU2_dat <- IMAGEN_FU2_dat[,c("SubID","Conduct","Hyperactivity","Emotional","Peer_Problem")]
colnames(IMAGEN_FU2_dat) <- c("eid","CD_FU2","HK_FU2","ED_FU2","PP_FU2")
IMAGEN_FU2_dat$External_FU2 <- IMAGEN_FU2_dat$CD_FU2 + IMAGEN_FU2_dat$HK_FU2
IMAGEN_FU2_dat$Internal_FU2 <- IMAGEN_FU2_dat$ED_FU2 + IMAGEN_FU2_dat$PP_FU2


IMAGEN_FU3_dat <- read_csv("/public/mig_old_storage/home1/ISTBI_data/IMAGEN_New_Preprocessed/First_Level/EFT_Decoder/Behaviour/FU3_Behaviour/IMAGEN_FU3_SDQ.csv")
IMAGEN_FU3_dat <- IMAGEN_FU3_dat[,c("SubID","Conduct","Hyperactivity","Emotional","Peer_Problem")]
colnames(IMAGEN_FU3_dat) <- c("eid","CD_FU3","HK_FU3","ED_FU3","PP_FU3")
IMAGEN_FU3_dat$External_FU3 <- IMAGEN_FU3_dat$CD_FU3 + IMAGEN_FU3_dat$HK_FU3
IMAGEN_FU3_dat$Internal_FU3 <- IMAGEN_FU3_dat$ED_FU3 + IMAGEN_FU3_dat$PP_FU3



IMAGEN_BL_dat$eid <- sprintf("%012d", as.numeric(IMAGEN_BL_dat$eid))
IMAGEN_FU1_dat$eid <- sprintf("%012d", as.numeric(IMAGEN_FU1_dat$eid))
IMAGEN_FU2_dat$eid <- sprintf("%012d", as.numeric(IMAGEN_FU2_dat$eid))
IMAGEN_FU3_dat$eid <- sprintf("%012d", as.numeric(IMAGEN_FU3_dat$eid))
IMAGEN_behave_dat <- merge(IMAGEN_BL_dat, IMAGEN_FU1_dat, by = "eid", all.x = TRUE)
IMAGEN_behave_dat <- merge(IMAGEN_behave_dat, IMAGEN_FU2_dat, by = "eid", all.x = TRUE)
IMAGEN_behave_dat <- merge(IMAGEN_behave_dat, IMAGEN_FU3_dat, by = "eid", all.x = TRUE)



IMAGEN_IMGN_CTQ_CHILD_FU2_IMAGEN_DIGEST <- read_csv("/public/mig_old_storage/home1/ISTBI_data/IMAGEN_New_Preprocessed/Behaviours/FU2/IMAGEN-IMGN_CTQ_CHILD_FU2-IMAGEN_DIGEST.csv")
IMAGEN_CTQ_dat <- IMAGEN_IMGN_CTQ_CHILD_FU2_IMAGEN_DIGEST %>%
  transmute(eid = `User code`,
            Family_Relationship_Satisfaction_BL = CTQ_22
  )
IMAGEN_CTQ_dat[IMAGEN_CTQ_dat == -1] <- NA
IMAGEN_CTQ_dat$eid <- sub("-I$", "", IMAGEN_CTQ_dat$eid)



IMAGEN_IMGN_NEO_FFI_CHILD_RC5_IMAGEN_SURVEY_DIGEST <- read_csv("/public/mig_old_storage/home1/ISTBI_data/IMAGEN_New_Preprocessed/Behaviours/BL/IMAGEN-IMGN_NEO_FFI_CHILD_RC5-IMAGEN_SURVEY_DIGEST.csv")
#IMAGEN_IMGN_NEO_FFI_CHILD_RC5_IMAGEN_SURVEY_DIGEST <- read_csv("/public/mig_old_storage/home1/ISTBI_data/IMAGEN_New_Preprocessed/Behaviours/FU1/IMAGEN-IMGN_NEO_FFI_CHILD_FU_RC5-IMAGEN_SURVEY_DIGEST.csv")
#IMAGEN_BIG5_dat <- IMAGEN_IMGN_NEO_FFI_CHILD_RC5_IMAGEN_SURVEY_DIGEST[,c(1,75,76,72,73,74)]
IMAGEN_BIG5_dat <- IMAGEN_IMGN_NEO_FFI_CHILD_RC5_IMAGEN_SURVEY_DIGEST %>%
  transmute(eid = `User code`, Big5_warmth = agre_mean, Big5_diligence = cons_mean, Big5_nervousness = neur_mean,
            Big5_sociability = extr_mean, Big5_curiosity = open_mean,
            Loneliness_BL = neoffi16R)
#colnames(IMAGEN_BIG5_dat) <- c("eid","Big5_warmth","Big5_diligence","Big5_nervousness","Big5_sociability","Big5_curiosity")
IMAGEN_BIG5_dat$eid <- sub("-C$", "", IMAGEN_BIG5_dat$eid)


IMAGEN_DAWBA_BL <- read_csv("/public/mig_old_storage/home1/ISTBI_data/IMAGEN_New_Preprocessed/Behaviours/BL/IMAGEN_DAWBA_BL.csv")
#temp_dat <- IMAGEN_DAWBA_BL$
#temp_dat <- as.data.frame(temp_dat)
#IMAGEN_DAWBA_dat <- IMAGEN_DAWBA_BL[,c(1,3,622)]
IMAGEN_DAWBA_dat <- IMAGEN_DAWBA_BL %>%
  transmute(eid = PSC2, gender = gender, 
            HH_Income_BL = p1fsses,
            Financial_Difficulties = p1fs3,
            Been_In_Confiding_Relationship_FU1 = p1flq13,
            Belittlement_Adult_FU1 = p1flq19,
            Social_Able_Confide_BL = sfriend,
            MET_Minutes_Per_Week_Vigorous_Activity_BL = sn2a,
            Work_Job_Satisfaction_BL = sn2k,
            Friendships_Satisfaction_BL = sn2b
            
  )

#colnames(IMAGEN_DAWBA_dat) <- c("eid","gender","HH_Income_BL")
#IMAGEN_DAWBA_dat$HH_Income_BL <- 8 - IMAGEN_DAWBA_dat$HH_Income_BL
IMAGEN_DAWBA_dat$eid <- sprintf("%012d", as.numeric(IMAGEN_DAWBA_dat$eid))
IMAGEN_DAWBA_dat$gender[IMAGEN_DAWBA_dat$gender == 2] <- 0


IMAGEN_IMGN_PBQ_RC1_BASIC_DIGEST <- read_csv("/public/mig_old_storage/home1/ISTBI_data/IMAGEN_New_Preprocessed/Behaviours/BL/IMAGEN-IMGN_PBQ_RC1-BASIC_DIGEST.csv")
IMAGEN_PBQ_dat <- IMAGEN_IMGN_PBQ_RC1_BASIC_DIGEST %>%
  transmute(eid = `User code`, Breastfed_Baby_BL = pbq_23)
IMAGEN_PBQ_dat[IMAGEN_PBQ_dat == -1] <- NA
IMAGEN_PBQ_dat[IMAGEN_PBQ_dat == -2] <- NA
colnames(IMAGEN_PBQ_dat) <- c("eid","Breastfed_Baby_BL")
IMAGEN_PBQ_dat$eid <- sub("-P$", "", IMAGEN_PBQ_dat$eid)



IMAGEN_test_dat <- merge(IMAGEN_DAWBA_dat, IMAGEN_PBQ_dat, by='eid', all.x = TRUE)
IMAGEN_test_dat <- merge(IMAGEN_test_dat, IMAGEN_CTQ_dat, by='eid', all.x = TRUE)
IMAGEN_test_dat <- merge(IMAGEN_test_dat, IMAGEN_BIG5_dat, by='eid', all.x = TRUE)


#IMAGEN_test_dat$HH_Num_Vehicle_BL <- NA
#IMAGEN_test_dat$HH_Own_Rent_BL <- NA
#IMAGEN_test_dat$Comp_Body_Size_Age_10_BL <- NA
#IMAGEN_test_dat$Comp_Height_Size_Age_10_BL <- NA
#IMAGEN_test_dat$Adopted_Child_BL <- NA

#IMAGEN_test_dat <- IMAGEN_test_dat[,c(1,2,16,3,17,4,18:20,5:15)]
glimpse(IMAGEN_test_dat)
#IMAGEN_test_dat[,c(3,4,5,7,8,11:20)] <- scale(IMAGEN_test_dat[,c(3,4,5,7,8,11:20)])
#控制缺失值
#IMAGEN_test_dat$percentage_NA <- NA
#na_count <- rowSums(is.na(IMAGEN_test_dat[, 2:20]))
#IMAGEN_test_dat$percentage_NA <- ifelse(0 < (na_count / (20 - 2 + 1)) & (na_count / (20 - 2 + 1) <= 0.5), "1", IMAGEN_test_dat$percentage_NA)
#IMAGEN_test1_dat <- IMAGEN_test_dat[IMAGEN_test_dat[, 21] == 1, ]
#IMAGEN_test1_dat <- IMAGEN_test_dat[!is.na(IMAGEN_test_dat[, 21]), ]
#进行lightGBM-----
## 用UKB的数据进行训练-----
## 导入基础数据--------
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
            gender,
            HH_Income_BL,
            Been_In_Confiding_Relationship_FU1,
            Social_Able_Confide_BL,
            Loneliness_BL,
            MET_Minutes_Per_Week_Vigorous_Activity_BL,
            Big5_warmth, Big5_diligence, Big5_nervousness, Big5_curiosity, Big5_sociability,
            #Work_Job_Satisfaction_BL,
            Family_Relationship_Satisfaction_BL,
            Friendships_Satisfaction_BL,
            self_resilience
  )


IMAGEN_test1_dat <- IMAGEN_test_dat %>% # 创建新数据集新变量  
  transmute(eid, 
            gender,
            HH_Income_BL,
            Been_In_Confiding_Relationship_FU1,
            Social_Able_Confide_BL,
            Loneliness_BL,
            MET_Minutes_Per_Week_Vigorous_Activity_BL,
            Big5_warmth, Big5_diligence, Big5_nervousness, Big5_curiosity, Big5_sociability,
            #Work_Job_Satisfaction_BL,
            Family_Relationship_Satisfaction_BL,
            Friendships_Satisfaction_BL,
  )

BRS_train_dat <- na.omit(BRS_train_dat)
IMAGEN_test1_dat <- na.omit(IMAGEN_test1_dat)
BRS_train_dat[,c(2:15)] <- scale(BRS_train_dat[,c(2:15)])
IMAGEN_test1_dat[,c(2:14)] <- scale(IMAGEN_test1_dat[,c(2:14)])

#BRS_train_dat$gender <- factor(BRS_train_dat$gender)
glimpse(BRS_train_dat)

meats_train <- BRS_train_dat
#meats_train <- train_val_dat[1:25000,]
#meats_val <- train_val_dat[25001:35726,]
meats_test <- IMAGEN_test1_dat
# Split the column names in X and Y
X_colnames <- colnames(BRS_train_dat)[c(2:14)]
Y_colnames <- colnames(BRS_train_dat)[15]
# Split each train, val, test into two matrices
X_train_matrix <- as.matrix(meats_train[X_colnames])
Y_train_matrix <- as.matrix(meats_train[Y_colnames])
#X_val_matrix <- as.matrix(meats_val[X_colnames])
#Y_val_matrix <- as.matrix(meats_val[Y_colnames])
X_test_matrix <- as.matrix(meats_test[X_colnames])
#Y_test_matrix <- as.matrix(meats_test[Y_colnames])

# fit the full model on train data
my_plsr <- plsr(Y_train_matrix ~ X_train_matrix, ncomp=13, scale = TRUE, validation='CV')  

#通过RMSEP功能分析模型使用不同个数的组分--------------------
plot(RMSEP(my_plsr))  

#每个变量的回归系数(每个变量对应一个波长)----------
plot(my_plsr, plottype = "coef", ncomp=c(1:13), legendpos = "bottomleft")  
#遍历1-50个组分组合的情况，并且使用validation data 去帮助筛选最优的组分数目--------------------
# Loop through possible values for n_comp to optimize R2 on validation data
ncomp.onesigma <- selectNcomp(my_plsr, method = "onesigma", plot = TRUE)
ncomp.permute <- selectNcomp(my_plsr, method = "randomization", plot = TRUE)



#验证我们是否也在测试数据集上是否也能得到较好的结果----------------------
# Predict on test for having a final R2 estimate
best_ncomp <- ncomp.permute
best_model <- plsr(Y_train_matrix ~ X_train_matrix, ncomp=best_ncomp, scale = TRUE, validation='CV')  
test_predictions <- as.matrix(data.frame(predict(best_model, ncomp=best_ncomp, X_test_matrix)))  



#将IMAGEN的数据提出来
predicited_IMAGEN_R_dat <- IMAGEN_test1_dat[,c(1)]
predicited_IMAGEN_R_dat <- as.data.frame(predicited_IMAGEN_R_dat)
predicited_IMAGEN_R_dat <- cbind(predicited_IMAGEN_R_dat,test_predictions)
colnames(predicited_IMAGEN_R_dat) <- c("eid","pred_resilience_IMAGEN")


#整理IMAGEN创伤数据-------------------------
##导入14岁（BL) 的LEQ\DAWBA\BULLY--------------
#LEQ_BL
library(readr)
LEQ_BL <- read_csv("/public/mig_old_storage/home1/ISTBI_data/IMAGEN_New_Preprocessed/Behaviours/BL/IMAGEN-IMGN_LEQ_RC5-BASIC_DIGEST.csv")
colnames(LEQ_BL)[1] <- "eid"
# 删除第一列值中的最后两个字符
LEQ_BL$eid <- substr(LEQ_BL$eid, 1, nchar(LEQ_BL$eid) - 2)

#CTQ_FU2
IMAGEN_FU2_CTQ <- read_csv("/public/mig_old_storage/home1/ISTBI_data/IMAGEN_New_Preprocessed/Behaviours/FU2/IMAGEN-IMGN_CTQ_CHILD_FU2-IMAGEN_DIGEST.csv")
colnames(IMAGEN_FU2_CTQ)[1] <- "eid"
# 删除第一列值中的最后两个字符
IMAGEN_FU2_CTQ$eid <- substr(IMAGEN_FU2_CTQ$eid, 1, nchar(IMAGEN_FU2_CTQ$eid) - 2)

#第2，5，7，13，19，26，28题进行反向处理
IMAGEN_FU2_CTQ <- IMAGEN_FU2_CTQ %>%
  mutate_at(vars(8:35), ~ replace(., . == -1, NA))
IMAGEN_FU2_CTQ <- na.omit(IMAGEN_FU2_CTQ)

IMAGEN_FU2_CTQ <- IMAGEN_FU2_CTQ %>%
  mutate(across(c(CTQ_2, CTQ_5, CTQ_7, CTQ_13, CTQ_19, CTQ_26, CTQ_28), ~ 4 - .))

#所有的值加一，变成5点评分
IMAGEN_FU2_CTQ <- IMAGEN_FU2_CTQ %>%
  mutate(across(8:35, ~ . + 1))

IMAGEN_FU2_CTQ$Emo_abuse <- rowSums(IMAGEN_FU2_CTQ[, c("CTQ_3", "CTQ_8", "CTQ_14", "CTQ_18", "CTQ_25")], na.rm = TRUE)
IMAGEN_FU2_CTQ$Physi_abuse <- rowSums(IMAGEN_FU2_CTQ[, c("CTQ_9", "CTQ_11", "CTQ_12", "CTQ_15", "CTQ_17")], na.rm = TRUE)
IMAGEN_FU2_CTQ$Sex_abuse <- rowSums(IMAGEN_FU2_CTQ[, c("CTQ_20", "CTQ_21", "CTQ_23", "CTQ_24", "CTQ_27")], na.rm = TRUE)
IMAGEN_FU2_CTQ$Emo_neglect <- rowSums(IMAGEN_FU2_CTQ[, c("CTQ_5", "CTQ_7", "CTQ_13", "CTQ_19", "CTQ_28")], na.rm = TRUE)
IMAGEN_FU2_CTQ$Physi_neglect <- rowSums(IMAGEN_FU2_CTQ[, c("CTQ_1", "CTQ_2", "CTQ_4", "CTQ_6", "CTQ_26")], na.rm = TRUE)


IMAGEN_FU2_CTQ <- IMAGEN_FU2_CTQ %>%
  mutate(Emo_abuse_ever = ifelse(Emo_abuse >= 13, 1, 0))
IMAGEN_FU2_CTQ <- IMAGEN_FU2_CTQ %>%
  mutate(Physi_abuse_ever = ifelse(Physi_abuse >= 10, 1, 0))
IMAGEN_FU2_CTQ <- IMAGEN_FU2_CTQ %>%
  mutate(Sex_abuse_ever = ifelse(Sex_abuse >= 8, 1, 0))
IMAGEN_FU2_CTQ <- IMAGEN_FU2_CTQ %>%
  mutate(Emo_neglect_ever = ifelse(Emo_neglect >= 15, 1, 0))
IMAGEN_FU2_CTQ <- IMAGEN_FU2_CTQ %>%
  mutate(Physi_neglect_ever = ifelse(Physi_neglect >= 10, 1, 0))


#计算童年创伤的数量
IMAGEN_FU2_CTQ$Trauma_Num<- rowSums(IMAGEN_FU2_CTQ[, c("Emo_abuse_ever","Physi_abuse_ever", "Sex_abuse_ever", "Emo_neglect_ever", "Physi_neglect_ever")])
# 计算新列trauma2_ever
IMAGEN_FU2_CTQ <- IMAGEN_FU2_CTQ %>%
  mutate(Child_maltreatment = ifelse(rowSums(IMAGEN_FU2_CTQ[, c("Emo_abuse_ever","Physi_abuse_ever", "Sex_abuse_ever", "Emo_neglect_ever", "Physi_neglect_ever")]) >= 1, 1, 0))


#统计创伤人数的比率
# 计算每列中值为1的比例
proportions <- IMAGEN_FU2_CTQ %>%
  summarise(Emo_abuse_prop = mean(Emo_abuse_ever),
            Physi_abuse_prop = mean(Physi_abuse_ever),
            Sex_abuse_prop = mean(Sex_abuse_ever),
            Emo_neglect_prop = mean(Emo_neglect_ever),
            Physi_neglect_prop = mean(Physi_neglect_ever),
            Child_maltreatment_prop = mean(Child_maltreatment))

#DAWBA的创伤数据
IMAGEN_BL_DAWBA<-read_csv("/public/mig_old_storage/home1/ISTBI_data/IMAGEN_New_Preprocessed/Behaviours/BL/IMAGEN_DAWBA_BL.csv")
colnames(IMAGEN_BL_DAWBA)[1] <- "eid"
IMAGEN_BL_DAWBA$eid <- sprintf("%012d", as.numeric(IMAGEN_BL_DAWBA$eid))

#ESPAD中的BULLY数据
IMAGEN_BL_ESPAD <- read_csv("/public/mig_old_storage/home1/ISTBI_data/IMAGEN_New_Preprocessed/Behaviours/BL/IMAGEN-IMGN_ESPAD_CHILD_RC5-IMAGEN_DIGEST.csv")
colnames(IMAGEN_BL_ESPAD)[1] <- "eid"
IMAGEN_BL_ESPAD$eid <- substr(IMAGEN_BL_ESPAD$eid, 1, nchar(IMAGEN_BL_ESPAD$eid) - 2)

#合并创伤数据进行处理----------------------
IMAGEN_trauma <- merge(LEQ_BL, IMAGEN_FU2_CTQ, by = "eid", all.x =T)
IMAGEN_trauma <- merge(IMAGEN_trauma, IMAGEN_BL_DAWBA, by = "eid", all.x =T)
IMAGEN_trauma <- merge(IMAGEN_trauma, IMAGEN_BL_ESPAD, by = "eid", all.x =T)


IMAGEN_trauma_selected <- IMAGEN_trauma %>%
  transmute(eid, Gender = gender, Age_BL = age,
            leq_22_ever, leq_24_ever, leq_39_ever,p1flq15, p1flq16, p1flq19,
            p1fs16, p1fs13, p1fs10, p1fs11, p1flq17,
            leq_02_ever, leq_08_ever,leq_37_ever,
            leq_01_ever, leq_34_ever, p1parsit1, p1parsit2c, p1parsit2g, p1parsit3c, p1parsit3g,
            Emo_abuse_ever, Physi_abuse_ever, Sex_abuse_ever, Emo_neglect_ever, Physi_neglect_ever,
            Child_maltreatment,
            bully01, bully02, bully03, bully04)

IMAGEN_trauma_selected <- IMAGEN_trauma_selected %>%
  mutate(p1parsit3c = ifelse(is.na(p1parsit3c), 0, p1parsit3c),
         p1parsit3g = ifelse(is.na(p1parsit3g), 0, p1parsit3g))

IMAGEN_trauma_selected <- na.omit(IMAGEN_trauma_selected)

IMAGEN_trauma_selected <- IMAGEN_trauma_selected %>%
  mutate(Family_Money = ifelse(leq_22_ever == 1, 1, 0),
         Domestic_Violence = ifelse(leq_24_ever == 1, 1, 0),
         Parent_Alcohol_Abuse = ifelse(leq_39_ever == 1, 1, 0),
         Harsh_Discipline = ifelse(p1flq15 >= 2 | p1flq16 >=2 , 1, 0),
         Scapegoating = ifelse(p1flq19 >= 2, 1, 0),
         Parent_Gambling = ifelse(p1fs16 == 2, 1, 0),
         Parent_Mental_Health = ifelse(p1fs13 == 2, 1, 0),
         Poor_Marital_Relations = ifelse(p1fs10 == 2 | p1fs11 ==2, 1, 0),
         Inadequate_Supervision = ifelse(p1flq17 == 0, 1, 0),
         Parental_Overinvolvement = ifelse(p1flq17 == 0, 3, 0),
         Family_Accident_Illness = ifelse(leq_02_ever == 1, 1, 0),
         Death_in_Family = ifelse(leq_08_ever == 1, 1, 0),
         Serious_Accident_Illness = ifelse(leq_37_ever == 1, 1, 0),
         Parents_Divorced = ifelse(leq_01_ever == 1, 1, 0),
         Parent_Remarried = ifelse(leq_34_ever == 1, 1, 0),
         Single_Parent_Family = ifelse(p1parsit1 <= 3, 1, 0),
         Stepparent_Household = ifelse(p1parsit2c == 1 | p1parsit2g == 1 | p1parsit3c == 1 | p1parsit3g == 1, 1, 0),
         Living_without_Parents = ifelse(p1parsit1 == 4, 1, 0),
         Physical_Abuse = ifelse(Physi_abuse_ever >= 1, 1, 0),
         Physical_Neglect = ifelse(Physi_neglect_ever >= 1, 1, 0),
         Sex_Abuse = ifelse(Sex_abuse_ever >= 1, 1, 0),
         Emotional_Abuse = ifelse(Emo_abuse_ever >= 1, 1, 0),
         Emotional_Neglect = ifelse(Emo_neglect_ever >= 1, 1, 0),
         General_Bullying = ifelse(bully01 >= 3, 1, 0),
         Verbal_Bullying = ifelse(bully02 >= 3, 1, 0),
         Psychological_Bullying = ifelse(bully03 >= 3, 1, 0),
         Physical_Bullying = ifelse(bully04 >= 3, 1, 0)
  )

#计算类别
IMAGEN_trauma_selected <- IMAGEN_trauma_selected %>%
  mutate(Family_Dysfunction = ifelse(
    Family_Money + Domestic_Violence + Parent_Alcohol_Abuse + Harsh_Discipline + Scapegoating +
      Parent_Gambling + Parent_Mental_Health + Poor_Marital_Relations + Inadequate_Supervision +
      Parental_Overinvolvement >= 2, 1, 0
  ),
  Accident = ifelse(
    Family_Accident_Illness + Death_in_Family + Serious_Accident_Illness >= 1, 1, 0
  ),
  Unstable_Family_Structure = ifelse(
    Parents_Divorced + Parent_Remarried + Single_Parent_Family + Stepparent_Household + Living_without_Parents >= 2, 1, 0
  ),
  Child_Maltreatment = Child_maltreatment, 
  Peer_Victimization = ifelse(
    General_Bullying + Verbal_Bullying + Psychological_Bullying + Physical_Bullying >= 1, 1, 0
  )
  )

IMAGEN_trauma_selected <- IMAGEN_trauma_selected %>%
  mutate(Trauma_Cate_Num = Family_Dysfunction + Accident + Unstable_Family_Structure + Child_Maltreatment + Peer_Victimization)

library(ggplot2)

# 创建直方图
ggplot(IMAGEN_trauma_selected, aes(x = Trauma_Cate_Num)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Trauma Categories",
       x = "Number of Trauma Categories",
       y = "Frequency") +
  theme_minimal()

#整理BL DAWBA----------------------------------
IMAGEN_BL_DAWBA<-read_csv("/public/mig_old_storage/home1/ISTBI_data/IMAGEN_New_Preprocessed/Behaviours/BL/IMAGEN_DAWBA_BL.csv")
colnames(IMAGEN_BL_DAWBA)[1] <- "eid"
IMAGEN_BL_DAWBA$eid <- sprintf("%012d", as.numeric(IMAGEN_BL_DAWBA$eid))

IMAGEN_BL_DAWBA <- IMAGEN_BL_DAWBA %>%
  transmute(eid, Age_BL = age,
            AnyDisorder_BL = dcany, Emotional_BL = dcemot, Spcific_Phobia_BL = dcspph,
            Social_Phobia_BL = dcsoph, Panic_Disorder_BL = dcpanic, PTSD_BL = dcptsd,
            OCD_BL = dcocd, GAD_BL = dcgena, MDD_BL = dcmadep, Mania_BL = dcmania,
            Social_BL = dcanyso, ADHD_BL = dcanyhk, CD_BL = dcanycd, Psychosis_BL = dcpsych,
            Other_BL = dcother
  )
IMAGEN_BL_DAWBA[, -c(1:2)] <- lapply(IMAGEN_BL_DAWBA[, -c(1:2)], function(x) replace(x, x == 2, 1))

IMAGEN_BL_DAWBA <- IMAGEN_BL_DAWBA %>%
  mutate(Internalizing_BL = Emotional_BL + Social_BL) %>%
  mutate(Internalizing_BL = ifelse(Internalizing_BL > 1, 1, Internalizing_BL)) %>%
  mutate(Externalizing_BL = ADHD_BL + CD_BL) %>%
  mutate(Externalizing_BL = ifelse(Externalizing_BL > 1, 1, Externalizing_BL))

trauma_dat <- merge(IMAGEN_trauma_selected, IMAGEN_BL_DAWBA, by = "eid", all.x = T)
trauma_dat <- na.omit(trauma_dat)




#整理FU1DAWBA----------------------------------
IMAGEN_FU1_DAWBA<-readr::read_tsv("/public/mig_old_storage/home1/ISTBI_data/IMAGEN_New_Preprocessed/Behaviours/FU1/IMAGEN_dawba_FU1.tsv")
colnames(IMAGEN_FU1_DAWBA)[1] <- "eid"

IMAGEN_FU1_DAWBA <- IMAGEN_FU1_DAWBA %>%
  transmute(eid, Age_FU1 = age,
            AnyDisorder_FU1 = dcany, Emotional_FU1 = dcemot, Spcific_Phobia_FU1 = dcspph,
            Social_Phobia_FU1 = dcsoph, Panic_Disorder_FU1 = dcpanic, PTSD_FU1 = dcptsd,
            OCD_FU1 = dcocd, GAD_FU1 = dcgena, MDD_FU1 = dcmadep, Mania_FU1 = dcmania,
            Social_FU1 = dcanyso, ADHD_FU1 = dcanyhk, CD_FU1 = dcanycd, Psychosis_FU1 = dcpsych,
            Other_FU1 = dcother
  )
IMAGEN_FU1_DAWBA[, -c(1:2)] <- lapply(IMAGEN_FU1_DAWBA[, -c(1:2)], function(x) replace(x, x == 2, 1))

IMAGEN_FU1_DAWBA <- IMAGEN_FU1_DAWBA %>%
  mutate(Internalizing_FU1 = Emotional_FU1 + Social_FU1) %>%
  mutate(Internalizing_FU1 = ifelse(Internalizing_FU1 > 1, 1, Internalizing_FU1)) %>%
  mutate(Externalizing_FU1 = ADHD_FU1 + CD_FU1) %>%
  mutate(Externalizing_FU1 = ifelse(Externalizing_FU1 > 1, 1, Externalizing_FU1))

#IMAGEN_FU1_DAWBA <- IMAGEN_FU1_DAWBA %>%
#  mutate(Diag_resilience = ifelse(AnyDisorder_FU1 == 0, 1, 0))

#1代表resilience，2代表健康被试，3代表疾病被试

trauma_dat <- merge(trauma_dat, IMAGEN_FU1_DAWBA, by = "eid", all.x = T)
trauma_dat <- na.omit(trauma_dat)

#合并19岁的DAWBA---------------------------------
library(readr)
IMAGEN_FU2_DAWBA <- readr::read_tsv("/public/mig_old_storage/home1/ISTBI_data/IMAGEN_New_Preprocessed/Behaviours/FU2/IMAGEN_dawba_FU2.tsv")
colnames(IMAGEN_FU2_DAWBA)[1] <- "eid"
IMAGEN_FU2_DAWBA$eid <- sprintf("%012d", as.numeric(IMAGEN_FU2_DAWBA$eid))

IMAGEN_FU2_DAWBA <- IMAGEN_FU2_DAWBA %>%
  transmute(eid, Age_FU2 = age,
            AnyDisorder_FU2 = dcany, Emotional_FU2 = dcemot, Spcific_Phobia_FU2 = dcspph,
            Social_Phobia_FU2 = dcsoph, Panic_Disorder_FU2 = dcpanic, PTSD_FU2 = dcptsd,
            OCD_FU2 = dcocd, GAD_FU2 = dcgena, MDD_FU2 = dcmadep, Mania_FU2 = dcmania,
            Social_FU2 = dcanyso, ADHD_FU2 = dcanyhk, CD_FU2 = dcanycd, Psychosis_FU2 = dcpsych,
            Other_FU2 = dcother
  )


IMAGEN_FU2_DAWBA[, -c(1:2)] <- lapply(IMAGEN_FU2_DAWBA[, -c(1:2)], function(x) replace(x, x == 2, 1))

IMAGEN_FU2_DAWBA <- IMAGEN_FU2_DAWBA %>%
  mutate(Internalizing_FU2 = Emotional_FU2 + Social_FU2) %>%
  mutate(Internalizing_FU2 = ifelse(Internalizing_FU2 > 1, 1, Internalizing_FU2)) %>%
  mutate(Externalizing_FU2 = ADHD_FU2 + CD_FU2) %>%
  mutate(Externalizing_FU2 = ifelse(Externalizing_FU2 > 1, 1, Externalizing_FU2))


trauma_dat <- merge(trauma_dat, IMAGEN_FU2_DAWBA, by = "eid", all = F)


#合并22岁的DAWBA和SDQ-------------------------
library(readr)
IMAGEN_FU3_DAWBA <- readr::read_tsv("/public/mig_old_storage/home1/ISTBI_data/IMAGEN_New_Preprocessed/Behaviours/FU3/IMAGEN_dawba_FU3.tsv")
colnames(IMAGEN_FU3_DAWBA)[1] <- "eid"
IMAGEN_FU3_DAWBA$eid <- sprintf("%012d", as.numeric(IMAGEN_FU3_DAWBA$eid))

IMAGEN_FU3_DAWBA <- IMAGEN_FU3_DAWBA %>%
  transmute(eid, Age_FU3 = age,
            AnyDisorder_FU3 = dcany, Emotional_FU3 = dcemot, Spcific_Phobia_FU3 = dcspph,
            Social_Phobia_FU3 = dcsoph, Panic_Disorder_FU3 = dcpanic, PTSD_FU3 = dcptsd,
            OCD_FU3 = dcocd, GAD_FU3 = dcgena, MDD_FU3 = dcmadep, Mania_FU3 = dcmania,
            Social_FU3 = dcanyso, ADHD_FU3 = dcanyhk, CD_FU3 = dcanycd, Psychosis_FU3 = dcpsych,
            Other_FU3 = dcother
  )


IMAGEN_FU3_DAWBA[, -c(1:2)] <- lapply(IMAGEN_FU3_DAWBA[, -c(1:2)], function(x) replace(x, x == 2, 1))

IMAGEN_FU3_DAWBA <- IMAGEN_FU3_DAWBA %>%
  mutate(Internalizing_FU3 = Emotional_FU3 + Social_FU3) %>%
  mutate(Internalizing_FU3 = ifelse(Internalizing_FU3 > 1, 1, Internalizing_FU3)) %>%
  mutate(Externalizing_FU3 = ADHD_FU3 + CD_FU3) %>%
  mutate(Externalizing_FU3 = ifelse(Externalizing_FU3 > 1, 1, Externalizing_FU3))


trauma_dat <- merge(trauma_dat, IMAGEN_FU3_DAWBA, by = "eid", all = F)

trauma_dat <- na.omit(trauma_dat)

#计算16岁是的诊断比例-----------
# 使用dplyr计算每个trauma_num值的诊断比例
summary_data <- trauma_dat %>%
  group_by(Trauma_Cate_Num) %>%
  summarize(diag_prop = mean(AnyDisorder_FU1))

# 使用ggplot2绘制条形图
ggplot(summary_data, aes(x = as.factor(Trauma_Cate_Num), y = diag_prop)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Diagnosis Proportion by Trauma Number",
       x = "Trauma Number", y = "Diagnosis Proportion") +
  theme_minimal()




trauma_dat$trauma <- ifelse(trauma_dat$Trauma_Cate_Num > 1, 1, 0)
trauma_dat <- merge(trauma_dat, predicited_IMAGEN_R_dat, by = "eid", all.x = T)
trauma_dat <- na.omit(trauma_dat)

trauma_dat %>% 
  tabyl(trauma) %>%
  adorn_totals("row") %>%
  adorn_pct_formatting()

# 计算 Age 的描述性统计
age_stats <- summary(trauma_dat$Age_BL.x)
age_range <- range(trauma_dat$Age_BL.x)
age_sd <- sd(trauma_dat$Age_BL.x)
age_mean <- mean(trauma_dat$Age_BL.x)

# 创建结果数据框
results <- data.frame(
  Statistic = c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile", "Maximum", "Range", "Standard Deviation"),
  Value = c(age_stats["Min."], age_stats["1st Qu."], age_stats["Median"], 
            age_mean, age_stats["3rd Qu."], age_stats["Max."], 
            paste(age_range, collapse = " to "), age_sd)
)

# 打印结果
print(results)

#确定分组(诊断分组）----------------------
trauma_dat <- trauma_dat %>%
  mutate(Diag_resilience = ifelse(AnyDisorder_BL == 0 & trauma == 0, 0,
                                  ifelse(AnyDisorder_BL == 1, 2,
                                         ifelse(AnyDisorder_BL == 0 & trauma == 1, 1, NA))))

trauma_percentage <- sum(trauma_dat$trauma == 1) / nrow(trauma_dat) * 100
print(paste("trauma 列中值为 1 的比例为:", trauma_percentage, "%"))
trauma_sub <- sum(trauma_dat$trauma ==1)
resilience_sub <- sum(trauma_dat$Diag_resilience == 1)
resilience_percentage <- resilience_sub / trauma_sub

# 使用 count 函数计算每种风险群体的人数
diag_resilience_counts <- trauma_dat %>%
  count(Diag_resilience, name = "Count")
# 打印结果
print(diag_resilience_counts)


#确定分组(模型分组）----------------------

#trauma_dat$trauma <- ifelse(trauma_dat$Trauma_Cate_Num > 1, 1, 0)

percentile_high <- trauma_dat %>%
  filter(Diag_resilience == 1) %>%
  summarize(percentile_high = quantile(pred_resilience_IMAGEN, 1 - 0.33, na.rm = TRUE))

# 查看结果
percentile_high

percentile_low <- trauma_dat %>%
  filter(Diag_resilience == 1) %>%
  summarize(percentile_low = quantile(pred_resilience_IMAGEN, 0.33, na.rm = TRUE))

# 查看结果
percentile_low


#trauma_dat <- trauma_dat %>%
#  mutate(Model_resilience = ifelse(AnyDisorder_FU1 == 0 & trauma == 0, 2,
#                                  ifelse(AnyDisorder_FU1 == 1, 3,
#                                         ifelse(pred_resilience_IMAGEN > percentile$percentile & trauma == 1, 1, NA))))

trauma_dat <- trauma_dat %>%
  mutate(Model_resilience = ifelse(AnyDisorder_BL == 0 & trauma == 0, 0,
                                   ifelse(AnyDisorder_BL == 1, 3,
                                          ifelse(AnyDisorder_BL == 0 & pred_resilience_IMAGEN >= percentile_high$percentile_high & trauma == 1, 1,
                                                 ifelse(AnyDisorder_BL == 0 & pred_resilience_IMAGEN < percentile_low$percentile_low & trauma == 1, 2, NA)))))

des_resilience <- trauma_dat[,c("eid","Model_resilience")]
des_resilience %>% 
  tabyl(Model_resilience) %>%
  adorn_totals("row") %>%
  adorn_pct_formatting()

#plot line---------------
#DIAG分组的FU1-FU3结果汇总-----------------------
#FU1
DAWBA_FU1_result <- trauma_dat %>%
  transmute(Internalizing_BL,
            Internalizing_FU1,
            Internalizing_FU2,
            Internalizing_FU3,
            Diag_resilience = factor(Diag_resilience, levels = c(0,1), labels = c("0_Low_Risk", "1_Diag_resilience"))
  ) %>%
  filter(!is.na(Diag_resilience))


#DAWBA_FU1_result %>% tbl_summary(by = Diag_resilience) %>% add_p() %>%   add_overall()

library(dplyr)
library(ggplot2)
library(tidyr)
library(ggrepel)


# 将数据整理成长格式
long_data <- DAWBA_FU1_result %>%
  gather(key = "Variable", value = "Value", -Diag_resilience)

long_data <- long_data %>%
  mutate(Variable = str_replace(Variable, "Internalizing_BL", "age 14")) %>%
  mutate(Variable = str_replace(Variable, "Internalizing_FU1", "age 16")) %>%
  mutate(Variable = str_replace(Variable, "Internalizing_FU2", "age 19")) %>%
  mutate(Variable = str_replace(Variable, "Internalizing_FU3", "age 22"))

# 确保 Variable 列是因子类型，并按照阶段顺序排序
long_data$Variable <- factor(long_data$Variable, levels = c("age 14", "age 16", "age 19", "age 22"))
long_data$Diag_resilience <- factor(long_data$Diag_resilience, levels = c("1_Diag_resilience","0_Low_Risk"))
# 确保 Value 列是数值型
long_data$Value <- as.numeric(long_data$Value)

# 检查数据结构
str(long_data)
# 计算每个组在不同年龄的患病率和被试数量
diag_summary_data <- long_data %>%
  group_by(Diag_resilience, Variable) %>%
  summarize(
    Total_Subjects = n(),                            # 计算每组每个年龄的被试数量
    Disease_Cases = sum(Value),                     # 计算患病人数
    Disease_Rate = mean(Value)                # 计算患病率（百分比）
  ) %>%
  ungroup()

# 打印结果
print(diag_summary_data)

#设置绘制误差棒errorbar时用到的函数：
topbar <- function(x){      
  return(mean(x)+sd(x)/sqrt(length(x))) #误差采用了mean+-sem
}
bottombar <- function(x){
  return(mean(x)-sd(x)/sqrt(length(x)))
}

# 绘制折线图
p1 <- ggplot(long_data, aes(x = Variable, y = Value, group = Diag_resilience, color = Diag_resilience)) +
  # 添加灰色背景矩形
  geom_rect(aes(xmin = 0, xmax = 1.5, ymin = -Inf, ymax = Inf), fill = 'grey90', color = NA) +
  # 添加竖直虚线
  geom_vline(xintercept = 1.5, linetype = "dashed", size = 0.9) +
  # 绘制每组的均值线
  stat_summary(geom = 'line', fun = 'mean', cex=1.5, alpha = 1, position = position_dodge(width = 0.15)) +
  stat_summary(geom = 'errorbar',
               fun.min = bottombar,fun.max = topbar,
               width=0.2,cex=0.7,aes(fill=Diag_resilience), position = position_dodge(width = 0.15))+
  # 绘制每组的均值点
  stat_summary(geom = 'point',fun='mean',aes(fill=Diag_resilience),
               size=4,pch=21,stroke = 1,
               key_glyph = 'polygon', position = position_dodge(width = 0.15))+
  # 设置标签和标题
  labs(y = "Internalizing disorder risk", x ="") +
  # 使用经典主题
  scale_color_manual(values = c("#EAAA60", "#9EACB4"),
                     labels = c("1_Diag_resilience","0_Low_Risk")) +
  scale_fill_manual(values = c("#EAAA60", "#9EACB4"),
                    labels = c("1_Diag_resilience","0_Low_Risk")) +
  theme_classic(base_size = 15) +
  scale_x_discrete()+
  coord_cartesian(ylim = c(0, 0.3))

# 打印图形
print(p1)


##caculate OR------
# 初始化 results 数据框
results <- data.frame(
  Time_Point = character(),
  OR = numeric(),
  CI_Lower = numeric(),
  CI_Upper = numeric(),
  P_Value = numeric(),  # 增加 P 值列
  Compare = character(),
  stringsAsFactors = FALSE
)

# 初始基线数据
results <- results %>%
  add_row(
    Time_Point = "Internalizing_BL",
    OR = 1,
    CI_Lower = 1,
    CI_Upper = 1,
    P_Value = NA,  # 基线没有 P 值
    Compare = "Diag_resilient-Low_risk"
  )

# 循环计算 OR 和 P 值
for (time in colnames(DAWBA_FU1_result)[2:4]) {
  # 提取子集数据并重命名 outcome
  temp_data <- DAWBA_FU1_result %>%
    select(Diag_resilience, !!sym(time)) %>%
    rename(outcome = !!sym(time))
  
  # Logistic 回归计算 OR
  model <- glm(outcome ~ Diag_resilience, data = temp_data, family = binomial)
  summary_result <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  
  # 提取 OR、置信区间和 P 值
  OR <- summary_result$estimate[2]
  CI_Lower <- summary_result$conf.low[2]
  CI_Upper <- summary_result$conf.high[2]
  P_Value <- summary_result$p.value[2]
  
  # 将结果加入数据框
  results <- results %>%
    add_row(
      Time_Point = time,
      OR = OR,
      CI_Lower = CI_Lower,
      CI_Upper = CI_Upper,
      P_Value = P_Value,
      Compare = "Diag_resilient-Low_risk"
    )
}

# 输出结果
print(results)






#MODEL分组的FU1-FU3结果汇总-----------------------
#FU1
DAWBA_FU1_result <- trauma_dat %>%
  transmute(Internalizing_BL,
            Internalizing_FU1,
            Internalizing_FU2,
            Internalizing_FU3,
            Model_resilience = factor(Model_resilience, levels = c(0,1,2), labels = c("0_Low_Risk", "1_Model_resilience","2_Model_vulnerable"))
  )%>% filter(!is.na(Model_resilience))

# 将数据整理成长格式
long_data <- DAWBA_FU1_result %>%
  gather(key = "Variable", value = "Value", -Model_resilience)

long_data <- long_data %>%
  mutate(Variable = str_replace(Variable, "Internalizing_BL", "age 14")) %>%
  mutate(Variable = str_replace(Variable, "Internalizing_FU1", "age 16")) %>%
  mutate(Variable = str_replace(Variable, "Internalizing_FU2", "age 19")) %>%
  mutate(Variable = str_replace(Variable, "Internalizing_FU3", "age 22"))

# 确保 Variable 列是因子类型，并按照阶段顺序排序
long_data$Variable <- factor(long_data$Variable, levels = c("age 14", "age 16", "age 19", "age 22"))
long_data$Model_resilience <- factor(long_data$Model_resilience, levels = c("2_Model_vulnerable", "1_Model_resilience", "0_Low_Risk"))
# 确保 Value 列是数值型
long_data$Value <- as.numeric(long_data$Value)

# 检查数据结构
str(long_data)

# 计算每个组在不同年龄的患病率和被试数量
model_summary_data <- long_data %>%
  group_by(Model_resilience, Variable) %>%
  summarize(
    Total_Subjects = n(),                            # 计算每组每个年龄的被试数量
    Disease_Cases = sum(Value),                     # 计算患病人数
    Disease_Rate = mean(Value)               # 计算患病率（百分比）
  ) %>%
  ungroup()

# 打印结果
print(model_summary_data)

#设置绘制误差棒errorbar时用到的函数：
topbar <- function(x){      
  return(mean(x)+sd(x)/sqrt(length(x))) #误差采用了mean+-sem
}
bottombar <- function(x){
  return(mean(x)-sd(x)/sqrt(length(x)))
}

# 绘制折线图
p2 <- ggplot(long_data, aes(x = Variable, y = Value, group = Model_resilience, color = Model_resilience)) +
  # 添加灰色背景矩形
  geom_rect(aes(xmin = 0, xmax = 1.5, ymin = -Inf, ymax = Inf), fill = 'grey90', color = NA) +
  # 添加竖直虚线
  geom_vline(xintercept = 1.5, linetype = "dashed", size = 0.9) +
  # 绘制每组的均值线
  stat_summary(geom = 'line', fun = 'mean', cex=1.5, alpha = 1, position = position_dodge(width = 0.15)) +
  stat_summary(geom = 'errorbar',
               fun.min = bottombar,fun.max = topbar,
               width=0.2,cex=0.7,aes(fill=Model_resilience), position = position_dodge(width = 0.15))+
  # 绘制每组的均值点
  stat_summary(geom = 'point',fun='mean',aes(fill=Model_resilience),
               size=4,pch=21,stroke = 1,
               key_glyph = 'polygon', position = position_dodge(width = 0.15))+
  # 设置标签和标题
  labs(y = "Internalizing disorder risk", x ="") +
  # 使用经典主题
  scale_color_manual(values = c("#E68B81", "#84C3B7", "#9EACB4" ),
                     labels = c("2_Model_vulnerable", "1_Model_resilience", "0_Low_Risk")) +
  scale_fill_manual(values = c("#E68B81", "#84C3B7", "#9EACB4" ),
                    labels = c("2_Model_vulnerable", "1_Model_resilience", "0_Low_Risk")) +
  theme_classic(base_size = 15) +
  scale_x_discrete()+
  coord_cartesian(ylim = c(0, 0.3))

# 打印图形
print(p2)

library(cowplot)
# 把患病率条形图组合成 3x2 的布局
combined_plot <- plot_grid(p1, p2, ncol = 1)

# 显示组合后的图形（导出700*500）
print(combined_plot)

# 使用 align_plots 对齐图形
aligned_plots <- align_plots(p1 + theme(legend.position = "none"), 
                             p2 + theme(legend.position = "none"),
                             align = 'v', axis = 'l')

# 提取图例
legend_p1 <- get_legend(p1)
legend_p2 <- get_legend(p2)

# 组合图形和图例
combined_plot <- plot_grid(
  plot_grid(aligned_plots[[1]], aligned_plots[[2]], ncol = 1),
  plot_grid(legend_p1, legend_p2, ncol = 1),
  ncol = 2, rel_heights = c(1, 0.1)
)

# 显示组合后的图形
print(combined_plot)


##calculate Resilience_Low-OR------
results <- results %>%
  add_row(
    Time_Point = "Internalizing_BL",
    OR = 1,
    CI_Lower = 1,
    CI_Upper = 1,
    P_Value = NA,  # 基线没有 P 值
    Compare = "Model_resilient-Low_risk"
  )


# ???????
for (time in colnames(DAWBA_FU1_result)[2:4]) {
  # ???????
  temp_data <- DAWBA_FU1_result %>%
    select(Model_resilience, !!sym(time)) %>%
    rename(outcome = !!sym(time))
  
  temp_data <- temp_data %>%
    filter(Model_resilience != "2_Model_vulnerable")
  
  # Logistic ???? OR
  model <- glm(outcome ~ Model_resilience, data = temp_data, family = binomial)
  summary_result <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  
  # 提取 OR、置信区间和 P 值
  OR <- summary_result$estimate[2]
  CI_Lower <- summary_result$conf.low[2]
  CI_Upper <- summary_result$conf.high[2]
  P_Value <- summary_result$p.value[2]
  
  # 将结果加入数据框
  results <- results %>%
    add_row(
      Time_Point = time,
      OR = OR,
      CI_Lower = CI_Lower,
      CI_Upper = CI_Upper,
      P_Value = P_Value,
      Compare = "Model_resilient-Low_risk"
    )
}

# ????
print(results)

##calculate Vulnerable_Low-OR------

results <- results %>%
  add_row(
    Time_Point = "Internalizing_BL",
    OR = 1,
    CI_Lower = 1,
    CI_Upper = 1,
    P_Value = NA,
    Compare = "Model_vulnerable-Low_risk"
  )

# ???????
for (time in colnames(DAWBA_FU1_result)[2:4]) {
  # ???????
  temp_data <- DAWBA_FU1_result %>%
    select(Model_resilience, !!sym(time)) %>%
    rename(outcome = !!sym(time))
  
  temp_data <- temp_data %>%
    filter(Model_resilience != "1_Model_resilience")
  
  # Logistic ???? OR
  model <- glm(outcome ~ Model_resilience, data = temp_data, family = binomial)
  summary_result <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  
  # 提取 OR、置信区间和 P 值
  OR <- summary_result$estimate[2]
  CI_Lower <- summary_result$conf.low[2]
  CI_Upper <- summary_result$conf.high[2]
  P_Value <- summary_result$p.value[2]
  
  # 将结果加入数据框
  results <- results %>%
    add_row(
      Time_Point = time,
      OR = OR,
      CI_Lower = CI_Lower,
      CI_Upper = CI_Upper,
      P_Value = P_Value,
      Compare = "Model_vulnerable-Low_risk"
    )
  
}

##calculate Vulnerable_Resilience-OR------

results <- results %>%
  add_row(
    Time_Point = "Internalizing_BL",
    OR = 1,
    CI_Lower = 1,
    CI_Upper = 1,
    P_Value = NA,
    Compare = "Model_vulnerable-Model_resilient"
  )

# ???????
for (time in colnames(DAWBA_FU1_result)[2:4]) {
  # ???????
  temp_data <- DAWBA_FU1_result %>%
    select(Model_resilience, !!sym(time)) %>%
    rename(outcome = !!sym(time))
  
  temp_data <- temp_data %>%
    filter(Model_resilience != "0_Low_Risk")
  
  # Logistic ???? OR
  model <- glm(outcome ~ Model_resilience, data = temp_data, family = binomial)
  summary_result <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  
  # 提取 OR、置信区间和 P 值
  OR <- summary_result$estimate[2]
  CI_Lower <- summary_result$conf.low[2]
  CI_Upper <- summary_result$conf.high[2]
  P_Value <- summary_result$p.value[2]
  
  # 将结果加入数据框
  results <- results %>%
    add_row(
      Time_Point = time,
      OR = OR,
      CI_Lower = CI_Lower,
      CI_Upper = CI_Upper,
      P_Value = P_Value,
      Compare = "Model_vulnerable-Model_resilient"
    )
  
}

# ????
print(results)
view(results)

#plot OR------------
# Load ggplot2 library
library(ggplot2)

# Ensure `results` is properly formatted
results$Compare <- as.factor(results$Compare)  # Convert Compare to a factor for grouping
results$Time_Point <- factor(results$Time_Point, levels = unique(results$Time_Point))  # Ensure Time_Point is ordered

# Plot(导出520*300)
or_results_plot <- results[-c(13:16),]
ggplot(or_results_plot, aes(x = Time_Point, y = OR, fill = Compare)) +
  # 添加灰色背景矩形
  geom_rect(aes(xmin = 0, xmax = 1.5, ymin = -Inf, ymax = Inf), fill = 'grey90', color = NA) +
  # 添加竖直虚线
  geom_vline(xintercept = 1.5, linetype = "dashed", size = 0.9) +
  #geom_hline(yintercept = 2.3, linetype = "dashed", color = "red", size = 0.9) +
  annotate("segment", x = 1.5, xend = Inf, y = 2.3, yend = 2.3, linetype = "dashed", color = "red", size = 0.9) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +  # Bar plot, dodging bars for groups
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper),  # Add error bars
                width = 0.2, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("#EAAA60", "#84C3B7", "#E68B81")) +  # Add colors for the three groups
  labs(x = "Time Point", y = "Odds Ratio (OR)", fill = "Group") +  # Add labels
  theme_classic() +  # Minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels


# Plot纵向OR(导出650*250)
or_results_plot <- results[-c(13:16),]
or_results_plot$Compare <- factor(or_results_plot$Compare, 
                                  levels = rev(levels(or_results_plot$Compare))) #为了保证竖向outcome-based排第一，进行倒置
ggplot(or_results_plot, aes(x = Time_Point, y = OR, color = Compare)) +
  # 添加灰色背景矩形
  geom_rect(aes(xmin = 3.5, xmax = Inf, ymin = -Inf, ymax = Inf), fill = 'grey90', color = NA) +
  # 添加竖直虚线
  #geom_vline(xintercept = 3.5, color = "black", linetype = "dashed", size = 0.9) +
  geom_vline(xintercept = 2.5, color = "gray", linetype = "dashed", size = 0.9) +
  geom_vline(xintercept = 1.5, color = "gray", linetype = "dashed", size = 0.9) +
  #geom_hline(yintercept = 2.3, linetype = "dashed", color = "red", size = 0.9) +
  annotate("segment", x = 0, xend = Inf, y = 1, yend = 1, linetype = "dashed", color = "black", size = 0.9) +
  geom_point(stat = "identity", position = position_dodge(width = 0.8), size = 3.5) +  # Bar plot, dodging bars for groups
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper),  # Add error bars
                width = 0, position = position_dodge(width = 0.8)) +
  scale_color_manual(values = c("#E68B81", "#84C3B7", "#EAAA60")) +  # Add colors for the three groups
  scale_y_continuous(limits = c(0, 8)) +  # 限制 y 轴刻度为 0 到 8
  labs(x = "Time Point", y = "Odds Ratio (OR)", fill = "Group") +  # Add labels
  theme_classic() +  # Minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line.y = element_line(color = "black")) + # Rotate x-axis labels
  coord_flip()  + # 翻转坐标轴
  xlim(rev(levels(or_results_plot$Time_Point)))   # 反转x轴顺序


# ?????
library(dplyr)
library(ggplot2)
library(ggsignif)
library(purrr)

# ?? results ??? OR?CI?Compare ? Time_Point ????

# ???????? p ???
pairwise_comparisons <- results %>%
  group_by(Time_Point) %>%
  do({
    group_combinations <- combn(unique(.$Compare), 2, simplify = FALSE)
    comparisons <- map(group_combinations, function(groups) {
      group1 <- groups[1]
      group2 <- groups[2]
      
      # ??OR?????
      or1 <- .$OR[.$Compare == group1]
      or2 <- .$OR[.$Compare == group2]
      se1 <- (log(.$CI_Upper[.$Compare == group1]) - log(or1)) / 1.96
      se2 <- (log(.$CI_Upper[.$Compare == group2]) - log(or2)) / 1.96
      
      # ??z??p?
      z_score <- abs(log(or1) - log(or2)) / sqrt(se1^2 + se2^2)
      p_value <- 2 * (1 - pnorm(z_score))
      
      # ??????
      data.frame(
        Comparison = paste0(group1, " vs ", group2),
        p_value = round(p_value, 3),
        Time_Point = unique(.$Time_Point) # ?? Time_Point ?
      )
    })
    
    # ????
    bind_rows(comparisons)
  })

# ?? pairwise_comparisons ??
print(pairwise_comparisons)


#进行非劣性分析--------------
#library(TOSTER)
### 非劣性函数-----------

twoprop_test = function(p1, p2,
                        n1, n2,
                        null = NULL,
                        alpha = .05,
                        alternative = c("two.sided",
                                        "less",
                                        "greater",
                                        "equivalence",
                                        "minimal.effect"),
                        effect_size = c("difference",
                                        "odds.ratio",
                                        "risk.ratio")){
  
  alternative = match.arg(alternative)
  effect_size = match.arg(effect_size)
  if(!is.numeric(alpha) || alpha <=0 || alpha >=1){
    stop("The alpha must be a numeric value between 0 and 1")
  }
  if (any(c(p1, p2) > 1 | c(p1, p2) < 0)
  ) {
    stop("elements of p1 or p2 must be in [0,1]")
  }
  if(any(c(n1, n2) < 10)){
    stop("elements of n1 or n2 must be greater than 9")
  }
  
  if(any(c(n1, n2) <= 50)){
    message("Small sample size in at least one group; proceed with caution")
  }
  
  # effect size ----
  res_tests = switch(effect_size,
                     difference = test_prop_dif(p1, p2, n1, n2,
                                                null,
                                                alternative,
                                                alpha),
                     odds.ratio = test_odds_ratio(p1, p2, n1, n2,
                                                  null,
                                                  alternative,
                                                  alpha),
                     risk.ratio = test_risk_ratio(p1, p2, n1, n2,
                                                  null,
                                                  alternative,
                                                  alpha))
  
  
  
  RVAL <- list(statistic = res_tests$STATISTIC,
               #parameter = PARAMETER,
               p.value = as.numeric(res_tests$PVAL),
               estimate = res_tests$ESTIMATE,
               null.value = res_tests$NVAL,
               conf.int = res_tests$CINT,
               alternative = alternative,
               method = res_tests$METHOD)
  class(RVAL) <- "htest"
  return(RVAL)
  
}

test_prop_dif = function(p1,p2,n1,n2,
                         null,
                         alternative,
                         alpha) {
  if(is.null(null)){
    null = 0
  }
  
  prop_dif <- p1 - p2
  # proportion se
  prop_se <- sqrt((p1*(1-p1))/n1 + (p2*(1-p2))/n2)
  p_bar = (n1*p1 + n2*p2) / (n1 + n2)
  prop_se_pool = sqrt(p_bar * (1-p_bar) * (1/n1+1/n2))
  
  #YATES <- abs(prop_dif) / sum(1 / n1, 1 / n2)
  if (any((null <= -1) | (null >= 1))) {
    stop("elements of 'null' must be in (-1,1)")
  }
  
  if(alternative %in% c("equivalence","minimal.effect")){
    
    if (length(null) == 1) {
      if (null ==  0) {
        stop("null cannot be zero if alternative is equivalence or minimal.effect")
      }
      null = c(null,-1 * null)
      
    }
    
    lo_ztest = (prop_dif - min(null))/prop_se
    hi_ztest = (prop_dif - max(null))/prop_se
    
    lo_pvalue = switch(
      alternative,
      "equivalence" = p_from_z(lo_ztest,
                               alternative = "greater"),
      "minimal.effect" = p_from_z(lo_ztest,
                                  alternative = "less")
    )
    
    hi_pvalue = switch(
      alternative,
      "equivalence" = p_from_z(hi_ztest,
                               alternative = "less"),
      "minimal.effect" = p_from_z(hi_ztest,
                                  alternative = "greater")
    )
    
    PVAL = switch(
      alternative,
      "equivalence" = max(lo_pvalue, hi_pvalue),
      "minimal.effect" = min(lo_pvalue, hi_pvalue)
    )
    
    test_p = PVAL == c(lo_pvalue, hi_pvalue)
    test_z = c(lo_ztest, hi_ztest)
    ZTEST = test_z[test_p]
    
    conf_level = 1-alpha*2
    conf = 1-alpha
  } else{
    if(length(null) != 1){
      stop("null must have length of 1 if alternative is not a TOST test.")
    }
    
    if(null == 0){
      message("For nil-hypothesis tests (null = 0), it is recommended that prop.test be utilized.")
    }
    ZTEST = (prop_dif - null) / prop_se
    PVAL = p_from_z(ZTEST,
                    alternative = alternative)
    
    conf_level = switch(
      alternative,
      "two.sided" = 1-alpha,
      "less" =  1-alpha*2,
      "greater" = 1-alpha*2
    )
    
    conf = switch(
      alternative,
      "two.sided" = 1-alpha/2,
      "less" =  1-alpha,
      "greater" = 1-alpha
    )
  }
  
  z_mult = qnorm(conf)
  ESTIMATE = prop_dif
  names(ESTIMATE) = "difference in proportions"
  
  CINT = prop_dif + c(-1,1)*(qnorm(conf) * prop_se)
  if(alternative == "less"){
    CINT[1] = -Inf
  }
  
  if(alternative == "greater"){
    CINT[2] = Inf
  }
  attr(CINT, "conf.level") <- conf_level
  
  STATISTIC = ZTEST
  names(STATISTIC) <- "z"
  
  NVAL = null
  names(NVAL) = rep("difference in proportions", length(null))
  METHOD = "difference in two proportions z-test"
  
  list(STATISTIC = STATISTIC,
       PVAL = PVAL,
       NVAL = NVAL,
       ESTIMATE = ESTIMATE,
       CINT = CINT,
       METHOD = METHOD)
}



test_odds_ratio = function(p1, p2, n1, n2,
                           null,
                           alternative,
                           alpha){
  if(is.null(null)){
    null = 1
  }
  # Fleiss, J. L., Levin, B., Paik, M.C. 2003. Statistical Methods for Rates and Proportions. Third Edition. John Wiley & Sons. New York.
  q1 = 1-p1
  q2 = 1-p2
  a = 1/(n1*p1+.5)
  b = 1/(n1*q1+.5)
  c = 1/(n2*p2+.5)
  d = 1/(n2*q2+.5)
  m1 = n1*p1 + n2*p2
  OR = (p1/q1) / (p2/q2)
  se_logodds = sqrt(sum(a,b,c,d))
  
  if(alternative %in% c("equivalence","minimal.effect")){
    
    if (length(null) == 1) {
      if (null ==  1) {
        stop("null for odds ratio cannot be 1 if alternative is equivalence or minimal.effect")
      }
      null = c(null,null^(-1))
      
    }
    
    lo_ztest = (log(OR) - min(log(null))) / se_logodds #修改了下限
    hi_ztest = (log(OR) - max(log(null))) / se_logodds
    
    lo_pvalue = switch(
      alternative,
      "equivalence" = p_from_z(lo_ztest,
                               alternative = "greater"),
      "minimal.effect" = p_from_z(lo_ztest,
                                  alternative = "less")
    )
    
    hi_pvalue = switch(
      alternative,
      "equivalence" = p_from_z(hi_ztest,
                               alternative = "less"),
      "minimal.effect" = p_from_z(hi_ztest,
                                  alternative = "greater")
    )
    
    PVAL = switch(
      alternative,
      "equivalence" = hi_pvalue,
      #"equivalence" = max(lo_pvalue, hi_pvalue),
      "minimal.effect" = min(lo_pvalue, hi_pvalue)
    )
    
    test_p = PVAL == c(lo_pvalue, hi_pvalue)
    test_z = c(lo_ztest, hi_ztest)
    ZTEST = test_z[test_p]
    
    conf_level = 1-alpha*2
    conf = 1-alpha
  } else{
    if(length(null) != 1){
      stop("null must have length of 1 if alternative is not a TOST test.")
    }
    if(null == 1){
      message("For nil-hypothesis tests (null = 1), it is recommended that prop.test be utilized.")
    }
    ZTEST = (log(OR) - log(null)) / se_logodds
    PVAL = p_from_z(ZTEST,
                    alternative = alternative)
    
    conf_level = switch(
      alternative,
      "two.sided" = 1-alpha,
      "less" =  1-alpha*2,
      "greater" = 1-alpha*2
    )
    
    conf = switch(
      alternative,
      "two.sided" = 1-alpha/2,
      "less" =  1-alpha,
      "greater" = 1-alpha
    )
  }
  
  z_mult = qnorm(conf)
  
  
  CINT = exp(
    log(OR) + c(-1,1)*z_mult*se_logodds
  )
  
  if(alternative == "less"){
    CINT[1] = -Inf
  }
  
  if(alternative == "greater"){
    CINT[2] = Inf
  }
  attr(CINT, "conf.level") <- conf_level
  
  ESTIMATE = OR
  names(ESTIMATE) = "Odds Ratio"
  
  STATISTIC = ZTEST
  names(STATISTIC) <- "z"
  
  NVAL = null
  names(NVAL) = rep("Odds Ratio", length(null))
  METHOD = "approximate Odds Ratio z-test"
  list(STATISTIC = STATISTIC,
       PVAL = PVAL,
       NVAL = NVAL,
       ESTIMATE = ESTIMATE,
       CINT = CINT,
       METHOD = METHOD)
  
}


test_risk_ratio = function(p1, p2, n1, n2,
                           null,
                           alternative,
                           alpha){
  if(is.null(null)){
    null = 1
  }
  # Gart and Nam (1988), page 324
  # Gart, John J. and Nam, Jun-mo. 1988. 'Approximate Interval Estimation of the Ratio of Binomial Parameters: Review and Corrections for Skewness.' Biometrics, Volume 44, 323-338
  phi = p1/p2
  #
  q1 = 1-p1
  q2 = 1-p2
  se_val = sqrt(q1/(n1*p1) + q2/(n2*p2))
  
  if(alternative %in% c("equivalence","minimal.effect")){
    
    if (length(null) == 1) {
      if (null ==  1) {
        stop("null cannot be zero if alternative is equivalence or minimal.effect")
      }
      null = c(null,null^(-1))
      
    }
    
    lo_ztest = (log(phi) - min(log(null))) / se_val
    hi_ztest = (log(phi) - max(log(null))) / se_val
    
    lo_pvalue = switch(
      alternative,
      "equivalence" = p_from_z(lo_ztest,
                               alternative = "greater"),
      "minimal.effect" = p_from_z(lo_ztest,
                                  alternative = "less")
    )
    
    hi_pvalue = switch(
      alternative,
      "equivalence" = p_from_z(hi_ztest,
                               alternative = "less"),
      "minimal.effect" = p_from_z(hi_ztest,
                                  alternative = "greater")
    )
    
    PVAL = switch(
      alternative,
      "equivalence" = max(lo_pvalue, hi_pvalue),
      "minimal.effect" = min(lo_pvalue, hi_pvalue)
    )
    
    test_p = PVAL == c(lo_pvalue, hi_pvalue)
    test_z = c(lo_ztest, hi_ztest)
    ZTEST = test_z[test_p]
    
    conf_level = 1-alpha*2
    conf = 1-alpha
  } else{
    if(length(null) != 1){
      stop("null must have length of 1 if alternative is not a TOST test.")
    }
    
    if(null == 1){
      message("For nil-hypothesis tests (null = 1), it is recommended that prop.test be utilized.")
    }
    ZTEST = (log(phi) - log(null)) / se_val
    PVAL = p_from_z(ZTEST,
                    alternative = alternative)
    
    conf_level = switch(
      alternative,
      "two.sided" = 1-alpha,
      "less" =  1-alpha*2,
      "greater" = 1-alpha*2
    )
    
    conf = switch(
      alternative,
      "two.sided" = 1-alpha/2,
      "less" =  1-alpha,
      "greater" = 1-alpha
    )
  }
  
  z_mult = qnorm(conf)
  CINT = phi * exp(c(-1,1)*z_mult*se_val)
  
  if(alternative == "less"){
    CINT[1] = -Inf
  }
  
  if(alternative == "greater"){
    CINT[2] = Inf
  }
  attr(CINT, "conf.level") <- conf_level
  
  ESTIMATE = phi
  names(ESTIMATE) = "Risk Ratio"
  
  STATISTIC = ZTEST
  names(STATISTIC) <- "z"
  
  NVAL = null
  names(NVAL) = rep("Risk Ratio", length(null))
  
  METHOD = "approximate Risk Ratio z-test"
  
  list(STATISTIC = STATISTIC,
       PVAL = PVAL,
       NVAL = NVAL,
       ESTIMATE = ESTIMATE,
       CINT = CINT,
       METHOD = METHOD)
}

p_from_z = function(x,
                    alternative = "two.sided"){
  if(alternative == "two.sided"){
    2*pnorm(-abs(unlist(x)))
  } else  if (alternative  == "greater"){
    pnorm(x, lower.tail = FALSE)
  } else if (alternative  == "less"){
    pnorm(x, lower.tail = TRUE)
  } else{
    stop("alternative must be two.sided, greater, or less")
  }
  
}

se_dz = function(smd,n){
  sqrt( 1/n + (smd^2/(2*n)) )
}

se_ds = function(smd,n){
  if(length(n) == 1){
    n = c(n,n)
  }
  
  sqrt((n[1]+n[2])/(n[1]*n[2]) + smd^2/(2*(n[1]+n[2])))
  
}

ci_perc = function(vec,
                   alternative = "two.sided",
                   alpha = 0.05){
  if(alternative == "two.sided"){
    alpha = alpha/2
    res = quantile(vec, c(alpha,1-alpha))
  } else {
    res = quantile(vec, c(alpha,1-alpha))
  }
  
}

#构建结果表格
NI_results <- data.frame(
  Time_Point = character(),
  Model_Resilient_diag_rate = numeric(),
  N_Model_Resilient = numeric(),
  Low_risk_diag_rate = numeric(),
  N_Low_risk = numeric(),
  SESOI = numeric(),
  OR = numeric(),
  CI_Lower = numeric(),
  CI_Upper = numeric(),
  P_Value = numeric(),  # 增加 P 值列
  stringsAsFactors = FALSE
)

#FU1的非劣性
p1 <- model_summary_data$Disease_Rate[model_summary_data$Model_resilience == "1_Model_resilience" &
                                        model_summary_data$Variable == "age 16"]
n1 <- model_summary_data$Total_Subjects[model_summary_data$Model_resilience == "1_Model_resilience" &
                                        model_summary_data$Variable == "age 16"]
p2 <- model_summary_data$Disease_Rate[model_summary_data$Model_resilience == "0_Low_Risk" &
                                        model_summary_data$Variable == "age 16"]
n2 <- model_summary_data$Total_Subjects[model_summary_data$Model_resilience == "0_Low_Risk" &
                                        model_summary_data$Variable == "age 16"]
NI <- twoprop_test(
  p1,
  p2,
  n1,
  n2,
  null = 2.61,
  alpha = 0.025,
  alternative = "equivalence",
  effect_size = "odds.ratio"
)
NI <- tidy(NI)

NI_results <- NI_results %>%
  add_row(
    Time_Point = "FU1",
    Model_Resilient_diag_rate = p1,
    N_Model_Resilient = n1,
    Low_risk_diag_rate = p2,
    N_Low_risk = n2,
    SESOI = 2.61,
    OR = NI$estimate[1],
    CI_Lower = NI$conf.low[1],
    CI_Upper = NI$conf.high[1],
    P_Value = NI$p.value[1]
  )

#FU2的非劣性
p1 <- model_summary_data$Disease_Rate[model_summary_data$Model_resilience == "1_Model_resilience" &
                                        model_summary_data$Variable == "age 19"]
n1 <- model_summary_data$Total_Subjects[model_summary_data$Model_resilience == "1_Model_resilience" &
                                          model_summary_data$Variable == "age 19"]
p2 <- model_summary_data$Disease_Rate[model_summary_data$Model_resilience == "0_Low_Risk" &
                                        model_summary_data$Variable == "age 19"]
n2 <- model_summary_data$Total_Subjects[model_summary_data$Model_resilience == "0_Low_Risk" &
                                          model_summary_data$Variable == "age 19"]
NI <- twoprop_test(
  p1,
  p2,
  n1,
  n2,
  null = 2.68,
  alpha = 0.025,
  alternative = "equivalence",
  effect_size = "odds.ratio"
)
NI <- tidy(NI)

NI_results <- NI_results %>%
  add_row(
    Time_Point = "FU1",
    Model_Resilient_diag_rate = p1,
    N_Model_Resilient = n1,
    Low_risk_diag_rate = p2,
    N_Low_risk = n2,
    SESOI = 2.68,
    OR = NI$estimate[1],
    CI_Lower = NI$conf.low[1],
    CI_Upper = NI$conf.high[1],
    P_Value = NI$p.value[1]
  )

#FU3的非劣性
p1 <- model_summary_data$Disease_Rate[model_summary_data$Model_resilience == "1_Model_resilience" &
                                        model_summary_data$Variable == "age 22"]
n1 <- model_summary_data$Total_Subjects[model_summary_data$Model_resilience == "1_Model_resilience" &
                                          model_summary_data$Variable == "age 22"]
p2 <- model_summary_data$Disease_Rate[model_summary_data$Model_resilience == "0_Low_Risk" &
                                        model_summary_data$Variable == "age 22"]
n2 <- model_summary_data$Total_Subjects[model_summary_data$Model_resilience == "0_Low_Risk" &
                                          model_summary_data$Variable == "age 22"]
NI <- twoprop_test(
  p1,
  p2,
  n1,
  n2,
  null = 2.3,
  alpha = 0.025,
  alternative = "equivalence",
  effect_size = "odds.ratio"
)
NI <- tidy(NI)

NI_results <- NI_results %>%
  add_row(
    Time_Point = "FU1",
    Model_Resilient_diag_rate = p1,
    N_Model_Resilient = n1,
    Low_risk_diag_rate = p2,
    N_Low_risk = n2,
    SESOI = 2.3,
    OR = NI$estimate[1],
    CI_Lower = NI$conf.low[1],
    CI_Upper = NI$conf.high[1],
    P_Value = NI$p.value[1]
  )

