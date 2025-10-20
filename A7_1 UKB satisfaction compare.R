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
resilience_corr_rename_dat <- read_csv("~/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/UKB_resilience_corr_rename_dat_11221.csv")
resilience_corr_rename_dat <- resilience_corr_rename_dat[,-1]
resilience_model_dat <- read_csv("~/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/predicited_resiliece_3w_norm_dat.csv")
resilience_model_dat <- resilience_model_dat[,-1]
soc_env_trauma_mental_dat <- read.csv("~/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/UKB_BL_FU_trauma_mental_env_dat.csv")
soc_env_trauma_mental_dat <- soc_env_trauma_mental_dat[,-1]
Trauma_BL_dat <- read.csv("~/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/UKB_BL_Trauma.csv")
Trauma_BL_dat <- Trauma_BL_dat[,-1]




##将resilience分组，循环进行作画-----------------------------------------
library(dplyr)
library(emmeans)
library(bruceR)
library(patchwork)
library("gridExtra")

#对数据进行整理，保证真实的resilience和模型计算的resilien能够兼容
resilience_group_R <- resilience_corr_rename_dat
resilience_group_R <- merge(resilience_group_R,resilience_model_dat[,c(1,3)], by = "eid", all.x = TRUE)
resilience_group_R <- merge(resilience_group_R,soc_env_trauma_mental_dat[,c(1,71,85,79)], by = "eid", all.x = TRUE)
resilience_group_R <- merge(resilience_group_R,Trauma_BL_dat[,c(1,3:8)], by = "eid", all.x = TRUE)

resilience_group_R$self_resilience <- (resilience_group_R$self_resilience + 6 )/6
resilience_group_R$FU2_recent_trauma <- resilience_group_R$FU2_recent_trauma * 10
resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 2, 6, Ethnic_group))
resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 3, 2, Ethnic_group))
resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 5, 2, Ethnic_group))
resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 4, 3, Ethnic_group))
resilience_group_R <- mutate(resilience_group_R, Ethnic_group = ifelse(Ethnic_group == 6, 4, Ethnic_group))

resilience_group_R$Ethnic_group <- resilience_group_R$Ethnic_group - 1

# 计算分位数
quantiles <- quantile(resilience_group_R$self_resilience, probs = c(1/3, 2/3),na.rm = TRUE)

# 替换值
resilience_group_R$self_resilience[resilience_group_R$self_resilience <= quantiles[1]] <- 0
resilience_group_R$self_resilience[resilience_group_R$self_resilience > quantiles[1] & resilience_group_R$self_resilience < quantiles[2]] <- 1
resilience_group_R$self_resilience[resilience_group_R$self_resilience >= quantiles[2]] <- 2
resilience_group_R$self_resilience <- as.factor(resilience_group_R$self_resilience)

#计算童年创伤
resilience_group_R$child_trauma <- resilience_group_R$Felt_Hated_As_Child_FU1 + 4 - resilience_group_R$Felt_Loved_As_Child_FU1 + resilience_group_R$Sex_Molested_As_Child_FU1 + resilience_group_R$Phys_Abused_As_Child_FU1 + 4 - resilience_group_R$Someone_Take_To_Doctor_As_Child_FU1
# 修改列名
colnames(resilience_group_R)[colnames(resilience_group_R) == "PM2.5-10um_Air_Pollution_2010_BL"] <- "PM2.5_10um_Air_Pollution_2010_BL"

#把满意度转过来
resilience_group_R$Work_Job_Satisfaction_BL[resilience_group_R$Work_Job_Satisfaction_BL == 7] <- NA
resilience_group_R$Financial_Situation_Satisfaction_BL <- 6 - resilience_group_R$Financial_Situation_Satisfaction_BL
resilience_group_R$Health_Satisfaction_BL <- 6 - resilience_group_R$Health_Satisfaction_BL
resilience_group_R$Work_Job_Satisfaction_BL <- 6 - resilience_group_R$Work_Job_Satisfaction_BL
resilience_group_R$Family_Relationship_Satisfaction_BL <- 6 - resilience_group_R$Family_Relationship_Satisfaction_BL
resilience_group_R$Friendships_Satisfaction_BL <- 6 - resilience_group_R$Friendships_Satisfaction_BL


##定义trauma的类型-------

trauma <- c("Big5_sociability","Big5_nervousness","Big5_warmth","Big5_curiosity","Big5_diligence")

trauma_variables_list <- list(
  Eco_Social = c(
    "HH_Num_Vehicle_BL",  "HH_Income_BL",  "HH_Own_Rent_BL",  "Social_Freq_Visits_BL",  "Social_Able_Confide_BL",
    "Num_People_Living_BL",  "Loneliness_BL",  "Num_People_HH_FU2",  "Freq_Confide_FU2",
    "Freq_In_Tune_FU2",  "Freq_Isolated_FU2",  "Freq_Left_Out_FU2",  "Freq_See_In_Person_FU2",
    "Freq_See_On_Video_FU2",  "Freq_Speak_On_Voice_FU2",  "Freq_Lack_Companionship_FU2"
  ),
  Early_risk = c(
    "Breastfed_Baby_BL",  "Comp_Body_Size_Age_10_BL",  "Comp_Height_Size_Age_10_BL",
    "Maternal_Smoking_Birth_BL",
    "Felt_Loved_As_Child_FU1", "Phys_Abused_As_Child_FU1", "Felt_Hated_As_Child_FU1",
    "Sex_Molested_As_Child_FU1", "Someone_Take_To_Doctor_As_Child_FU1"
  ),
  Enviroment = c(
    "NO2_Air_Pollution_2010_BL", "NOx_Air_Pollution_2010_BL",
    "PM10_Air_Pollution_2010_BL", "PM2.5_Air_Pollution_2010_BL",
    "PM2.5_Absorbance_2010_BL", "PM2.5_10um_Air_Pollution_2010_BL",
    "Traffic_Intensity_Nearest_Road_BL", "Inv_Dist_Nearest_Road_BL",
    "Traffic_Intensity_Nearest_Major_Road_BL", "Inv_Dist_Nearest_Major_Road_BL",
    "Total_Traffic_Load_Major_Roads_BL", "Close_To_Major_Road_BL",
    "Sum_Major_Road_Length_100m_BL", "NO2_Air_Pollution_2005_BL",
    "NO2_Air_Pollution_2006_BL", "NO2_Air_Pollution_2007_BL",
    "PM10_Air_Pollution_2007_BL", "Avg_Daytime_Sound_Level_BL",
    "Avg_Evening_Sound_Level_BL", "Avg_Nighttime_Sound_Level_BL",
    "Greenspace_Percentage_1000m_BL", "Domestic_Garden_Percentage_1000m_BL",
    "Water_Percentage_1000m_BL", "Greenspace_Percentage_300m_BL",
    "Domestic_Garden_Percentage_300m_BL", "Water_Percentage_300m_BL",
    "Natural_Env_Percentage_1000m_BL", "Natural_Env_Percentage_300m_BL",
    "Distance_To_Coast_Euclidean_BL"
  )
)

#BL阶段-------------
trauma_variables_list <- list(
  Eco_Social = c(
    "HH_Num_Vehicle_BL",  "HH_Income_BL",  "HH_Own_Rent_BL",  "Social_Freq_Visits_BL",  "Social_Able_Confide_BL",
    "Num_People_Living_BL",  "Loneliness_BL"),
  Early_risk = c(
    "Breastfed_Baby_BL",  "Comp_Body_Size_Age_10_BL",  "Comp_Height_Size_Age_10_BL",
    "Maternal_Smoking_Birth_BL",
    "Felt_Loved_As_Child_FU1", "Phys_Abused_As_Child_FU1", "Felt_Hated_As_Child_FU1",
    "Sex_Molested_As_Child_FU1", "Someone_Take_To_Doctor_As_Child_FU1"
  ),
  Enviroment = c(
    "NO2_Air_Pollution_2010_BL", "NOx_Air_Pollution_2010_BL",
    "PM10_Air_Pollution_2010_BL", "PM2.5_Air_Pollution_2010_BL",
    "PM2.5_Absorbance_2010_BL", "PM2.5_10um_Air_Pollution_2010_BL",
    "Traffic_Intensity_Nearest_Road_BL", "Inv_Dist_Nearest_Road_BL",
    "Traffic_Intensity_Nearest_Major_Road_BL", "Inv_Dist_Nearest_Major_Road_BL",
    "Total_Traffic_Load_Major_Roads_BL", "Close_To_Major_Road_BL",
    "Sum_Major_Road_Length_100m_BL", "NO2_Air_Pollution_2005_BL",
    "NO2_Air_Pollution_2006_BL", "NO2_Air_Pollution_2007_BL",
    "PM10_Air_Pollution_2007_BL", "Avg_Daytime_Sound_Level_BL",
    "Avg_Evening_Sound_Level_BL", "Avg_Nighttime_Sound_Level_BL",
    "Greenspace_Percentage_1000m_BL", "Domestic_Garden_Percentage_1000m_BL",
    "Water_Percentage_1000m_BL", "Greenspace_Percentage_300m_BL",
    "Domestic_Garden_Percentage_300m_BL", "Water_Percentage_300m_BL",
    "Natural_Env_Percentage_1000m_BL", "Natural_Env_Percentage_300m_BL",
    "Distance_To_Coast_Euclidean_BL"
  )
)

trauma_variables_list <- list(
  Eco_Social = c(
    "HH_Income_BL","HH_Num_Vehicle_BL","Financial_Difficulties_BL"))
trauma_variables_list <- list(
  Eco_Social = c(
    "IMD"))
trauma_variables_list <- list(
  Eco_Social = c(
    "Frailty_BL"))


trauma_variables_list <- list(
  Eco_Social = c(
    "Dist_Home_Job_Workplace_BL",  "Job_Walk_Stand_BL",  "Job_Heavy_Manual_Work_BL","Job_Shift_Work_BL","Job_Night_Shift_BL"))
#满意度：Health_Satisfaction_BL, Financial_Situation_Satisfaction_BL, Work_Job_Satisfaction_BL
sati <- "Health_Satisfaction_BL"


#进行匹配分析------------------------
resilience_group_R1 <- resilience_group_R[resilience_group_R$self_resilience != 1, ]
resilience_test <- resilience_group_R1 %>% 
  transmute(
    eid,
    Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")),
    Age = age_BL,
    Ethnic = factor(Ethnic_group, levels = c("0", "1", "2", "3"), labels = c("1_White", "2_Asian", "3_Black", "4_Other")),
    Education = Education_year,
    BMI = BMI_BL,
    selfresilience = factor(self_resilience, levels = c(0, 2), labels = c("1_Low", "3_High")),
    HH_Income_BL, HH_Num_Vehicle_BL, IMD, Financial_Difficulties_BL,
    Dist_Home_Job_Workplace_BL,  Job_Walk_Stand_BL,  Job_Heavy_Manual_Work_BL,Job_Shift_Work_BL,
    Frailty_BL,
    Financial_Situation_Satisfaction_BL,Work_Job_Satisfaction_BL, Health_Satisfaction_BL
  )

resilience_test <- na.omit(resilience_test)

model <- lm(Financial_Situation_Satisfaction_BL ~ IMD, data = resilience_test)
summary(model)

library(MatchIt)
m.out0 <- matchit(selfresilience ~ HH_Income_BL+ HH_Num_Vehicle_BL +IMD+ Financial_Difficulties_BL+
                    Dist_Home_Job_Workplace_BL+  Job_Walk_Stand_BL+  Job_Heavy_Manual_Work_BL+Job_Shift_Work_BL+ Frailty_BL +
                    Age + Sex + Ethnic + BMI + Education, data = resilience_test,
                  method = "nearest", distance = "glm", caliper = 0.001)
summary(m.out0)

resilience_test <- match.data(m.out0)
resilience_test <- subset(resilience_test, weights == 1)

resilience_test_sum <- resilience_test[,-c(1,17,18,19,20,21,22)]
resilience_test_sum$Financial_Difficulties_BL <- as.factor(resilience_test_sum$Financial_Difficulties_BL)
#resilience_test$HH_Own_Rent_BL <- as.factor(resilience_test$HH_Own_Rent_BL)
library(gtsummary)
resilience_test_sum %>% tbl_summary(by = selfresilience, digits = list(Age ~ 0),type = list(where(is.numeric) ~ "continuous")) %>% add_p() %>%   add_overall()




model <- lm(Financial_Situation_Satisfaction_BL ~ selfresilience, data = resilience_test)
summary(model)

model <- lm(Work_Job_Satisfaction_BL ~ selfresilience, data = resilience_test)
summary(model)
model <- lm(Work_Job_Satisfaction_BL ~ selfresilience, data = resilience_test)
summary(model)


library(ggplot2)
library(ggpubr)


library(tidyverse)
library(rstatix)



plot_dat <- resilience_test[,c(7,17,18,19)]


stat.test <- plot_dat %>% pivot_longer(-selfresilience) %>%
  mutate(group=str_sub(name,start = 1,end = 4)) %>% 
  group_by(group,name) %>% 
  t_test(value ~ selfresilience) %>%
  adjust_pvalue() %>% add_significance("p.adj") %>% 
  add_xy_position(x="name",scales="free",fun = "max") %>% 
  select(-3,-6,-7,-8,-9,-10) %>% 
  mutate(across("xmin",str_replace,"1.8","0.8"),
         across("xmin",str_replace,"2.8","0.8"),
         across("xmax",str_replace,"2.2","1.2"),
         across("xmax",str_replace,"3.2","1.2")) %>% 
  mutate(xmin=as.numeric(xmin),xmax=as.numeric(xmax))



plot_dat %>% pivot_longer(-selfresilience) %>%
  mutate(group = str_sub(name, start = 1, end = 4)) %>% 
  ggplot(aes(x = name, y = value)) +
  stat_summary(geom = "bar", position = "dodge", aes(fill = selfresilience), width = 0.5) +
  stat_summary(geom = "errorbar", fun.data = "mean_cl_normal",
               aes(fill = selfresilience), position = position_dodge(width = 0.5),
               width = 0.2, color = "black") +
  stat_pvalue_manual(stat.test, label = "p.adj.signif", label.size = 6, hide.ns = TRUE,
                     tip.length = 0.01, position = position_dodge(width = 0.5)) +
  facet_wrap(. ~ group, scale = "free_x", nrow = 1) +
  labs(x = NULL, y = NULL) +
  scale_fill_manual(values = c("#BA7A70", "#829BAB")) +
  scale_y_continuous(limits = c(0, 6), expand = c(0, 0)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(color = "black", size = 12, margin = margin(r = 3)),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 10, margin = margin(r = 2)),
        axis.text.x = element_text(color = "black"),
        panel.background = element_rect(fill = NA, color = NA),
        panel.grid.minor = element_line(size = 0.2, color = "#e5e5e5"),
        panel.grid.major = element_line(size = 0.2, color = "#e5e5e5"),
        panel.border = element_rect(fill = NA, color = "black", size = 0.3, linetype = "solid"),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 8),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.key.width = unit(0.5, 'cm'),
        legend.key.height = unit(0.5, 'cm'),
        legend.position = c(1, 1), legend.justification = c(1, 1),
        legend.background = element_blank(),
        legend.box.margin = margin(0, 0, 0, 0),
        strip.text = element_text(color = "black", size = 10),
        panel.spacing.x = unit(0.3, "cm"))


#删除工作满意度进行匹配---------
resilience_test <- resilience_group_R1 %>% 
  transmute(
    eid,
    Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")),
    Age = age_BL,
    Ethnic = factor(Ethnic_group, levels = c("0", "1", "2", "3"), labels = c("1_White", "2_Asian", "3_Black", "4_Other")),
    Education = Education_year,
    BMI = BMI_BL,
    selfresilience = factor(self_resilience, levels = c(0, 2), labels = c("1_Low", "3_High")),
    HH_Income_BL, HH_Num_Vehicle_BL, IMD, Financial_Difficulties_BL,
    Frailty_BL,
    Financial_Situation_Satisfaction_BL, Health_Satisfaction_BL
  )
resilience_test <- na.omit(resilience_test)


library(MatchIt)
m.out0 <- matchit(selfresilience ~ HH_Income_BL+ HH_Num_Vehicle_BL +IMD+ Financial_Difficulties_BL+ Frailty_BL +
                    Age + Sex + Ethnic + BMI + Education, data = resilience_test,
                  method = "nearest", distance = "glm", caliper = 0.001)
summary(m.out0)

resilience_test <- match.data(m.out0)
resilience_test <- subset(resilience_test, weights == 1)

resilience_test_sum <- resilience_test %>%
  transmute(Sex, Age, Ethnic,Education, BMI, HH_Income_BL, HH_Num_Vehicle_BL, IMD, Financial_Difficulties_BL,
            Frailty_BL,selfresilience)
resilience_test_sum$Financial_Difficulties_BL <- as.factor(resilience_test_sum$Financial_Difficulties_BL)
#resilience_test$HH_Own_Rent_BL <- as.factor(resilience_test$HH_Own_Rent_BL)
library(gtsummary)
matched_group <- resilience_test_sum %>% tbl_summary(by = selfresilience, digits = list(Age ~ 0),type = list(where(is.numeric) ~ "continuous")) %>% add_p() %>%   add_overall()
matched_group_table <- as_tibble(matched_group)



model <- lm(Financial_Situation_Satisfaction_BL ~ selfresilience, data = resilience_test)
summary(model)



library(ggplot2)
library(ggpubr)


library(tidyverse)
library(rstatix)



plot_dat <- resilience_test %>%
  transmute(selfresilience, Financial_Situation_Satisfaction_BL, Health_Satisfaction_BL)


stat.test <- plot_dat %>% pivot_longer(-selfresilience) %>%
  mutate(group=str_sub(name,start = 1,end = 4)) %>% 
  group_by(group,name) %>% 
  t_test(value ~ selfresilience) %>%
  adjust_pvalue() %>% add_significance("p.adj") %>% 
  add_xy_position(x="name",scales="free",fun = "max") %>% 
  select(-3,-6,-7,-8,-9,-10) %>% 
  mutate(across("xmin",str_replace,"1.8","0.8"),
         across("xmin",str_replace,"2.8","0.8"),
         across("xmax",str_replace,"2.2","1.2"),
         across("xmax",str_replace,"3.2","1.2")) %>% 
  mutate(xmin=as.numeric(xmin),xmax=as.numeric(xmax))



plot_dat %>% pivot_longer(-selfresilience) %>%
  mutate(group = str_sub(name, start = 1, end = 4)) %>% 
  ggplot(aes(x = name, y = value)) +
  stat_summary(geom = "bar", position = position_dodge(width = 0.7), aes(fill = selfresilience), width = 0.4) +
  stat_summary(geom = "errorbar", fun.data = "mean_cl_normal",
               aes(fill = selfresilience), position = position_dodge(width = 0.7),
               width = 0.2, color = "black") +
  stat_pvalue_manual(stat.test, label = "p.adj.signif", label.size = 6, hide.ns = TRUE,
                     tip.length = 0.01, position = position_dodge(width = 0.7)) +
  facet_wrap(. ~ group, scale = "free_x", nrow = 1) +
  labs(x = NULL, y = NULL) +
  scale_fill_manual(values = c("#fA9E38", "#4995C6")) +
  scale_y_continuous(limits = c(0, 6), expand = c(0, 0)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(color = "black", size = 12, margin = margin(r = 3)),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 10, margin = margin(r = 2)),
        axis.text.x = element_text(color = "black"),
        panel.background = element_rect(fill = NA, color = NA),
        panel.grid.minor = element_line(size = 0.2, color = "#e5e5e5"),
        panel.grid.major = element_line(size = 0.2, color = "#e5e5e5"),
        panel.border = element_rect(fill = NA, color = "black", size = 0.3, linetype = "solid"),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(color = "black", size = 8),
        legend.spacing.x = unit(0.1, 'cm'),
        legend.key.width = unit(0.5, 'cm'),
        legend.key.height = unit(0.5, 'cm'),
        legend.position = c(1, 1), legend.justification = c(1, 1),
        legend.background = element_blank(),
        legend.box.margin = margin(0, 0, 0, 0),
        strip.text = element_text(color = "black", size = 10),
        panel.spacing.x = unit(0.3, "cm"))

