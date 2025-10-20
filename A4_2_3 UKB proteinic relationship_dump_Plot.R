#BL阶段-------------



# 定义保存路径
file_path <- "/public/home/yangzy/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/blood_mediation_data_1203.csv"
# 导入数据
mediation_results_table <- read.csv(file_path)
mediation_results_table_short <- mediation_results_table[mediation_results_table$P_value < 0.001,]

file_path <- "/public/home/yangzy/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/blood_protein_both_result_175.csv"
# 将数据保存为 CSV 文件
both_result_adj <- read.csv(file_path)

# 计算分位数
quantiles <- quantile(resilience_group_R$self_resilience, probs = c(1/3, 2/3),na.rm = TRUE)

# 替换值
resilience_group_R$self_resilience[resilience_group_R$self_resilience <= quantiles[1]] <- 0
resilience_group_R$self_resilience[resilience_group_R$self_resilience > quantiles[1] & resilience_group_R$self_resilience < quantiles[2]] <- 1
resilience_group_R$self_resilience[resilience_group_R$self_resilience >= quantiles[2]] <- 2
resilience_group_R$self_resilience <- as.factor(resilience_group_R$self_resilience)


trauma_variables_list <- mediation_results_table_short$Biomarker
trauma_variables_list <- c("GUSB","PCBD1","PIGR")
trauma_variables_list <- c("NC30140","WBC30000","CRP30710")
trauma_variables_list <- c("Albumin", "ALP", "ALT", "CRP", "Cystatin C", "GGT", "HbA1c", 
               "HCT", "HDL_C", "HGB", "HLSR Percentage", "IGF_1", "IRF", 
               "Lymph Percentage", "MPV", "Neut Count", "Neut Percentage", 
               "Platelet Count", "RBC Count", "RDW", "TG", "Total Bili", 
               "Total Protein", "Vitamin D", "WBC Count")

trauma_variables_list <- gsub(" ", "_", trauma_variables_list)
trauma_var <- "Protein"
trauma_lines <- list()
trauma_reg <- list()
result_table <- data.frame()
trauma_effect_table <- data.frame()
interaction_effct_table <- data.frame()
resilience_effect_table <- data.frame()

# 初始化一个空的数据框，用于存储分组相关比较结果
correlation_results_BL_all <- data.frame(Trauma = character(),Category = character(), Timepoint = character(),Resilience_Level = character(), Correlation = numeric(), Subject_Count = integer(), stringsAsFactors = FALSE)
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
        selfresilience = factor(self_resilience, levels = c(0, 1, 2), labels = c("1_Low", "2_Medium", "3_High")),
        PHQ_BL = Depressive_Symptoms_PHQ4_BL,
        !!t := !!sym(t)
      ) %>%
     filter(abs(!!sym(t) - mean(!!sym(t), na.rm = TRUE)) <= 3 * sd(!!sym(t), na.rm = TRUE))
    resilience_test <- subset(resilience_test, selfresilience != "2_Medium")
    resilience_test <- na.omit(resilience_test)
    
    lme2 <- lm(
      as.formula(paste("PHQ_BL ~ selfresilience +", t, "+ selfresilience *", t, "+ Age + Sex + Ethnic + BMI + Education + site")),
      data = resilience_test
    )
    
    results <- tidy(lme2)
    
    # Add trauma variable as a column to identify results from each iteration
    results$Trauma_Variable <- t
    
    # Append the results to the result_table
    result_table <- bind_rows(result_table, results)
    
    #filtered_results <- results %>% filter(term %in% c("selfresilience2_Medium", "selfresilience3_High", t, paste0("selfresilience2_Medium:", t), paste0("selfresilience3_High:", t)))
    filtered_results <- results %>% filter(term %in% c(t))
    filtered_results$Trauma_Variable <- t
    trauma_effect_table <- bind_rows(trauma_effect_table, filtered_results)
    
    resilience_results <- results %>% filter(term %in% c("selfresilience3_High"))
    resilience_results$Trauma_Variable <- t
    resilience_effect_table <- bind_rows(resilience_effect_table, resilience_results)
    
    filtered_high_results <- results %>% filter(term %in% c(paste0("selfresilience3_High:", t)))
    filtered_high_results$Trauma_Variable <- t
    filtered_high_results$Stage <- trauma_var
    interaction_effct_table <- bind_rows(interaction_effct_table, filtered_high_results)
    
    # 创建变量名
    variable_name <- paste0("R_", t)
    
    # 创建图形并存储到列表中
    assign(
      variable_name,
      visreg(lme2, t, by = "selfresilience", overlay = TRUE, partial = FALSE, alpha = 0.05, gg = TRUE, band = TRUE, rug = FALSE, legend = FALSE, ylim = c(4, 8)) +
        theme(legend.position = "none")
    )
    trauma_reg[[length(trauma_reg) + 1]] <- get(variable_name)
    
    
    #进行相关分析的比较
    #比较不同组的创伤相关情况---------
    resilience <- c("1_Low","3_High")
    # 初始化一个空的数据框，用于存储分组相关比较结果
    correlation_results_BL <- data.frame(Trauma = character(), Timepoint = character(),Resilience_Level = character(), Correlation = numeric(), Subject_Count = integer(), stringsAsFactors = FALSE)
    
    
    # 循环遍历每个 resilience 水平
    for (resilience_level in resilience) {
      # 从数据中筛选出当前 resilience 水平的观测值
      #resilient_group <- resilience_test[resilience_test$selfresilience == resilience_level, ]
      
      # 在当前 resilience 水平下，计算线性模型
      #model_m <- lm(as.formula(paste("PHQ_BL ~  Age + Sex + Ethnic + BMI + Education + site")), 
      #              data = resilient_group)
      #model_m <- lm(
      #  as.formula(paste0("PHQ_BL ~ ", t," + Age+ Sex+ Ethnic + BMI + Education")), #, " + Age + Sex + Ethnic + BMI + Education + site")),
       # data = resilient_group)
      # 提取回归结果
      #results <- tidy(model_m)
      #trauma_results <- results %>% filter(term %in% t)
      
      # 计算自由度 (df = n - k - 1)
      #n <- nrow(resilient_group)  # 使用 resilient_group，与 model_m 一致
      #k <- length(coef(model_m)) - 1  # 自变量数量（不含截距），这里 k = 1
      #df <- n - k - 1  # 剩余自由度
      
      # 计算相关系数 r
      #trauma_results <- trauma_results %>%
      #  mutate(correlation_r = statistic / sqrt(statistic^2 + df))
      
      # 方法 1：从回归得到的 r
     # correlation <- trauma_results$correlation_r
      
      
      #旧做法
      resilient_group <- resilience_test
      # 在当前 resilience 水平下，计算线性模型
      model_m <- lm(as.formula(paste("PHQ_BL ~  Age + Sex + Ethnic + BMI + Education +site")), 
                    data = resilient_group)
      resilient_group$PHQ_BL_reg <- residuals(model_m)

      resilient_group <- resilient_group[resilient_group$selfresilience == resilience_level, ]
      
      correlation_model <- cor.test(resilient_group$PHQ_BL_reg, resilient_group[[t]])
      
      correlation <- correlation_model$estimate
      
      #cor_value <- cor.test(resilient_group$LPC30180, resilient_group$PHQ_BL)$estimate
      # 获取当前 resilience 水平的被试量
      subject_count <- nrow(resilient_group)
      
      # 将结果添加到数据框中
      correlation_results_BL <- rbind(correlation_results_BL, 
                                      data.frame(Trauma = t,
                                                 Category = trauma_var,
                                                 Timepoint = "BL",
                                                 Resilience_Level = resilience_level, 
                                                 Correlation = correlation, 
                                                 Subject_Count = subject_count))
    }
    
    # 打印结果
    #print(correlation_results_BL)
    
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
significant_count <- sum(sum_result_p$resilience_adjusted_p < 0.05)
cat("Number of resilience main effects p.value < 0.05:", significant_count, "\n")


significant_count <- sum(sum_result_p$trauma_adjusted_p < 0.05)
cat("Number of trauma effects p.value < 0.05:", significant_count, "\n")

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

#整理数据
cor_plot_dat <- merge(correlation_results_BL_all, comparison_results_BL_all[,c(1,5)], by = "Trauma", all.x = T)
z_plot_dat <- merge(comparison_results_BL_all,correlation_results_BL_all[,c(1,2)], by = "Trauma", all.x = T)
z_plot_dat <- unique(z_plot_dat)
#colnames(both_result_adj)[colnames(both_result_adj) == "Biomarker"] <- "Trauma"
#cor_plot_dat <- merge(cor_plot_dat, both_result_adj[,c(1,5)], by = "Trauma", all.x = T)



cor_plot_dat <- cor_plot_dat %>%
  arrange(factor(Resilience_Level, levels = c("1_Low","3_High")), desc(Correlation))

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


library(tidyr)
library(ggprism)

# 使用 pivot_wider() 函数将数据从长格式转换为宽格式
wide_cor_plot_dat <- correlation_results_BL_all[,-6]
wide_cor_plot_dat <- wide_cor_plot_dat %>%
  pivot_wider(names_from = Resilience_Level, values_from = Correlation)


#wide_cor_plot_dat <- wide_cor_plot_dat %>%
#  arrange(factor(Category, levels = c("Eco_Social", "Early_risk", "Enviroment")), desc(`1_Low`))

#wide_cor_plot_dat$Trauma <- factor(wide_cor_plot_dat$Trauma,
#                                   levels = unique(wide_cor_plot_dat$Trauma),
#                                   ordered = T)

#拼接画法（哑铃+z）--------------
#cor_plot_dat$Category <- factor(cor_plot_dat$Category, 
#                                levels = c("Eco_Social", "Early_risk", "Enviroment"),
#                                ordered = TRUE)
#z_plot_dat$Category <- factor(z_plot_dat$Category, 
#                              levels = c("Eco_Social", "Early_risk", "Enviroment"),
#                              ordered = TRUE)
# 拼接画法（哑铃+z）
dum_plot <- ggplot(cor_plot_dat, aes(x = Trauma, y = Correlation)) +
  geom_line(aes(group = Trauma), size = 1.5) + 
  geom_point(aes(color = Resilience_Level), size = 5) +
  scale_color_manual(values = c("#ff9900", "#146eb4"),
                     labels = c("Resilience Level 1", "Resilience Level 2")) +  
  labs(x = "", y = "Correlation Coefficient", title = "Baseline") +
  guides(color = FALSE) +
  #facet_grid(.~Category, scales = "free_x",space = 'free_x')+
  theme(panel.spacing=unit(0.5,"lines"),
        panel.border = element_blank(),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_blank(),
        
        panel.background = element_rect(color = "white", fill = NA),
        #panel.grid.major.y = element_line(color = "gray", linewidth = 0.5, linetype = "dashed"),
        panel.grid.major.x = element_line(color = "gray", linewidth = 0.5, 
                                          linetype = ifelse(cor_plot_dat$Z > 2.2, "solid", "dashed")),
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

