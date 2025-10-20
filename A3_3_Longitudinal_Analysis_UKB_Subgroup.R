#进行匹配-----------------
#PHQ---------
resilience_test <- resilience_group_R %>% # 创建新数据集新变量  
  transmute(eid, Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")), Age = age_BL, 
            Ethnic = factor(Ethnic_group, levels = c("0","1","2","3"), labels = c("1_White","2_Asian","3_Black","4_Other")),  
            Education = Education_year,
            BMI = BMI_BL,
            selfresilience = factor(self_resilience, levels = c(0,1,2), labels = c("1_Low","2_Medium","3_High")),
            data_FU1 = Mental_onine_date_FU1, data_FU2 = Mental_onine_date_FU2, PHQ_FU1 = PHQ9_Severity_FU1,
            PHQ_FU2 = `PHQ-9_FU2`,
            GAD_FU1 = General_Anxiety_Disorder_Severity_FU1,
            GAD_FU2 = General_Anxiety_Disorder_Severity_FU2,
  ) %>%
  filter(selfresilience != "2_Medium") %>% # 删除 selfresilience 为 "2_Medium" 的行
  mutate(selfresilience = droplevels(selfresilience))  # 重置因子水平
resilience_test <- na.omit(resilience_test)


resilience_test$FU1_severity <- NA
resilience_test$FU1_severity[resilience_test$PHQ_FU1 >= 10] <- "3" #高风险群体
resilience_test$FU1_severity[resilience_test$PHQ_FU1 >= 5 & resilience_test$PHQ_FU1 < 10] <- "2" #中风险群体
resilience_test$FU1_severity[resilience_test$PHQ_FU1 < 5] <- "1" #低风险群体

resilience_test$FU2_severity <- NA
resilience_test$FU2_severity[resilience_test$PHQ_FU2 >= 10] <- "3"  #高风险群体
resilience_test$FU2_severity[resilience_test$PHQ_FU2 >= 5 & resilience_test$PHQ_FU2 < 10] <- "2"  #中风险群体
resilience_test$FU2_severity[resilience_test$PHQ_FU2 < 5] <- "1"  #低风险群体

#高风险人群
data_high <- resilience_test %>%
  filter(FU1_severity == "1")
m.out1 <- matchit(selfresilience ~ PHQ_FU1 + GAD_FU1 + Age + Sex + Ethnic + BMI + Education, data = data_high,
                  method = "nearest", distance = "glm", caliper = 0.000001)
summary(m.out1)

data_high_matched <- match.data(m.out1)
data_high_matched <- subset(data_high_matched, weights == 1)
high_PHQ_matched <- data_high_matched %>%
  transmute(eid, Sex, Age, Ethnic,  Education,BMI,selfresilience,PHQ_FU1, GAD_FU1)
matched_PHQ_high <- high_PHQ_matched %>% tbl_summary(by = selfresilience,
                                                     statistic = list(
                                                       all_continuous() ~ "{mean} ({sd})",
                                                       all_categorical() ~ "{n} ({p}%)"
                                                     ),
                                                     type = list(Age ~ "continuous", Education ~ "continuous",PHQ_FU1 ~ "continuous",GAD_FU1 ~ "continuous")  # 合并 type 选项
) %>%
  add_p() %>% 
  add_overall()
matched_PHQ_high
matched_PHQ_high_table <- as_tibble(matched_PHQ_high)

model_check <- lm(PHQ_FU1 ~ selfresilience, data = data_high_matched)
summary(model_check)

data_mid <- resilience_test %>%
  filter(FU1_severity == "2")
m.out2 <- matchit(selfresilience ~ PHQ_FU1 + GAD_FU1 + Age + Sex + Ethnic + BMI + Education, data = data_mid,
                  method = "nearest", distance = "glm", caliper = 0.001)
summary(m.out2)

data_mid_matched <- match.data(m.out2)
data_mid_matched <- subset(data_mid_matched, weights == 1)
mid_PHQ_matched <- data_mid_matched %>%
  transmute(eid, Sex, Age, Ethnic,  Education,BMI,selfresilience,PHQ_FU1, GAD_FU1)
matched_PHQ_mid <- mid_PHQ_matched %>% tbl_summary(by = selfresilience,
                                                   statistic = list(
                                                     all_continuous() ~ "{mean} ({sd})",
                                                     all_categorical() ~ "{n} ({p}%)"
                                                   ),
                                                   type = list(Age ~ "continuous", Education ~ "continuous",PHQ_FU1 ~ "continuous",GAD_FU1 ~ "continuous")  # 合并 type 选项
) %>%
  add_p() %>% 
  add_overall()
matched_PHQ_mid
matched_PHQ_mid_table <- as_tibble(matched_PHQ_mid)

model_check <- lm(PHQ_FU1 ~ selfresilience, data = data_mid_matched)
summary(model_check)

data_low <- resilience_test %>%
  filter(FU1_severity == "3")
m.out3 <- matchit(selfresilience ~ PHQ_FU1 + GAD_FU1 + Age + Sex + Ethnic + BMI + Education, data = data_low,
                  method = "nearest", distance = "glm", caliper = 0.001)
summary(m.out3)


data_low_matched <- match.data(m.out3)
data_low_matched <- subset(data_low_matched, weights == 1)
low_PHQ_matched <- data_low_matched %>%
  transmute(eid, Sex, Age, Ethnic,  Education,BMI,selfresilience,PHQ_FU1, GAD_FU1)
matched_PHQ_low <- low_PHQ_matched %>% tbl_summary(by = selfresilience,
                                                   statistic = list(
                                                     all_continuous() ~ "{mean} ({sd})",
                                                     all_categorical() ~ "{n} ({p}%)"
                                                   ),
                                                   type = list(Age ~ "continuous", Education ~ "continuous",PHQ_FU1 ~ "continuous",GAD_FU1 ~ "continuous")  # 合并 type 选项
) %>%
  add_p() %>% 
  add_overall()
matched_PHQ_low
matched_PHQ_low_table <- as_tibble(matched_PHQ_low)

model_check <- lm(PHQ_FU1 ~ selfresilience, data = data_low_matched)
summary(model_check)

resilience_matched_data <- bind_rows(data_high_matched, data_mid_matched, data_low_matched)



# 查看拼接后的数据框
head(resilience_matched_data)

#低韧性群体
resilience_matched_filter_data <- resilience_matched_data %>%
  filter(selfresilience == "1_Low")

# 使用 count 函数计算每种风险群体的人数
risk_counts <- resilience_matched_filter_data %>%
  count(FU1_severity, name = "Count")
print(risk_counts)
risk_counts <- resilience_matched_filter_data %>%
  count(FU2_severity, name = "Count")
print(risk_counts)


df <- resilience_matched_filter_data %>%
  transmute(FU1_severity,FU2_severity) %>%
  make_long(FU1_severity,FU2_severity)

p1 <- ggplot(df, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
  geom_alluvial(flow.alpha = .6, width = 0.06, space = 300) +
  geom_alluvial_text(size = 3, color = "black") +
  scale_fill_manual(values = c("#F2BC8A", "#E67548","#BE1F4E")) +
  #scale_fill_viridis_d(drop = FALSE) +
  theme_alluvial(base_size = 18) +
  labs(x = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5))

p1

#高韧性群体
resilience_matched_filter_data <- resilience_matched_data %>%
  filter(selfresilience == "3_High")

# 使用 count 函数计算每种风险群体的人数
risk_counts <- resilience_matched_filter_data %>%
  count(FU1_severity, name = "Count")
print(risk_counts)
risk_counts <- resilience_matched_filter_data %>%
  count(FU2_severity, name = "Count")
print(risk_counts)


df <- resilience_matched_filter_data %>%
  transmute(FU1_severity,FU2_severity) %>%
  make_long(FU1_severity,FU2_severity)

p2 <- ggplot(df, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
  geom_alluvial(flow.alpha = .6, width = 0.06, space = 300) +
  geom_alluvial_text(size = 3, color = "black") +
  scale_fill_manual(values = c("#F2BC8A", "#E67548","#BE1F4E")) +
  #scale_fill_viridis_d(drop = FALSE) +
  theme_alluvial(base_size = 18) +
  labs(x = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5))

p2

resilience_compare <- resilience_matched_data %>%
  mutate(
    recovery = if_else(FU2_severity < FU1_severity, 1, 0),
    worse = if_else(FU2_severity > FU1_severity, 1, 0))
plot_data <- resilience_compare %>% freq_table(selfresilience, FU1_severity,recovery) %>%
  filter(recovery == 1)
head(plot_data)


##---
# 函数用于两两卡方检验
pairwise_chisq_test <- function(data, group_var, compare_var) {
  # 获取组别
  groups <- levels(data[[group_var]])
  # 创建空的结果矩阵
  p_matrix <- matrix(NA, nrow = length(groups), ncol = length(groups), dimnames = list(groups, groups))
  
  for (i in 1:(length(groups) - 1)) {
    for (j in (i + 1):length(groups)) {
      subset_data <- data %>% filter(data[[group_var]] %in% c(groups[i], groups[j]))
      subset_data[[group_var]] <- droplevels(subset_data[[group_var]])  # 移除未使用的因子水平
      table_data <- table(subset_data[[group_var]], subset_data[[compare_var]])
      
      if (all(table_data > 0)) {  # 确保表中没有零值行或列
        test_result <- chisq.test(table_data, correct = FALSE)
        #test_result <- fisher.test(table_data)
        p_matrix[i, j] <- test_result$p.value
        p_matrix[j, i] <- test_result$p.value
      } else {
        p_matrix[i, j] <- NA
        p_matrix[j, i] <- NA
      }
    }
  }
  return(p_matrix)
}

##检验恢复率--------
# 函数用于两两卡方检验
pairwise_chisq_test <- function(data, group_var, compare_var) {
  # 获取组别
  groups <- levels(data[[group_var]])
  # 创建空的结果矩阵
  p_matrix <- matrix(NA, nrow = length(groups), ncol = length(groups), dimnames = list(groups, groups))
  
  for (i in 1:(length(groups) - 1)) {
    for (j in (i + 1):length(groups)) {
      subset_data <- data %>% filter(data[[group_var]] %in% c(groups[i], groups[j]))
      subset_data[[group_var]] <- droplevels(subset_data[[group_var]])  # 移除未使用的因子水平
      table_data <- table(subset_data[[group_var]], subset_data[[compare_var]])
      
      if (all(table_data > 0)) {  # 确保表中没有零值行或列
        test_result <- chisq.test(table_data, correct = FALSE)
        #test_result <- fisher.test(table_data)
        p_matrix[i, j] <- test_result$p.value
        p_matrix[j, i] <- test_result$p.value
      } else {
        p_matrix[i, j] <- NA
        p_matrix[j, i] <- NA
      }
    }
  }
  return(p_matrix)
}

#高风险群体-----
resilience_compare_high <- resilience_compare[resilience_compare$FU1_severity == 3,]
# 创建空列表保存结果
pairwise_p_values <- list()

# 变量列表
variables <- c( "recovery")#,"worse")

# 循环遍历每个变量
for (var in variables) {
  p_values <- pairwise_chisq_test(resilience_compare_high, "selfresilience", var)
  pairwise_p_values[[var]] <- p_values
}

# 打印结果
print(pairwise_p_values)

#中风险群体-----
resilience_compare_mid <- resilience_compare[resilience_compare$FU1_severity == 2,]
# 创建空列表保存结果
pairwise_p_values <- list()

# 变量列表
variables <- c( "recovery","worse")

# 循环遍历每个变量
for (var in variables) {
  p_values <- pairwise_chisq_test(resilience_compare_mid, "selfresilience", var)
  pairwise_p_values[[var]] <- p_values
}

# 打印结果
print(pairwise_p_values)


# 将数据整理成长格式
compare_dat <- resilience_compare %>%
  transmute(recovery, FU1_severity, selfresilience) %>%
  filter(FU1_severity != "1")

proportion_results <- compare_dat %>%
  group_by(selfresilience, FU1_severity) %>%  # 按照 selfresilience 和 FU1_severity 分组
  summarize(
    total = n(),                              # 统计每组总数
    recovered = sum(recovery == 1),           # 统计 recovery 为 1 的数量
    proportion = recovered / total            # 计算比例
  ) %>%
  ungroup()

# 查看恢复的比例结果
print(proportion_results)

# 将Variable转换为因子，并指定级别顺序
compare_dat$FU1_severity <- factor(compare_dat$FU1_severity, levels = c("3","2"), label = c("High-Serverity", "Medium-Serverity"))

bar_1 <- ggplot(data = compare_dat,
                aes(x=FU1_severity, y=recovery, fill=selfresilience)) +
  geom_bar(stat = "summary", fun="mean",
           position = position_dodge(), width = 0.6, color="black") +
  stat_summary(geom = "errorbar", position = position_dodge(width = 0.6),
               width = 0.15) +
  scale_fill_manual(values = c("#fA9E38","#4995C6"), name=NULL) +
  scale_x_discrete(limits = rev(levels(compare_dat$FU1_severity))) + # 反转x轴顺序
  scale_y_continuous(expand = c(0, 0), # 修饰y轴的多个部分
                     #limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.2),
                     labels = scales::number_format(accuracy = 0.1)) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic() +
  theme( 
    legend.position = c(0.5, 0.95), # 修饰legend
    legend.title = element_text(face = "bold", hjust = 0.5),
    legend.text = element_text(margin = margin(1, 2, 1, 2)),
    legend.key.size = unit(0.4, "cm"),
    legend.spacing = unit(1, 'cm'),
    axis.title = element_text(face = "bold"), # 修饰坐标轴
    axis.line = element_line(size = 0.6),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_line(size = 0.6),
    axis.ticks.length.y = unit(0.2, "cm"),
    axis.text.x = element_text(face = "bold", colour = "black", size = 10)
  ) +
  guides(fill = guide_legend(nrow = 1)) + # legend中排列
  labs(
    x = "",
    y = "",
    fill = ""
  )
bar_1

#worse
# 将数据整理成长格式
compare_dat <- resilience_compare %>%
  transmute(worse, FU1_severity, selfresilience) %>%
  filter(FU1_severity != "3")

# 将Variable转换为因子，并指定级别顺序
compare_dat$FU1_severity <- factor(compare_dat$FU1_severity, levels = c("2","1"), label = c("Medium-Serverity","Low-Serverity"))

bar_2 <- ggplot(data = compare_dat,
                aes(x=FU1_severity, y=worse, fill=selfresilience)) +
  geom_bar(stat = "summary", fun="mean",
           position = position_dodge(), width = 0.6, color="black") +
  stat_summary(geom = "errorbar", position = position_dodge(width = 0.6),
               width = 0.15) +
  scale_fill_manual(values = c("#fA9E38","#4995C6"), name=NULL) +
  scale_x_discrete(limits = rev(levels(compare_dat$FU1_severity))) + # 反转x轴顺序
  scale_y_continuous(expand = c(0, 0), # 修饰y轴的多个部分
                     #limits = c(0, 0.4),
                     breaks = seq(0, 1, by = 0.2),
                     labels = scales::number_format(accuracy = 0.1)) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic() +
  theme( 
    legend.position = c(0.5, 0.95), # 修饰legend
    legend.title = element_text(face = "bold", hjust = 0.5),
    legend.text = element_text(margin = margin(1, 2, 1, 2)),
    legend.key.size = unit(0.4, "cm"),
    legend.spacing = unit(1, 'cm'),
    axis.title = element_text(face = "bold"), # 修饰坐标轴
    axis.line = element_line(size = 0.6),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_line(size = 0.6),
    axis.ticks.length.y = unit(0.2, "cm"),
    axis.text.x = element_text(face = "bold", colour = "black", size = 10)
  ) +
  guides(fill = guide_legend(nrow = 1)) + # legend中排列
  labs(
    x = "",
    y = "",
    fill = ""
  )
bar_2


##合并2个图----

# 合并四个图
combined_plot <- grid.arrange(bar_1, bar_2, ncol = 1)

# 显示合并后的图
print(combined_plot)


#GAD---------
resilience_test <- resilience_group_R %>% # 创建新数据集新变量  
  transmute(eid, Sex = factor(gender, levels = c(0, 1), labels = c("Female", "Male")), Age = age_BL, 
            Ethnic = factor(Ethnic_group, levels = c("0","1","2","3"), labels = c("1_White","2_Asian","3_Black","4_Other")),  
            Education = Education_year,
            BMI = BMI_BL,
            selfresilience = factor(self_resilience, levels = c(0,1,2), labels = c("1_Low","2_Medium","3_High")),
            data_FU1 = Mental_onine_date_FU1, data_FU2 = Mental_onine_date_FU2, PHQ_FU1 = PHQ9_Severity_FU1,
            PHQ_FU2 = `PHQ-9_FU2`,
            GAD_FU1 = General_Anxiety_Disorder_Severity_FU1,
            GAD_FU2 = General_Anxiety_Disorder_Severity_FU2,
  ) %>%
  filter(selfresilience != "2_Medium") %>% # 删除 selfresilience 为 "2_Medium" 的行
  mutate(selfresilience = droplevels(selfresilience))  # 重置因子水平
resilience_test <- na.omit(resilience_test)


resilience_test$FU1_severity <- NA
resilience_test$FU1_severity[resilience_test$GAD_FU1 >= 10] <- "3" #高风险群体
resilience_test$FU1_severity[resilience_test$GAD_FU1 >= 5 & resilience_test$GAD_FU1 < 10] <- "2" #中风险群体
resilience_test$FU1_severity[resilience_test$GAD_FU1 < 5] <- "1" #低风险群体

resilience_test$FU2_severity <- NA
resilience_test$FU2_severity[resilience_test$GAD_FU2 >= 10] <- "3"  #高风险群体
resilience_test$FU2_severity[resilience_test$GAD_FU2 >= 5 & resilience_test$GAD_FU2 < 10] <- "2"  #中风险群体
resilience_test$FU2_severity[resilience_test$GAD_FU2 < 5] <- "1"  #低风险群体

#高风险人群
data_high <- resilience_test %>%
  filter(FU1_severity == "1")
m.out1 <- matchit(selfresilience ~ PHQ_FU1 + GAD_FU1 + Age + Sex + Ethnic + BMI + Education, data = data_high,
                  method = "nearest", distance = "glm", caliper = 0.000001)
summary(m.out1)

data_high_matched <- match.data(m.out1)
data_high_matched <- subset(data_high_matched, weights == 1)
high_PHQ_matched <- data_high_matched %>%
  transmute(eid, Sex, Age, Ethnic,  Education,BMI,selfresilience,PHQ_FU1, GAD_FU1)
matched_PHQ_high <- high_PHQ_matched %>% tbl_summary(by = selfresilience,
                                                     statistic = list(
                                                       all_continuous() ~ "{mean} ({sd})",
                                                       all_categorical() ~ "{n} ({p}%)"
                                                     ),
                                                     type = list(Age ~ "continuous", Education ~ "continuous",PHQ_FU1 ~ "continuous",GAD_FU1 ~ "continuous")  # 合并 type 选项
) %>%
  add_p() %>% 
  add_overall()
matched_PHQ_high
matched_PHQ_high_table <- as_tibble(matched_PHQ_high)

model_check <- lm(PHQ_FU1 ~ selfresilience, data = data_high_matched)
summary(model_check)

data_mid <- resilience_test %>%
  filter(FU1_severity == "2")
m.out2 <- matchit(selfresilience ~ PHQ_FU1 + GAD_FU1 + Age + Sex + Ethnic + BMI + Education, data = data_mid,
                  method = "nearest", distance = "glm", caliper = 0.001)
summary(m.out2)

data_mid_matched <- match.data(m.out2)
data_mid_matched <- subset(data_mid_matched, weights == 1)
mid_PHQ_matched <- data_mid_matched %>%
  transmute(eid, Sex, Age, Ethnic,  Education,BMI,selfresilience,PHQ_FU1, GAD_FU1)
matched_PHQ_mid <- mid_PHQ_matched %>% tbl_summary(by = selfresilience,
                                                   statistic = list(
                                                     all_continuous() ~ "{mean} ({sd})",
                                                     all_categorical() ~ "{n} ({p}%)"
                                                   ),
                                                   type = list(Age ~ "continuous", Education ~ "continuous",PHQ_FU1 ~ "continuous",GAD_FU1 ~ "continuous")  # 合并 type 选项
) %>%
  add_p() %>% 
  add_overall()
matched_PHQ_mid
matched_PHQ_mid_table <- as_tibble(matched_PHQ_mid)

model_check <- lm(PHQ_FU1 ~ selfresilience, data = data_mid_matched)
summary(model_check)

data_low <- resilience_test %>%
  filter(FU1_severity == "3")
m.out3 <- matchit(selfresilience ~ PHQ_FU1 + GAD_FU1 + Age + Sex + Ethnic + BMI + Education, data = data_low,
                  method = "nearest", distance = "glm", caliper = 0.001)
summary(m.out3)


data_low_matched <- match.data(m.out3)
data_low_matched <- subset(data_low_matched, weights == 1)
low_PHQ_matched <- data_low_matched %>%
  transmute(eid, Sex, Age, Ethnic,  Education,BMI,selfresilience,PHQ_FU1, GAD_FU1)
matched_PHQ_low <- low_PHQ_matched %>% tbl_summary(by = selfresilience,
                                                   statistic = list(
                                                     all_continuous() ~ "{mean} ({sd})",
                                                     all_categorical() ~ "{n} ({p}%)"
                                                   ),
                                                   type = list(Age ~ "continuous", Education ~ "continuous",PHQ_FU1 ~ "continuous",GAD_FU1 ~ "continuous")  # 合并 type 选项
) %>%
  add_p() %>% 
  add_overall()
matched_PHQ_low
matched_PHQ_low_table <- as_tibble(matched_PHQ_low)
model_check <- lm(GAD_FU1 ~ selfresilience, data = data_low_matched)
summary(model_check)

resilience_matched_data <- bind_rows(data_high_matched, data_mid_matched, data_low_matched)



# 查看拼接后的数据框
head(resilience_matched_data)

#低韧性群体
resilience_matched_filter_data <- resilience_matched_data %>%
  filter(selfresilience == "1_Low")

# 使用 count 函数计算每种风险群体的人数
risk_counts <- resilience_matched_filter_data %>%
  count(FU1_severity, name = "Count")
print(risk_counts)
risk_counts <- resilience_matched_filter_data %>%
  count(FU2_severity, name = "Count")
print(risk_counts)


df <- resilience_matched_filter_data %>%
  transmute(FU1_severity,FU2_severity) %>%
  make_long(FU1_severity,FU2_severity)

p1 <- ggplot(df, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
  geom_alluvial(flow.alpha = .6, width = 0.06, space = 300) +
  geom_alluvial_text(size = 3, color = "black") +
  scale_fill_manual(values = c("#F2BC8A", "#E67548","#BE1F4E")) +
  #scale_fill_viridis_d(drop = FALSE) +
  theme_alluvial(base_size = 18) +
  labs(x = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5))

p1

#高韧性群体
resilience_matched_filter_data <- resilience_matched_data %>%
  filter(selfresilience == "3_High")

# 使用 count 函数计算每种风险群体的人数
risk_counts <- resilience_matched_filter_data %>%
  count(FU1_severity, name = "Count")
print(risk_counts)
risk_counts <- resilience_matched_filter_data %>%
  count(FU2_severity, name = "Count")
print(risk_counts)


df <- resilience_matched_filter_data %>%
  transmute(FU1_severity,FU2_severity) %>%
  make_long(FU1_severity,FU2_severity)

p2 <- ggplot(df, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
  geom_alluvial(flow.alpha = .6, width = 0.06, space = 300) +
  geom_alluvial_text(size = 3, color = "black") +
  scale_fill_manual(values = c("#F2BC8A", "#E67548","#BE1F4E")) +
  #scale_fill_viridis_d(drop = FALSE) +
  theme_alluvial(base_size = 18) +
  labs(x = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5))

p2

resilience_compare <- resilience_matched_data %>%
  mutate(
    recovery = if_else(FU2_severity < FU1_severity, 1, 0),
    worse = if_else(FU2_severity > FU1_severity, 1, 0))
plot_data <- resilience_compare %>% freq_table(selfresilience, FU1_severity,recovery) %>%
  filter(recovery == 1)
head(plot_data)


##---
# 函数用于两两卡方检验
pairwise_chisq_test <- function(data, group_var, compare_var) {
  # 获取组别
  groups <- levels(data[[group_var]])
  # 创建空的结果矩阵
  p_matrix <- matrix(NA, nrow = length(groups), ncol = length(groups), dimnames = list(groups, groups))
  
  for (i in 1:(length(groups) - 1)) {
    for (j in (i + 1):length(groups)) {
      subset_data <- data %>% filter(data[[group_var]] %in% c(groups[i], groups[j]))
      subset_data[[group_var]] <- droplevels(subset_data[[group_var]])  # 移除未使用的因子水平
      table_data <- table(subset_data[[group_var]], subset_data[[compare_var]])
      
      if (all(table_data > 0)) {  # 确保表中没有零值行或列
        test_result <- chisq.test(table_data, correct = FALSE)
        #test_result <- fisher.test(table_data)
        p_matrix[i, j] <- test_result$p.value
        p_matrix[j, i] <- test_result$p.value
      } else {
        p_matrix[i, j] <- NA
        p_matrix[j, i] <- NA
      }
    }
  }
  return(p_matrix)
}

##检验恢复率--------
# 函数用于两两卡方检验
pairwise_chisq_test <- function(data, group_var, compare_var) {
  # 获取组别
  groups <- levels(data[[group_var]])
  # 创建空的结果矩阵
  p_matrix <- matrix(NA, nrow = length(groups), ncol = length(groups), dimnames = list(groups, groups))
  
  for (i in 1:(length(groups) - 1)) {
    for (j in (i + 1):length(groups)) {
      subset_data <- data %>% filter(data[[group_var]] %in% c(groups[i], groups[j]))
      subset_data[[group_var]] <- droplevels(subset_data[[group_var]])  # 移除未使用的因子水平
      table_data <- table(subset_data[[group_var]], subset_data[[compare_var]])
      
      if (all(table_data > 0)) {  # 确保表中没有零值行或列
        test_result <- chisq.test(table_data, correct = FALSE)
        #test_result <- fisher.test(table_data)
        p_matrix[i, j] <- test_result$p.value
        p_matrix[j, i] <- test_result$p.value
      } else {
        p_matrix[i, j] <- NA
        p_matrix[j, i] <- NA
      }
    }
  }
  return(p_matrix)
}

#高风险群体-----
resilience_compare_high <- resilience_compare[resilience_compare$FU1_severity == 3,]
# 创建空列表保存结果
pairwise_p_values <- list()

# 变量列表
variables <- c( "recovery")#,"worse")

# 循环遍历每个变量
for (var in variables) {
  p_values <- pairwise_chisq_test(resilience_compare_high, "selfresilience", var)
  pairwise_p_values[[var]] <- p_values
}

# 打印结果
print(pairwise_p_values)

#中风险群体-----
resilience_compare_mid <- resilience_compare[resilience_compare$FU1_severity == 2,]
# 创建空列表保存结果
pairwise_p_values <- list()

# 变量列表
variables <- c( "recovery","worse")

# 循环遍历每个变量
for (var in variables) {
  p_values <- pairwise_chisq_test(resilience_compare_mid, "selfresilience", var)
  pairwise_p_values[[var]] <- p_values
}

# 打印结果
print(pairwise_p_values)


# 将数据整理成长格式
compare_dat <- resilience_compare %>%
  transmute(recovery, FU1_severity, selfresilience) %>%
  filter(FU1_severity != "1")

proportion_results <- compare_dat %>%
  group_by(selfresilience, FU1_severity) %>%  # 按照 selfresilience 和 FU1_severity 分组
  summarize(
    total = n(),                              # 统计每组总数
    recovered = sum(recovery == 1),           # 统计 recovery 为 1 的数量
    proportion = recovered / total            # 计算比例
  ) %>%
  ungroup()

# 查看恢复的比例结果
print(proportion_results)

# 将Variable转换为因子，并指定级别顺序
compare_dat$FU1_severity <- factor(compare_dat$FU1_severity, levels = c("3","2"), label = c("High-Serverity", "Medium-Serverity"))

bar_1 <- ggplot(data = compare_dat,
                aes(x=FU1_severity, y=recovery, fill=selfresilience)) +
  geom_bar(stat = "summary", fun="mean",
           position = position_dodge(), width = 0.6, color="black") +
  stat_summary(geom = "errorbar", position = position_dodge(width = 0.6),
               width = 0.15) +
  scale_fill_manual(values = c("#fA9E38","#4995C6"), name=NULL) +
  scale_x_discrete(limits = rev(levels(compare_dat$FU1_severity))) + # 反转x轴顺序
  scale_y_continuous(expand = c(0, 0), # 修饰y轴的多个部分
                     #limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.2),
                     labels = scales::number_format(accuracy = 0.1)) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic() +
  theme( 
    legend.position = c(0.5, 0.95), # 修饰legend
    legend.title = element_text(face = "bold", hjust = 0.5),
    legend.text = element_text(margin = margin(1, 2, 1, 2)),
    legend.key.size = unit(0.4, "cm"),
    legend.spacing = unit(1, 'cm'),
    axis.title = element_text(face = "bold"), # 修饰坐标轴
    axis.line = element_line(size = 0.6),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_line(size = 0.6),
    axis.ticks.length.y = unit(0.2, "cm"),
    axis.text.x = element_text(face = "bold", colour = "black", size = 10)
  ) +
  guides(fill = guide_legend(nrow = 1)) + # legend中排列
  labs(
    x = "",
    y = "",
    fill = ""
  )
bar_1

#worse
# 将数据整理成长格式
compare_dat <- resilience_compare %>%
  transmute(worse, FU1_severity, selfresilience) %>%
  filter(FU1_severity != "3")

# 将Variable转换为因子，并指定级别顺序
compare_dat$FU1_severity <- factor(compare_dat$FU1_severity, levels = c("2","1"), label = c("Medium-Serverity","Low-Serverity"))

bar_2 <- ggplot(data = compare_dat,
                aes(x=FU1_severity, y=worse, fill=selfresilience)) +
  geom_bar(stat = "summary", fun="mean",
           position = position_dodge(), width = 0.6, color="black") +
  stat_summary(geom = "errorbar", position = position_dodge(width = 0.6),
               width = 0.15) +
  scale_fill_manual(values = c("#fA9E38","#4995C6"), name=NULL) +
  scale_x_discrete(limits = rev(levels(compare_dat$FU1_severity))) + # 反转x轴顺序
  scale_y_continuous(expand = c(0, 0), # 修饰y轴的多个部分
                     #limits = c(0, 0.4),
                     breaks = seq(0, 1, by = 0.2),
                     labels = scales::number_format(accuracy = 0.1)) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic() +
  theme( 
    legend.position = c(0.5, 0.95), # 修饰legend
    legend.title = element_text(face = "bold", hjust = 0.5),
    legend.text = element_text(margin = margin(1, 2, 1, 2)),
    legend.key.size = unit(0.4, "cm"),
    legend.spacing = unit(1, 'cm'),
    axis.title = element_text(face = "bold"), # 修饰坐标轴
    axis.line = element_line(size = 0.6),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_line(size = 0.6),
    axis.ticks.length.y = unit(0.2, "cm"),
    axis.text.x = element_text(face = "bold", colour = "black", size = 10)
  ) +
  guides(fill = guide_legend(nrow = 1)) + # legend中排列
  labs(
    x = "",
    y = "",
    fill = ""
  )
bar_2


##合并2个图----

# 合并四个图
combined_plot <- grid.arrange(bar_1, bar_2, ncol = 1)

# 显示合并后的图
print(combined_plot)
