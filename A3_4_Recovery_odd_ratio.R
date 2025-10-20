library(dplyr)
library(epitools)
library(tidyr)

# -----------------------------
# 两两计算 OR 和 p 值函数
# -----------------------------
pairwise_or_test <- function(data, group_var, compare_var) {
  groups <- levels(data[[group_var]])
  # 创建空矩阵
  or_matrix <- matrix(NA, nrow=length(groups), ncol=length(groups), dimnames=list(groups, groups))
  p_matrix <- matrix(NA, nrow=length(groups), ncol=length(groups), dimnames=list(groups, groups))
  
  for (i in 1:(length(groups)-1)) {
    for (j in (i+1):length(groups)) {
      subset_data <- data %>% filter(data[[group_var]] %in% c(groups[i], groups[j]))
      subset_data[[group_var]] <- droplevels(subset_data[[group_var]])
      tab <- table(subset_data[[group_var]], subset_data[[compare_var]])
      
      if(all(dim(tab) == c(2,2))) {
        or_val <- oddsratio(tab)$measure[2,1]   # OR
        test_result <- chisq.test(tab, correct = FALSE)
        or_matrix[i,j] <- or_val
        or_matrix[j,i] <- or_val
        p_matrix[i,j] <- test_result$p.value
        p_matrix[j,i] <- test_result$p.value
      } else {
        or_matrix[i,j] <- NA
        or_matrix[j,i] <- NA
        p_matrix[i,j] <- NA
        p_matrix[j,i] <- NA
      }
    }
  }
  
  # 转成长格式
  or_long <- as.data.frame(as.table(or_matrix))
  colnames(or_long) <- c(group_var, "CompareGroup", "OR")
  p_long <- as.data.frame(as.table(p_matrix))
  colnames(p_long) <- c(group_var, "CompareGroup", "P")
  
  # 合并 OR 和 P
  long_table <- left_join(or_long, p_long, by=c(group_var, "CompareGroup"))
  long_table$Variable <- compare_var
  return(long_table)
}

# -----------------------------
# 高风险群体
# -----------------------------
resilience_high <- resilience_compare %>% filter(FU1_severity == 3)
resilience_high$selfresilience <- factor(resilience_high$selfresilience)

variables <- c("recovery","worse")
high_results <- lapply(variables, function(var){
  pairwise_or_test(resilience_high, "selfresilience", var)
})
high_results <- bind_rows(high_results)  # 合并为一个表格
high_results$Severity <- "High"

# -----------------------------
# 中风险群体
# -----------------------------
resilience_mid <- resilience_compare %>% filter(FU1_severity == 2)
resilience_mid$selfresilience <- factor(resilience_mid$selfresilience)

variables <- c("recovery","worse")
mid_results <- lapply(variables, function(var){
  pairwise_or_test(resilience_mid, "selfresilience", var)
})
mid_results <- bind_rows(mid_results)
mid_results$Severity <- "Medium"

# -----------------------------
# 合并所有结果
# -----------------------------
all_results <- bind_rows(high_results, mid_results) %>%
  select(Severity, Variable, selfresilience, CompareGroup, OR, P)

# 查看结果
print(all_results)