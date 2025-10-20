
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

