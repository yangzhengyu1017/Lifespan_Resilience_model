# ======================================================================
# Purpose:
# This script integrates normalized UK Biobank protein data with resilience data,
# selects significant biomarkers (P < 0.001) from mediation analysis,
# and performs GO, KEGG, and Reactome pathway enrichment analyses.
# ======================================================================

# ---------- 1. Load and Prepare Protein Data ----------
library(R.matlab)
library(readr)
library(dplyr)

# Load normalized protein matrix (.mat)
file_path <- "~/Documents/Data/UKB/UKB_Protein_norm.mat"
mat_data <- readMat(file_path)
proteinic_dat <- as.data.frame(mat_data$ukb.protein.norm)

# Remove redundant column and rename the first column as 'eid'
proteinic_dat <- proteinic_dat[, -1]
colnames(proteinic_dat)[1] <- "eid"

# Load protein names
proteinic_name <- read_csv("~/Documents/Data/UKB/id.csv")
new_colnames <- as.character(proteinic_name[[1]])

# Assign column names (check consistency)
if (length(new_colnames) == (2921 - 2 + 1)) {
  colnames(proteinic_dat)[2:2921] <- new_colnames
} else {
  stop("Number of protein names does not match dataset columns.")
}

# ---------- 2. Merge with Main Dataset ----------
resilience_group_R <- read.csv("/public/home/yangzy/Documents/Data/UKB/resilience_group_R.csv")
resilience_group_R <- merge(resilience_group_R, proteinic_dat, by = "eid", all.x = TRUE)

# ---------- 3. Select Significant Biomarkers ----------
# Import mediation results and select biomarkers with P < 0.001
file_path <- "/public/home/yangzy/Documents/ongoing_project/child_maltreatment_UKB/Resilience_modle/protein_mediation_data.csv"
mediation_results_table <- read.csv(file_path)
mediation_results_table_short <- mediation_results_table %>% filter(P_value < 0.001)

# ---------- 4. Functional Enrichment Analyses ----------
library(clusterProfiler)
library(org.Hs.eg.db)
library(ReactomePA)
library(DOSE)
library(ggplot2)

# Prepare gene list for enrichment (SYMBOL → ENTREZID)
protein_list <- mediation_results_table_short$Biomarker
entrez_ids <- bitr(protein_list, fromType = "SYMBOL", toType = "ENTREZID", OrgDb = org.Hs.eg.db)

# ---- GO Enrichment Analysis ----
go_enrich <- enrichGO(
  gene = entrez_ids$ENTREZID,
  OrgDb = org.Hs.eg.db,
  keyType = "ENTREZID",
  ont = "all",                    # options: "BP", "CC", "MF", or "all"
  pAdjustMethod = "BH",
  pvalueCutoff = 0.05,
  qvalueCutoff = 0.05
)

# Make GO terms human-readable
go_enrich <- DOSE::setReadable(go_enrich, OrgDb = org.Hs.eg.db, keyType = "ENTREZID")

# Remove redundant GO terms
go_simplified <- simplify(go_enrich, cutoff = 0.7, by = "p.adjust", select_fun = min)

# Extract GO results table
go_result <- go_simplified@result
head(go_result)

# Plot GO enrichment
dotplot(go_simplified,
        x = "GeneRatio",
        color = "p.adjust",
        showCategory = 10,
        split = "ONTOLOGY",
        label_format = Inf) +
  facet_grid(ONTOLOGY ~ ., space = "free_y", scale = "free_y")

# ---- KEGG Enrichment Analysis ----
kegg_result <- enrichKEGG(
  gene = entrez_ids$ENTREZID,
  organism = "hsa",
  pAdjustMethod = "BH"
)

# ---- Reactome Pathway Analysis ----
reactome_result <- enrichPathway(
  gene = entrez_ids$ENTREZID,
  organism = "human",
  pAdjustMethod = "BH"
)

# Extract results as data frames
kegg_table <- as.data.frame(kegg_result@result)
reactome_table <- as.data.frame(reactome_result@result)

# ---- Plot Results ----
barplot(kegg_result, showCategory = 10, title = "KEGG Pathway Enrichment")
barplot(reactome_result, showCategory = 10, title = "Reactome Pathway Enrichment")

# ======================================================================
# End of Script
# ======================================================================