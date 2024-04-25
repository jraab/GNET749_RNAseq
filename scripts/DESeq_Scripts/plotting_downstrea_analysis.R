# Plotting and downstream analysis
library(tidyverse) # cran

# all below from bioconductor 
# BiocManager::install('<PACKAGENAME>')
library(DESeq2)
library(ComplexHeatmap)
library(clusterProfiler)

# Data sets to work with
load('data/DE_output.Rda')
class_des <- des
class_res <- res

load('data/GSE102560_dds.Rda')
swi_dds <- dds
swi_res <- res

# Basic plotting of counts - quick way to look at data
class_res |> as.data.frame() |> arrange(padj)
plotCounts(class_des, gene = 'ENSG00000129824', intgroup = 'Group', norm = T)

# Can also plot this by hand
class_res |> as.data.frame() |> arrange(padj) 
normd <- counts(class_des, norm = T)
normd_l <- normd |> 
  as.data.frame() |> 
  rownames_to_column() |> 
  pivot_longer(names_to = 'Sample', values_to = 'counts', -rowname) |> 
  left_join(colData(class_des) |> as.data.frame(), by = 'Sample') |> 
  left_join(rowData(class_des) |> as.data.frame(), by = c('rowname' = 'gene_id') )

normd_l |> 
  filter(gene_name == 'DDX3Y') |> 
  ggplot(aes(x = Group, y = counts, color = Group)) + 
  geom_point()


# Heatmap of differential genes

# get top 30 changed genes
top_30 <- class_res |> as.data.frame() |> arrange(padj) |> head(30)
# filter data on these
normd_30 <-  normd[rownames(normd) %in% rownames(top_30), ]

# Plot heatmap
Heatmap(normd_30)

# probably not what you wanted since expresion levels of genes vary so much and we want to look at differences between samples
# scale by row
scaled_30 <- t(scale(t(normd_30), center = T, scale = T))
# center subtracts column means
# scale divides by sd
Heatmap(scaled_30)

# who can remember ensmebl ids
rownames(scaled_30) <- top_30[match(rownames(scaled_30), rownames(top_30)), ]$gene_name
# be careful with the above - it is possible to get the ordering wrong and mislabel
# match looks for which row in arg1 matches row in arg2 - I'm subsetting arg2 by this value to get the gene_name
Heatmap(scaled_30)

# lets add an annotation bar
hm_anno <- HeatmapAnnotation(Group= colData(des)$Group, col = list(Group = c('A' = 'steelblue', 'B' = 'goldenrod')))
Heatmap(scaled_30, top_anno = hm_anno)

# Enrichment################################################################################ 
# lets look at more interseting data
# lrt to to find genes signficantly different
lrt_des <- DESeq(swi_dds, test = 'LRT', full = design(swi_dds), reduced = ~1)
lrt_des$condition <- relevel(lrt_des$condition, 'NS')
lrt_des <- nbinomLRT(lrt_des, full = design(lrt_des), reduced =~1)
resultsNames(lrt_des)
res <- lfcShrink(lrt_des, coef = 2, type = 'apeglm')

sig_genes <- res |> 
  as.data.frame() |> 
  rownames_to_column('gene_name') |>
  filter(padj < 0.05)

sig_up <- res |> 
  as.data.frame() |> 
  rownames_to_column('gene_name') |> 
  filter(padj < 0.05) |> 
  filter(log2FoldChange > 0)

# bring in some data for enrichment analysis 
msigdb <- msigdbr::msigdbr(species = 'Homo sapiens', category = 'H')
hallmark_db <- msigdb |> dplyr::select(gs_name, human_gene_symbol)
# do not need the category f you want the whole thing - for speed I'm doing just Hallmarks
# https://www.gsea-msigdb.org/gsea/msigdb/

# RUN GSEA 
# GSEA runs on your whole data set - values for fold changes as a list with names of genes attached
ns_vs_brg1 <- res$log2FoldChange
names(ns_vs_brg1) <- rownames(res)
ns_vs_brg1 <- ns_vs_brg1[!is.na(ns_vs_brg1)] # remove an NA genes since they won't be useful for us
# need to sort the list from highest to lowest
sorted_vals <- rev(sort(ns_vs_brg1))
ns_v_brg1_h <- GSEA(geneList = sorted_vals, TERM2GENE = hallmark_db, by = 'fgsea') 
ns_v_brg1_h

# Plot GSEA output
ns_v_brg1_h |> 
  as.data.frame() |> 
  mutate(ID = str_replace(string = ID, pattern = 'HALLMARK_', replacement = '')) |> 
  arrange(NES ) |> 
  mutate(ID = factor(ID, levels = ID)) |> 
  ggplot(aes(x = ID, y = NES, fill = NES > 0) ) + 
  geom_col(color = 'grey30') + 
  coord_flip() +
  scale_fill_manual(values = c('steelblue', 'goldenrod')) 

gseaplot(ns_v_brg1_h, geneSetID = 'HALLMARK_TNFA_SIGNALING_VIA_NFKB')

################################################################################
#Enricher
sig_up
hall_mark_up <- enricher(gene = sig_up$gene_name, 
                         universe = res[!is.na(res$log2FoldChange),]$gene_name, 
                         TERM2GENE = hallmark_db)
hall_mark_up |> 
  as.data.frame() |> 
  mutate(ID = str_replace(string = ID, pattern = 'HALLMARK_', replacement = '')) |> 
  arrange(-log10(qvalue)) |> 
  mutate(ID = factor(ID, levels = ID)) |> 
  ggplot(aes(x = ID, y = -log10(qvalue) ) )+ 
  geom_col(color = 'grey30') + 
  coord_flip() 

  
