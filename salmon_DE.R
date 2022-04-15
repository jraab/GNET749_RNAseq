# This script will start the same way as we did for exploratory analysis and visualization
# I still usually keep them separate in my own work, so that its easier to keep my 
# scripts small and maintainable

library(DESeq2)
library(tximport)
library(tidyverse) 
library(biomaRt)
library(ComplexHeatmap)

# Import design data
design <- read_csv('~/proj/teaching/GNET749_S21/data/sample_sheet.csv') 
design$path <- file.path('/Users/jraab/proj/teaching/GNET749_S21/data/raw/salmon', paste0(design$Sample, '_decoy_quant'), 'quant.sf')
design 
# Setup mart  - must be connected to internet for this step
mart <- useMart(biomart = 'ensembl', dataset = 'hsapiens_gene_ensembl'  )
mart_res <- getBM(attributes = c('ensembl_transcript_id', 'external_gene_name'), mart = mart)
write_csv(mart_res, file = '~/Desktop/mmusculus_ensembl.csv') 
# You can save this mart_res object as a csv file and reload it as below if you expect to need this mapping when not on the internet
#write_csv(mart_res, '~/proj/teaching/GNET749_S21/data/hsapiens_mart.csv')
#mart_res <- read_csv('~/proj/teaching/GNET749_S21/data/hsapiens_mart.csv')

# Import quant files
txi <- tximport(design$path, type = 'salmon', tx2gene = mart_res, ignoreTxVersion = T)
colnames(txi$counts) <- design$Sample
colnames(txi$counts)
head(txi$counts)

dds <- DESeqDataSetFromTximport(txi , colData = design, design = ~ Group) 
# You could filter this to remove genes with very low expression levels
# but for DE its not necessary as deseq2 will perform an independent filtering step that does this
# removing these would make the following steps run faster, but this data set is not large
# and it can be skipped

#################################################################################
# This is where we differ from last time - and you can see actually running DESeq 
# is very easy if you have a simple experimental design and analysis approach
###############################################################################
des <- DESeq(dds) # This runs all the DESeq steps
res <- results(des, independentFiltering = T) # this returns a DESeq results object
summary(res)
res_df <- as.data.frame(res) %>% rownames_to_column()

################################################################################
# At this point you have a data frame with all your results - you can simply filter
# based on your pvalue or your log2 fold change if you want 
################################################################################


# Other data analysis and additional topics
# This is an important plot to look at. 
# P-values should be uniformly spread between 0-1
hist(res_df[res_df$baseMean > 1, ]$pvalue) 
# This histogram shows a bit of an enrichment of smaller p-values, this is good and
# is what you expect if some genes do not fit our null hypothesis and actually differ between groups
summary(res) # this shows ~31 genes go up and ~16 go down at an adjusted  p-value of 0.1
table(is.na(res_df$pvalue))
plotMA(res)
# Lots of low count genes with high fold changes, lets shrink the lfc

shrunk <- lfcShrink(des, type = 'apeglm', coef = 2) # coef here comes from the list of possible results 1 is intercept 2 is our main effect
# see resultsNames(des)

plotMA(shrunk, ylim = c(-4,4)) 

# We can plot these as a Volcano Plot
sdf <- shrunk %>% as.data.frame() %>% rownames_to_column() 
sdf %>%
   mutate(padj = ifelse(is.na(padj), 1, padj) ) %>% # replace NAs with 1 for padj for this plot
   ggplot(aes(x = log2FoldChange, y = -log10(padj), color = padj < 0.1)) +
   geom_point() + 
   scale_color_manual(values = c('grey30', 'red2')) + 
   theme_bw() + 
   theme(panel.grid = element_blank()) + 
   geom_vline(aes(xintercept = 0), linetype = 'dashed', color ='grey30' )


# Now we can look at our results - we might specifically want to pick out our most significant genes and take a look
sdf %>%
  filter(!is.na(padj) ) %>%
  arrange(padj) %>%
   head(30)

# Plot some of these using plotCounts
plotCounts(des, gene = 'DDX3Y', intgroup = 'Group', norm = T)
plotCounts(des, gene = 'POP1', intgroup = 'Group', norm = T)
plotCounts(des, gene = 'IGHV1-24', intgroup = 'Group', norm = T)
plotCounts(des, gene = 'DDX3X', intgroup  = 'Group', norm = T) 

# Threshold by an arbitrary logfoldchange cutoff
res_filt <- results(des, lfcThreshold = abs(1.5) )
summary(res_filt)
plotMA(res_filt)

# Heatmap of significantly changed genes
norm_counts <- counts(des, norm = T) 

# get a list of genes
genes <- sdf %>% filter(padj < 0.1) %>% pull(rowname) # pull is a new verb I just learned! it selects one column and returns a vector
norm_counts <- norm_counts[rownames(norm_counts) %in% genes, ]


# I Like ComplexHeatmap for making heatmaps, although there are other packages (pheatmap, heatmap.2)
column_ha <- HeatmapAnnotation(group = design$Group, 
                               col = list(group =  c("A" = 'Grey10', 'B' = 'Steelblue')) ) # I get the syntax for colors wrong here all the time
Heatmap(norm_counts, top_annotation = column_ha) 
# That heatmap basically just shows which genes are highly exprssed (not scaled by row)

# Scale heatmap by row - this is usually what you want  
scaled_hm <- t(scale(t(norm_counts), center = T, scale = T) )  # lets z-score our data by row
column_ha <- HeatmapAnnotation(group = design$Group, 
                               col = list(group =  c("A" = 'Grey10', 'B' = 'Steelblue')) )
Heatmap(scaled_hm, top_annotation = column_ha)  

