# Visualizing DESeq Results
library(tidyverse)
library(DESeq2)
# Load our data from DESeq analysis
load(file = 'data/DE_output.Rda')
# Has two variables (des, res) 

# Investigate the output
resultsNames(des)
summary(res) # this shows 28 genes go up and ~15 go down 
#at an adjusted  p-value of 0.1
 
# MA Plot 
plotMA(res, ylim = c(-5,5))

#(notice the high lfc in the lower expression)
# This is likely a poor estimate of the true fold chnage ( low expression with inflated lfc)
# Need to specify which coefficient from our linear model to shrink

shrunk <- lfcShrink(des, coef = 2, type = 'apeglm') 
plotMA(shrunk, ylim = c(-5,5))

shrunk <- lfcShrink(des, coef = 2, type = 'normal') 
plotMA(shrunk, ylim = c(-5,5))

shrunk <- lfcShrink(des, coef = 2, type = 'ashr') 
plotMA(shrunk, ylim = c(-5,5))
# There are a few ways to shrink the lfc, see the DESeq doucumentation for help
# https://bioconductor.org/packages/devel/bioc/vignettes/DESeq2/inst/doc/DESeq2.html

# Other data analysis and additional topics
# This is an important plot to look at. 
# P-values should be uniformly spread between 0-1
res_df <- as.data.frame(res)
hist(res_df[res_df$baseMean > 1, ]$pvalue) 
# This histogram shows a bit of an enrichment of smaller p-values, this is good and
# is what you expect if some genes do not fit our null hypothesis and actually differ between groups

sig_results <- as.data.frame(shrunk) %>% 
   rownames_to_column() %>% 
   filter(padj < 0.1)
sig_results

# We can plot these as a Volcano Plot
# Convert to a 
sdf <- shrunk %>% as.data.frame() %>% rownames_to_column() 
sdf %>%
   mutate(padj = ifelse(is.na(padj), 1, padj) ) %>% # replace NAs with 1 for padj for this plot
   ggplot(aes(x = log2FoldChange, y = -log10(padj), color = padj < 0.1)) +
   geom_point() + 
   scale_color_manual(values = c('grey30', 'red2')) + 
   theme_bw() + 
   theme(panel.grid = element_blank()) + 
   geom_vline(aes(xintercept = 0), linetype = 'dashed', color ='grey30' )

# label some points that are signficant
top_10 <- sdf %>% arrange(padj) %>% head(10)
top_10
sdf %>%
   mutate(padj = ifelse(is.na(padj), 1, padj) ) %>% # replace NAs with 1 for padj for this plot
   ggplot(aes(x = log2FoldChange, y = -log10(padj), color = padj < 0.1)) +
   geom_point() + 
   scale_color_manual(values = c('grey30', 'red2')) + 
   theme_bw() + 
   theme(panel.grid = element_blank()) + 
   geom_vline(aes(xintercept = 0), linetype = 'dashed', color ='grey30' ) + 
   ggrepel::geom_text_repel(aes(label = rowname), data= top_10) #new aesthetic, remember we can supply different dataframe here (top_10)

# Now we can look at our results - we might specifically want to pick out our most significant genes and take a look
sdf %>%
   filter(!is.na(padj) ) %>%
   arrange(padj) %>%
   head(30)

# Plot some of these using plotCounts
plotCounts(des, gene = 'DDX3Y', intgroup = 'Group', norm = T)
plotCounts(des, gene = 'POP1', intgroup = 'Group', norm = T)
plotCounts(des, gene = 'DDX3X', intgroup  = 'Group', norm = T) 

# Threshold by an arbitrary logfoldchange cutoff
res_filt <- results(des, lfcThreshold = abs(0.6) )
# The above tests if the |LFC| is at least > 0.6, not if the LFC is different from 0
# This is NOT the same as filtering your genes for |LFC| > 0.6
summary(res_filt)
plotMA(res_filt)

# Heatmap of significantly changed genes
norm_counts <- counts(des, norm = T) 

# get a list of genes
genes <- sdf %>% filter(padj < 0.1) %>% pull(rowname) # pull is a new verb I just learned! it selects one column and returns a vector
# keep just tne rows for those genes ( could use filters here as well, but b/c gene names are the rownames and not a column in the dataframe, the following is easier)
norm_counts <- norm_counts[rownames(norm_counts) %in% genes, ]

# I Like ComplexHeatmap for making heatmaps, although there are other packages (pheatmap, heatmap.2)
design <- colData(des)
column_ha <- HeatmapAnnotation(group = design$Group, 
                               col = list(group =  c("A" = 'Grey10', 'B' = 'Steelblue')) ) # I get the syntax for colors wrong here all the timek
Heatmap(norm_counts, top_annotation = column_ha) 
# That heatmap basically just shows which genes are highly exprssed (not scaled by row)

# Scale heatmap by row - this is usually what you want  
scaled_hm <- t(scale(t(norm_counts), center = T, scale = T) )  # lets z-score our data by row
# Lots happening above, key function is scale
column_ha <- HeatmapAnnotation(group = design$Group, 
                               col = list(group =  c("A" = 'Grey10', 'B' = 'Steelblue')) )
Heatmap(scaled_hm, top_annotation = column_ha)  

