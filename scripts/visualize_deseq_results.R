# Visualizing DESeq Results

# Other data analysis and additional topics
# This is an important plot to look at. 
# P-values should be uniformly spread between 0-1
hist(res_df[res_df$baseMean > 1, ]$pvalue) 
# This histogram shows a bit of an enrichment of smaller p-values, this is good and
# is what you expect if some genes do not fit our null hypothesis and actually differ between groups
summary(res) # this shows 281 genes go up and ~15 go down at an adjusted  p-value of 0.1

plotMA(res)
# Lots of low count genes with high fold changes, lets shrink the lfc
resultsNames(des)
# we need to konw which coefficient of our linear model we want to shrink
# resultsNames shows 2 , intercept and Group_B_vs_A (our main effect)
# we will shrink coef 2
shrunk <- lfcShrink(des, type = 'apeglm', coef = 2)  
# There are a few ways to shrink the lfc, see the DESeq doucumentation for help
# https://bioconductor.org/packages/devel/bioc/vignettes/DESeq2/inst/doc/DESeq2.html

plotMA(shrunk, ylim = c(-4,4)) 
sig_results <- as.data.frame(shrunk) %>% 
   rownames_to_column() %>% 
   filter(padj < 0.1)
sig_results

write_csv()

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

