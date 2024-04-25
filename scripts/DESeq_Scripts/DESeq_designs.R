# Advanced Experimental Designs
# How to handle more complex experimental designs 
library(DESeq2)
library(limma)
library(sva)
library(tidyverse)
library(ComplexHeatmap)

################################################################################
# In this example  we are testing how to look at mulitple gruops
# we have  knockdown experiments of two different proteins, both proteins together, or a control
load('data/GSE102560_dds.Rda')

# Here is our data converted to a DESeq Dataset
dds$condition



# We can perform a normal Wald test using DESEq as we usually would
# wald is default
dds <- DESeq(dds)
# Now you can see the mulitple comparisons available
resultsNames(dds)
# Extracting different comparisons to NS is easy using the contrast argument
res_brg <- results(dds, contrast = c('condition', 'Brg1', 'NS') ) # contrast takes the form of factor, numerator, denominator
res_brm <- results(dds, contrast = c('condition', 'Brm', 'NS') ) 
res_double <- results(dds, contrast = c('condition', 'Double', 'NS') ) 

plotCounts(dds, gene = 'CA4', intgroup = 'condition')

# apeglm requires use of coef, so use resultsNames(dds) to get the right coef number
#need to relevel to get these correctly
dds$condition <- relevel(dds$condition, 'NS')  
dds$condition
dds <- nbinomWaldTest(dds)
resultsNames(dds)
res_brg <- lfcShrink(dds, coef = 2, res= res_brg, type = 'apeglm')
res_brm <- lfcShrink(dds, coef = 3, res= res_brm, type = 'apeglm')
res_double <- lfcShrink(dds, coef = 4, res= res_double, type = 'apeglm')

# saving these data for enrichment analysis
write_csv(as.data.frame(res_brg) |> rownames_to_column() , file = 'data/results_brg1.csv') 
write_csv(as.data.frame(res_brm) %>% rownames_to_column(), file = 'data/results_brm.csv')
write_csv(as.data.frame(res_double) %>% rownames_to_column(), file = 'data/results_double.csv') 

# what if we don't want to compare things pair-wise, this is where its a bit more complicated
# lets say we want to know if there is a difference between one group and the average of the others. 

 Now we can see that there is a coefficient for each condition rather than for the comparison
resultsNames(dds_beta)
# We can then compare the numerator of interest (Brg1) to the average of the remaining conditions. 
# listValues() lets you specify what value you will multiply each coefficent by
x <- results(dds_beta, contrast = list('conditionBrg1', c('conditionNS', 'conditionBrm', 'conditionDouble')),
                               listValues = c(1, -1/3 ) )
x |> as.data.frame() |> rownames_to_column() |> arrange(padj)
plotCounts(dds_beta, gene = 'BMP6', intgroup = 'condition')
# This comparison asks which genes are differential expressed in BRG1 relative to the average of the other 3 groups
#
# This is possibly better if you have many potential pair-wise comparisons
# or do not have a natural reference population
# Also better than having a new column called notBrg1 where Brg1 is 1 and all others are 0 b/c that treats the other samples as a single group, which may have highly different read counts from each other

# Heatmap
top_50 <- as.data.frame(x) %>% rownames_to_column() %>% arrange(padj) %>% head(50)
counts <- assay(dds_beta)[rownames(dds_beta) %in% top_50$rowname, ]
counts <- t(scale(t(counts), center = T, scale =T))
Heatmap(counts)

# LRT Test to find genes where the additional term adds something to the model
# An alternative with this sort of design is to use the liklihood ratio test
# Using the LRT version of this to identify any genes with changes
design(dds)
dds_lrt <- DESeq(dds, test = 'LRT', full = ~condition, reduced = ~1)
# p-values here mean is there a difference between a model that contains our 'condition' and one without that
# But we can extract specific logFoldChanges as before
# but p-values will always be the same ( difference between full model and reduced)
resultsNames(dds_lrt)
res_lrt_brg <- lfcShrink(dds_lrt, coef = 2, type = 'apeglm')
res_lrt_brm <- lfcShrink(dds_lrt, coef = 3, type = 'apeglm')
res_lrt_brg
# fold changes differ
plot(res_lrt_brg$log2FoldChange, res_lrt_brm$log2FoldChange)
# but pvalues are the same
table(res_lrt_brg$padj == res_lrt_brm$padj)

#######################################################################################################################
# Multiple conditions
# i.e genotype + treatment
# I will post an Rdata object of the DEseq Data for this
# Make sure you change this to match your own files location
######################################################################################################################

load('data/arid2_es.Rda')
es_dds <- dds
es_dds <- DESeq(es_dds)
design(es_dds)
# controlling for treatment, does genotype have an effect
resultsNames(es_dds)
res_genotype <- lfcShrink(es_dds, coef = 2, type = 'apeglm' )  

summary(res_genotype)
res_genotype %>%
   as.data.frame() %>%
   rownames_to_column() %>%
   arrange(padj) %>%
   head(20)
df <- plotCounts(dds, gene = 'Mapk8ip2', intgroup = c('genotype', 'treat' ), returnData = T)
df %>% 
   ggplot(aes(x = treat, y = log2(count), color = genotype) ) +
   geom_point(position = position_dodge(0.5))
# controlling for genotype, does treatment have an effect

# Since we have multiple genotype and treatments, and LRT test might be better 
design(es_dds) <- formula(~genotype + treat+genotype:treat)
dds_treat <- DESeq(es_dds, test = 'LRT', full = ~ genotype + treat + genotype:treat, reduced = ~genotype + treat) 
resultsNames(dds_treat)
res_treat <- results(dds_treat, contrast= c('treat', 'EB', 'ES') )
res_treat
summary(res_treat)
# This will give us p-values for genes that have an interaction effect

res_genotype2 <- results(dds_treat, contrast = c('genotype' , 'ARID2', 'WT')) 
res_genotype2 
summary(res_genotype2)
# this will STILL give us p-values for the interaction effect, but we can extract the logfoldchangesr for arid2 vs wt
summary(res_treat)
table(res_treat$padj == res_genotype2$padj) 

res_treat %>%
   as.data.frame() %>%
   rownames_to_column() %>%
   arrange(padj) %>%
   head(20)

df <- plotCounts(dds, gene = 'Cited1', intgroup = c('genotype', 'treat' ), returnData = T)
df %>% 
   ggplot(aes(x = treat, y = log2(count), color = genotype) ) + geom_point(position = position_dodge(0.5))

# Is there an effect that differs by genotype 
dds_interaction <- dds
design(dds_interaction) <- formula(~treat + genotype + treat:genotype)
dds_interaction <- DESeq(dds_interaction, test = 'LRT', 
                        full = design(dds_interaction),
                        reduced = ~genotype + treat)   

resultsNames(dds_interaction) # while it looks like you could extract other comparisons, 
# all the p-values come from the LRT models we tested, so they will differ than if we perform the Wald tests above
res_interaction <- lfcShrink(dds_interaction, coef = 'treatFGF48.genotypeWT', type = 'ashr' )
summary(res_interaction) 
res_interaction %>%
   as.data.frame() %>%
   rownames_to_column() %>%
   arrange(padj) %>%head(20) 

plotCounts(dds_interaction, gene = 'Podxl', intgroup = c('genotype', 'treat' ), returnData=T) %>%
   ggplot(aes(x = genotype, y = count, color = treat ))  + geom_point() + 
   facet_wrap(~treat) 



