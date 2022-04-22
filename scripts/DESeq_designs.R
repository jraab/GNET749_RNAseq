# Advanced Experimental Designs
# How to handle more complex experimental designs 
library(DESeq2)
library(limma)
library(sva)
library(tximport)
library(tidyverse)
library(biomaRt)
library(ComplexHeatmap)

################################################################################
# In this example  we are testing how to look at mulitple gruops
# we have  knockdown experiments of two different proteins, both proteins together, or a control
df <- read_csv('/Users/jraab/GitHub/GNET749_RNAseq/data/GSE102560_count_matrix.csv.gz') 

# As always we can read in our data and create a DESeqDataSet object.
design <- data.frame(samples = colnames(df)[2:ncol(df)])
# Using the filenames we can create a sample sheet for our groups
design <- design %>%
   separate(samples, into= c('condition', 'rep'), sep = '_', remove = F) 
rn <-df$rowname 
mat <- as.matrix(df[,2:ncol(df) ])
rownames(mat) <- rn

# Here is our data converted to a DESeq Dataset
dds <- DESeqDataSetFromMatrix(mat, colData = design, design = ~ condition) 
dds$condition <- relevel(dds$condition, 'NS')  


# We can perform a normal Wald test using DESEq as we usually would
dds <- DESeq(dds)
# Now you can see the mulitple comparisons available
resultsNames(dds)
# Extracting different comparisons to NS is easy using the contrast argument
res_brg <- results(dds, contrast = c('condition', 'Brg1', 'NS') ) # contrast takes the form of factor, numerator, denominator
res_brm <- results(dds, contrast = c('condition', 'Brm', 'NS') ) 
res_double <- results(dds, contrast = c('condition', 'Double', 'NS') ) 

# saving these data for enrichment analysis
write_csv(as.data.frame(res_brg) %>% rownames_to_column() , file = '/Users/jraab/GitHub/GNET749_RNAseq/data/results_brg1.csv') 
write_csv(as.data.frame(res_brm) %>% rownames_to_column(), file = '/Users/jraab/GitHub/GNET749_RNAseq/data/results_brm.csv')
write_csv(as.data.frame(res_double) %>% rownames_to_column(), file = '/Users/jraab/GitHub/GNET749_RNAseq/data/results_double.csv') 

#what if we wanted to compare the two single mutants
res_brg_vs_brm <- results(dds, contrast = c('condition', 'Brg1', 'Brm') ) 
# that works the same way - specifying which groups we wish to compare


# what if we don't want to compare things pair-wise, this is where its a bit more complicated
# lets say we want to know if there is a difference between one group and the average of the others. 
# We need to rerun our DESeq analysis with one additional argument 
# betaPrior = T reverts DESeq to how a prior version worked and gives us access to 
# the average value for multiple groups. An alternative approach is to change the design matrix 
# to ~ 0 + condition (forcing the intercept to be 0). See the DESeq vignette for more info
dds_beta <- DESeq(dds, betaPrior = T) 
dds_beta
# Now we can see that there is a coefficient for each condition rather than for the comparison
resultsNames(dds_beta)
# We can then compare the numerator of interest (Brg1) to the average of the remaining conditions. 
# listValues() lets you specify what value you will multiply each coefficent by
x <- results(dds_beta, contrast = list('conditionBrg1', c('conditionNS', 'conditionBrm', 'conditionDouble')), listValues = c(1, -1/3) ) 
x
# This comparison asks which genes are differential expressed in BRG1 relative to the average of the other 3 groups
#
# This is possibly better if you have many potential pair-wise comparisons
# or do not have a natural reference population
# Also better than having a new column called notBrg1 where Brg1 is 1 and all others are 0 b/c that treats the other samples as a single group, which may have highly different read counts from each other

# Heatmap
top_100 <- as.data.frame(x) %>% rownames_to_column() %>% arrange(padj) %>% head(100)
counts <- assay(dds_beta)[rownames(dds_beta) %in% top_100$rowname, ]
counts <- t(scale(t(counts), center = T, scale =T))
Heatmap(counts)

# An alternative with this sort of design is to use the liklihood ratio test
# Using the LRT version of this to identify any genes with changes
dds_lrt <- DESeq(dds, test = 'LRT', full = ~condition, reduced = ~1)
# p-values here mean is there a difference between a model that contains our 'condition' and one without that
# But we can extract specific logFoldChanges as before
# but p-values will always be the same ( difference between full model and reduced)
resultsNames(dds_lrt)
res_lrt_brg <- results(dds_lrt, contrast = c('condition', 'Brg1', 'NS') ) 
res_lrt_brm <- results(dds_lrt, contrast = c('condition', 'Brm', 'NS') ) 
# fold changes differ
plot(res_lrt_brg$log2FoldChange, res_lrt_brm$log2FoldChange)
# but pvalues are the same
table(res_lrt_brg$padj == res_lrt_brm$padj)

#######################################################################################################################
# Multiple conditions
# i.e genotype + treatment
# I will post an Rdata object of the DEseq Data for this - see line 93 below to load the data
# Make sure you change this to match your own files location
######################################################################################################################
#design <- data.frame(path = list.files('/Users/jraab/proj/arid2_es/processed/', pattern = 'quant.sf', full.names = T, recursive = T) )
#design$name <- str_replace(basename(dirname(design$path)), '.fastq.gz', '') 
#design$name <- str_replace(design$name, 'X0091_', '') 
#design$name <- str_replace(design$name, '_\\w{6}_L00._R1_001', '') 
#design$name <- str_replace(design$name, '\\d+-', '')
#design <- design %>%
#   separate(name, into = c('genotype', 'rep', 'treat'), remove = F, sep = '_') 

# Setup mart 
#mart <- useMart(biomart = 'ensembl', dataset = 'mmusculus_gene_ensembl' )
#mart_res <- getBM(attributes = c('ensembl_transcript_id', 'external_gene_name'), mart = mart)

#txi <- tximport(files = design$path, type = 'salmon', tx2gene = mart_res, ignoreTxVersion = T) 
#dds <- DESeqDataSetFromTximport(txi, colData = design, design = ~genotype + treat) 
#save(dds, file = '~/proj/teaching/GNET749_S21/data/arid2_es.Rda')
load('/Users/jraab/GitHub/GNET749_RNAseq/data/arid2_es.Rda')

dds <- DESeq(dds)
# controlling for treatment, does genotype have an effect
res_genotype <- results(dds, contrast = c('genotype', 'ARID2', 'WT') )  
summary(res_genotype)
res_genotype %>%
   as.data.frame() %>%
   rownames_to_column() %>%
   arrange(padj) %>%
   head(20)
df <- plotCounts(dds, gene = 'Mapk8ip2', intgroup = c('genotype', 'treat' ), returnData = T)
df %>% 
   ggplot(aes(x = treat, y = log2(count), color = genotype) ) + geom_point(position = position_dodge(0.5))
# controlling for genotype, does treatment have an effect

# Since we have multiple treatments, and LRT test might be better 
dds_treat <- DESeq(dds, test = 'LRT', full = ~ genotype + treat, reduced = ~genotype) 
res_treat <- results(dds_treat) # This will give us p-values for genes that have a treatment effect
# however, the logfoldchanges do not tell us anything useful
# we also don't know which treatments have an effect

summary(res_treat)
res_treat %>%
   as.data.frame() %>%
   rownames_to_column() %>%
   arrange(padj) %>%
   head(20)

df <- plotCounts(dds, gene = 'Olfr1459', intgroup = c('genotype', 'treat' ), returnData = T)
df %>% 
   ggplot(aes(x = treat, y = log2(count), color = genotype) ) + geom_point(position = position_dodge(0.5))

# Is there an effect that differs by genotype
dds_interaction <- dds
design(dds_interaction) <- formula(~treat + genotype + treat:genotype)
dds_interaction <-DESeq(dds_interaction, test = 'LRT', 
                        full = design(dds_interaction),
                        reduced = ~genotype + treat)   

resultsNames(dds_interaction) # while it looks like you could extract other comparisons, 
# all the p-values come from the LRT models we tested, so they will differ than if we perform the Wald tests above
res_interaction <- results(dds_interaction, name = 'treatFGF48.genotypeWT' )
summary(res_interaction) 
res_interaction %>%
   as.data.frame() %>%
   rownames_to_column() %>%
   arrange(padj) %>%head(20) 

plotCounts(dds_interaction, gene = 'Srp54b', intgroup = c('genotype', 'treat' ), returnData=T) %>%
   ggplot(aes(x = genotype, y = count, color = treat ))  + geom_point() + 
   facet_wrap(~treat) 


res_genotype_WT_vs_ARID2 <- results(dds_interaction, contrast = c('genotype', 'WT', 'ARID2')) 
table(res_genotype_WT_vs_ARID2$padj < 0.1)
table(res_interaction$padj < 0.1)
table(res_genotype_WT_vs_ARID2$log2FoldChange > 5)
table(res_interaction$log2FoldChange > 5)

res_genotype_WT_vs_ARID2 %>% 
   as.data.frame() %>% 
   rownames_to_column() %>% 
   arrange(log2FoldChange) %>% head(20)



