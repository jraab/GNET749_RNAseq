# This script will go through a simple DESEq analysis using the 
# example data we have used in this class so far
# Reminder: That data is located at 
# data/counts/
# You need to copy that directory to your computer to run DESeq locally
# Or using ondemand.rc.unc.edu

# we will use most of same packages as we did for salmon
# but we will import the data directly ourselves, rather than using a helper from DESeq

# As always, start by loading needed libraries
library(tidyverse) 
library(DESeq2) # main package for differential expression

# Import design data
design <- read_csv('data/class_data_info.csv')
design
# This line creates a new column to keep track of where each Salmon output file is
# file.path makes sticks the arguments together with / between them to make path names

fcounts <- read_tsv('data/counts/count_w_annot.txt', skip = 1)
# the column names need to match our sample names so we'll have to fix them up
# this line removes some of the path with basename() and then removes the _full.bam part of the name
colnames(fcounts) <- colnames(fcounts) |> basename() |> str_replace(pattern = '_full.bam', '') 

# we can check we have the same groups
colnames(fcounts)[7:ncol(fcounts)] == design$Sample
# not in the same order, so we'll need to fix that
fcounts_matrix <- as.matrix(fcounts[,7:ncol(fcounts)])
rownames(fcounts_matrix) <- fcounts$Geneid
fcounts_matrix <- fcounts_matrix[,match(design$Sample, colnames(fcounts_matrix) )]
colnames(fcounts_matrix) == design$Sample
# Now they match and are in the same order

# The next line creates a summarizedExperiment object that can be usd for
#     differential testing
dds <- DESeqDataSetFromMatrix(fcounts_matrix, colData = design, design = ~ Group ) 
dds
metadata(dds) # DESEq version
assays(dds)$counts
# some of our previous data we no longer have
assays(dds)$avgTxLength
#################################################################################
# This is how we run the actual differential test with defaults 
# is very easy if you have a simple experimental design and analysis approach
###############################################################################
# everything else is mostly the same
des <- DESeq(dds) # This runs all the DESeq steps
mcols(des) # note other columns from this object we may want
# you'll notice compared to importing with tximeta you have less information about the genes
resultsNames(des)# what comparisons are available
res <- results(des) # this returns a DESeq results object but without the ranges
res
res |> as_tibble() |> ggplot( aes(x = log2(baseMean), y = log2FoldChange, color = padj < 0.05)) + geom_point()
################################################################################
# In General: I suggest shrinking logfold change  to lower the impact of high variability genes
shrunk <- lfcShrink(des, coef = 2, type = 'ape')
shrunk |> as_tibble() |> ggplot( aes(x = log2(baseMean), y = log2FoldChange, color = padj < 0.5)) + geom_point()

table(shrunk$padj <0.05, shrunk$log2FoldChange > 0)
table(res$padj < 0.05, res$log2FoldChange > 0)


################################################################################
# At this point you have a data frame with all your results - you can simply filter
# based on your pvalue or your log2 fold change if you want 
################################################################################

# We are going to save our deseq results object so we can use it for additional QC/Visualization
save(des, res , file = 'data/DE_fcounts_output.Rda')
res_df <- as.data.frame(res) %>% rownames_to_column()
res_df
# Now your results have ensembl names, but maybe not names you'd like
# lets use merging to bring that information in 
write_tsv(res_df, 'fcounts_class_data_results.tsv')


