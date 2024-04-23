# This script will go through a simple DESEq analysis using the 
# example data we have used in this class so far
# Reminder: That data is located on longleaf at 
# /proj/seq/data/RNAseq-training/reads/full/salmon
# <GITHUB>data/salmon/<samplename>/quant.sf
# You need to copy that salmon directory to your computer to run DESeq locally
# Or using ondemand.rc.unc.edu

# As always, start by loading needed libraries
library(tidyverse) 
# If you need to install DESeq or tximport use bioconductor
#install.packages('BiocManager') # This installs the function to use Bioconductor
#BiocManager::install(c('DESeq2', 'tximport', 'tximeta', 'apeglm') ) # This installs DESEq and tximport
library(DESeq2) # main package for differential expression
# Helper functions for reading in count data 
library(tximeta)
# Import design data
design <- read_csv('data/class_data_info.csv') 
design
# This line creates a new column to keep track of where each Salmon output file is
# file.path makes sticks the arguments together with / between them to make path names
design$path <- file.path('data/salmon', paste0(design$Sample, '_decoy_quant'), 'quant.sf')
design 


# Import Salmon quant files
txi <- tximeta(coldata = design, type = 'salmon')
se <- summarizeToGene(txi)
txi
rowData(txi)
colData(txi)
assays(txi)

# The next line creates a summarizedExperiment object that can be usd for
#     differential testing
dds <- DESeqDataSet(se , design = ~ Group ) 
dds
metadata(dds) # DESEq version
assays(dds)$counts
assays(dds)$avgTxLength
#################################################################################
# This is how we run the actual differential test with defaults 
# is very easy if you have a simple experimental design and analysis approach
###############################################################################
des <- DESeq(dds) # This runs all the DESeq steps
res <- results(des, format = 'GRanges', saveCols=2) # this returns a DESeq results object
summary(res)

################################################################################
# In General: I suggest shrinking logfold change  to lower the impact of high variablity genes
resultsNames(des) # what comparisons are available
shrunk <- lfcShrink(des, coef = 2, type = 'ape', format = 'GRanges', saveCols =2 )
table(shrunk$padj <0.05, shrunk$log2FoldChange > 0)
table(res$padj < 0.05, res$log2FoldChange > 0)

################################################################################
# At this point you have a data frame with all your results - you can simply filter
# based on your pvalue or your log2 fold change if you want 
################################################################################

# We are going to save our deseq results object so we can use it for additional QC/Visualization
save(des, res , file = 'data/DE_output.Rda')
res_df <- as.data.frame(res) %>% rownames_to_column()
res_df
# Now your results have ensembl names, but maybe not names you'd like
# lets use merging to bring that information in 
write_tsv(res_df, 'class_data_results.tsv')
  

