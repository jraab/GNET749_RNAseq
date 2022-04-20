# This script will go through a simple DESEq analysis using the 
# example data we have used in this class so far
# Reminder: That data is located on longleaf at 
# /proj/seq/data/RNAseq-training/reads/full/salmon
# You need to copy that salmon directory to your computer to run DESeq locally
# Or using ondemand.rc.unc.edu

# As always, start by loading needed libraries
library(tidyverse) 
library(DESeq2) # main package for differential expression
library(tximport) # Helper functions for reading in count data 
library(biomaRt) # for mapping transcript IDs to genes

# Import design data
design <- read_csv('/Users/jraab/GitHub/GNET749_RNAseq/data/class_data_info.csv') 
design
# This line creates a new column to keep track of where each Salmon output file is
# file.path makes sticks the arguments together with / between them to make path names
design$path <- file.path('/Users/jraab/GitHub/GNET749_RNAseq/data/salmon', paste0(design$Sample, '_decoy_quant'), 'quant.sf')
design 

# Setup mart  - must be connected to internet for this step
# IF you do not have an internet connect, the file is located on the course github

mart <- useMart(biomart = 'ensembl', dataset = 'hsapiens_gene_ensembl'  )
mart_res <- getBM(attributes = c('ensembl_transcript_id', 'external_gene_name'), mart = mart)
mart_res %>% head()
# You can save this mart_res object as a csv file and reload it as below if you expect to need this mapping when not on the internet
#write_csv(mart_res, file = '/Users/jraab/GitHub/GNET749_RNAseq/data/hsapiens_ensembl_mart.csv') 
#mart_res <- read_csv('/Users/jraab/GitHub/GNET749_RNAseq/data/hsapiens_ensembl_mart.csv') 

# Import Salmon quant files
txi <- tximport(design$path, type = 'salmon', tx2gene = mart_res, ignoreTxVersion = T)
txi$counts %>% head()
colnames(txi$counts) <- design$Sample
colnames(txi$counts)
head(txi$counts)
txi
# The next line creates a summarizedExperiment object that can be usd for
#     differential testing
dds <- DESeqDataSetFromTximport(txi , colData = design, design = ~ Group ) 
dds
#################################################################################
# This is how we run the actual differential test with defaults 
# is very easy if you have a simple experimental design and analysis approach
###############################################################################
des <- DESeq(dds) # This runs all the DESeq steps
res <- results(des) # this returns a DESeq results object
summary(res)

################################################################################
# At this point you have a data frame with all your results - you can simply filter
# based on your pvalue or your log2 fold change if you want 
################################################################################

# We are going to save our deseq results object so we can use it for additional QC/Visualization
save(des, res , file = 'data/DE_output.Rda')
res_df <- as.data.frame(res) %>% rownames_to_column()
write_tsv(res_df, 'class_data_results.tsv')

