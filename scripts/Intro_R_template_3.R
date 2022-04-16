# Working with and plotting data

library(tidyverse)
# Load count data and sample info data we download from github
# github.com/jraab/GNET749_RNAseq
# drug_norm_counts.csv
# drug_samples.csv

# Need to read in data from whereer you saed the files above - 
# Here is how I do it on my system
#counts <- read_csv('/Users/jraab/GitHub/GNET749_RNAseq/drug_norm_counts.csv')
#sample_info <- read_csv('/Users/jraab/GitHub/GNET749_RNAseq/drug_samples.csv')

# How many treatments are there
# How Many genotypes
# How many of each combination


# Make simple plots for a few genes to see if there is a difference
# Some genes we can use to look at differences
genes - c('MCRS1', 'MAP3K7', 'ARID2')
# Make the count data long style then include sample information 

# Join with sample information

# Plot data to explore relationship between treatment and genotype
