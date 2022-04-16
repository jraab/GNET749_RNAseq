# Working with and plotting data

library(tidyverse)
# Load count data and sample info data we download from github
# github.com/jraab/GNET749_RNAseq
# drug_norm_counts.csv
# drug_samples.csv

counts <- read_csv('/Users/jraab/GitHub/GNET749_RNAseq/data/drug_norm_counts.csv')
sample_info <- read_csv('/Users/jraab/GitHub/GNET749_RNAseq/data/drug_samples.csv')
sample_info

# How many treatments are there
sample_info %>% group_by(treatment) %>% summarise(n())
table(sample_info$genotype)
# How Many genotypes
sample_info %>% group_by(genotype) %>% summarise(n())
table(sample_info$genotype)

# How many of each combination
sample_info %>% group_by(genotype, treatment) %>% summarise(n()) 
table(sample_info$genotype, sample_info$treatment)

# Make simple plots for a few genes to see if there is a difference
genes - c('MCRS1', 'MAP3K7', 'ARID2')
# Make the count data long style then include sample information 

counts_l <- counts %>% 
   pivot_longer(names_to = 'samples', values_to = 'counts', -rowname)
# Join with sample information
counts_l <- counts_l %>% 
   left_join(sample_info, by = 'samples') 

# some genes we can plot
genes <- c('MCRS1', 'MAP3K7', 'ARID2')
# Plot data to explore 
counts_l %>% 
   filter(rowname %in% genes) %>% 
   ggplot(aes(x = genotype, y = log2(counts), color = treatment)) + 
   geom_point(position = position_dodge(0.9)) + 
   facet_wrap(~rowname, scales = 'free_y')
