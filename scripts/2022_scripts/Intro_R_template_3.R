# Working with and plotting data
getwd()
setwd('/Users/jraab/GitHub/GNET749_RNAseq/')
getwd()
library(tidyverse)
# Load count data and sample info data we download from github
# github.com/jraab/GNET749_RNAseq
# drug_norm_counts.csv
# drug_samples.csv

# Need to read in data from whereer you saed the files above - 
# Here is how I do it on my system
counts <- read_csv('/Users/jraab/GitHub/GNET749_RNAseq/data/drug_norm_counts.csv')
sample_info <- read_csv('/Users/jraab/GitHub/GNET749_RNAseq/data/drug_samples.csv')
counts
sample_info
# How many treatments are there
table(sample_info$treatment)
sample_info %>% 
   group_by(treatment) %>% 
   summarise(count = n() )

# How Many genotypes
table(sample_info$genotype)
sample_info %>% 
   group_by(genotype) %>% 
   summarise(count = n () )

# How many of each combination
table(sample_info$genotype, sample_info$treatment)
sample_info %>% 
   group_by(genotype, treatment) %>% 
   summarise(count = n () ) %>% 
   ggplot(aes( x = genotype, y = count, color = treatment ))  + geom_point()


# Make simple plots for a few genes to see if there is a difference
# Some genes we can use to look at differences

# Make the count data long style then include sample information 
counts_long <- counts %>% 
   pivot_longer(names_to = 'samples', values_to = 'count', cols = 2:11)
pivot_longer()
counts_long
# Join with sample information
joined_data <- counts_long %>% left_join(sample_info, by = 'samples')
joined_data <- counts_long %>% left_join(sample_info, by = c('samples' = 'samples') ) 

# Plot data to explore relationship between treatment and genotype
genes
joined_data %>% 
   filter(rowname == 'MCRS1') %>% 
   ggplot(aes(x = genotype, y = count, color = treatment))  + 
   geom_point() 

genes <- c('MCRS1', 'MAP3K7', 'ARID2')
joined_data %>% 
   filter(rowname %in% genes) %>% 
   ggplot(aes(x = genotype, y = count, color = treatment))  + 
   geom_point()  + 
   facet_wrap(~rowname)
