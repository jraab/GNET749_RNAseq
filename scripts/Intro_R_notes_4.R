#Class 4 R practice notes
#load data from GSE102560

library(tidyverse)

counts <- read_csv('data/GSE102560_count_matrix.csv.gz')
counts

# pivot to long form
counts_long <- counts %>% pivot_longer(names_to = 'sample', values_to = 'count', cols = -rowname)
counts_wide <- counts_long %>% pivot_wider(names_from = 'sample', values_from = 'count') 
counts_wide # this is the same as the original dataset

# plot counts for BRG1 and BRM - remember  %in%  to match one list against another
counts_long %>% 
   filter(rowname %in% c('SMARCA2', 'SMARCA4')) %>% 
   ggplot(aes( x = sample, y = count)) + geom_point() + 
   facet_wrap(~rowname, scales = 'free_y')


# This time we don't have a table of sample information - look at column names, 
# Can we use that to separate replicates and samples labels
# ?separate (look at the help for that function)

counts_long_detailed <- counts_long %>% 
   separate(sample, into = c('genotype', 'replicate'), remove = F) 

counts_long_detailed %>% 
   filter(rowname %in% c('SMARCA2', 'SMARCA4')) %>% 
   ggplot(aes(x = genotype, y = count)) + 
   geom_point()+
   facet_wrap(~rowname, scales = 'free_y')


