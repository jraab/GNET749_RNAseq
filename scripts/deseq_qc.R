# DESeq QC
# Purpose here is to take our count data and do some basic QC on it
library(tidyverse)
library(DESeq2)
library(ComplexHeatmap)

# Lets load our putuput from our DESeq results
load('data/DE_output.Rda')

# This will load two variables into our environment , des and res

# get the raw count data
raw <- counts(des) 
normalized <- counts(des, normalize = T)
sample_info <- colData(des) %>% as.data.frame() # gets the sample information from the summarized experiment

################################################################################# 
# pivot data into long form
# can use basic barplot here - barplot(raw); barplot(normalized)
raw_l <- raw %>% 
   as.data.frame() %>% 
   rownames_to_column() %>% 
   pivot_longer(names_to = 'Sample', values_to = 'count', -rowname)  

norm_l <- normalized %>% 
   as.data.frame() %>% 
   rownames_to_column() %>% 
   pivot_longer(names_to = 'Sample', values_to = 'count', -rowname)  

raw_l <- raw_l %>%  left_join(sample_info, by = 'Sample' ) %>% mutate(method = 'raw')
norm_l <- norm_l %>% left_join(sample_info, by = 'Sample') %>% mutate(method = 'norm')
all_counts <- bind_rows(raw_l, norm_l)
#################################################################################  
all_counts %>% 
   group_by(Sample, Group, method) %>% 
   summarise(total_reads = mean(count, na.rm = T) )  %>% 
   ggplot(aes(x = Sample, y = total_reads, fill = Group)) + 
   geom_col() + 
   coord_flip() + 
   facet_wrap(~method)
   
################################################################################
# Look at distribution before and after normalization
all_counts %>% 
   ggplot(aes( x= Sample, y= log2(count), fill = Group))  + 
   geom_violin() + 
   geom_boxplot(fill = 'grey80', width = 0.1) + 
   coord_flip() + 
   facet_wrap(~method)


# QC using PCA - samples should group by the condition(s) we are interested in
# Let's do some QC on our input data (des) 
# PCA analysis is a good technique ot make sure samples are clustering 
# by the expected grouping
# First convert raw count dat 
vst <- varianceStabilizingTransformation(des, blind = T)
rlog <- rlog(des, blind = T)

plotPCA(vst, intgroup = 'Group')
plotPCA(rlog, intgroup = 'Group') 
# Not perfect, but there is some grouping

################################################################################
## Let's cluster samples using a heatmap and correlation 
cor_samples <- as.matrix(dist(t(assay(vst)) )  )
sample_df <- colData(vst) 
sample_df
anno_df <- sample_df %>% as.data.frame() %>% select(Sample, Group) 
hm_anno <-  HeatmapAnnotation(df  = sample_df$Group ) 
Heatmap(cor_samples, top_annotation = hm_anno) 





