# DESeq QC
# Purpose here is to take our count data and do some basic QC on it
library(tidyverse)
library(DESeq2)
library(ComplexHeatmap)

# Lets load our outuput from our DESeq results
load('data/DE_output.Rda')
# This will load two variables into our environment , des and res

# get the raw count data
raw <- counts(des) 
normalized <- counts(des, normalize = T)
sample_info <- colData(des) %>% as.data.frame() # gets the sample information from the summarized experiment

################################################################################# 
# pivot data into long form
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
# Now you can plot them together to see how the average counts per sample differ
all_counts %>% 
   group_by(Sample, Group, method) %>% 
   summarise(total_reads = mean(count, na.rm = T) )  %>% 
   ggplot(aes(x = Sample, y = total_reads, fill = Group)) + 
   geom_col() + 
   coord_flip() + 
   facet_wrap(~method)
   
# QC using PCA - samples should group by the condition(s) we are interested in
# Let's do some QC on our input data (des) 
# PCA analysis is a good technique ot make sure samples are clustering 
# by the expected grouping
# First convert raw count data
vst <- varianceStabilizingTransformation(des, blind = T)
rlog <- rlog(des, blind = T)

plotPCA(vst, intgroup = 'Group')
plotPCA(rlog, intgroup = 'Group') 
# Not perfect, but there is some grouping

################################################################################
## Let's cluster samples using a heatmap and correlation 
# lots going on in this next line
# assay(vst) - gets the variance stabilized count information
# t() - transposes a matrix (i.e. makes the rows into columns)
         #m <- matrix(c(1,2,3,4,5,6), nrow = 2)
         #m
         #t(m)
# dist() - calculates distance between samples - way to look at groupings
# as.matrix() - dist needs a matrix to operate on - so we convert out counts data to a matrix
cor_samples <- as.matrix(dist(t(assay(vst)) )  )
cor_samples
sample_df <- colData(vst) 
sample_df
anno_df <- sample_df %>% as.data.frame() %>% dplyr::select(Sample, Group) 
# heatmap of similarities between samples - we'll discuss this in more detail next week
hm_anno <-  HeatmapAnnotation(df  = sample_df$Group, col = list(df =c('A' = 'goldenrod', 'B' = 'steelblue') ) )
Heatmap(cor_samples, top_annotation = hm_anno) 





