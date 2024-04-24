# DESeq QC
# Purpose here is to take our count data and do some basic QC on it
library(tidyverse)
library(DESeq2)
library(ComplexHeatmap) # bioconductor

# Lets load our outuput from our DESeq results
load('data/DE_output.Rda')
# This will load two variables into our environment, des and res
des <- estimateSizeFactors(des)
# get the raw count data
raw <- counts(des, normalize = F) 
normalized <- counts(des, normalize = T)

sample_info <- colData(des) |> as.data.frame() # gets the sample information from the summarized experiment
sample_info

################################################################################# 
# pivot data into long form
raw_l <- raw |> 
   as.data.frame() |> 
   rownames_to_column() |> 
   pivot_longer(names_to = 'Sample', values_to = 'count', -rowname)  
raw_l
norm_l <- normalized |> 
   as.data.frame() |> 
   rownames_to_column() |> 
   pivot_longer(names_to = 'Sample', values_to = 'count', -rowname)  
norm_l
raw_l <- raw_l |>  left_join(sample_info, by = 'Sample' ) |> mutate(method = 'raw')
norm_l <- norm_l |> left_join(sample_info, by = 'Sample') |> mutate(method = 'norm')
all_counts <- bind_rows(raw_l, norm_l)
all_counts
#################################################################################  
# Now you can plot them together to see how the  counts per sample differ
all_counts |> 
   ggplot(aes(x = Sample, y = log2(count+1), fill = Group)) + 
   geom_boxplot() + 
   coord_flip() + 
   facet_wrap(~method)

# why use log2 counts
all_counts |> 
  ggplot(aes(x = count, fill = method)) + 
  geom_density()
 
all_counts |> 
  ggplot(aes(x = log2(count), fill = method)) + 
  geom_density(alpha = 0.5) + 
  facet_wrap(~method , ncol = 1) 

all_counts |> 
  ggplot(aes(x = log2(count+1), fill = method)) + 
  geom_density()

# So log2 counts or counts+1 not a great way to visualize - see vst/rlog below
# Can also summarize by group for total counts
all_counts |> 
   group_by(Sample, Group, method) |> 
   summarise(total_reads = sum(count, na.rm = T) )  |> 
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
rlog_t <- rlog(des, blind = T)

DESeq2::plotPCA(vst, intgroup = 'Group', ntop = 1000)
DESeq2::plotPCA(rlog_t, intgroup = 'Group')
# can return the underlying data for the plot with returnData=T
DESeq2::plotPCA(vst, intgroup = 'Group', returnData = T)
DESeq2::plotPCA(rlog_t, intgroup = 'Group', ntop = 100) 
# Not perfect, but there is some grouping

################################################################################
## Let's cluster samples using a heatmap and correlation 
# lots going on in this next line
# assay(vst) - gets the variance stabilized count information
# t() - transposes a matrix (i.e. makes the rows into columns)
         m <- matrix(c(1,2,3,4,5,6), nrow = 2)
         m
         t(m)
# dist() - calculates distance between samples - way to look at groupings
# as.matrix() - dist needs a matrix to operate on - so we convert out counts data to a matrix
cor_samples <- as.matrix(dist(t(assay(vst)) )  )
cor_samples
sample_df <- colData(vst) 

sample_df
anno_df <- sample_df |> as.data.frame() |> dplyr::select(Sample, Group) 
#heatmap of similarities between samples - 
#library(ComplexHeatmap)
hm_anno <-  HeatmapAnnotation(df  = sample_df$Group, col = list(df =c('A' = 'goldenrod', 'B' = 'steelblue') ) )
# Heatmap comes from ComplexHeatmap
ComplexHeatmap::Heatmap(cor_samples, top_annotation = hm_anno) 
Heatmap(cor(cor_samples) ) 

# A different data set - multiple groups and known replicates
load('data/GSE102560_dds.Rda')
# Need to know or remember what comes with an Rda object
# In this case it is dds - which I'll rename to limit confusion
swi_dds <- dds
design(swi_dds)
# Need to VST transform
swi_vst <- varianceStabilizingTransformation(swi_dds, blind = T) 
plotPCA(swi_vst, intgroup = 'condition')

colData(swi_dds)

pc_plot <- plotPCA(swi_vst, intgroup = 'condition', returnData = T)
pc_plot

pc_plot <- pc_plot  |> left_join(as.data.frame(colData(swi_dds)), by = c('name' = 'samples') ) 
pc_plot
pc_plot |> 
  ggplot(aes(x = PC1, y = PC2, color = group, shape = rep) ) + 
  geom_point(size = 3)

# Note the batch effect by replicate  
