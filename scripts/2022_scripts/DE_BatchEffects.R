# These data are come from GEO - GSE102560 and the raw count matrix can be downloaded - I'll also put this on Sakai
# This is a bit more complicated as we have 5 different experimental conditions and multiple batches
library(DESeq2)
library(limma)
library(sva)

# Load in data that has a batch associated with it
df <- read_csv('/Users/jraab/GitHub/GNET749_RNAseq/data/GSE102560_count_matrix.csv.gz') # CHANGE to match where you have this file
# Data is the same as we used in Intro_R_notes_4.R
design <- data.frame(samples = colnames(df)[2:ncol(df)]) # generate sample names using the column names of the count matrix
design <- design %>%
   separate(samples, into= c('condition', 'rep'), sep = '_', remove = F)  # use the sample name to define the condition and replicate informatio   
rn <-df$rowname # set up rownames for the matrix
mat <- as.matrix(df[,2:ncol(df) ]) # make a count matrix 
rownames(mat) <- rn # add the gene names back as rownames

# Our colData design and matrix of counts
design
head(mat)

# Create DESeqDataSet using this matrix
# Slightly different function because we didn't import this from tximport/salmon
dds <- DESeqDataSetFromMatrix(mat, colData = design, design = ~ rep + condition) # note: the design formula here includes replicate
# when controlling for batch/replicate it goes first in the formula
# The last part of the formula is the condition that is being tested for 

#Now use VST and PCA to look for batch effects
vsd <- varianceStabilizingTransformation(dds, blind = T) 
plotPCA(vsd) # looks like there is not perfect grouping

# Let's replot and show replicate as shape
pp <- plotPCA(vsd, intgroup = c('condition', 'rep'), returnData = T) 
pp
pp %>%
   ggplot(aes( x = PC1, y = PC2, color = condition, shape = rep)) + 
   geom_point(size = 3)

#Now its clear that there is a batch effect
# Lets compare results accounting for batch and ignoring batch
# DEseq with batch accounted for
dds <- DESeq(dds)
# DESEq without batch accounted for
no_batch <- dds # making a duplicate of dds we can alter the design of
design(no_batch) <- formula(~condition)  # replace the original design
design(no_batch) 
no_batch <- DESeq(no_batch)

res_nobatch <- results(no_batch, contrast  = c('condition', 'Brg1', 'NS') ) 
res_batch   <- results(dds, contrast = c('condition', 'Brg1', 'NS') )                        



# 
# Let's plot number of signifcant genes as a bargraph
nobatch_sig <- res_nobatch %>%
   as.data.frame() %>%
   rownames_to_column() %>%
   filter(padj < 0.05) %>% 
   mutate(test = 'no-batch') 

batch_sig <- res_batch %>%
   as.data.frame() %>%
   rownames_to_column() %>%
   filter(padj < 0.05 ) %>%
   mutate(test = 'batch')

rbind(nobatch_sig, batch_sig ) %>%
   group_by(test) %>%
   summarise(total_sig = n() ) %>%
   ggplot(aes(x = test, y = total_sig)) + 
   geom_col()

# But that just accounts for the batch effect when DE testing, it does not alter underlying counts
# If we know the batch information, we can use the limma package to remove them from the data
# this is useful if you want to PCA or work on the transformed count data

vsd_batch_removed <- vsd
vsd$rep
# This step alters the variance stabilized data to remove the batch effect using limma
assay(vsd_batch_removed) <- limma::removeBatchEffect(assay(vsd_batch_removed), batch = vsd$rep) 
# Now we can replot as before
pbatch <- plotPCA(vsd_batch_removed, c('condition', 'rep'), returnData = T) 
pbatch %>% 
   ggplot(aes( x = PC1, y = PC2, color = condition, shape = rep)) + 
   geom_point(size = 3)

# Now clustering looks much better
################################################################################
# What if we don't know the batch information, but suspect there is a batch effect
# Can use SVA or RUVseq or ComBat to try and identify a batch effect 
# This can then be used in both the DESeq analysis and with limma to remove effect

#  Here, we'll try SVA - surrogate variable analysis.
# we need to give it a full model with our variables of interest, here just condition
full <- model.matrix(~condition, data = design)
full
# and a reduced model with any remaining variables, here just the intercept (1) 
reduced <- model.matrix(~1, data = design) 
reduced
# Then we can get the normalized count data from dds
dat <- counts(dds, normalized = T) 
# And remove any very lowly expressed genes since the won't e informative
idx <- rowMeans(dat) > 1
table(idx)
dat <- dat[idx,]
# now we use sva on our count data, comparing the full and reduced model
# We are asking sva to create new variables that preserve the effect of interest but remove others

svobj <- svaseq(dat, full, reduced) # we can ad the number of surrogate variables here as n.sv = , but I"m leaving it to sva to estimate
svobj # this gave 2 surrogate variable
svobj$sv # and a value for each of these, we can think of them as weights that can be added to DESeq2

# Now we can use these variables in our DESeq analysis as covariates in our model
dds_sva <- dds
dds_sva
dds_sva$V1 <- svobj$sv[,1] # This adds the surrogate variables to our colData
dds_sva$V2 <- svobj$sv[,2]
dds_sva
design(dds_sva) <- ~V1 +V2 + condition # Including them here asks DESEq to account for those variables
dds_sva <- DESeq(dds_sva)
sva_res <- results(dds_sva, contrast = c('condition', 'Brg1', 'NS') ) 
summary(sva_res)
summary(res_batch) 
summary(res_nobatch)
# Including this batch effect parameter improved our power compared to no batch

# we can also try to fix the pca using this new component
vsd_sva <- varianceStabilizingTransformation(dds_sva, blind = T)
assay(vsd_sva) <- limma::removeBatchEffect(assay(vsd_sva), covariates = svobj$sv) 

plotPCA(vsd, intgroup = c('condition') ) 
plotPCA(vsd_batch_removed, intgroup = 'condition') #using known batch
plotPCA(vsd_sva, intgroup = 'condition')   # SVA batches

# Does this method actually work - well lets permute the labels on our design matrix
design_permute <- design
design_permute$condition <- sample(design_permute$condition, replace = F) # shuffle the labels
design_permute # now our labelled condition no longer matches our real condition
full_wrong <- model.matrix(~condition, data = design_permute)
reduced_wrong <- model.matrix(~1, data = design_permute) 
sva_wrong <- svaseq(dat, full_wrong, reduced_wrong) 
# This came up with 3 surrogate variables
dds_wrong <- DESeqDataSetFromMatrix(mat, colData = design_permute, design = ~ condition)  
dds_wrong$V1 <- sva_wrong$sv[,1]
dds_wrong$V2 <- sva_wrong$sv[,2]
dds_wrong$V3 <- sva_wrong$sv[,3]
design(dds_wrong) <- ~V1 + V2 + V3 + condition
design(dds_wrong)
dds_wrong <- DESeq(dds_wrong) 
res_wrong <- results(dds_wrong, contrast = c('condition', 'Brg1', 'NS') ) 
summary(res_wrong) 
# That kills most of the DE genes

# How about clustering - does adding inferred batches cause inappropriate clustering
vsd_wrong <- varianceStabilizingTransformation(dds_wrong, blind = T) 
assay(vsd_wrong) <- limma::removeBatchEffect(assay(vsd_wrong), covariates = sva_wrong$sv)
plotPCA(vsd_wrong, intgroup = c('condition', 'rep'), returnData = T)  %>%
   separate(name, into = c('type', 'rep'), sep ='_')  %>%
   ggplot(aes(x = PC1, y = PC2, color = type, shape = rep)) + 
   geom_point(size = 3)
   
