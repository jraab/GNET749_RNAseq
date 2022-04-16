# DESeq QC
# Purpose here is to take our count data and do some basic QC on it
library(tidyverse)
library(DESeq2)

# Lets load our putuput from our DESeq results
load('data/DE_output.Rda')

# This will load two variables into our environment , des and res

# Let's do some QC on our input data (des) 

# PCA analysis is a good technique ot make sure samples are clustering 
# by the expected grouping
# First convert raw count dat 
vst <- varianceStabilizingTransformation(des, blind = T)
rlog <- rlog(des, blind = T)

plotPCA(vst, intgroup = 'Group')
plotPCA(rlog, intgroup = 'Group') 

# 
