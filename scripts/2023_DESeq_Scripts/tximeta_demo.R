# using tximeta

library(tximeta)
library(tidyverse)
library(DESeq2)

#devtools::install_github("RobinHankin/Brobdingnag")
# Import design data 
cdata <- read_csv('data/class_data_info.csv') 
cdata
colnames(cdata)[1] <- 'names'
cdata
# This line creates a new column to keep track of where each Salmon output file is
# file.path makes sticks the arguments together with / between them to make path names
cdata$files <- file.path('data/salmon', paste0(cdata$names, '_decoy_quant'), 'quant.sf')

se <- tximeta::tximeta(cdata)
se

gse <- summarizeToGene(se)
rowRanges(gse)
