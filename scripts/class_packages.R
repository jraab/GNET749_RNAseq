# Packages needed for this class
# These are some packages we will use in this section
# Jesse will update this if we need another packages
if (!require("BiocManager", quietly = TRUE))
BiocManager::install(version = "3.22")
# R version 4.5.1 matches Bioconductor 3.22 (mismatching these can be an issue)


cran_packages <- c('tidyverse', 'nycflights13', 'janitor', 'palmerpenguins', 'BiocManager', 'apeglm', 'ashr', 'taylor')
biocon_packages <- c('DESeq2', 'sva', 'ComplexHeatmap', 
                     'tximport', 'biomaRt', 'limma', 'pasilla', 
                     'msigdbr', 'BiocFileCache')

install.packages(cran_packages)
BiocManager::install(biocon_packages )
#BiocManager::install('')
 