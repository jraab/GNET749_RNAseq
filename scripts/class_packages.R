# Packages needed for this week

cran_packages <- c('tidyverse', 'nycflights13', 'BiocManager', 'apeglm', 'ashr')
biocon_packages <- c('DESeq2', 'sva', 'ComplexHeatmap', 
                     'tximport', 'biomaRt', 'limma', 'pasilla')

install.packages(cran_packages)
BiocManager::install(biocon_packages )

