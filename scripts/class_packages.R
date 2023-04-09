# Packages needed for this week
# These are some pacakges we will use in this section

cran_packages <- c('tidyverse', 'nycflights13', 'BiocManager', 'apeglm', 'ashr')
biocon_packages <- c('DESeq2', 'sva', 'ComplexHeatmap', 
                     'tximport', 'biomaRt', 'limma', 'pasilla')

install.packages(cran_packages)
BiocManager::install(biocon_packages )

