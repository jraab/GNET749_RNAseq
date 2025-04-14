# Packages needed for this class
# These are some packages we will use in this section
# Jesse will update this if we need another packages


cran_packages <- c('tidyverse', 'nycflights13', 'janitor', 'palmerpenguins', 'BiocManager', 'apeglm', 'ashr')
biocon_packages <- c('DESeq2', 'sva', 'ComplexHeatmap', 
                     'tximport', 'biomaRt', 'limma', 'pasilla', 
                     'msigdbr')

install.packages(cran_packages)
BiocManager::install(biocon_packages )

