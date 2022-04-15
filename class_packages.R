# Packages needed for this week

cran_packages <- c('tidyverse', 'nycflights13', 'BiocManagers')
biocon_packages <- c('DESeq2', 'sva', 'ComplexHeatmap', 'tximport', 'biomaRt', 'limma')

install.packages(cran_packages)
BiocManager::install(biocon_packages )
