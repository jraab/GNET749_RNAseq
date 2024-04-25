# Packages for Class 4

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.18")

BiocManager::install(c('DOSE', 'clusterProfiler', 'GSVA', 'singscore', 'enrichplot', 'biomaRt', 'msigdbr', 'limma', 'sva', 'GSVAdata', 'org.Hs.eg.db', 'fgsea'))
install.packages(c('UpSetR', 'VennDiagram', 'ggnewscale')) 
if (!require(devtools)) install.packages("devtools")
devtools::install_github("yanlinlin82/ggvenn")
devtools::install_github('gaospecial/ggVennDiagram')
BiocManager::install('')

