# Packages for Class 4
BiocManager::install(c('DOSE', 'clusterProfiler', 'GSVA', 'singscore', 'enrichplot', 'biomaRt', 'msigdbr', 'limma', 'sva', 'GSVAdata', 'org.Hs.eg.db', 'fgsea'))
install.packages(c('UpSetR', 'VennDiagram', 'ggnewscale')) 
if (!require(devtools)) install.packages("devtools")
devtools::install_github("yanlinlin82/ggvenn")
devtools::install_github('gaospecial/ggVennDiagram')
