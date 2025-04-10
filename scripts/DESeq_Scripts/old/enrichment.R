# Enrichment analysis in R
library(tidyverse)
library(msigdbr)  # install.packages('msigdbr')

# Below packages can be installed with BiocManager::install(c('singscore', 'clusterProfiler', 'DOSE', 'org.Hs.eg.db') ) 
# I tried to put these in a script to install - see enrichment_packages.R
library(singscore) 
library(clusterProfiler)
library(enrichplot)
#library(biomaRt)
library(GSVA)
library(GSVAdata)
library(UpSetR) 
library(ggvenn)
library(ggVennDiagram)
library(fgsea)
# There may be an issue with RSqlite affecting loading of some packages
# If you get an issue with RSqlite - try the following
#remotes::install_version("RSQLite", version = "2.2.5")
# Either install the older version of rsqlite using above code or run 
#options(connectionObserver=NULL) # prior to running the next line 
# THe above no longer seems required,  but I'm leaving it there in case anyone has issues install/loading these libraires
library(org.Hs.eg.db) 

# The vignette for clusterprofiler has lots of useful information
# https://bioconductor.org/packages/release/bioc/vignettes/clusterProfiler/inst/doc/clusterProfiler.html

################################################################################
# Establish some of the data in the formats we'll need today
# Load the log2 fold change data set for the BRG1 knockdown experiment
brg_res <- read_csv('data/results_brg1.csv')

# Many of these tools require entrez geneIDs 
# we can use bioMart to look these up and add to our results frame. Included here for completeness - I like bitr or annotables better
#mart <- useMart(biomart = 'ensembl', dataset = 'hsapiens_gene_ensembl' )
#mart_res <- getBM(attributes = c('entrezgene_id', 'external_gene_name'), mart = mart) 

# an alternative is to use the bitr function from clusterprofiler. 
gene.df <- bitr(brg_res$rowname, fromType = "SYMBOL",
               toType = c("ENSEMBL", "SYMBOL", 'ENTREZID'),
               OrgDb = org.Hs.eg.db)

# Annotables could also be used if it has the annotations you want
# Depending on which approach you use, you'll need to make sure column names line up in the code below
anno.df <- brg_res |> left_join(annotables::grch38, by = c('rowname' = 'symbol'))
anno.df |> dplyr::filter(is.na(entrez))  |> summarise(count = n() )
# since annotables wasn't tons better, I'm going ot stick with bitr for this
# Add the entrez Ids
brg_res <- brg_res %>% left_join(gene.df, by = c('rowname' = 'SYMBOL'))

# Some of these don't have ensembl or entrezids. This could be an annotation issue or a gene where not much is known (psuedogene) 
# Remove these genes b/c we don't want to include them in downstream analysis. 
# alternatively spend time trying to figure out a better or newer set of annotations, but in this case
brg_res <- brg_res |> dplyr::filter(!is.na(ENSEMBL) & !is.na(ENTREZID) ) 
# This make sure our 'universe is correct'

# Identify significant up and downregulated genes  - These are useful for overrepresentation analysis
brg1_up <- brg_res %>% dplyr::filter(padj < 0.05, log2FoldChange > 1 ) # significant upregulated
brg1_down <- brg_res %>% dplyr::filter(padj < 0.05, log2FoldChange < -1) # significant downregulated

# Create a rank ordered list of genes - These ranked lists are useful for Gene Set Enrichment analyssi
brg1_res_ordered <- brg_res %>% arrange(desc(log2FoldChange))  
brg1_ol <- brg1_res_ordered$log2FoldChange
brg1_ids <- brg1_res_ordered$log2FoldChange
names(brg1_ol) <- brg1_res_ordered$rowname
names(brg1_ids) <- brg1_res_ordered$ENTREZID
head(brg1_ol)
head(brg1_ids)
# A few methods, (GSVA and SingScore) need full data (count/gene ranks)
load('data/GSE102560_dds.Rda')# load dds
dds <- DESeq(dds) # running this here so we get proper size factors etc 
design(dds)
dds_fix <- dds
assay(dds_fix) <- limma::removeBatchEffect(assay(dds_fix), dds$rep)

counts <- counts(dds_fix, norm = T)
#counts <- counts[rowMeans(counts) > 10, ]
dim(counts)

################################################################################
# Following are various ways and resources for doing over representation analysis
# Remember, these require a cutoff to define our list of genes of interest
# We're using genes that are up or downregulated by 1.5 fold

# Over representation analysis using KEGG 
kegg_up <-clusterProfiler::enrichKEGG(gene = brg1_up$ENTREZID, 
                                      universe = brg_res$ENTREZID, 
                                      organism     = 'hsa',
                                      pvalueCutoff = 0.05)

kegg_down <- clusterProfiler::enrichKEGG(gene   = brg1_down$ENTREZID, 
                                         universe = brg_res$ENTREZID,
                                         organism = 'hsa', 
                                         pvalueCutoff = 0.05)
dotplot(kegg_up) 
barplot(kegg_up)


dotplot(kegg_down) 
barplot(kegg_down)

kegg_up |> as_tibble()
kegg_up |> as.data.frame() |> 
   ggplot(aes(x = -log10(qvalue), y = Description, size = Count)) + 
   geom_point()

# How to make them ordered in some more interesting way
kegg_up |> as.data.frame() |> 
  arrange(desc(p.adjust))  |> 
  mutate(Description = factor(Description, levels = Description) ) |>  
  # this is a little trick to arrange by something and then relevel the factor based on that arrangement
  ggplot(aes(x = -log10(qvalue), y = Description, size = Count)) + 
  geom_point()

# Over representation of GO terms
brg_up_go <- clusterProfiler::enrichGO(gene    = brg1_up$ENTREZID,
                universe      = brg_res$ENTREZID,
                OrgDb         = org.Hs.eg.db,
                ont           = c("MF"),
                pAdjustMethod = "BH",
                pvalueCutoff  = 0.01,
                qvalueCutoff  = 0.05,
                readable      = TRUE)

brg_down_go <- clusterProfiler::enrichGO(gene    = brg1_down$ENTREZID,
                universe      = brg_res$ENTREZID,
                OrgDb         = org.Hs.eg.db,
                ont           = c("MF"),
                pAdjustMethod = "BH",
                pvalueCutoff  = 0.01,
                qvalueCutoff  = 0.05,
                readable      = TRUE)

dotplot(brg_up_go) 
dotplot(brg_down_go)

# termsim can be useful for seeing how similar categories are
emapplot(pairwise_termsim(brg_down_go)) 
emapplot(pairwise_termsim(kegg_down))
# Can remove similar terms - does not work if using ont = 'ALL'
dotplot(brg_down_go)
dotplot(simplify(x = brg_down_go, cutoff = 0.2)) # 0.7 is default, but I wanted to make a point

# Can look at gene expression within categories using heatplot
heatplot(brg_down_go, foldChange = brg1_ol, showCategory = 10)  #not terribly interesting since we limited to up or downregulated genes

# lets try with all genes
brg_all_go <- enrichGO(gene = c(brg1_up$ENTREZID, brg1_down$ENTREZID), 
                       universe      = brg_res$ENTREZID,
                       OrgDb         = org.Hs.eg.db,
                       ont           = 'MF',
                       pAdjustMethod = "BH",
                       pvalueCutoff  = 0.01,
                       qvalueCutoff  = 0.05,
                       readable      = TRUE)
heatplot(brg_all_go, foldChange = brg1_ol)

#  You can also do overrepresentation with any arbitrary gene set
# Here we'll get the msigdb data from the msigdbr package   
msigdbr_df <- msigdbr::msigdbr(species = 'Homo sapiens') # this retrieves all the data sets
msigdbr_species()
length(unique(msigdbr_df$gs_name) ) # ~32,000 gene sets
msigdbr_df |> head()

msigdbr_t2g = msigdbr_df %>% dplyr::select(gs_name, human_gene_symbol) %>% as.data.frame() # Make a df that contains the terms and genes
msigdbr_t2g %>% head()

# Generic enricher functions - run on the whole data set - not for class, too slow
#brg1_up_enr <- clusterProfiler::enricher(gene = brg1_up$rowname, TERM2GENE = msigdbr_t2g)
#brg1_down_enr <- clusterProfiler::enricher(gene = brg1_down$rowname, TERM2GENE = msigdbr_t2g)
#dotplot(brg1_up_enr)
#barplot(brg1_down_enr)
#cnetplot(brg1_up_enr)
#emapplot(pairwise_termsim(brg1_up_enr), showCategory = 20 )
#emapplot(pairwise_termsim(brg1_down_enr))

# I often like the hallmark data set, but I'm also filtering here so this runs a little faster, you could run this on the whole data frame
# Let's run the above but just on the hallmark genes
hallmark <- msigdbr_df %>% dplyr::filter(gs_cat == 'H') 
hallmark_t2g <- hallmark %>% dplyr::select(gs_name, human_gene_symbol) %>% as.data.frame() 
colnames(hallmark_t2g) <- c('TERM', 'GENE') 
e_hall <- enricher(gene = c(brg1_down$rowname, brg1_up$rowname), TERM2GENE = hallmark_t2g )
dotplot(e_hall)
emapplot(pairwise_termsim(e_hall) ) 
################################################################################
#################################################################################
################################################################################

#### Gene Set Enrichment Analysis - main benefit is that it does not require an arbitrary threshold ######
brg1_hallmark <- GSEA(geneList = rev(sort(brg1_ol)), TERM2GENE = hallmark_t2g, by = 'fgsea' )
brg1_hallmark %>% as.data.frame() %>% head()


brg1_hallmark %>%
   as.data.frame() %>%
   rownames_to_column() %>%
   arrange(NES) %>%
   mutate(rowname = factor(rowname, levels = rowname) ) %>%
   ggplot(aes(x = rowname, y = NES, fill = as.factor(sign(NES)))) + 
   geom_col(color = 'grey30') + 
   coord_flip() + 
   scale_fill_manual(values = c('blue', 'gold2') )  
   

gseaplot2(brg1_hallmark, geneSetID = 'HALLMARK_TNFA_SIGNALING_VIA_NFKB') 
gseaplot(brg1_hallmark, geneSetID = 'HALLMARK_TNFA_SIGNALING_VIA_NFKB')
ridgeplot(brg1_hallmark, fill = 'p.adjust', core_enrichment = T) 
# THe core_enrichment argument tells the plot to only include those genes that were in the core_enrichment column
# these are the genes that contribue the most to the scores
ridgeplot(brg1_hallmark, fill = 'p.adjust', core_enrichment = F) 

# We can extract a specific set of these genes
hallmark_subset <- hallmark_t2g %>% dplyr::filter(TERM == 'HALLMARK_TNFA_SIGNALING_VIA_NFKB') %>% pull(GENE)
# and plot this subset as a volcano plot
brg_res %>%
   dplyr::filter(rowname %in% hallmark_subset) %>%
   ggplot(aes(x =log2FoldChange, y = -log10(padj), color = padj < 0.05 )) + 
   geom_point() + 
   scale_color_manual(values = c('grey70', 'red2')) + 
   theme_bw()  + 
   ggrepel::geom_label_repel(aes(label = rowname), color = 'steelblue')



################################################################################
### Scoring for signatures - this gives a score to each sample, so you don't need to 
# decide on your comparison ahead of time. One way I've used this in the past is to 
# score multiple different populations of cells based on their expression of known marker genes 
# derived from single cell experiments. 
################################################################################
# Since we already know the HALLMARK_TNFA_SIGNALING_VIA_NFKB is enriched in our samples, lets score our data for this signature

# We want our original data, which we can load from a saved DESeqDataSet
# Remember counts is coming from the entire dds object we loaded up top
rg <- rankGenes(counts) # this ranks the expression level in each of our samples
rg
# Now we pass this to simplescore along with a list of genes for our data set
ss <- simpleScore(rg, upSet = unique(hallmark_subset) ) 
ss

table(rownames(rg) == 'CYR61')
# Using this we can compare our scores across our samples
# best to incorporate our conditions
ss %>% as.data.frame() %>%
   rownames_to_column() %>%
   separate(rowname, into = c('group', 'rep'), sep = '_', remove =F )  %>%
   ggplot(aes(x = group, y = TotalScore)) + geom_point() 

#You can also give simplescore an upregulated and downregulated set
# HSIAO_LIVER_SPECIFIC_GENES

liver_genes <- msigdbr_df[grepl('HSIAO_LIVER_SPECIFIC_GENES', msigdbr_df$gs_name), ] %>% dplyr::select(gs_name, gene_symbol)
liver_genes
# lets score for increase TNFA and decrease liver specific genes
ss_liver <- simpleScore(rg, upSet= unique(hallmark_subset), downSet = unique(liver_genes$gene_symbol) )
ss_liver %>% as.data.frame() %>%
   rownames_to_column() %>%
   separate(rowname, into = c('group', 'rep'), sep = '_', remove =F )  %>%
   ggplot(aes(x = group, y = TotalScore)) + geom_point() 


################################################################################
# Overlaps between groups  #################################################### 
################################################################################
brg_res <- read_csv('data/results_brg1.csv')
brm_res <- read_csv('data/results_brm.csv')
double_res <- read_csv('data/results_double.csv')

brg_sig <- brg_res %>% dplyr::filter(padj < 0.05) 
brm_sig <- brm_res %>% dplyr::filter(padj <0.05)
double_sig <- double_res %>% dplyr::filter(padj < 0.05)
x <- list(brg=brg_sig$rowname, brm=brm_sig$rowname, double=double_sig$rowname)  # list of our gene names, 1 element for each data set
head(x)
# If you want separate colors for each, this works well
ggvenn::ggvenn(x)
ggvenn::ggvenn(x[1:2])
ggvenn::ggvenn(x, fill_color = c('red', 'blue', 'green') ) 


# IF you want color to map to the counts in a section use this
ggVennDiagram::ggVennDiagram(x)
ggVennDiagram::ggVennDiagram(x, label_alpha  = 0) + 
   ggplot2::scale_fill_gradient(low="blue",high = "yellow") + 
   scale_color_manual(values = c('grey30', 'grey30', 'grey30') ) 

ggVennDiagram::ggVennDiagram(x, label_alpha  = 0) + 
   ggplot2::scale_fill_gradient(low = 'white', high = 'red') + 
   scale_color_manual(values = c('grey30', 'grey30', 'grey30') ) 

# UpSet plots - overlap of categories - similar to venn
# but much better 
UpSetR::upset(fromList(x)) 
upset(fromList(x), matrix.color = 'steelblue', main.bar.color = 'hotpink' )
