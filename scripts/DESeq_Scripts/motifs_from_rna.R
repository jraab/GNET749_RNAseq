################################################################################
#further downstream - motifs
# see memems package
# https://snystrom.github.io/memes-manual/
library(BSgenome.Hsapiens.UCSC.hg38)
#BiocManager::install('BSgenome.Hsapiens.UCSC.hg38')
library(plyranges)
library(DESeq2)
library(memes)
library(universalmotif)

load('data/rna_de_obj.Rda')
des$treatment <- relevel(des$treatment, 'BAF155-WT')
des <- nbinomWaldTest(des)
resultsNames(des)
res_baf <- lfcShrink(des, coef = 3, type = 'apeglm', format = 'GRanges', saveCols = 2) |> 
  keepStandardChromosomes(pruning.mode = 'coarse')
# need the above calls to remove scaffold and patch chromosomes so style can be changed below
seqlevelsStyle(res_baf) <- 'UCSC'


# get the promoters of upregulated genes
# this makes use of some tidygenomics framework - library(plyranges)
# this lets us do things like filter granges on a column as we would with tidyverse
upreg <- res_baf |> filter(padj < 0.05) |> filter(log2FoldChange > 0)
downreg <- res_baf |> filter(padj < 0.05) |> filter(log2FoldChange < 0) 

upreg_prom <- promoters(upreg, upstream = 200, downstream = 1000)
downreg_prom <- promoters(downreg, upstream = 200, downstream = 1000)
hg38 <- BSgenome.Hsapiens.UCSC.hg38
up_sequences <- get_sequence(upreg_prom, genome = hg38)
down_sequences <- get_sequence(downreg_prom, genome = hg38)
up_sequences |> head()
#discover motifs
streme_results <- runStreme(up_sequences, control = 'shuffle')
# match to known motifs
matched <- streme_results |> runTomTom('~/proj/motif_databases/HUMAN/HOCOMOCOv11_core_HUMAN_mono_meme_format.meme')

streme_results |> 
  universalmotif::to_list()  |> 
  universalmotif::view_motifs()

# run comparison between two gruops
by_group <- list(up = up_sequences, down = down_sequences)
names(by_group)

compare <- runStreme(by_group, control = 'shuffle')
# match to motifs
meme_db <- read_meme("~/proj/motif_databases/HUMAN/HOCOMOCOv11_core_HUMAN_mono_meme_format.meme") |> 
  to_df() |> 
  mutate(altname = as.character(dplyr::row_number())) |> 
  to_list() 

#above is spencer's fix for that one issue
compare <- compare |> runTomTom(database = meme_db)
ame_by_group <- runAme(by_group, database = meme_db)

ame_by_group |> bind_rows(.id = 'direction') |> plot_ame_heatmap(group = direction, value = 'normalize')

compare
ame_by_group
