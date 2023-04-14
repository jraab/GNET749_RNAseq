# Make UNC/Duke Data for class
library(tidyvers)
library(janitor)
duke_file <- '~/Desktop/duke_stats.csv'
unc_file  <- '~/Desktop/unc_stats.csv'

cnames <- c('row', 'season', 'conf', 'overall_w', 'overall_l', 'overall_w_l_percent', 
            'conf_w', 'conf_l', 'conf_w_l_percent', 'srs', 'sos', 'pts_for_avg', 'pts_against_avg', 
            'ap_pre', 'ap_high', 'ap_final', 'tourney', 'coach') 

duke <- read_csv(duke_file, skip = 2, col_names = cnames) |> mutate('BadGuys')
unc <- read_csv(unc_file, skip = 2, col_names = cnames) |> mutate('GoodGuys')

combined <- bind_rows(duke, unc)
write_csv(combined, file = 'GitHub/GNET749_RNAseq/data/duke_unc_hoops.csv')
