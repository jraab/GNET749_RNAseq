# taylor swift time
library(tidyverse)
# download taylor facts from kaggle
# save the zip file somewhere, and then unzip
download.file(url = download.file('https://raw.githubusercontent.com/jraab/GNET749_RNAseq/main/data/taylor_swift_spotify.csv', 
                                  method = 'curl', 
                                  destfile = '/Users/jraab/Desktop/taylor_swift_spotify.csv'))

taylor <- read_csv('~/GitHub/GNET749_RNAseq/data/taylor_swift_spotify.csv')
taylor

taylor |> 
  ggplot(aes(x = danceability, y = popularity)) + 
  geom_point()

pairs(taylor[,8:ncol(taylor)])
      