# Taylor Swift Data
# what makes a taylor swift song popular
library(tidyverse)

taylor <- read_csv(here::here('data/taylor_swift_spotify.csv'))

pairs(taylor[,8:ncol(taylor)])


# how many albums are there

# how many songs per album

# does any of the attributes change over time

# what album was loudest

# what was most popular

# plot albums by popularity

# plot songs by popularity for top 20 songs - color by album




