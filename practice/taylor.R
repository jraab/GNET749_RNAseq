# Taylor Swift Data
# what makes a taylor swift song popular
library(tidyverse)
#install.packages("taylor")
library(taylor)


# what data exists in this package
library(help = 'taylor')

taylor_albums
taylor_all_songs
# how many albums are there
nrow(taylor_albums)

taylor_all_songs |> 
  group_by(album_name) |> 
  summarise(song_count = n()) |> 
  arrange(desc(song_count))

# how many songs per album
taylor_all_songs |> 
  group_by(album_name) |> 
  summarise(song_count = n()) |> 
  arrange(desc(song_count))

length(unique(taylor_all_songs$album_name) )
length(unique(taylor_albums))

# does any of the attributes change over time

# how to get song 'attributes'
colnames(taylor_all_songs)
taylor_attributes_summarized <- taylor_all_songs |> 
  group_by(album_name, year(album_release)) |> 
  summarise(album_loudness = mean(loudness, na.rm = T), 
            album_danceability = mean(danceability, na.rm = T), 
            album_energy = mean(energy, na.rm = T),  
            album_insturmentalness = mean(instrumentalness, na.rm = T), 
            album_acousticness = mean(acousticness, na.rm = T), 
            album_valence = mean(valence, na.rm = T), 
            album_temp = mean(tempo, na.rm = T))

taylor_attributes_summarized |> 
   janitor::clean_names() |> 
   filter(!is.na(year_album_release)) |> 
  pivot_longer(names_to = 'attribute', values_to = 'value',  cols = c(-album_name, -year_album_release)) |> 
  ggplot(aes(x = year_album_release, y = value)) + geom_point() + facet_wrap(~attribute, scales = "free_y") +
  geom_smooth(method = 'lm')

# what album was loudest
taylor_attributes_summarized |> 
  arrange(desc(album_loudness))

# what was most popular
taylor_albums |> 
  arrange(desc(metacritic_score))
  
taylor_albums |> #
  arrange(desc(user_score))

# plot albums by popularity
taylor_albums |> 
  ggplot(aes(x = metacritic_score, y = user_score)) + 
  geom_point()
  
# whats the outlier point
taylor_albums |> 
  ggplot(aes(x = metacritic_score, y = user_score)) + 
  geom_point() + 
  geom_label(aes(label = album_name))

# plot songs by popularity for top 20 songs - color by album
# slightly older data set where I have per track information
taylor <- read_csv(here::here('data/taylor_swift_spotify.csv'))

taylor |> 
  arrange(desc(popularity)) |> 
  head(n = 20) |> 
  mutate(name = factor(name, levels = unique(name))) |> 
  mutate(album_name = album) |> 
  ggplot(aes(x = name, y = popularity, fill = album)) + geom_col()+
  theme_minimal()+
  scale_fill_albums() + 
  coord_flip() 

#a more analyses at 
#https://taylor.wjakethompson.com/



