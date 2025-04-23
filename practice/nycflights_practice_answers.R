# Recap
# Grouping, filtering, summarising, plotting data
# 
library(nycflights13)
library(tidyverse)

# total number of flights per origin per day

flights |> 
  group_by(origin, day) |> 
  summarise(total_flights_per_day = n())


# plot this and color by origin
flights |> 
  group_by(origin, day) |> 
  summarise(total_flights_per_day = n()) |> 
  ggplot(aes(x = day, y = total_flights_per_day, color = origin)) + 
    geom_point()+ 
  geom_line()

# Filter out flights with a departure delay greater than 60 minutes
# add weather data for these airports - look at table weather
del_flights <- flights |> 
  filter(dep_delay > 60) |> 
  left_join(weather, by = c('year', 'month', 'day', 'hour', 'origin') )

# does precipitation or windspeed correlate with delay length
# hypothesis - weather is the cause of longer delays
del_flights |> 
  ggplot(aes(x = dep_delay, y = precip, color = origin)) + 
    geom_point()
            
del_flights |> 
  ggplot(aes(x = dep_delay, y = wind_speed, color = origin)) + 
    geom_point() + 
  facet_wrap(~origin)

del_flights |> 
  ggplot(aes(x = precip, y = wind_speed)) + 
  geom_point()

# complicated: make some cutoff for 'bad weather' and see if those have more delays
del_flights |> 
  mutate(has_weather = case_when(precip > 0 ~ 'weather', 
                                 wind_speed > 10 ~ 'weather', 
                                .default  = 'clear') ) |> 
  ggplot(aes(x = has_weather, y = dep_delay)) + 
  geom_boxplot(notch = T) 

test_data <- del_flights |> 
  mutate(has_weather = case_when(precip > 0 ~ 'weather', 
                                 wind_speed > 10 ~ 'weather', 
                                .default  = 'clear') ) 
 
# test with a wilcox.test ? wilcox.test
w <- wilcox.test(dep_delay ~ has_weather, data = test_data)

