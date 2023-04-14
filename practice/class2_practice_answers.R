# Class 2 practice

# Beginning Class - nycflights






# End of class

library(nycflights13)
flights
weather
# How many aiports originated at each airport flights
flights |>  count(origin) 


# How long was the average flight that left  EWR
flights |> 
  filter(origin == 'EWR') |> 
  group_by(dest) |> 
  summarise(avg_flight = mean(air_time, na.rm = T) ) 

# What aiport had the longest arrival delay (average)
flights |> 
  group_by(dest) |> 
  summarise(avg_arr_delay = mean(arr_delay, na.rm = T) )  |> 
  arrange(desc(avg_arr_delay) ) 

# What carrier had the most delays > 20
flights |> 
  filter(arr_delay > 20) |> 
  group_by(carrier) |> 
  count() |> 
  arrange(desc(n) ) 
  


# End of class

# How many flights each year

# What airport had the highest average wind

# Which month had the highest average wind

# Which airport and month had the most rain

# Can you answer these questions with plots



