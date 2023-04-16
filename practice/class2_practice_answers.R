# Class 2 practice

# Using nycflights 13 package
# Do the calculations and think if there is an effective plot to show your result

# What airport had the highest average wind
weather |> 
  group_by(origin) |> 
  summarise(avg_wind_speed = mean(wind_speed, na.rm = T) ) 

weather |> 
  group_by(origin) |> 
  summarise(avg_wind_speed = mean(wind_speed, na.rm = T) ) |>  
  ggplot(aes(x = origin, y = avg_wind_speed)) + 
  geom_col()

weather |> 
  ggplot(aes(x = origin, y = wind_speed)) + geom_boxplot()
#Wait - what? who had 1000+ mile an hour windspeed
weather |> 
  filter(wind_speed > 1000) 
# Maybe we should exclude that value since it seems like an error
weather |> 
  filter(wind_speed < 1000) |> 
  ggplot(aes(x = origin, y = wind_speed)) + geom_boxplot()
 
# Which month had the highest average wind
weather |> 
  filter(wind_speed < 1000) |> 
  group_by(month) |> 
  summarise(mean_wind_speed = mean(wind_speed, na.rm = ))

weather |> 
  filter(wind_speed < 1000) |> 
  ggplot(aes(x = as.factor(month), y = wind_speed))+ geom_boxplot()
# Did you get an error about 'group' x needs to be a factor to plot this the way you want

# Which airport and month had the most rain
weather |> 
  group_by(origin) |> 
  summarise(total_rain= sum(precip, na.rm = T) )
weather |> 
  group_by(origin) |> 
  summarise(total_rain= sum(precip, na.rm = T) ) |> 
  ggplot(aes(x = origin, y = total_rain) ) + 
  geom_col()

# are rainy days windier?
weather |> 
  filter(wind_speed <1000) |> 
  ggplot(aes(x=precip, y = wind_speed)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

# USing the rnaseq in data/drug_norm_counts.csv and drug_samples.csv try the following
# load data
drug_counts <- read_csv('data/drug_norm_counts.csv')
drug_samples <- read_csv('data/drug_samples.csv')
drug_counts
drug_samples

#Plot GAPDH expression in treatment (DMSO vs EPZ)
# First pivot the wide data
drug_counts_long <- pivot_longer(drug_counts, -rowname, names_to = 'samples', values_to = 'counts') 
# Then combine with sample info (samples key)
drug_data <- drug_counts_long |> left_join(drug_samples, by = 'samples') 
drug_data
# Now you can plot a gene of interest
drug_data |> 
  filter(rowname == 'GAPDH') |> 
  ggplot(aes(x = treatment, y = log2(counts))) + 
  geom_point()

