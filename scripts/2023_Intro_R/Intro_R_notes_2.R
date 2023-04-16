#Intro to R - Class two
################################################################################
### RECAP/PRACTICE FROM CLASSS 1        ######################################## 
# NYC Flights practcice - recap of class 1
#install.packages('nycflights13') 
library(nycflights13)
flights


#How many flights originated at each airport
flights |> 
  group_by(origin) |> 
  count() 

# How long was the average flight that left EWR
flights |> 
  filter(origin == 'EWR') |> 
  group_by(origin) |> 
  summarise(mean_flight_legth = mean(air_time, na.rm = T) )  

# what airport had the longest average arrival delay
flights |> 
  group_by(dest) |> 
  summarise(mean_delay = mean(arr_delay, na.rm = T) ) |> 
  arrange(desc( mean_delay) ) 

# Which carrier had the most departure delays ( dep_delay > 0)
flights |> 
  filter(dep_delay > 0) |> 
  group_by(carrier) |> 
  count()   |> 
  arrange(desc(n) ) 

#How many flights per month 
flights |> 
  group_by(month) |> 
  count() 
################################################################################
# Pivoting
library(tidyverse) # you probably already have this loaded but I'm breaking this script up by topic


iris  # not tidy data -  each row has observations for mulitple measurements
iris_long <- pivot_longer(iris, names_to = 'measurement', values_to = 'vals', cols = -Species)
iris_long |> ggplot(aes(x = measurement, y = vals)) + geom_boxplot()
# can go the other way too 
iris_wide <- pivot_wider(iris_long,  id_cols = Species, names_from = measurement, values_from = vals)
# why doesn't this work - because now each row isn't unique
iris_wide <- iris |> rowid_to_column() |> 
  pivot_longer(names_to = 'measurement', values_to = 'vals', cols = c(-Species,-rowid) ) |> 
  pivot_wider(id_cols = c(rowid, Species), names_from =  measurement, values_from = vals) 


################################################################################
## PLOTTING  ################################################################### 
# Now for more fun - plotting
library(tidyverse) # includes ggplot2

### Lets look at the data - Plotting --------------------------------------------------
colnames(iris) # here are the names of our columns - we'll use these to plot
ggplot(iris, aes( x = Sepal.Length, y = Sepal.Width)) + geom_point()

# ggplot is very powerful, you can add things to the plots very easily
# Let's make each points color match its species - assign Species to the color aesthetic
ggplot(iris, aes( x = Sepal.Length, y = Sepal.Width, color = Species)) + 
  geom_point()  

# What if we want each speices on its own plot - use facet_wrap/facet_grid 
ggplot(iris, aes( x = Sepal.Length, y = Sepal.Width, color = Species)) + 
  geom_point() +  
  facet_wrap(~Species)

# How about adding a regression line
ggplot(iris, aes( x = Sepal.Length, y = Sepal.Width, color = Species)) + 
  geom_point()  +  
  facet_wrap(~Species) + 
  geom_smooth(method = 'lm')


#You can do even calculations right in the plot call 
# Plot sepal.area vs petal.area. 
ggplot(iris, aes( x = Sepal.Length*Sepal.Width, y = Petal.Length*Petal.Width, color = Species)) + 
  geom_point()

# What if we want to make a different kind of plot
# use a different geom
# points
ggplot(iris, aes( x = Species, y = Sepal.Length)) + 
  geom_point()
# jitter
ggplot(iris, aes( x = Species, y = Sepal.Length)) + 
  geom_jitter()
# change width
ggplot(iris, aes( x = Species, y = Sepal.Length)) + 
  geom_jitter(width = 0.1)
#boxplot
ggplot(iris, aes( x = Species, y = Sepal.Length)) + 
  geom_boxplot()

# You can even combine mutliple geom's - make a box plot with points on top
ggplot(iris, aes( x = Species, y = Sepal.Length)) + 
  geom_boxplot() + 
  geom_point()

#jitter the points
ggplot(iris, aes( x = Species, y = Sepal.Length)) + 
  geom_boxplot() + 
  geom_jitter(width =0.1)

# Aggregate some data so we have speices averages
iris_summary <- iris |> group_by(Species) |> 
  summarise (mean_sepal_width= mean(Sepal.Width), 
             sd_sepal_width = sd(Sepal.Width) ) 

#Make a barplot
iris_summary |> 
  ggplot(aes(x = Species, y = mean_sepal_width)) + 
  geom_col() + 

# Dynamite plot
iris_summary |> 
  ggplot(aes(x = Species, y = mean_sepal_width)) + 
  geom_col() + 
  geom_errorbar(aes(ymin = mean_sepal_width-sd_sepal_width, 
                    ymax = mean_sepal_width+sd_sepal_width) ) 

# Make it look better
iris_summary |> 
  ggplot(aes(x = Species, y = mean_sepal_width)) + 
  geom_col(fill = 'grey90', color = 'grey20') + 
  geom_errorbar(aes(ymin = mean_sepal_width-sd_sepal_width, 
                    ymax = mean_sepal_width+sd_sepal_width), 
                width = 0.1) 
###### Lets talk about how to make plots prettier  ########################

# add a nicer theme (theme_bw)
iris_summary |> 
  ggplot(aes(x = Species, y = mean_sepal_width)) + 
  geom_col(fill = 'grey90', color = 'grey20') + 
  geom_errorbar(aes(ymin = mean_sepal_width-sd_sepal_width, 
                    ymax = mean_sepal_width+sd_sepal_width), 
                width = 0.1) +
  theme_bw()

#change to classic
iris_summary |> 
  ggplot(aes(x = Species, y = mean_sepal_width)) + 
  geom_col(fill = 'grey90', color = 'grey20') + 
  geom_errorbar(aes(ymin = mean_sepal_width-sd_sepal_width, 
                    ymax = mean_sepal_width+sd_sepal_width), 
                width = 0.1) +
  theme_classic()

#make it look awful
iris_summary |> 
  ggplot(aes(x = Species, y = mean_sepal_width)) + 
  geom_col(fill = 'grey90', color = 'grey20') + 
  geom_errorbar(aes(ymin = mean_sepal_width-sd_sepal_width, 
                    ymax = mean_sepal_width+sd_sepal_width), 
                width = 0.1) +
  ggthemes::theme_excel()

# change the panel grids
iris_summary |> 
  ggplot(aes(x = Species, y = mean_sepal_width)) + 
  geom_col(fill = 'grey90', color = 'grey20') + 
  geom_errorbar(aes(ymin = mean_sepal_width-sd_sepal_width, 
                    ymax = mean_sepal_width+sd_sepal_width), 
                width = 0.1) +
  theme_bw() + 
  theme(panel.grid = element_line(linetype = 'dashed', color = 'steelblue') ) 



# Make the bars the color you want
iris_summary |> 
  ggplot(aes(x = Species, y = mean_sepal_width, fill = Species)) + 
  geom_col() + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text.x = element_text(size = 24, family = 'serif')) + 
  scale_fill_manual(values = c('steelblue', 'forestgreen', 'goldenrod'))



# Relational Data#############################################################################       
# Goal is to explain functions / merges / aggregating data / more advanced wrangling
#install.packages('nycflights13)
library(nycflights13)
#nycflights13::
# has 5 tables
flights
airlines
weather
airports
planes

# make data a little smaller so we can see better
flights_small <- flights %>% select(flight, tailnum, air_time, distance, origin, dest) 
flights_small
# We want to answer questiosn about how long flights are based on the type of plane
# We have tail numbers for all planes, but we don't know what those planes are

flights_small <- left_join(flights_small, planes, by = 'tailnum')
flights_small

# Notice some data is missing, always keep an eye out for this and think about how to deal
#average speed by number of engines
flights_small %>% 
  group_by(engines) %>% 
  summarise(mean_speed = mean(speed) )
# why didn't that work (NAs)
flights_small %>% 
  group_by(engines) %>% 
  summarise(mean_speed = mean(speed, na.rm = T) )
#
flights_small %>% 
  filter(engines == 3)

# how long are flights for different sized planes
flights_small %>% 
  ggplot(aes(x = seats, y =distance)) + geom_point()

flights_small %>% 
  mutate(capacity = cut(seats, breaks = 10 ) ) %>% 
  ggplot(aes(x = capacity, y = distance)) + geom_boxplot()


################################################################################
#Class exercise time
# Importing Data
# Data sets from sports-reference.com
# see clean_duke_unc.R to see how I merged the data and cleaned it u  p
comb <- read_csv('data/duke_unc_hoops.csv')

#Use geom_density to plot which team has a better win distribution
comb |> 
  ggplot(aes(x = overall_w_l_percent, color = team) ) + 
  geom_density() 

# Look at this by boxplot
comb |> 
  ggplot(aes(y = overall_w_l_percent, x = team, color = team) ) + 
  geom_boxplot(notch = T) 
# conclusive proof that UNC > Duke
# Fine probaby not significant
# use glimpse to get a quick view of the data
comb |> glimpse

# Does pre-season ranking predict post-season ranking
comb |> 
  ggplot(aes(x = ap_pre, y = ap_final, color = team)) + 
  geom_point()

# lots of missing data - why?
table(is.na(comb$ap_final), comb$team)

# if one is NA - gets removed; hhow to deal
# Lets make up a number, 26 and put all unranked there
comb |> 
  mutate(ap_pre = ifelse(is.na(ap_pre), 26, ap_pre), 
         ap_final = ifelse(is.na(ap_final), 26, ap_final)) |> 
  ggplot(aes( x = ap_pre, y = ap_final, color = team)) + 
  geom_point()







