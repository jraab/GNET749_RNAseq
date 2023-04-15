#Intro to R - Class two
 
# Review of plotting
library(tidyverse)
data('ToothGrowth')
ToothGrowth
# Take 10 minutes to explore the toothgrowth data and answer some questions

# How many observations are there for each treatment
# How many for each dose
# How many treat/dose

#multiple ways to do this
ToothGrowth %>% 
   group_by(supp) %>% 
   summarise(count = n())

ToothGrowth %>% 
   group_by(dose) %>% 
   summarise(count = n())

ToothGrowth %>% 
   group_by(supp, dose) %>% 
   summarise(count = n())

# Can do this with a base function easily
table(ToothGrowth$supp)
table(ToothGrowth$dose)
table(ToothGrowth$dose, ToothGrowth$supp)

#What is the average length for each dose, is it different for each treatment at each dose
ToothGrowth %>% 
   group_by(dose) %>% 
   summarise(u_len = mean(len))

ToothGrowth %>% 
   group_by(dose, supp) %>% 
   summarise(u_len = mean(len))

# Make some plots
# Does treatment affect ToothGrowth

ToothGrowth %>% 
   ggplot(aes(x = as.factor(dose), y = len, color = supp) ) + 
   geom_point(position  = position_jitterdodge(jitter.width = 0.05))

ToothGrowth %>% 
   ggplot(aes( x = as.factor(dose), y = len, fill = supp)) + 
   geom_boxplot()

ToothGrowth %>% 
   ggplot(aes( x = as.factor(dose), y = len, fill = supp)) + 
   geom_violin() + 
   geom_boxplot(position = position_dodge(width = 0.9), width = 0.1, color = 'grey30')


# Make a bar plot of the average  length of each treatment/dosage - reuse what you learned above
 ToothGrowth %>% 
   group_by(dose, supp) %>% 
   summarise(u_len = mean(len)) %>% 
   ggplot(aes( x= as.factor(dose), y = u_len, fill = supp)) + 
   geom_col(position = 'dodge', col = 'grey30')
 
 
# add some points b/c dynamite plots without points are bad
# Geoms can take a different data frame as an argument -hint
 
ToothGrowth %>% 
   group_by(dose, supp) %>% 
   summarise(u_len = mean(len)) %>% 
   ggplot(aes( x= as.factor(dose), y = u_len, fill = supp)) + 
   geom_col(position = 'dodge', col = 'grey30') + 
   geom_point(data = ToothGrowth, aes(x = as.factor(dose),y = len), 
              color = 'grey30', 
              position = position_jitterdodge(jitter.width =0.1))

#Tidy Data ############################################################################
us_rent_income
us_rent_wide <- us_rent_income %>% pivot_wider(names_from = variable, values_from = estimate, id_cols = c(-GEOID, -moe) ) 
us_rent_wide 
us_rent_long <- us_rent_wide %>% pivot_longer(names_to = 'variable', cols = -NAME) 
us_rent_long

# Sometimes the wide form allow a specific plot that would be hard to make otherwise
# Or a specific calculation
 # For each state, what percentage of income is rent
us_rent_wide %>% 
   mutate(rent_to_income = rent/income) 


us_rent_long %>% 
   group_by(NAME) %>% 
   mutate(income = max(value)) %>%  # Cheater line, what if rent was higher than income
   mutate(percent_income = value/income) %>% 
   filter(!variable == 'income') 
# Cheats a bit , not a clear

# What is the relationship between rent and income
us_rent_wide %>% 
   ggplot(aes(x = rent, y = income)) + geom_point()

# Very common to pivot data when dealing with gene expression 
# File availalbe at github.com/jraab/GNET749_RNAseq
gm <- read_csv('~/GitHub/GNET749_RNAseq/GM_results_countnorm.csv')
gm %>% ggplot(aes (x = Gm10847, y = Gm10851)) + geom_point()
gm_long <- gm %>% 
   pivot_longer(cols = -rowname, names_to = 'sample') 

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



# Functions ################################################################################ 
#Simple function to add two numbers together
add <- function(x, y) {
  return(x + y)  
}
add(1,2)

# make a simple data frame
make_df <- function(){
   df <- data.frame(a = rnorm(10), b = rnorm(10), c = c(rep('a',5), rep('b', 5) ) )
   return(df)  
}
 
make_df()

# Change it so that it makes it however many rows long you want
make_df <- function(rows){
   df <- data.frame(a = rnorm(rows), b = rnorm(rows), c = c(rep('a',rows/2), rep('b', rows/2) ) )
   return(df)  
}
make_df(4) 
make_df(30)

get_sig_results <- function(deseq_res, threshold = 0.1) { 
   out <- as_tibble(deseq_res)%>%
     rownames_to_column(var = 'gene') %>%
      filter(padj < threshold)
   return(out)
   }

