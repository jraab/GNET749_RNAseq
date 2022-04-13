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
#tidyverse

# Can do this with a base function easily

#What is the average length for each dose, is it different for each treatment at each dose


# Make some plots
# Does treatment affect ToothGrowth

#points

#boxplot

#combine



# Make a bar plot of the average  length of each treatment/dosage - reuse what you learned above

 
# add some points b/c dynamite plots without points are bad
# Geoms can take a different data frame as an argument -hint
 
# Back to slides
#Tidy Data ############################################################################
us_rent_income
#Pivot wider


#pivot longer



# Sometimes the wide form allow a specific plot that would be hard to make otherwise
# Or a specific calculation
 # For each state, what percentage of income is rent


# can this be done from the long data?
 
# Cheats a bit , not as clear

# Some plots are easier - What is the relationship between rent and income
# Remember you can set any axis to be a column from the data frame (or color, or fill, or shape, any aesthetic)

# Very common to pivot data when dealing with gene expression 
# File availalbe at github.com/jraab/GNET749_RNAseq
gm <- read_csv('~/GitHub/GNET749_RNAseq/GM_results_countnorm.csv')


# pivot and plot


# Relational Data#############################################################################       
# back to slides
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




# Notice some data is missing, always keep an eye out for this and think about how to deal
#average speed by number of engines

# why didn't that work (NAs)

#


# how long are flights for different sized planes
#How might we answer that question
# Make a plot
flights_small %>% 
   ggplot(aes(x = seats, y =distance)) + geom_point()

# come up with some categories


#Back to slides
# Functions ################################################################################ 
#Simple function to add two numbers together
add <- function(x, y) {
  return(x + y)  
}
add(1,2)

# Function to calculate average

average((1:5))
average(c(3,19,20,15))
mean(1:5)

# make a simple data frame

# Change it so that it makes it however many rows long you want


