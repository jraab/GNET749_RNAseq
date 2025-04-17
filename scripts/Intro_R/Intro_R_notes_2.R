#Intro to R - Class two
################################################################################
### RECAP/PRACTICE FROM CLASSS 1        ######################################## 
# NYC Flights practcice - recap of class 1
#install.packages('nycflights13') 
library(nycflights13)
#nycflights13::
#flights


#How many flights originated at each airport


# How long was the average flight that left EWR

# what airport had the longest average arrival delay

# Which carrier had the most departure delays ( dep_delay > 0)

#How many flights per month 
################################################################################
# Pivoting
library(tidyverse) # you probably already have this loaded but I'm breaking this script up by topic


iris  # not tidy data -  each row has observations for mulitple measurements

# can go the other way too 
# why doesn't this work - because now each row isn't unique



################################################################################
## PLOTTING  ################################################################### 
# Now for more fun - plotting
library(tidyverse) # includes ggplot2

### Lets look at the data - Plotting --------------------------------------------------
 # here are the names of our columns - we'll use these to plot

# ggplot is very powerful, you can add things to the plots very easily
# Let's make each points color match its species - assign Species to the color aesthetic
  

# What if we want each speices on its own plot - use facet_wrap/facet_grid 

# How about adding a regression line



#You can do even calculations right in the plot call 
# Plot sepal.area vs petal.area. 


# What if we want to make a different kind of plot
# use a different geom
# points

# jitter

# change width

#boxplot


# You can even combine mutliple geom's - make a box plot with points on top


#jitter the points


# Aggregate some data so we have speices averages


#Make a barplot


# Dynamite plot


# Make it look better

###### Lets talk about how to make plots prettier  ########################

# add a nicer theme (theme_bw)


#change to classic


#make it look awful


# change the panel grids


# Make the bars the color you want



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
flights_small <- flights |> select(flight, tailnum, air_time, distance, origin, dest) 
flights_small
# We want to answer questiosn about how long flights are based on the type of plane
# We have tail numbers for all planes, but we don't know what those planes are


# Notice some data is missing, always keep an eye out for this and think about how to deal
#average speed by number of engines

# why didn't that work (NAs)

#

# how long are flights for different sized planes


################################################################################
#Class exercise time
# Importing Data
# Data sets from sports-reference.com
# see clean_duke_unc.R to see how I merged the data and cleaned it u  p
comb <- read_csv('data/duke_unc_hoops.csv')

#Use geom_density to plot which team has a better win distribution

# Look at this by boxplot

# conclusive proof that UNC > Duke
# Fine probaby not significant
# use glimpse to get a quick view of the data

# Does pre-season ranking predict post-season ranking

# lots of missing data - why?


# if one is NA - gets removed; hhow to deal
# Lets make up a number, 26 and put all unranked there







