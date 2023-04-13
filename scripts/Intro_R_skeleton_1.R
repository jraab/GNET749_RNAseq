#Intro to R class ``
# Installing packages
# These may already be installed on longleaf
#install.packages('tidyverse')

# Loading packages


# What is loaded  - Good to run at the end


# Overview of windows, where to find help, 


#### Basics of data types
#variable #assignment #value
 # integer
 # vector
a
b
      #character
# adding
a + b 
# reassignment
a <- 3
a

a + b
#multiplying
a * b

# Lists and vectors 
l <- c('histones', 'are', 'cool')
m <- list('histones','are', 'cool')
l
vals <- c( 5, 3, 9, 7) 
vals
sort(vals)

# Start with a built in dataset
# R has tons of built in datasets 
library(help = 'datasets')
iris
iris <- as_tibble(iris)
iris

#column names

# dimensions


# how to access the first row - bracket notation


# How to access the first column


# How to access the first 5 rows


# How to access the first 5 columns


# How to access the first and third row


#How to access the first and third column


## How to work with tables of data 
# Tidyverse makes this easy - there are many funcionts which you can think of as verbs
# they let you tell R what you want to do with the data

# Filtering

# Arranging


# Group by  
# How many measurements for each species


# What is the average petal length for each species


# what is the stardard deviation


# Adding new data - mutate


##### Practice Time# ######################################################################  
ToothGrowth
?ToothGrowth

# How many observations for each supplement
# How about observations for supplement + dose (# you can group_by on two or more things)
# What is the average tooth length (overall, and per supplment/dose)
# Filter out samples with ToothGrowth < 20
# add a column if toothgrowth > 20 


