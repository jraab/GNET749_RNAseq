#Intro to R class ``
# Installing packages
# These may already be installed on longleaf
#install.packages('tidyverse')

# Loading packages
library(tidyverse)

# What is loaded  - Good to run at the end
sessionInfo() 

# Overview of windows, where to find help, 
?ggplot # Help window
?as_tibble

#### Basics of data types
#variable #assignment #value
a <- 1 # integer
b <- 1:5 # vector
a
b
d <- 'character'
a + b 
a <- 3
a
a + b
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
colnames(iris)
# dimensions
dim(iris)
nrow(iris)
ncol(iris)

# how to access the first row - bracket notation
iris[1, ]

# How to access the first column
iris[, 1]

# How to access the first 5 rows
iris[1:5, ]

# How to access the first 5 columns
iris[,1:5]

# How to access the first and third row
iris[c(1,3)]

#How to access the first and third column
iris[,c(1,3)]

## How to work with tables of data 
# Tidyverse makes this easy - there are many funcionts which you can think of as verbs
# they let you tell R what you want to do with the data

# Filtering
iris |>   filter( Sepal.Length > 5) 
iris |>   filter(Species == 'setosa')

iris |>   arrange(Sepal.Length)
iris |>   arrange(desc(Sepal.Length))

# Group by  
# How many measurements for each species
iris |>  group_by(Species) |>  
  summarise(total_n = n() )

# What is the average petal length for each species
iris |> group_by(Species) |> 
  summarise(mean_petal_length = mean(Petal.Length))

# what is the stardard deviation
iris |> group_by(Species) |> 
  summarise(mean_petal_length = mean(Petal.Length), 
            sd_petal_length   = sd(Petal.Length))

# Adding new data - mutate
iris |> mutate(Sepal.area = Sepal.Length * Sepal.Width) 

##### Practice Time# ######################################################################  
ToothGrowth
?ToothGrowth

# How many observations for each supplement
# How about observations for supplement + dose (# you can group_by on two or more things)
# What is the average tooth length (overall, and per supplment/dose)
# Filter out samples with ToothGrowth < 20

