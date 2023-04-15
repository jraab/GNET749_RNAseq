#Intro to R class ``
#Overview of GUI
# Installing packages
# install.packages('tidyverse')
# Install tidyverse / ggthemes
library(tidyverse)
library(ggthemes)

# What is a function


# Loading packages
# Load tidyverse / ggthemes

# What is loaded  - Good to run at the end
sessionInfo()
# where to find help
# Help window
1+2

#### Basics of data types
#variable #assignment #value
a <- 1
a
# integer

# vector
a <- c(1,2,3,4)
b <- 1:4
a == b
# character
a <- c('histones', 'are' ,'cool')
b <- list('histones', 'are', 'cool')
a == b
#addition
d <- 4
e <- 5
d + e 
# Mulitplication
d*e
d^2 * sqrt(e) 
# Lists 
#character

#numbers

# sort
sort(a) 
n <- c(4,9,1,19,2)
sort(n)
rev(sort(n))
###############################
# Data frames - main workhorse of R
# Start with a built in datase - iris
iris
# change to tibble 
iris <- as_tibble(iris)
# how to access the first row
iris[1,]
# How to access the first column
iris[,1]
# How to access the first 5 rows
iris[1:5,]

# How to access the first 5 columns
iris[,2:4]
# How to access the first and third row
iris[c(1,3), ]

#How to access the first and third column
View(iris[,c(1,3)]) 
# Get a value from one cell (row 1 column 3)
iris[1,3]
)### Lets look at the data - Plotting --------------------------------------------------
#What are the column names 
colnames(iris)

#  Basic scatter plot - points
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
   geom_point()

# Basic Plot - bars
ggplot(iris, aes(x = Species, y = Sepal.Length)) + 
   geom_col()

# ggplot is very powerful, you can add things to the plots very easily
# add color by species
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) + 
   geom_point()

# can make a differt plot for each species
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) + 
   geom_point() + 
   facet_wrap(~Species)



#You can do even calculations right in the plot call 
# PLot sepal area vs petal area
ggplot(iris, aes(x = Sepal.Length * Sepal.Width, 
                 y = Petal.Length * Petal.Width, 
                 color = Species) ) + 
   geom_point()



# What if we want to make a kind of plot - species vs length
#Points
ggplot(iris, aes( x = Species, y = Sepal.Length))  + 
   geom_point()
# Jitter - see points better
ggplot(iris, aes( x= Species, y = Sepal.Length)) + 
   geom_jitter(width = .1)
?geom_jitter
# Boxplot
ggplot(iris, aes( x  = Species, y = Sepal.Length)) + 
   geom_boxplot()

# Violin
ggplot(iris, aes( x = Species, y = Sepal.Length)) + 
   geom_violin() 

# Can combin box + point
ggplot(iris, aes( x= Species, y = Sepal.Length)) + 
   geom_boxplot() + 
   geom_jitter(width = 0.1)


  
###### Lets talk about how to make plots prettier  ########################
#BW Theme
ggplot(iris, aes( x= Species, y = Sepal.Length)) + 
   geom_boxplot() + 
   geom_jitter(width = 0.1) + 
   theme_bw() + 
   theme(panel.grid = element_blank() )

?theme

ggplot(iris, aes( x= Species, y = Sepal.Length)) + 
   geom_boxplot() + 
   geom_jitter(width = 0.1) + 
   theme_excel()

# Many built-in themes - many extra themes - see ggextra
# can make your own, useful for keeping plots consistent for publication


########### Introduce pipe    ########## 
iris %>% ggplot(aes(x=Species, y = Sepal.Length)) + geom_point()
ggplot(iris, aes(Species, y = Sepal.Length)) + geom_point()
# Can use this to string together data wrangling steps

# What if we want to group by species and calculate mean
mean_iris <- iris %>% 
   group_by(Species) %>% 
   summarise(mean_sepal_length = mean(Sepal.Length), 
             mean_sepal_width = mean(Sepal.Width)) 

# can do more than one at a time (width and length)

# Can make new columns  - mutate
iris %>% 
   mutate(sepal_area  = Sepal.Length * Sepal.Width)

# Back to plotting
# Plot of mean - bars
iris %>% 
   group_by(Species) %>% 
   summarise(Sepal_length_mean = mean(Sepal.Length)) %>% 
   ggplot(aes(x = Species, y= Sepal_length_mean)) + 
   geom_col()
# BW  - make look better
iris %>% 
   group_by(Species) %>% 
   summarise(Sepal_length_mean = mean(Sepal.Length)) %>% 
   ggplot(aes(x = Species, y= Sepal_length_mean)) + 
   geom_col() + 
   theme_bw()

# what else can the pipe do

# can pipe to a base function - head
iris %>% head()

# can filter
iris %>% 
   filter(Sepal.Length  < 5) 

# can keep some columns - select
iris %>% 
   dplyr::select(Species, Sepal.Length) 

#can order the data
iris %>% 
   arrange(desc(Sepal.Length)) %>% 
   View


#Importing Data
# Data sets from sports-reference.com
duke_file <- '~/Desktop/duke_stats.csv' # change this to match where your file is
unc_file <-  '~/Desktop/unc_stats.csv' # change this to match where your file is 
# read file unc
read_csv(unc_file)
# read file unc skip row
unc <- read_csv(unc_file, skip = 1)

# note column names are messy - information is encoded in row1 - but not easy to read automatically
# lets make our own column names
# Get column names from gist.github.com/jraab
cnames <- c('row', 'season', 'conf', 'overall_w', 'overall_l', 'overall_w_l_percent', 
            'conf_w', 'conf_l', 'conf_w_l_percent',
            'srs', 'sos', 'pts_for_avg', 'pts_against_avg', 
            'ap_pre', 'ap_high', 'ap_final','tourney', 'coach')

# Now we can re-read the data in with our own column names - skip first 2  rows
unc <- read_csv(unc_file, skip = 2, col_names = cnames)
duke <- read_csv(duke_file, skip = 2, col_names = cnames)

# we can add a label for team 
unc <- unc %>% mutate(team = 'GoodGuys')
duke <- duke %>%  mutate(team = 'BadGuys')

unc

# now we can stick the whole thing together
comb <- rbind(unc, duke) 


# plot by overall_w_l_percent
comb %>% 
   ggplot(aes(x = overall_w_l_percent, color = team)) + 
   geom_density()

comb %>% 
   ggplot(aes(x = overall_w_l_percent, color = team)) + 
   geom_boxplot()

# conclusive proof that UNC > Duke

# What else could we figure out with these data

#Other fun data sets to practice with 
data(ToothGrowth) # Guinea Pig Teeth Growth and Vitamin C 
nih <- read_csv('~/proj/teaching/GNET749_S21/Worldwide2020.csv')
nih
colnames(nih)
