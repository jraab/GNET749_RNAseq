# Class 3 
# RECAP OF TIDY/PLOTTING/RELATIONAL DATA
# Putting together the main concepts we've learned
library(tidyverse)
#Data Wrangling - transformation - visualization
#Load nycflights13 again
library(nycflights13)
flights
airports
planes
weather
airlines


# Which airline had the most flights depart from each airport?

# Check out the slice function ( slice_head() ) to get the top 3


# Plot the top 3 carriers




#Can you plot with their actual name?
# need to bring in the airlines information




#calculate the average airtime by origin and month


#Now plot these values

# facet them by airport


#  make a faceted barplot where each facet is a month and you're comparing airports


# look at the planes table
# make a plot for the  speed  for planes grouped by having more or less than 100 seats, facted by engines
# remove the 3 engine plane too
# == is equal to 
# != is not equal to


################################################################################
# Lets look at RNA-seq data 
# This is an experiment where cells were treated with a non-targeting siRNA or
# an siRNA against MCRS1. Cells were also treated with DMSO or an H3K27me3 inhibitor (EPZ)
# read in the following files - you may need to change the path if they are somewhere else on your system 

count_file <- 'data/drug_norm_counts.csv'
samples_file <- 'data/drug_samples.csv')


# counts long form


# Pivot back to a wide form 

# Combine long form with sample information

#How many replicates for each experimental group (pair of treatment/genotype)

# what is the total counts for each sample?

# Can you plot this?

# an easy way to see labels like this is use  + coord_flip()

# great - but notice we lost sample info when we did this
# lets add it back and save the intermediate

# now plot this result, but color by treatment

# change the colors of treatment to grey and blue (or whatever looks good to you)


# can you think of other visualizations that would be informative
# How about a boxplot of reads

# geom violin?

# combine violin with a box?

# Lets look at expresion of a specific gene 
# Plot the expression of MCRS1 - what plot would be informative


# try to make it look a little nicer

# can plot the points on top of a boxplot - but thats a little tricky (need position_dodge(0.75))


# Now plot this for HOXA10


# did we just copy and paste a big chunk of code - shame on us

# write a function that takes a gene name and makes this plot


# Now use this to plot your favorite gene

# What if you want to plot a list of genes- NEW CONCEPT - FACETS
# use + facet_wrap( ~ gene)

# need to rewrite our function to allow a list of genes and faceting
# special way to filter a list instead of == use %in% 


