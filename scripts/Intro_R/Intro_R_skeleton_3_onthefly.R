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
flights |>  
    group_by(carrier, origin) |> 
    count()
# Check out the slice function ( slice_head() ) to get the top 3
flights |> 
    group_by(carrier, origin) |> 
    count() |> 
    arrange(desc(n)) |> 
    group_by(origin) |> 
    slice_head(n = 3)

# Plot the top 3 carriers by number of flights
flights |> 
    group_by(carrier, origin) |> 
    count() |> 
    arrange(desc(n)) |> 
    group_by(origin) |> 
    slice_head(n = 3) |> 
    ggplot(aes(x = origin, y = n, fill = carrier)) + 
    geom_col(position = 'dodge')




#Can you plot with their actual name?
# need to bring in the airlines information

flights |> 
    group_by(carrier, origin) |> 
    count() |> 
    arrange(desc(n)) |> 
    group_by(origin) |> 
    slice_head(n = 3) |> 
    left_join(airlines) |> 
    ggplot(aes(x = origin, y = n, fill = name)) + 
    geom_col(position = 'dodge')

joined <- left_join(flights, airlines, by = 'carrier')

#calculate the average airtime by origin and month
flights |> 
    group_by(origin, month) |> 
    summarise(mean(air_time, na.rm =T) ) 

#Now plot these values
flights |> 
    group_by(origin, month) |> 
    summarise(mean_airtime = mean(air_time, na.rm =T) ) |> 
    ggplot(aes(x = month, y = mean_airtime, color = origin)) + 
    geom_point()

# facet them by airport
flights |> 
    group_by(origin, month) |> 
    summarise(mean_airtime = mean(air_time, na.rm =T) ) |> 
    ggplot(aes(x = month, y = mean_airtime, color = origin)) + 
    geom_point() + 
    facet_wrap(~origin, scale = 'free_y')

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
samples_file <- 'data/drug_samples.csv'


# counts long form
count_wide <- read_csv(count_file)
count_long <- pivot_longer(count_wide,
                           names_to = 'sample_name', 
                           values_to  = 'counts',
                           cols = -rowname)
# Pivot back to a wide form 
pivot_wider(count_long, names_from = sample_name, values_from = counts)
# Combine long form with sample information

sample_info <- read_csv(samples_file)
sample_info
count_long
full_long <- count_long |> 
    left_join(sample_info, by = c('sample_name' = 'samples'))


#How many replicates for each exp
# what is the total counts for each sample?
sample_info |> 
    count(treatment, genotype) 
    
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
full_long |> 
    filter(rowname == 'MCRS1') |> 
    ggplot(aes(x = genotype, y=  counts, color = treatment, )) + 
    geom_point(position = position_dodge(width = 0.5))

# try to make it look a little nicer

# can plot the points on top of a boxplot - but thats a little tricky (need position_dodge(0.75))
full_long |> 
    filter(rowname == 'MCRS1') |> 
    ggplot(aes(x = genotype, y=  counts, color = treatment, )) + 
    geom_boxplot() + 
    geom_point(position = position_dodge(width = 0.5))

# Now plot this for HOXA10
full_long |> 
    filter(rowname == 'HOXA10') |> 
    ggplot(aes(x = genotype, y=  counts, color = treatment, )) + 
    geom_boxplot() + 
    geom_point(position = position_dodge(width = 0.5))

# did we just copy and paste a big chunk of code - shame on us

# write a function that takes a gene name and makes this plot
plot_gene <- function(gene_name) { 
    plt <- full_long |> 
        filter(rowname == gene_name) |> 
        ggplot(aes(x = genotype, y=  counts, color = treatment, )) + 
        geom_boxplot() + 
        geom_point(position = position_dodge(width = 0.5))
    return(plt)
}

plot_gene('ARID1A')
plot_gene('ARID2')
# Now use this to plot your favorite gene

# What if you want to plot a list of genes- NEW CONCEPT - FACETS
# use + facet_wrap( ~ gene)
# 
gene_list <- c('ARID1A', 'ARID1B', 'MCRS1', 'INO80') 
for (i in gene_list) { 
    print(plot_gene(i))
}


full_long |> 
    filter(rowname %in% gene_list) |> 
    ggplot(aes(x = genotype, y=  counts, color = treatment, )) + 
    geom_boxplot() + 
    geom_point(position = position_dodge(width = 0.5)) + 
    facet_wrap(~rowname)
# need to rewrite our function to allow a list of genes and faceting
# special way to filter a list instead of == use %in% 


