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

# Check out the slice function ( slice_head() )
flights |> 
 group_by(carrier, origin) |> 
  count() |> 
  arrange(desc(n) ) |> 
  group_by(origin) |> 
  slice_head(n = 3)

# Plot the top 3 carriers
flights |> 
 group_by(carrier, origin) |> 
  count() |> 
  arrange(desc(n) ) |> 
  group_by(origin) |> 
  slice_head(n = 3) |> 
  ggplot(aes(x = origin, y = n, fill = carrier)) + 
  geom_col(position = 'dodge') 



#Can you plot with their actual name?
# need to bring in the airlines information
flights |> 
  group_by(carrier, origin) |> 
  count() |> 
  arrange(desc(n) ) |> 
  group_by(origin) |> 
  slice_head(n = 3) |> 
  left_join(airlines, by = 'carrier') |> 
  ggplot(aes(x = origin, y = n, fill = name)) + 
  geom_col(position = 'dodge') 


flights
#calculate the average airtime by origin and month
flights |> 
  group_by(origin, month) |> 
  summarise(avg_airtime_month = mean(air_time, na.rm = T)) 

#Now plot these values
flights |> 
  group_by(origin, month) |> 
  summarise(avg_airtime_month = mean(air_time, na.rm = T))  |> 
  ggplot(aes(x = month, y = avg_airtime_month, color = origin)) + 
  geom_point()

# facet them by airport
flights |> 
  group_by(origin, month) |> 
  summarise(avg_airtime_month = mean(air_time, na.rm = T))  |> 
  ggplot(aes(x = month, y = avg_airtime_month, color = origin)) + 
  geom_point() + 
  facet_wrap( ~ origin)

#  make a faceted barplot where each facet is a month and you're comparing airports
flights |> 
  group_by(origin, month) |> 
  summarise(avg_airtime_month = mean(air_time, na.rm = T))  |> 
  ggplot(aes(x = origin, y = avg_airtime_month, fill = origin)) + 
  geom_col(color = 'grey20') + 
  facet_wrap( ~ month, nrow = 2)

# look at the planes table
# make a plot for the  speed  for planes grouped by having more or less than 100 seats, facted by engines
# remove the 3 engine plane too
# == is equal to 
# != is not equal to
planes |> 
  filter(engines != 3) |> 
  group_by(engines, seats > 100) |> 
  summarise(avg_speed = mean(speed, na.rm = T) ) |> 
  janitor::clean_names() |> 
  ggplot(aes(x = seats_100, y = avg_speed)) + 
  geom_col() + 
  facet_wrap(~engines)

################################################################################
# Lets look at RNA-seq data 
# This is an experiment where cells were treated with a non-targeting siRNA or
# an siRNA against MCRS1. Cells were also treated with DMSO or an H3K27me3 inhibitor (EPZ)
counts <- read_csv('data/drug_norm_counts.csv')
samples <- read_csv('data/drug_samples.csv')
counts
samples


# counts long form
counts_long <- pivot_longer(counts, names_to = 'samples', values_to = 'counts', -rowname)
counts_long


# Pivot back to a wide form 
counts_wide <- pivot_wider(counts_long, names_from = samples, values_from = counts, id_cols = rowname)
counts_wide

# Combine long form with sample information
counts_long <- left_join(counts_long, samples, by = 'samples') 
counts_long

#How many replicates for each experimental group (pair of treatment/genotype)
counts_long |> 
  select(samples, treatment, genotype) |> 
  unique() |> 
  group_by(treatment, genotype) |>
  count()

# what is the total counts for each sample?
counts_long |> 
  group_by(samples) |> 
  summarise(total_counts = sum(counts) ) 

# Can you plot this?
counts_long |> 
  group_by(samples) |> 
  summarise(total_counts = sum(counts) ) |> 
  ggplot(aes(x =samples, y = total_counts)) + 
  geom_col(color ='grey20')

# an easy way to see labels like this is use  + coord_flip()
counts_long |> 
  group_by(samples) |> 
  summarise(total_counts = sum(counts) ) |> 
  ggplot(aes(x =samples, y = total_counts)) + 
  geom_col(color ='grey20') + 
  coord_flip()

# great - but notice we lost sample info when we did this
# lets add it back and save the intermediate
counts_long_full <- counts_long |> 
  group_by(samples) |> 
  summarise(total_counts = sum(counts) ) |> 
  left_join(samples , by = 'samples')

# now plot this result, but color by treatment
counts_long_full |> 
  ggplot(aes(x = samples, y = total_counts, fill = treatment)) + 
  geom_col(color = 'grey30') + 
  coord_flip()

# change the colors of treatment to grey and blue (or whatever looks good to you)
counts_long_full |> 
  ggplot(aes(x = samples, y = total_counts, fill = treatment)) + 
  geom_col(color = 'grey30') + 
  coord_flip() + 
  scale_fill_manual(values = c('grey60', 'steelblue') ) 


# can you think of other visualizations that would be informative
# How about a boxplot of reads
counts_long |> 
  ggplot(aes(x = samples, y = log2(counts), fill = treatment) ) + 
  geom_boxplot() 

# geom violin?
counts_long |> 
  ggplot(aes(x = samples, y = log2(counts), fill = treatment) ) + 
  geom_violin() 

# combine?
counts_long |> 
  ggplot(aes(x = samples, y = log2(counts), fill = treatment) ) + 
  geom_violin()  + 
  geom_boxplot(width = 0.1, color = 'grey90', fill = 'grey20')


# Lets look at expresion of a specific gene 
# Plot the expression of MCRS1 - what plot would be informative

counts_long |> 
  filter(rowname == 'MCRS1') |> 
  ggplot(aes(x = treatment, y = counts, fill = genotype)) + 
  geom_point(pch = 21, size = 4) 

# try to make it look a little nicer
counts_long |> 
  filter(rowname == 'MCRS1') |> 
  ggplot(aes(x = treatment, y = counts, fill = genotype)) + 
  geom_point(pch = 21, size = 4)  + 
  theme_classic() + 
  ylab('Counts') + xlab('')  + 
  scale_fill_manual(values = c('steelblue', 'goldenrod'))


# can plot the points on top of a boxplot - but thats a little tricky (need position_dodge(0.75))
counts_long |> 
  filter(rowname == 'MCRS1') |> 
  ggplot(aes(x = treatment, y = counts, fill = genotype  )) + 
  geom_boxplot() + 
  geom_point(position = position_dodge(0.75))  + 
  theme_classic() + 
  ylab('Counts') + xlab('')  + 
  scale_fill_manual(values = c('steelblue', 'goldenrod'))


# Now plot this for HOXA10
counts_long |> 
  filter(rowname == 'HOXA10') |> 
  ggplot(aes(x = treatment, y = counts, fill = genotype  )) + 
  geom_boxplot() + 
  geom_point(position = position_dodge(0.75))  + 
  theme_classic() + 
  ylab('Counts') + xlab('')  + 
  scale_fill_manual(values = c('steelblue', 'goldenrod'))


# did we just copy and paste a big chunk of code - shame on us

# write a function that takes a gene name and makes this plot
plot_mfg <- function(gene) { 
  # Need to have the correct data frame in our environment 
  # in this case counts_long
  counts_long |> 
    filter(rowname == gene) |> 
    ggplot(aes(x = treatment, y = counts, fill = genotype  )) + 
    geom_boxplot() + 
    geom_point(position = position_dodge(0.75))  + 
    theme_classic() + 
    ylab('Counts') + xlab('')  + 
    scale_fill_manual(values = c('steelblue', 'goldenrod'))
  
  
}


# Now use this to plot your favorite gene
plot_mfg('ARID2')
plot_mfg('ARID1A')

# What if you want to plot a list of genes- NEW CONCEPT - FACETS
# use + facet_wrap( ~ gene)

# need to rewrite our function to allow a list of genes and faceting
# special way to filter a list instead of == use %in% 
plot_mfg2 <- function(list_of_genes) { 
  # Need to have the correct data frame in our environment 
  # in this case counts_long
  counts_long |> 
    filter(rowname %in% list_of_genes) |> 
    ggplot(aes(x = treatment, y = counts, fill = genotype  )) + 
    geom_boxplot() + 
    geom_point(position = position_dodge(0.75))  + 
    theme_classic() + 
    ylab('Counts') + xlab('')  + 
    scale_fill_manual(values = c('steelblue', 'goldenrod')) + 
    facet_wrap(~rowname, scales = 'free_y') # this allows each plot to have its own y axis scale
}

some_cool_genes <- c('MCRS1', 'EZH2', 'EED', 'MYC')
plot_mfg2(some_cool_genes)
