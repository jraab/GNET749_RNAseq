# Practice on working with data - Class 1

# Load tidyverse
library(tidyverse)
#Install the janitor package - has a handy function for cleaning up column names
install.packages('janitor')
#Load janitor
library(janitor)

# Read in NIH data Worldwide2020.csv
nih <- read_csv('data/Worldwide2020.csv')

#Clean up the column names
nih <- clean_names(nih)

# How many rows/columns
nrow(nih)
ncol(nih)
# 

# Access the 10th row
nih[10,]

# Access rows 20-50
nih[20:50,]

# Filter to get only grants from NC
nih |> filter(state_or_country_name == 'NORTH CAROLINA') 

# How many are there
nih |> 
  filter(state_or_country_name == 'NORTH CAROLINA') |> 
  group_by(organization_name) |> 
  summarise(n_awards = n() )  |> print(n = 'all') 

# is UNC better than Duke

# What university has the most money
# Hint: look at the type of column if you get an error you  may need to fix something
glimpse(nih)
# Funding is a character column, we need it to be a number
as.numeric(nih$funding) # why doesn't this work? 
# Need to remove the $ and , from the string - then as.numeric will work

nih <- nih |> mutate(funding = str_replace(funding, '\\$', '') ) |> 
      mutate(funding = as.numeric(str_replace(funding, ',', '') ) )

nih |> 
  filter(state_or_country_name == 'NORTH CAROLINA') |> 
  group_by(organization_name) |> 
  summarise(total_amounts = sum(funding, na.rm = T) )  

# Arrange this so you can see who has the most money

nih |> 
  filter(state_or_country_name == 'NORTH CAROLINA') |> 
  group_by(organization_name) |> 
  summarise(total_amounts = sum(funding, na.rm = T) )  |> 
  arrange(desc(total_amounts) ) 

# is UNC better than Duke

# What is the biggest grant in each state?
# some functions return NA if any value is NA (e.g. sum, max) so use na.rm = T to ignore these
nih |> 
  group_by(state_or_country_name) |> 
  summarise(max_grant = max(funding, na.rm = T)) |> 
  arrange(desc(max_grant) ) 

#What's interesting about this?


# How many different grant mechanisms are there?
table(nih$funding_mechanism) # base R version
nih |> 
  group_by(funding_mechanism) |> 
  count() # count is the same as summarise(value = n() )
# Note these two appraoches return different kinds of tables 
# I find the tidyverse version easier use for plotting downstream

# How many PIs per state
# hint: group by state an pi, then add up how many PIs in each state 
nih |> 
  group_by(state_or_country_name, pi_name) |> 
  tally() |> 
  summarise(x = length(pi_name) ) 

# How many PIs with 500,000 of funding
nih |> 
  filter(funding > 500000) |> 
  group_by(state_or_country_name, pi_name) |> 
  tally() |> 
  summarise(x = length(pi_name) ) 
