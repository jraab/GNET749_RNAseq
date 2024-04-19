# Practice on working with data - Class 1

# Load tidyverse

#Install the janitor package - has a handy function for cleaning up column names

#Load janitor


# Read in NIH data Worldwide2020.csv


#Clean up the column names (clean_names function from janitor package)


# How many rows/columns

# 

# Access the 10th row


# Access rows 20-50


# Filter to get only grants from NC
 

# How many are there


# is UNC better than Duke

# What university has the most money
# Hint: look at the type of column if you get an error you  may need to fix something
glimpse(nih)
# Funding is a character column, we need it to be a number
as.numeric(nih$funding) # why doesn't this work? 
# Need to remove the $ and , from the string - then as.numeric will work
# I left this answer in here on purpose - str_replace lets you replace some portion of  string using a pattern 
# see ?str_replace
pattern <- '\\$' # this is a regex
# regex can be made to match a wide range of things in a string, but can be hard to figure out
# hint: chatgpt is good at giving you a valid regex 

nih <- nih |> mutate(funding = str_replace(funding, pattern, '') ) |> 
  mutate(funding = as.numeric(str_replace(funding, ',', '') ) )



# Arrange this so you can see who has the most money



# is UNC better than Duke

# What is the biggest grant in each state?
# some functions return NA if any value is NA (e.g. sum, max) so use na.rm = T to ignore these


#What's interesting about this?


# How many different grant mechanisms are there?
table(nih$funding_mechanism) # base R version
#tidyverse
# Note these two appraoches return different kinds of tables 
# I find the tidyverse version easier use for plotting downstream

# How many PIs per state
# hint: group by state an pi, then add up how many PIs in each state 


# How many PIs with 500,000 of funding

