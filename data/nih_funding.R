library(tidyverse)

ww <- read_csv('') # You'll need to replace this path with a path to the file on your own computer

# New data set using NIH Funding - Fun

# This data set is much dirtier - column names are very annoying and have spaces in them
# here is how to fix that

cn <- colnames(ww) #let's just pull the column names out
cn <- str_replace_all(cn, '\\s+', '_') # \s+ is a regular expression that matches one or more spaces, the extra slash is b/c in R we need to escape the regular expression
# we replace with an underscore
cn # looks better, one still has some annoying parenthesis in there, let's get rid of those too
cn <- str_replace_all(cn, '\\(|\\)', '')  # I usually have to do this by trial and error , the pipe character | means "or" in this context, so remove the ( or the ) - again, the slashes are to escape the parantheses.
# Having to try a couple of ways of specifigying the pattern is why I pull the colnames out into their own variable. 
colnames(ww) <- cn


ww

# who has the most money - similar to above FUNDING (and probably DIRECT_COST, INDIRECT_COST) are 
# annoying. They are set as characters and include dollar signs and commas, when what we really want here are numbers
# The below code will fix this using str_replace_all, try to give fixing those columns a shot before you use the code below.  

fixed <- ww %>% 
   mutate(FUNDING = str_replace_all(string = FUNDING, pattern ='\\$', replace = '')) %>%
   mutate(FUNDING = str_replace_all(string = FUNDING, pattern = ',',replace = '')) %>%
   mutate(FUNDING = as.numeric(FUNDING) ) 
   
fixed 
# Open ended  - find something interesting in the data - make a cool plot, bring to class

