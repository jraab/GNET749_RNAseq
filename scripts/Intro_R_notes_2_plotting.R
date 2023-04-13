

### Lets look at the data - Plotting --------------------------------------------------
colnames(iris) # here are the names of our columns - we'll use these to plot
ggplot(iris, aes( x = Sepal.Length, y = Sepal.Width)) + geom_point()

# ggplot is very powerful, you can add things to the plots very easily
# Let's make each points color match its species - assign Species to the color aesthetic
ggplot(iris, aes( x = Sepal.Length, y = Sepal.Width, color = Species)) + 
  geom_point()  

# What if we want each speices on its own plot - use facet_wrap/facet_grid 
ggplot(iris, aes( x = Sepal.Length, y = Sepal.Width, color = Species)) + 
  geom_point()  +  
  facet_wrap(~Species)

# How about adding a regression line
ggplot(iris, aes( x = Sepal.Length, y = Sepal.Width, color = Species)) + 
  geom_point()  +  
  facet_wrap(~Species) + 
  geom_smooth(method = 'lm')


#You can do even calculations right in the plot call 
# Plot sepal.area vs petal.area. 
ggplot(iris, aes( x = Sepal.Length*Sepal.Width, y = Petal.Length*Petal.Width, color = Species)) + 
  geom_point()

# What if we want to make a different kind of plot
# use a different geom
ggplot(iris, aes( x = Species, y = Sepal.Length)) + 
  geom_point()

ggplot(iris, aes( x = Species, y = Sepal.Length)) + 
  geom_jitter()

ggplot(iris, aes( x = Species, y = Sepal.Length)) + 
  geom_jitter(width = 0.1)

ggplot(iris, aes( x = Species, y = Sepal.Length)) + 
  geom_boxplot()

# You can even combine mutliple geom's - make a box plot with points on top
ggplot(iris, aes( x = Species, y = Sepal.Length)) + 
  geom_boxplot() + 
  geom_point()

ggplot(iris, aes( x = Species, y = Sepal.Length)) + 
  geom_boxplot() + 
  geom_jitter(width =0.1)

###### Lets talk about how to make plots prettier  ########################

iris_summary %>% 
  ggplot(aes(x = Species, y = mean_sepal_width)) + 
  geom_col() + 
  theme_bw()

iris_summary %>% 
  ggplot(aes(x = Species, y = mean_sepal_width)) + 
  geom_col() + 
  theme_fivethirtyeight()

iris_summary %>% 
  ggplot(aes(x = Species, y = mean_sepal_width)) + 
  geom_col() + 
  theme_excel()

iris_summary %>% 
  ggplot(aes(x = Species, y = mean_sepal_width)) + 
  geom_col() + 
  theme_economist()

iris_summary %>% 
  ggplot(aes(x = Species, y = mean_sepal_width)) + 
  geom_col() + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text.x = element_text(size = 24, family = 'serif'))

iris_summary %>% 
  ggplot(aes(x = Species, y = mean_sepal_width, fill = Species)) + 
  geom_col() + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text.x = element_text(size = 24, family = 'serif')) + 
  scale_fill_wsj()

iris_summary %>% 
  ggplot(aes(x = Species, y = mean_sepal_width, fill = Species)) + 
  geom_col() + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text.x = element_text(size = 24, family = 'serif')) + 
  scale_fill_manual(values = c('steelblue', 'forestgreen', 'goldenrod'))




########### How can we make summary statistics  - Introduce pipe    ########## 
iris %>% 
  ggplot(aes(x = Sepal.Length, y = Sepal.Width, color = Species)) + 
  geom_point()

iris %>%
  group_by(Species) %>% 
  summarise(mean_sepal_width = mean(Sepal.Width), 
            mean_sepal_length = mean(Sepal.Length) )

iris_summary <- iris %>%
  group_by(Species) %>% 
  summarise(mean_sepal_width = mean(Sepal.Width), 
            mean_sepal_length = mean(Sepal.Length) )
iris_summary

iris_summary %>% 
  ggplot(aes(x = Species, y = mean_sepal_width)) + 
  geom_col()

iris_summary %>% 
  ggplot(aes(x = Species, y = mean_sepal_width, fill = Species)) + 
  geom_col(color = 'grey30') + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text.x = element_text(size = 24, family = 'serif')) + 
  scale_fill_manual(values = c('steelblue', 'forestgreen', 'goldenrod'))

# what else can the pipe do
iris %>% head() # can pipe to a base function
# can filter
iris %>%  filter(!Species == 'setosa')
# can keep some columns
iris %>% select(!Species)
#can order the data
iris %>% arrange(Petal.Length)
iris %>% arrange(desc(Petal.Length))
iris %>%  # add column
  mutate(sepal.area = Sepal.Length * Sepal.Width)
iris <- iris %>% 
  mutate(sepal.area = Sepal.Length * Sepal.Width)

# can combine these with groupings
iris %>% 
  group_by(Species) %>% 
  summarise(mean_area  = mean(sepal.area) ) 

# can be more complicated
iris %>% 
  group_by(Species) %>% 
  mutate(z  = (sepal.area - mean(sepal.area)) / sd(sepal.area) ) 

# Can go right into plotting
iris %>% 
  group_by(Species) %>% 
  mutate(z  = (sepal.area - mean(sepal.area)) / sd(sepal.area) )  %>% 
  ggplot(aes(x =z)) + geom_density()

iris %>% 
  group_by(Species) %>% 
  mutate(z  = (sepal.area - mean(sepal.area)) / sd(sepal.area) )  %>% 
  group_by(Species) %>% 
  summarise(u = mean(z) )  

# sanity check - shoudl be 0
iris %>% 
  group_by(Species) %>% 
  mutate(z  = (sepal.area - mean(sepal.area)) / sd(sepal.area) )  %>% 
  ggplot(aes(x =z, color = Species)) + geom_density()


# Importing Data
# Data sets from sports-reference.com
duke_file <- '~/Desktop/duke_stats.csv' # change this to match where your file is
unc_file <-  '~/Desktop/unc_stats.csv' # change this to match where your file is 
unc  <- read_csv(unc_file, skip = 1)
unc
unc  <- read_csv(unc_file)
unc
# note column names are messy - information is encoded in row1 - but not easy to read automatically
# lets make our own column names
cnames <- c('row', 'season', 'conf', 'overall_w', 'overall_l', 'overall_w_l_percent', 
            'conf_w', 'conf_l', 'conf_w_l_percent',
            'srs', 'sos', 'pts_for_avg', 'pts_against_avg', 
            'ap_pre', 'ap_high', 'ap_final','tourney', 'coach')

# Now we can re-read the data in with our own column names - skip first 2  rows
duke <- read_csv(duke_file, col_names = cnames, skip = 2)
unc  <- read_csv(unc_file, col_names = cnames, skip = 2)
# we can add a label for team 
duke <- duke %>% mutate(team = 'BadGuys')
unc  <- unc %>%  mutate(team = 'GoodGuys')
# now we can stick the whole thing together
comb <- rbind(duke, unc) # rbind is a function that sticks one date frame on top the other
comb
comb %>% 
  ggplot(aes(x = overall_w_l_percent, color = team) ) + 
  geom_density() 

comb %>% 
  ggplot(aes(y = overall_w_l_percent, x = team, color = team) ) + 
  geom_boxplot(notch = T) 
# conclusive proof that UNC > Duke
comb %>% glimpse
comb %>% 
  ggplot(aes(x = ap_pre, y = ap_final, color = team)) + 
  geom_point()

# lots of missing data - why?

# if one is NA - gets removed; hhow to deal
# Lets make up a number, 26 and put all unranked there
comb %>% 
  mutate(ap_pre = ifelse(is.na(ap_pre), 26, ap_pre), 
         ap_final = ifelse(is.na(ap_final), 26, ap_final)) %>% 
  ggplot(aes( x = ap_pre, y = ap_final, color = team)) + 
  geom_point()

#Practices


