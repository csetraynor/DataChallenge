# Day 1

library(tidyverse)
library(dplyr)

# read in all three datasets (you'll pick one to use later)
recpies <- read_csv("epi_r.csv")
bikes <- read_csv("nyc-east-river-bicycle-counts.csv")
weather <- read_csv("weatherHistory.csv")

# quickly clean our dataset
recpies <- recpies %>%
  filter(calories < 10000) %>% # remove outliers
  na.omit() # remove rows with NA values

# are the ratings all numeric?
print("Is this variable numeric?")
is.numeric(recpies$rating)

# are the ratings all integers?
print("Is this variable only integers?")
all.equal(recpies$rating, as.integer(recpies$rating)) == T


# plot & add a regression line
ggplot(recpies, aes(x = calories, y = dessert)) + # draw a 
  geom_point() + # add points
  geom_smooth(method = "glm", # plot a regression...
              method.args = list(family = "binomial")) # ...from the binomial family

#we choose weather
weather <- read_csv("weatherHistory.csv")

# clean our dataset
weather <- weather %>%
  na.omit() # remove rows with NA values
#Factor data

weather$Summary <- as.factor(weather$Summary)
weather$ID <- 1:nrow(weather)

weather %>% 
  group_by(Summary) %>%
  mutate(no_rows = length(Summary))
# check category temperature
print("Is temperature a numeric variable?")
is.numeric(weather$`Temperature (C)`)


# plot & add a regression line
ggplot(weather, aes(x = `Temperature (C)`, y = `Daily Summary` )) + # draw a 
  geom_point() # ...from the multinomial family