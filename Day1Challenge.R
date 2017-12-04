# Day 1

library(tidyverse)


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
#we would like to see if temperature and windspeed are correlated, using linear regression
weather <- read_csv("weatherHistory.csv")

# clean our dataset
weather <- weather %>%
  filter(Humidity > 0) 
  na.omit() 
# remove rows with NA values
#Factor data

# check category temperature (predictor)
print("Is temperature a numeric variable?")
is.numeric(weather$`Temperature (C)`)

#check category humidity (response)
print("Is wind speed a numeric variable?")
is.numeric(weather$Humidity)

# plot & add a regression line
ggplot(weather, aes(x = `Temperature (C)`, y = Humidity)) + # draw a 
  geom_point() +
geom_smooth(method = "glm", # plot a regression...
            method.args = list(family = "gaussian")) # ...from the binomial family

# plot & add a regression line
