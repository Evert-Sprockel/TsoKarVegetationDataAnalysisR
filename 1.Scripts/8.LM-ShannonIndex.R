rm(list = ls()) # Cleaning the environment
# ctrl + L will clear console
# in plot window click broom

library(ggplot2)
library(vegan)

########################### Importing data

# NOTE: SEE WHICH PLOTS ARE REMOVED AND WHY IN THE SCRIPT WHERE THE DATA IS CLEANED
# Load envData and vegData from CSV files, setting the first column as row names
envData <- read.csv("2.Data/envDataForMVATransformed.csv", row.names = 1)





# TODO: there's now a new dataset with all the variables necessary. However, they aren't transformed.
# Maybe I should just use the same dataset everywhere and select the variables I need, instead of
# creating a ton of copies with different variables? In that way, I don't have to transform every-
# thing again.

# I guess in general, it's not very good practice to work with different copies of the same dataset,
# since this might lead to differences/confusion/etc.

# So workflow will be: clean data, calculate the shannon and richness, then transform the 

# Anyway, after tranforming the dataset, use a model selection tool to see which variables are most
# important. And then use the box cox tranformation










# mixed linear model contains random variables: lmer() instead of lm(). Generalized means that a 
# non-normal variable is used anyway or something: glm()
# heteroschedasticity: variance residuals is not constant or something. plot(simulateResiduals(mdl))