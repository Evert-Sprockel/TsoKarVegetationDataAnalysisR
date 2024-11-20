rm(list = ls()) # Cleaning the environment
# ctrl + L will clear console
# in plot window click broom

library(ggplot2)
library(vegan)

########################### Importing data

# NOTE: SEE WHICH PLOTS ARE REMOVED AND WHY IN THE SCRIPT WHERE THE DATA IS CLEANED
# Load envData and vegData from CSV files, setting the first column as row names
envData <- as.matrix(read.csv("2.Data/envDataWithShannonTransformed.csv", row.names = 1))





# TODO: use a model selection tool to see which variables are most
# important. And then use the box cox transformation


# mixed linear model contains random variables: lmer() instead of lm(). Generalized means that a 
# non-normal variable is used anyway or something: glm()
# heteroschedasticity: variance residuals is not constant or something. plot(simulateResiduals(mdl))

