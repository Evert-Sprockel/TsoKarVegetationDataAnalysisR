# The dependent variable is vegetation biomass (PlantBiomassLog, not normal). The independent 
# variables are:
# - VerticalWaterDistanceLog (normal)
# - SoilMoisureAvrg (normal)
# - pHLog (normal)
# - ECLog (not normal)
# - SalinityAdjustedLog (not normal)
# - BulkDensityIncRootsLog (not normal)


rm(list = ls()) # Cleaning the environment
# ctrl + L will clear console
# in plot window click broom

library(ggplot2)
library(vegan)
library(DHARMa)

########################### Importing data

# NOTE: SEE WHICH PLOTS ARE REMOVED AND WHY IN THE SCRIPT WHERE THE DATA IS CLEANED
# Load envData and vegData from CSV files, setting the first column as row names
envData <- read.csv("2.Data/envDataWithShannonTransformed.csv", row.names = 1)


# first use un-transformed variables
model <- glm(PlantBiomassLog ~ VerticalWaterDistanceLog + SoilMoistureAvrg + pHLog +
             ECLog + SalinityAdjustedLog + BulkDensityIncRootsLog,
             family = Gamma(link = "log"), data = envData)
summary(model)

par(mfrow = c(2, 2))
plot(model)
simulationOutput <- simulateResiduals(fittedModel = model)
plot(simulationOutput)

dispersion <- sum(resid(model, type = "pearson")^2) / df.residual(model)
dispersion

step(model, direction = "both")



# TODO: use a model selection tool to see which variables are most
# important. And then use the box cox transformation


# mixed linear model contains random variables: lmer() instead of lm(). Generalized means that a 
# non-normal variable is used anyway or something: glm()
# heteroschedasticity: variance residuals is not constant or something. plot(simulateResiduals(mdl))

