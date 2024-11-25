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

# library(ggplot2)
# library(vegan)
library(MuMIn)
library(car)


########################### Importing data

# NOTE: SEE WHICH PLOTS ARE REMOVED AND WHY IN THE SCRIPT WHERE THE DATA IS CLEANED
# Load envData and vegData from CSV files, setting the first column as row names
envData <- read.csv("3.TemporaryFiles/envDataWithShannonTransformed.csv", row.names = 1)


########################### Function for linear model

runGLM <- function(depVar, data, type) {
  model <- glm(depVar ~ scale(VerticalWaterDistanceLog) + scale(SoilMoistureAvrg) + scale(pHLog) +
                 scale(SalinityAdjustedLog) + scale(BulkDensityIncRootsLog),
               family = Gamma(link = type), data = data)
  cat("\n\n===================== model summary\n")
  print(summary(model))
  cat("\n\n===================== vif/collinearity\n")
  print(vif(model))
  
  par(mfrow = c(2, 2))
  plot(model)
  
  # Model averaging is mainly for EXPLORATORY research, i.e. you went out into the field and measured 
  # lots of things and you don't know what might effect what or which variables to keep in the model.
  options(na.action=na.fail)
  # All possible models. you can limit the dredge in many ways, e.g. to only a few variables or 
  # excluding combinations of variables. see help pages.
  models <- dredge(model)
  cat("\n\n===================== models\n")
  print(models)
  modelSet <- get.models(models, subset = delta < 2) #subsetting models within 2 AICc of best model
  modelTable <- model.sel(modelSet) #table of the selected models
  cat("\n\n===================== model table\n")
  print(modelTable)
  avgMod <- model.avg(modelSet) #averaging the selected models
  avgModSumm <- summary(avgMod) #storing the average model summary
  cat("\n\n===================== avgModSumm\n")
  print(avgModSumm)
  # Table of the "full" coefficients, which means that variables were assigned a coefficient of zero 
  # if they are not present in the model.
  coefTable <- as.data.frame(avgModSumm$coefmat.full) 
  cat("\n\n===================== coefTable\n")
  print(coefTable)
}


########################### Analysis

runGLM(envData$PlantBiomassLog, envData, "log")

runGLM(envData$ShannonIndex, envData, "identity")

runGLM(envData$SpeciesRichness, envData, "identity")








