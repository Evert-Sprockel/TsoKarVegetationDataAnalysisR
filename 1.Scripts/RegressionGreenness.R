### OUTPUT OF THIS SCRIPT:
# Regression results
# (No data set)


rm(list = ls()) # Cleaning the environment
# ctrl + L in console will clear everything
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # Cleaning plot window (or click broom)

library(ggplot2)
library(gridExtra)


########################### Importing data

envData <- read.csv("3.TemporaryFiles/envDataWithShannonTransformed.csv", row.names = 1)
envDataVars <- envData[, c("VerticalWaterDistanceLog",
                           "SoilMoistureAvrg",
                           "pHLog",
                           "SalinityAdjustedLog",
                           "ECLog",
                           "BulkDensityIncRootsLog",
                           "PlantBiomassLog",
                           "ShannonIndex",
                           "SpeciesRichness")]


########################### Function for running regression

run_regressions <- function(response, predictors) {
  results <- lapply(names(predictors), function(var) {
    formula <- as.formula(paste("response ~", var))
    model <- lm(formula, data = cbind(response, predictors))
    summary(model)
  })
  names(results) <- names(predictors)
  return(results)
}


########################### Runnng the regressions

regression_results <- run_regressions(envData$GreennessIndex, envDataVars)
regression_results
