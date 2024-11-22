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
library(MuMIn)
library(car)

########################### Importing data

# NOTE: SEE WHICH PLOTS ARE REMOVED AND WHY IN THE SCRIPT WHERE THE DATA IS CLEANED
# Load envData and vegData from CSV files, setting the first column as row names
envData <- read.csv("3.TemporaryFiles/envDataWithShannonTransformed.csv", row.names = 1)


# first use un-transformed variables
model <- glm(PlantBiomassLog ~ scale(VerticalWaterDistanceLog) + scale(SoilMoistureAvrg) + scale(pHLog) +
               scale(SalinityAdjustedLog) + scale(BulkDensityIncRootsLog),
             family = Gamma(link = "log"), data = envData)
summary(model)
vif(model)

par(mfrow = c(2, 2))
plot(model)


#option 3: model averaging
#Mainly for EXPLORATORY research i.e. you went out into the field and measured lots of things and you don't know what might effect what or which variables to keep in the model.
options(na.action=na.fail)
models <- dredge(model) #all possible models. you can limit the dredge in many ways, e.g. to only a few variables or excluding combinations of variables. see help pages.
models
modelset <- get.models(models, subset = delta < 2) #subsetting models within 2 AICc of best model
modeltable <- model.sel(modelset) #table of the selected models
modeltable
avgmod <- model.avg(modelset) #averaging the selected models
avgmodsumm <- summary(avgmod) #storing the average model summary
avgmodsumm
coeftable <- as.data.frame(avgmodsumm$coefmat.full) #table of the "full" coefficients, which means that variables were assigned a coefficient of zero if they are not present in the model.







