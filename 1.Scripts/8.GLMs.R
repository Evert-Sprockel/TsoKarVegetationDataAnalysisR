### OUTPUT OF THIS SCRIPT:
# Test results from generalized linear model printed to the terminal, scatter plots
# (No data sets)

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
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # Cleaning plot window (or click broom)

library(ggplot2)
library(gridExtra)
library(MuMIn)
library(car)


########################### Importing data

# NOTE: SEE WHICH PLOTS ARE REMOVED AND WHY IN THE SCRIPT WHERE THE DATA IS CLEANED
# Load envData and vegData from CSV files, setting the first column as row names
envData <- read.csv("3.TemporaryFiles/envDataWithShannonTransformed.csv", row.names = 1)
envData$ThufurHollowNA <- as.factor(envData$ThufurHollowNA)
envData$AnimalActivity <- as.factor(envData$AnimalActivity)


########################### Function for linear model

# Creates a single scatter or box plot
# Takes a type ('box' or 'scatter'), two vectors and a name for the y var
# Returns a ggplot2 object
createSinglePlot <- function(type, varY, varX, varY_name, varX_name, categories=NULL, cName=NULL) {
  if (type == "scatter") {
    if (!is.null(categories)) {
      plot <- ggplot(data = NULL, mapping = aes(y= varY, x = varX)) +
        geom_point(aes(color = categories)) +
        geom_smooth(method = "lm") +
        labs(x = varX_name, y = varY_name, color = cName)
    } else {
      plot <- ggplot(data = NULL, mapping = aes(y= varY, x = varX)) +
        geom_point() +
        geom_smooth(method = "lm") +
        labs(x = varX_name, y = varY_name)
    }
  } else if (type == "box") {
    plot <- ggplot(data = NULL, mapping = aes(y= varY, x = varX)) +
      geom_boxplot() +
      labs(x = varX_name, y = varY_name)
  } else {
    stop("Invalid plot type. Use 'scatter' or 'box'.")
  }
  return(plot)
}


# Creates a group of plots and saves them, possibly with different colors
# Takes a type ('box' or 'scatter'), a dataframe of X variables, a vector for Y, and a group name
# Returns nothing
# NOTE: WITHOUT THE ggplotGrob() FUNCTION, ALL PLOTS WILL CONTAIN THE SAME DATA: LAZY EVALUATION
createAndSavePlots <- function(type, varY, varsX, groupName, categories=NULL) {
  nameY <- deparse(substitute(varY))
  nameY <- sub(".*\\$", "", nameY)
  if (!is.null(categories)) {
    cName <- deparse(substitute(categories))
  cName <- sub(".*\\$", "", cName)
  }
  plotList <- list()
  for (name in colnames(varsX)) {
    currentPlot <- ggplotGrob(createSinglePlot(type, varsX[[name]], varY, nameY, name, categories, cName))
    plotList[[name]] <-  currentPlot
  }
  combined_plot <- do.call(grid.arrange, c(plotList, ncol = ceiling(ncol(varsX) / 2)))
  ggsave(paste0("4.Results/GLMs.", groupName, ".png"),
         plot = combined_plot,
         dpi = 200,
         width = (ncol(varsX) * 6 / 2),
         height = 8,
         units = "in")
}


# Create a linear model and prints output
# Takes a vector for the dependent variable, and a dataframe with the independent variables
# Returns nothing
runGLM <- function(depVar, data, useGamma = FALSE, link="log") {
  family_arg <- if (useGamma) {
    Gamma(link = link)
  } else {
    gaussian() # Default to Gaussian family if Gamma is not used
  }
  model <- glm(depVar ~ scale(VerticalWaterDistanceLog) + scale(SoilMoistureAvrg) + scale(pHLog) +
                 scale(SalinityAdjustedLog) + scale(BulkDensityIncRootsLog),
               family = family_arg, data = data)
  print(5)
  par(mfrow = c(2, 2))
  plot(model)
  par(mfrow = c(1, 1))
  
  cat("\n\n===================== model summary\n")
  print(summary(model))
  cat("\n\n===================== vif/collinearity\n")
  print(vif(model))
  
  # Model averaging is mainly for EXPLORATORY research, i.e. you went out into the field and measured 
  # lots of things and you don't know what might effect what or which variables to keep in the model.
  options(na.action=na.fail)
  # All possible models. you can limit the dredge in many ways, e.g. to only a few variables or 
  # excluding combinations of variables. see help pages.
  models <- dredge(model)
  
  modelSet <- get.models(models, subset = delta < 2) #subsetting models within 2 AICc of best model
  modelTable <- model.sel(modelSet) #table of the selected models
  avgMod <- model.avg(modelSet) #averaging the selected models
  avgModSumm <- summary(avgMod) #storing the average model summary
  con <- confint(avgMod, level=.95, full=TRUE)
  # Table of the "full" coefficients, which means that variables were assigned a coefficient of zero 
  # if they are not present in the model.
  coefTable <- as.data.frame(avgModSumm$coefmat.full) 
  
  cat("\n\n===================== models\n")
  print(models)
  cat("\n\n===================== model table\n")
  print(modelTable)  # include this in report
  cat("\n\n===================== avgModSumm\n")
  print(avgModSumm)
  print(con)
  cat("\n\n===================== coefTable\n")
  print(coefTable)
}


########################### Analysis

runGLM(envData$PlantBiomassLog, envData, FALSE)  # Worse
runGLM(envData$PlantBiomassLog, envData, TRUE, "log")

runGLM(envData$ShannonIndex, envData, FALSE)
# runGLM(envData$ShannonIndex, envData, TRUE, "log")  # Worse (after scaling)

runGLM(envData$SpeciesRichness, envData, FALSE)
runGLM(envData$SpeciesRichness, envData, TRUE, "log")  # Worse


########################### Visualization for plant biomass

envDataPlantBiomass <- envData[, c("SoilMoistureAvrg",
                                   "pHLog",
                                   "SalinityAdjustedLog",
                                   "BulkDensityIncRootsLog")]
createAndSavePlots("scatter", envData$PlantBiomassLog, envDataPlantBiomass, "PlantBiomassAA", envData$AnimalActivity)
createAndSavePlots("scatter", envData$PlantBiomassLog, envDataPlantBiomass, "PlantBiomassTHA", envData$ThufurHollowNA)
createAndSavePlots("scatter", envData$PlantBiomassLog, envDataPlantBiomass, "PlantBiomass")






