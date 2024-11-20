### OUTPUT OF THIS SCRIPT:
# A lot of graphs
# (No data set)

rm(list = ls()) # Cleaning the environment
# ctrl + L in console will clear everything
# in plot window click broom

library(readxl)
library(ggplot2)  # includes ggplot
library(gridExtra)
library(car)


########################### Importing data

vegData <- read.csv("2.Data/vegData.csv", row.names = 1)
envData <- read.csv("2.Data/envDataWithShannonTransformed.csv", row.names = 1)

# FData$SoilTexture <- as.factor(FData$SoilTexture)  # Change the data type of a column
# FData$AnimalActivity <- as.factor(FData$AnimalActivity)  # Change the data type of a column
# FData$ThufurHollowNA <- as.factor(FData$ThufurHollowNA)  # Change the data type of a column


########################### Function for creating plots

# Creates a single scatter or box plot and saves the image
# Takes a type ('box' or 'scatter'), and two vectors
# Returns a ggplot2 object
createSinglePlot <- function(type, varY, varX) {
  varY_name <- deparse(substitute(varY))
  varY_name <- sub(".*\\$", "", varY_name)
  varX_name <- deparse(substitute(varX))
  varX_name <- sub(".*\\$", "", varX_name)
  if (type == "scatter") {
    plot <- ggplot(data = NULL, mapping = aes(y= varY, x = varX)) +
      geom_point() +
      geom_smooth(method = "lm") +
      labs(x = varX_name, y = varY_name)
    ggsave(filename = paste0("4.Results/Expl.Sctr.",
                             varY_name,
                             varX_name,
                             ".png"),
         plot=plot, width=8, height=6, dpi=300)
  } else if (type == "box") {
    plot <- ggplot(data = NULL, mapping = aes(y= varY, x = varX)) +
      geom_boxplot() +
      labs(x = varX_name, y = varY_name)
    ggsave(filename = paste0("4.Results/Expl.Box.",
                             varY_name,
                             varX_name,
                             ".png"),
           plot=plot, width=8, height=6, dpi=300)
  } else {
    stop("Invalid plot type. Use 'scatter' or 'box'.")
  }
  return(plot)
}

# Creates two scatter or box plots and saves them as one image
# Takes a type ('box' or 'scatter'), and three vectors
# Returns a ggplot2 object
createDoublePlot <- function(type, varY1, varY2, varX) {
  varY1_name <- deparse(substitute(varY1))
  varY1_name <- sub(".*\\$", "", varY1_name)
  varY2_name <- deparse(substitute(varY2))
  varY2_name <- sub(".*\\$", "", varY2_name)
  varX_name <- deparse(substitute(varX))
  varX_name <- sub(".*\\$", "", varX_name)
  if (type == "scatter") {
    plot1 <- ggplot(data = NULL, mapping = aes(y= varY1, x = varX)) +
      geom_point() +
      geom_smooth(method = "lm") +
      labs(x = varX_name, y = varY1_name)
    plot2 <- ggplot(data = NULL, mapping = aes(y= varY2, x = varX)) +
      geom_point() +
      geom_smooth(method = "lm") +
      labs(x = varX_name, y = varY2_name)
    combinedPlot <- grid.arrange(plot1, plot2, ncol = 2)
    ggsave(filename = paste0("4.Results/Expl.Sctr.",
                             varY1_name,
                             varY2_name,
                             varX_name,
                             ".png"), 
           plot = combinedPlot, width = 12, height = 6, dpi = 300)
  } else if (type == "box") {
    plot1 <- ggplot(data = NULL, mapping = aes(y= varY1, x = varX)) +
      geom_boxplot() +
      labs(x = varX_name, y = varY1_name)
    plot2 <- ggplot(data = NULL, mapping = aes(y= varY2, x = varX)) +
      geom_boxplot() +
      labs(x = varX_name, y = varY1_name)
    combinedPlot <- grid.arrange(plot1, plot2, ncol = 2)
    ggsave(filename = paste0("4.Results/Expl.Box.",
                             varY1_name,
                             varY2_name,
                             varX_name,
                             ".png"),
           plot = combinedPlot, width = 12, height = 6, dpi = 300)
  } else {
    stop("Invalid plot type. Use 'scatter' or 'box'.")
  }
  return(plot)
}




####################################################### scatter plots

########### greenness plant biomass
# TODO: maybe logarithmic transformation: test if residuals are normally distributed
#  couple it to plant cover
#  give all pictures with very bright condition different color dots in the graph
createSinglePlot("scatter", envData$GreennessIndex, envData$PlantBiomassLog)

createDoublePlot("scatter", envData$GreennessIndex, envData$PlantBiomassLog, envData$VerticalWaterDistanceLog)
createDoublePlot("scatter", envData$GreennessIndex, envData$PlantBiomassLog, envData$SoilMoistureAvrg)
createDoublePlot("scatter", envData$GreennessIndex, envData$PlantBiomassLog, envData$pHLog)
createDoublePlot("scatter", envData$GreennessIndex, envData$PlantBiomassLog, envData$ECLog)
createDoublePlot("scatter", envData$GreennessIndex, envData$PlantBiomassLog, envData$SalinityAdjustedLog)
createDoublePlot("scatter", envData$GreennessIndex, envData$PlantBiomassLog, envData$BulkDensityIncRootsLog)

createSinglePlot("scatter", envData$VerticalWaterDistanceLog, envData$SoilMoistureAvrg)
createSinglePlot("scatter", envData$SalinityAdjustedLog, envData$ECLog)
createSinglePlot("scatter", envData$SalinityAdjustedLog, envData$pHLog)
createSinglePlot("scatter", envData$VerticalWaterDistanceLog, envData$SoilMoistureAvrg)



####################################################### box plots

# ########### greenness animal activity, plant biomass animal activity
# plot16 <- ggplot(data = FData, mapping = aes(y= GreennessIndex, x = AnimalActivity)) +
#   geom_boxplot()
# plot17 <- ggplot(data = FData, mapping = aes(y= PlantBiomass, x = AnimalActivity)) +
#   geom_boxplot()
# # Arrange the plots side by side
# combinedPlot16.17 <- grid.arrange(plot16, plot17, ncol = 2)
# ggsave(filename = "4.Results/Expl.Box.GreennessBiomassAnimalAct.png", 
#        plot = combinedPlot16.17, width = 12, height = 6, dpi = 300)
# 
# 
# ########### greenness thufur/hollow, plant biomass thufur/hollow
# plot18 <- ggplot(data = FData, mapping = aes(y= GreennessIndex, x = ThufurHollowNA)) +
#   geom_boxplot()
# plot19 <- ggplot(data = FData, mapping = aes(y= PlantBiomass, x = ThufurHollowNA)) +
#   geom_boxplot()
# # Arrange the plots side by side
# combinedPlot18.19 <- grid.arrange(plot18, plot19, ncol = 2)
# ggsave(filename = "4.Results/Expl.Box.GreennessBiomassThufurHollow.png", 
#        plot = combinedPlot18.19, width = 12, height = 6, dpi = 300)

