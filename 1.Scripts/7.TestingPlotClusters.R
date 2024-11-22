### OUTPUT OF THIS SCRIPT:
# Images of box plots
# (No data sets)


rm(list = ls()) # Cleaning the environment
# ctrl + L in console will clear everything
# in plot window click broom

library(vegan)
library(ggplot2)
library(pairwiseAdonis)

########################### Importing data

# NOTE: SEE WHICH PLOTS ARE REMOVED AND WHY IN THE SCRIPT WHERE THE DATA IS CLEANED
# Load envData and vegData from CSV files, setting the first column as row names
envData <- read.csv("3.TemporaryFiles/envDataWithShannonTransformed.csv", row.names = 1)
envData <- envData[, c("GreennessIndex",
                       "PlantBiomassLog",
                       "VerticalWaterDistanceLog",
                       "SoilMoistureAvrg",
                       "pHLog",
                       "SalinityAdjustedLog",
                       "BulkDensityIncRootsLog",
                       "ShannonIndex",
                       "SpeciesRichness")]
# vegData <- read.csv("3.TemporaryFiles/vegData.csv", row.names = 1)

# Loading the cluster data from the script "Clustering.R"
plotClusters <- read.csv("3.TemporaryFiles/Clusters.Plots.1ward.D.csv")
plotClusters$Cluster <- as.factor(plotClusters$Cluster)
# speciesClusters <- read.csv("3.TemporaryFiles/Clusters.Species.1ward.D.csv")


########################### Functions

# Creates a single scatter or box plot
# Takes a type ('box' or 'scatter'), and two vectors
# Returns a ggplot2 object
createSinglePlot <- function(type, varY, varX, varY_name) {
  varX_name <- deparse(substitute(varX))
  varX_name <- sub(".*\\$", "", varX_name)
  if (type == "scatter") {
    plot <- ggplot(data = NULL, mapping = aes(y= varY, x = varX)) +
      geom_point() +
      geom_smooth(method = "lm") +
      labs(x = varX_name, y = varY_name)
  } else if (type == "box") {
    plot <- ggplot(data = NULL, mapping = aes(y= varY, x = varX)) +
      geom_boxplot() +
      labs(x = varX_name, y = varY_name)
  } else {
    stop("Invalid plot type. Use 'scatter' or 'box'.")
  }
  return(plot)
}


########################### Creating figures

plotList <- list()
for (name in colnames(envData)) {
  plotList[[name]] <- createSinglePlot("box", envData[[name]], plotClusters$Cluster, name)
}
combined_plot <- do.call(grid.arrange, c(plotList, ncol = 3))
ggsave("4.Results/TestingClusters.EnvBoxPlots.png", 
       plot = combined_plot,
       dpi = 200,
       width = 12,
       height = 12,
       units = "in")













########################### Analysis

# permnv <- adonis2(
#   envData ~ plotClusters$Cluster,
#   data = envData,
#   method = "euc"
# )
# permnv



# only for six habitat variables
permnv <- adonis2(
  envData ~ Cluster,
  data = plotClusters,
  method = "euc"
)
permnv
permnvPostHoc <- pairwise.adonis2(
  envData ~ Cluster,
  data = plotClusters,
  method = "euc"
)
permnvPostHoc



# do this for the six habitat variables, do bonferoni adjustment (multiply p by 6)
# do this for the three remaining variables separately, no adjustment
# only do the tukeyHSD for the significant anovas (all 9)
anovaList <- list()
for (name in colnames(envData)) {
  anova1 <- aov(envData[[name]] ~ plotClusters$Cluster)
  anovaList[[name]] <- anova1
}

lapply(anovaList, summary)

lapply(anovaList, TukeyHSD)





















