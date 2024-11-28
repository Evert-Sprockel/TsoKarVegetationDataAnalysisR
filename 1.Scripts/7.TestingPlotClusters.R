### OUTPUT OF THIS SCRIPT:
# Images of box plots
# (No data sets)


rm(list = ls()) # Cleaning the environment
# ctrl + L will clear console
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # Cleaning plot window (or click broom)

library(vegan)
library(ggplot2)
library(gridExtra)
library(pairwiseAdonis)

########################### Importing data

# NOTE: SEE WHICH PLOTS ARE REMOVED AND WHY IN THE SCRIPT WHERE THE DATA IS CLEANED
# Load envData and vegData from CSV files, setting the first column as row names
envData <- read.csv("3.TemporaryFiles/envDataWithShannonTransformed.csv", row.names = 1)
envDataHabitat <- envData[, c("VerticalWaterDistanceLog",
                              "SoilMoistureAvrg",
                              "pHLog",
                              "SalinityAdjustedLog",
                              "BulkDensityIncRootsLog")]
envDataOtherVars <- envData[, c("GreennessIndex",
                                "PlantBiomassLog",
                                "ShannonIndex",
                                "SpeciesRichness")]

# Loading the cluster data from the script "Clustering.R"
plotClusters <- read.csv("3.TemporaryFiles/Clusters.Plots.1ward.D.csv")
plotClusters$Cluster <- as.factor(plotClusters$Cluster)


########################### Functions

# Creates a single scatter or box plot
# Takes a type ('box' or 'scatter'), two vectors and a name for the y var
# Returns a ggplot2 object
createSinglePlot <- function(type, varY, varX, varY_name, varX_name) {
  if (type == "scatter") {
    plot <- ggplot(data = NULL, mapping = aes(y= varY, x = varX)) +
      geom_point() +
      geom_smooth(method = "lm") +
      labs(x = varX_name, y = varY_name)
  } else if (type == "box") {
    plot <- ggplot(data = NULL, mapping = aes(y= varY, x = varX, color = varX)) +
      geom_boxplot() +
      labs(x = varX_name, y = varY_name)+
      theme(legend.position = "none")  # Removes the legend
  } else {
    stop("Invalid plot type. Use 'scatter' or 'box'.")
  }
  return(plot)
}


# Creates a group of plots and saves them
# Takes a type ('box' or 'scatter'), a dataframe of Y variables, a vector for x, and a group name
# Returns nothing
# NOTE: WITHOUT THE ggplotGrob() FUNCTION, ALL PLOTS WILL CONTAIN THE SAME DATA: LAZY EVALUATION
createAndSavePlots <- function(type, varsY, varX, groupName) {
  nameX <- deparse(substitute(varX))
  nameX <- sub(".*\\$", "", nameX)
  plotList <- list()
  for (name in colnames(varsY)) {
    currentPlot <- ggplotGrob(createSinglePlot(type, varsY[[name]], varX, name, nameX))
    plotList[[name]] <-  currentPlot
  }
  combined_plot <- do.call(grid.arrange, c(plotList, ncol = ncol(varsY)))
  ggsave(paste0("4.Results/TestingClusters.", groupName, ".png"),
         plot = combined_plot,
         dpi = 200,
         width = (ncol(varsY) * 3),
         height = 5,
         units = "in")
}


# Runs an anova for all vars in a data frame, and runs a Tukey post-hoc only for those significant
# Takes an multiplication integer for the bonferoni adjustment, a dataframe, and a vector of groups
# Returns nothing
# Prints outputs of the tests
runAnovaAndPostHoc <- function(doBonferoniAdjust, data, groups) {
  anovaList <- list()
  pValueList <- list()
  adjust <- ifelse(doBonferoniAdjust, ncol(data), 1)
  # Loop through all variables in the df, run an anova and save results and p-value separately
  for (name in colnames(data)) {
    model <- aov(data[[name]] ~ groups)
    summary <- summary(model)
    pValue <- summary[[1]]$`Pr(>F)`[1]
    anovaList[[name]] <- model
    pValueList[[name]] <- pValue * adjust
  }
  # Print results
  cat("\n\n===================== p-values\n")
  print(pValueList)
  cat("\n\n===================== anova summaries\n")
  print(lapply(anovaList, summary))
  # Decide which anovas require a post-hoc
  postHocList <- list()
  for (element in names(pValueList)) {
    if (pValueList[element] < 0.05) {
      postHocList[[element]] <- anovaList[[element]]
    }
  }
  # Apply the post-hoc to those anovas
  cat("\n\n===================== Post-hocs\n")
  print(lapply(postHocList, TukeyHSD))
}


########################### Creating figures

# Create one figure for all five environmental gradients: combined as the "habitat" variables
createAndSavePlots("box", envDataHabitat, plotClusters$Cluster, "HabitatVariables")

# Create one figure for the remaining variables
createAndSavePlots("box", envDataOtherVars, plotClusters$Cluster, "OtherVariables")


########################### Analysis

# only for five habitat variables
permnv <- adonis2(
  envDataHabitat ~ Cluster,
  data = plotClusters,
  method = "euc"
)
permnv
permnvPostHoc <- pairwise.adonis2(
  envDataHabitat ~ Cluster,
  data = plotClusters,
  method = "euc"
)
permnvPostHoc
# Cluster significance:
# 1,   2,   3,   4,   5
# A    A    B    A    AB


runAnovaAndPostHoc(TRUE, envDataHabitat, plotClusters$Cluster)
runAnovaAndPostHoc(FALSE, envDataOtherVars, plotClusters$Cluster)


























