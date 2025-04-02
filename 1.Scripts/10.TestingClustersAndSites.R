### PURPOSE OF THIS SCRIPT:
# Testing differences for the environmental/vegetation gradients per cluster and site

### OUTPUT OF THIS SCRIPT:
# pdf's of box plots


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
# Add site numbers as a column
envData$Site <- as.factor(sub("s(\\d+).*", "\\1", rownames(envData)))
# Loading the cluster data from the script "Clustering.R"
envData$Cluster <- as.factor(read.csv("3.TemporaryFiles/Clusters.Plots.1ward.D.csv")$Cluster)
# For the plots
envDataHabitat <- envData[, c("VerticalWaterDistance",
                              "SoilMoistureAvrg",
                              "pH",
                              "SalinityAdjusted",
                              "BulkDensityIncRoots")]
# For testing
envDataHabitatTransf <- envData[, c("VerticalWaterDistanceLog",
                                    "SoilMoistureAvrg",
                                    "pHLog",
                                    "SalinityAdjustedLog",
                                    "BulkDensityIncRootsLog")]
# For the plot names
habitatVarsNames <- c("Vertical water distance (cm)",
                      "Soil moisture (%)",
                      "pH",
                      "Salinity (µS/m)",
                      "Bulk density (g/cm3)")
# For the plots
envDataOtherVars <- envData[, c("GreennessIndex",
                                "PlantBiomass",
                                "ShannonIndex",
                                "SpeciesRichness")]
# For testing
envDataOtherVarsTransf <- envData[, c("GreennessIndex",
                                      "PlantBiomassLog",
                                      "ShannonIndex",
                                      "SpeciesRichness")]
# For the plot names
otherVarsNames <- c("Greenness index",
                    "Plant biomass (g)",
                    "Shannon diversity index",
                    "Species richness")




########################### Functions

# Creates a single scatter or box plot
# Takes a type ('box' or 'scatter'), two vectors with name, and integer for font size in points
# Returns a ggplot2 object
createSinglePlot <- function(type, varY, varX, varY_name, varX_name, fontSize = 6) {
  # Theme for the plots
  base_theme <- theme_minimal() +
    theme(
      axis.text = element_text(size = fontSize),
      axis.title = element_text(size = fontSize),
    )
  if (type == "scatter") {
    plot <- ggplot(mapping = aes(y = varY, x = varX)) +
      geom_point() +
      geom_smooth(method = "lm") +
      labs(x = varX_name, y = varY_name) +
      base_theme
  } else if (type == "box") {
    plot <- ggplot(mapping = aes(y = varY, x = varX, color = varX)) +
      geom_boxplot() +
      labs(x = varX_name, y = varY_name) +
      base_theme +
      theme(legend.position = "none")
  } else {
    stop("Invalid plot type. Use 'scatter' or 'box'.")
  }
  return(plot)
}


# Creates a group of plots and saves them
# Takes a type ('box' or 'scatter'), a dataframe of Y variables, a vector for x, and a group name
# Returns nothing
# NOTE: WITHOUT THE ggplotGrob() FUNCTION, ALL PLOTS WILL CONTAIN THE SAME DATA: LAZY EVALUATION
createAndSavePlots <- function(type, varsY, varX, groupName, namesVector) {
  if (length(namesVector) != ncol(varsY)) {
    stop("Length of namesVector must match the number of columns in varsY.")
  }
  
  nameX <- deparse(substitute(varX))
  nameX <- sub(".*\\$", "", nameX)
  plotList <- list()
  # Create plots for each column in varsY
  for (i in seq_along(namesVector)) {
    varName <- namesVector[i]
    plotList[[varName]] <- ggplotGrob(createSinglePlot(type, varsY[[i]], varX, varName, nameX))
  }
  # Combine plots using gridExtra
  combined_plot <- do.call(grid.arrange, c(plotList, ncol = ncol(varsY)))
  # Save the combined plot
  ggsave(
    filename = paste0("4.Results/Testing", groupName, ".pdf"),
    plot = combined_plot,
    device = cairo_pdf,
    dpi = 300,
    width = (ncol(varsY) * 3.68),
    height = 6,
    units = "cm"
  )
}


# Runs a permanova for all vars in a data frame with pairwise comparison
# Takes a dataframe with independent variables, and a string for group col name, and the full dataframe
# Returns nothing
# Prints outputs of the tests
doPermanova <- function(dataVar, groupName, fullData) {
  formulaObj <- as.formula(paste("dataVar ~", groupName))
  
  permnv <- adonis2(
    formulaObj,
    data = fullData,
    method = "euc"
  )
  print(permnv)
  permnvPostHoc <- pairwise.adonis2(
    formulaObj,
    data = fullData,
    method = "euc"
  )
  print(permnvPostHoc)
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

createAndSavePlots("box", envDataHabitat, envData$Cluster, "Clusters.HabitatVariables", habitatVarsNames)
createAndSavePlots("box", envDataOtherVars, envData$Cluster, "Clusters.OtherVariables", otherVarsNames)
createAndSavePlots("box", envDataHabitat, envData$Site, "Sites.HabitatVariables", habitatVarsNames)
createAndSavePlots("box", envDataOtherVars, envData$Site, "Sites.OtherVariables", otherVarsNames)


########################### Analysis

doPermanova(envDataHabitatTransf, "Cluster", envData)
# Cluster significance:
# 1,   2,   3,   4,   5
# A    A    B    A    AB
runAnovaAndPostHoc(TRUE, envDataHabitatTransf, envData$Cluster)
runAnovaAndPostHoc(FALSE, envDataOtherVarsTransf, envData$Cluster)

doPermanova(envDataHabitatTransf, "Site", envData)
# Site significance:
# 1,   2,   3
# A    A    B
runAnovaAndPostHoc(TRUE, envDataHabitatTransf, envData$Site)
runAnovaAndPostHoc(FALSE, envDataOtherVarsTransf, envData$Site)




























