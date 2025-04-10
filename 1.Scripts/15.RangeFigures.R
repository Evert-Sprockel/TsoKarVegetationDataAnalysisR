### PURPOSE OF THIS SCRIPT:
# Creating figures to visualize the habitat preferences of species

### OUTPUT OF THIS SCRIPT:
# Images containing several graphs of species range data


rm(list = ls()) # Cleaning the environment
# ctrl + L in console will clear everything
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # Cleaning plot window (or click broom)

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(gridExtra)
library(patchwork)


########################### Importing data

# Load envData and vegData from CSV files, setting the first column as row names
envData <- read.csv("3.TemporaryFiles/envDataWithShannonTransformed.csv")
vegData <- read.csv("3.TemporaryFiles/vegData.csv")
envData <- envData %>%
  rename("Plot" = "X")
vegData <- vegData %>%
  rename("Plot" = "X")

# NOTE: SEE WHICH PLOTS ARE REMOVED AND WHY IN THE SCRIPT WHERE THE DATA IS CLEANED


########################### Function for creating the plots

createPlot <- function(vegetData, environData, envVar, short = FALSE, fontSize = 6) {
  # Reshape species data
  speciesLong <- vegetData %>%
    pivot_longer(cols = -Plot, names_to = "Species", values_to = "Abundance") %>%
    filter(Abundance > 0)
  
  if (short) {
    speciesLong <- speciesLong %>%
      mutate(Species = if_else(
        str_detect(Species, "unknown"), 
        str_replace_all(Species, "\\.([a-zA-Z])", " \\1"), 
        str_replace(Species, "^([A-Za-z])[a-zA-Z]*\\.([a-zA-Z]+).*", "\\1. \\2")
      ))
  } else {
    speciesLong <- speciesLong %>%
      mutate(Species = str_replace_all(Species, "\\.([a-zA-Z])", " \\1"))
  }
  
  # Merge with environmental data
  mergedData <- speciesLong %>%
    left_join(environData, by = "Plot") %>%
    select(Species, Plot, all_of(envVar))
  
  # Calculate the min, max, and mean for each species based on the environmental variable
  meanValuePerSpecies <- mergedData %>%
    group_by(Species) %>%
    summarise(min_envVar = min(!!sym(envVar), na.rm = TRUE), 
              max_envVar = max(!!sym(envVar), na.rm = TRUE), 
              mean_envVar = mean(!!sym(envVar), na.rm = TRUE)) %>%
    arrange(mean_envVar)  # Sort by mean
  
  # Merge the environmental data with mean values for ordering the species
  mergedDataSorted <- mergedData %>%
    left_join(meanValuePerSpecies, by = "Species") %>%
    arrange(mean_envVar)
  
  # Create the plot
  plot <- ggplot(mergedDataSorted, aes(factor(Species, levels = unique(Species)), y = !!sym(envVar))) +
    # Individual measurements of each physical plot
    geom_point(color =  '#CCCCCC', show.legend = FALSE, size = fontSize * 0.2) +
    # Add vertical lines from min to max variable range for each species
    geom_segment(data = meanValuePerSpecies, aes(x = Species, xend = Species, y = min_envVar, yend = max_envVar), color = "#00BFC4") +
    # Add mean environmental variable as a point for each species
    geom_point(data = meanValuePerSpecies, aes(x = Species, y = mean_envVar), color = "#F8766D", size = fontSize * 0.2) +
    # Add labels
    labs(y = envVar, x = NULL) + 
    # Theme specification
    theme_minimal() +
    theme(plot.title = element_text(size = fontSize, face = 'plain', hjust = 0.5), 
          axis.line = element_line(linewidth = 0.4, linetype = "solid", colour = "black"), 
          plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
          axis.title.x = element_text(size = fontSize*.6),  # Axis titles
          axis.title.y = element_text(size = fontSize),  # Axis titles
          axis.text.x = element_text(size = fontSize*.9, angle = 50, hjust = 1),
          axis.text.y = element_text(size = fontSize))
  return(plot)
}


########################### Using the function to create several plots

plot1 <- createPlot(vegData, envData, "VerticalWaterDistance")
plot2 <- createPlot(vegData, envData, "SoilMoistureAvrg")
plot3 <- createPlot(vegData, envData, "pH", TRUE)
plot4 <- createPlot(vegData, envData, "SalinityAdjusted", TRUE)
plot5 <- createPlot(vegData, envData, "EC", TRUE)
plot6 <- createPlot(vegData, envData, "BulkDensityIncRoots", TRUE)

p1 <- createPlot(vegData, envData, "GreennessIndex")
p2 <- createPlot(vegData, envData, "Contrast")
p3 <- createPlot(vegData, envData, "PlantBiomass", TRUE)
p4 <- createPlot(vegData, envData, "ShannonIndex", TRUE)


########################### Saving image

# Arrange the plots in a grid (e.g., 2 rows, 3 columns)
final_plot <- (plot1 | plot2) / (plot3 | plot4) / (plot5 | plot6)
ggsave("4.Results/SpeciesRangesHabitat.pdf", 
       plot = final_plot,
       width = 18.4,
       height = 16.2,
       units = "cm")

# Arrange the plots in a grid (e.g., 2 rows, 3 columns)
final_plot <- (p1 | p2) / (p3 | p4)
ggsave("4.Results/SpeciesRangesOtherVars.pdf", 
       plot = final_plot,
       width = 18.4,
       height = 10.8,
       units = "cm")


