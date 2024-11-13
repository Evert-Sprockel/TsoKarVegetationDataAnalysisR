rm(list = ls()) # Cleaning the environment
# ctrl + L in console will clear everything
# in plot window click broom

library(ggplot2)
library(tidyverse)
library(gridExtra)
# library(dplyr)
# library(hrbrthemes)

########################### Importing data

# Load envData and vegData from CSV files, setting the first column as row names
envData <- read.csv("2.Data/envDataForMVA.csv")
vegData <- read.csv("2.Data/vegDataForMVA.csv")
envData <- envData %>%
  rename("Plot" = "X")
vegData <- vegData %>%
  rename("Plot" = "X")

# NOTE: SEE WHICH PLOTS ARE REMOVED AND WHY IN THE SCRIPT WHERE THE DATA IS CLEANED


########################### Function for creating the plots

createPlot <- function(vegData, envData, envVar) {
  # Reshape species data
  speciesLong <- vegData %>%
    pivot_longer(cols = -Plot, names_to = "Species", values_to = "Abundance") %>%
    filter(Abundance > 0)
  
  # Merge with environmental data
  mergedData <- speciesLong %>%
    left_join(envData, by = "Plot") %>%
    select(Species, Plot, envVar)
  
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
    geom_point(aes(color = Species), show.legend = FALSE) +  # Plot environmental variable for each species
    scale_colour_manual(values = c('species 1' = "#252525", 'species 2' = "#1B9E77")) +  # Color by species
    labs(y = envVar, x = 'Species') + 
    theme_ipsum_pub(base_size = 8.5, axis_title_size = 9) +  # Use theme
    theme(plot.title = element_text(size = 11, face = 'plain', hjust = 0.5), 
          axis.line = element_line(linewidth = 0.4, linetype = "solid", colour = "black"), 
          plot.margin = margin(t = 5, r = 5, b = 5, l = 5),
          axis.text.x = element_text(angle = 90, hjust = 1)) +
    # Add vertical lines from min to max environmental variable for each species
    geom_segment(data = meanValuePerSpecies, aes(x = Species, xend = Species, y = min_envVar, yend = max_envVar), color = "#1B9E77") +
    # Add mean environmental variable as a point for each species
    geom_point(data = meanValuePerSpecies, aes(x = Species, y = mean_envVar), color = "#FFD561", size = 3)  # Yellow for the mean
  
  return(plot)
}


########################### Using the function to create several plots

plot1 <- createPlot(vegData, envData, "VerticalWaterDistance")
plot2 <- createPlot(vegData, envData, "SoilMoistureAvrg")
plot3 <- createPlot(vegData, envData, "pH")
plot4 <- createPlot(vegData, envData, "SalinityAdjusted")
plot5 <- createPlot(vegData, envData, "BulkDensityIncRoots")


# Arrange the plots in a grid (e.g., 2 rows, 3 columns)
combined_plot <- grid.arrange(plot1, plot2, plot3, plot4, plot5, ncol = 3)
ggsave("4.Results/SpeciesRanges.png", plot = combined_plot, dpi = 200, width = 12, height = 8, units = "in")
