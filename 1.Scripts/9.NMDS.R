### PURPOSE OF THIS SCRIPT:
# Visualize the vegetation data using an ordination (NMDS)

### OUTPUT OF THIS SCRIPT:
# Ordination plot images


rm(list = ls()) # Cleaning the environment
# ctrl + L in console will clear everything
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # Cleaning plot window (or click broom)

library(ggplot2)
library(vegan)
library(dplyr)
library(stringr)


########################### Importing data

# NOTE: SEE WHICH PLOTS ARE REMOVED AND WHY IN THE SCRIPT WHERE THE DATA IS CLEANED
# Load envData and vegData from CSV files, setting the first column as row names
envData <- read.csv("3.TemporaryFiles/envDataWithShannonTransformed.csv", row.names = 1)
envData <- envData[, c("VerticalWaterDistanceLog",
                       "SoilMoistureAvrg",
                       "pHLog",
                       "SalinityAdjustedLog",
                       "BulkDensityIncRootsLog")]
vegData <- read.csv("3.TemporaryFiles/vegData.csv", row.names = 1)

# Loading the cluster data from the script "Clustering.R"
plotclusters <- read.csv("3.TemporaryFiles/Clusters.Plots.1ward.D.csv")
speciesClusters <- read.csv("3.TemporaryFiles/Clusters.Species.1ward.D.csv")
plot_counts_per_cluster <- table(plotclusters$Cluster)
plot_counts_per_cluster


########################### Plotting function

# This function takes the result from the nmds and fit, and creates and saves an ordination plot
# Takes a nmds object, a fit object, a file name, a vector of clusters (optional), and a bool
# Returns nothing
plotNMMDS <- function(nmds, file_name, fit = NULL, clusters = NULL, ellipses = FALSE) {
  # set font size in points
  fontSize = 6
  # Extract NMDS coordinates for sites and species
  site_scores <- as.data.frame(scores(nmds, display = "sites"))
  species_scores <- as.data.frame(scores(nmds, display = "species"))
  
  # Add site and species labels
  site_scores$label <- rownames(site_scores)
  species_scores$label <- rownames(species_scores)
  
  species_scores <- species_scores %>%
    mutate(label = str_replace_all(label, "\\.([a-zA-Z])", " \\1"))
  
  # Add clusters if provided
  if (!is.null(clusters)) {
    if (length(clusters) != nrow(site_scores)) {
      stop("The length of 'clusters' must match the number of sites in the NMDS object.")
    }
    site_scores$cluster <- as.factor(clusters)
  }
  
  # Base ggplot
  p <- ggplot() 
  
  if (!is.null(clusters)) {
    # Add sites with different shapes for clusters or default to "+"
    p <- p + geom_point(data = site_scores, aes(x = NMDS1, y = NMDS2, shape = cluster, color = cluster), 
               size = 0.3528 * fontSize) + 
               scale_shape_discrete(name = "Cluster") +   # Use the same name for both scales
               scale_color_discrete(name = "Cluster")
    if (ellipses) {
      # Add ellipses around each cluster
      p <- p + stat_ellipse(data = site_scores, aes(x = NMDS1, y = NMDS2, color = cluster),
                            level = 0.95, linewidth = 0.8, linetype = "dashed")
    }
  } else {
    # Add sites with different shapes for clusters or default to "+"
    p <- p + geom_point(data = site_scores, aes(x = NMDS1, y = NMDS2), 
                        color = "darkgreen", shape = "+", size = 0.3528 * fontSize) +
             geom_text(data = site_scores, aes(x = NMDS1, y = NMDS2, label = label), 
                       color = "darkgreen", hjust = -0.3, vjust = -0.3, size = 0.3528 * fontSize)
  }
  
  # Add species
  p <- p + geom_text(data = species_scores, aes(x = NMDS1, y = NMDS2, label = label), 
                     color = "black", size = 0.3 * fontSize)
  
  # Add fitted vectors if provided
  if (!is.null(fit)) {
    fit_scores <- as.data.frame(fit$vectors$arrows)
    fit_scores$label <- rownames(fit_scores)
    # Scale vectors for better visualization
    p <- p +
      geom_segment(data = fit_scores, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
                   arrow = arrow(length = unit(0.3528 * fontSize, "mm")), color = "darkblue") +
      geom_text(data = fit_scores, aes(x = NMDS1, y = NMDS2, label = label), 
                color = "darkblue", size = 0.3 * fontSize, hjust = 0, vjust = 0.3)
  }
  
  # General plot appearance
  p <- p + labs(x = "NMDS1", y = "NMDS2", shape = "Cluster") +
    theme_minimal() +
    theme(
      axis.text = element_text(size = fontSize),
      axis.title = element_text(size = fontSize),
      legend.position = "right",
      legend.text = element_text(size = fontSize),
      legend.title = element_text(size = fontSize)
    )
  
  # Save the plot
  ggsave(filename = paste0("4.Results/NMDS.", file_name, ".pdf"), 
         plot = p, width = 18.4, height = 14, dpi = 300, bg = "white", units = "cm")  # Set PNG background to white
}


########################### The analysis itself

# Set the seed for reproducibility
set.seed(123) 

ord_nmds <- metaMDS(vegData)
ord_nmds$stress  # Directly prints stress value: 0.1611827
# < 0.05 is excellent
# < 0.1 is great
# < 0.2 is good/ok
# < 0.3 is poor
species_scores <- scores(ord_nmds, display = "species")
# print(species_scores)

# in vegan you can use formulas similar to regression to indicate which environmental data you want 
# to fit. A dot means all columns of the dataframe.
ord.fit <- envfit(ord_nmds ~ ., data = as.data.frame(envData), perm = 999) 
# perm = 999 means that the envfit() function will run 999 permutations (=fits with different 
# subsets of the data) to determine the significance of associations between species and environ-
# mental data
ord.fit 


########################### Plots

plotNMMDS(ord_nmds, "OrdPlotWithoutGradients")
plotNMMDS(ord_nmds, "OrdPlot", ord.fit)
plotNMMDS(ord_nmds, "OrdPlotClusters", ord.fit, plotclusters$Cluster, FALSE)
plotNMMDS(ord_nmds, "OrdPlotClustersE", ord.fit, plotclusters$Cluster, TRUE)


