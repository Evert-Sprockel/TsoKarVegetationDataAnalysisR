# Polar ordination, DCA, CCA, PCA, RDA and MDS all fit together in the group of linear/gradient-
# based models. t-SNE and UMAP are non-linear/distance-based models.


# https://youtu.be/GEn-_dAyYME?si=4_CIHXwCMFN8L-bd
# Multidimensional scaling (MDS)

# A PCA converts correlations (or lack thereof) among samples into a 2d-plot. Highly correlated
# samples form clusters. MDS is similar, but instead of correlations, it converts the distances
# among the sampes into a 2d-plot.

# There are two versions of multi-dimensional scaling: metric/classical or non-metric. The former is
# also called the principal coordinate analysis (PCoA). It uses the actual distances instead of just
# the "rank order" that the non-metric version uses. The non-metric MDS is therefore uses ordinal
# data instead of scale.

# A metric MDS/PCoA that uses the Euclidian distance produces the exact same graph as a PCA. But 
# there are more ways to measure distances, such as the average of the absolute values of the log
# fold change, the manhatten distance, hamming distance, great circle distance, etc. Selecting the
# "best" distance is part of the art of data science.

# Once again, coming back to the comparison with a PCA: the only difference with the MDS is what's
# initially calculated: the correlations between samples, or the distances. The math behind it, and
# the types of results (coordinates for a graph, percent of variation each axis accounts for, and 
# the loading scores) are exactly the same.

# Non-metric multidimensional scaling is quite appropriate for data with many zeroes.


# https://youtu.be/eN0wFzBA4Sc?si=VtLB7BGhtdYUgh2B
# The problem with a PCA or similar analyses is that it only works well if the first 2 axes account
# most of the variation in the data. I.e., with a complicated dataset, a PCA may not work very well. 
# A UMAP (uniform manifold approximation and projection) is one alternative.

# https://youtu.be/NEaUSP4YerM?si=aisltXBbikrqtjRo
# t-SNE is very similar to a UMAP, apart from two big differences: t-SNE always starts with a random
# initialization of the low-dimensional graph; every time it is ran on the same data, it starts out
# with a different graph. This is not the case for UMAP, which does exactly the same every time. The
# second difference, is that UMAP scales better with big datasets.

######################################################







### OUTPUT OF THIS SCRIPT:
# Ordination plot images
# (No data sets)


rm(list = ls()) # Cleaning the environment
# ctrl + L in console will clear everything
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # Cleaning plot window (or click broom)

library(ggplot2)
library(vegan)

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


########################### Plotting function

# This function takes the result from the nmds and fit, and creates and saves an ordination plot
# Takes a nmds object, a fit object, a file name, a vector of clusters (optional), and a bool
# Returns nothing
plotNMMDS <- function(nmds, file_name, fit = NULL, clusters = NULL, ellipses = FALSE) {
  # Extract NMDS coordinates for sites and species
  site_scores <- as.data.frame(scores(nmds, display = "sites"))
  species_scores <- as.data.frame(scores(nmds, display = "species"))
  
  # Add site and species labels
  site_scores$label <- rownames(site_scores)
  species_scores$label <- rownames(species_scores)
  
  # Add clusters if provided
  if (!is.null(clusters)) {
    if (length(clusters) != nrow(site_scores)) {
      stop("The length of 'clusters' must match the number of sites in the NMDS object.")
    }
    site_scores$cluster <- as.factor(clusters)
  }
  
  # Base ggplot
  p <- ggplot() +
    # Add species as blue text
    geom_text(data = species_scores, aes(x = NMDS1, y = NMDS2, label = label), 
              color = "black", size = 4)
  
  if (!is.null(clusters)) {
    # Add sites with different shapes for clusters or default to "+"
    p <- p + geom_point(data = site_scores, aes(x = NMDS1, y = NMDS2, shape = cluster, color = cluster), 
               size = 3)
    if (ellipses) {
      # Add ellipses around each cluster
      p <- p + stat_ellipse(data = site_scores, aes(x = NMDS1, y = NMDS2, color = cluster),
                            level = 0.95, size = 1, linetype = "dashed")
    }
  } else {
    # Add sites with different shapes for clusters or default to "+"
    p <- p + geom_point(data = site_scores, aes(x = NMDS1, y = NMDS2), 
                        color = "darkgreen", shape = "+", size = 3) +
             geom_text(data = site_scores, aes(x = NMDS1, y = NMDS2, label = label), 
                       color = "darkgreen", hjust = -0.3, vjust = -0.3)
  }
    
  # General plot appearance
  p <- p + labs(x = "NMDS1", y = "NMDS2", shape = "Cluster") +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 11),
      legend.position = "right",
    ) 
  
  # Add fitted vectors if provided
  if (!is.null(fit)) {
    fit_scores <- as.data.frame(fit$vectors$arrows)
    fit_scores$label <- rownames(fit_scores)
    # Scale vectors for better visualization
    p <- p +
      geom_segment(data = fit_scores, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
                   arrow = arrow(length = unit(0.2, "cm")), color = "darkblue") +
      geom_text(data = fit_scores, aes(x = NMDS1, y = NMDS2, label = label), 
                color = "darkblue", size = 4, hjust = -0.3, vjust = -0.3)
  }
  
  # Save the plot
  ggsave(filename = paste0("4.Results/NMMDS.", file_name, ".png"), 
         plot = p, width = 15, height = 12, dpi = 300, bg = "white")  # Set PNG background to white
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
print(species_scores)

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







