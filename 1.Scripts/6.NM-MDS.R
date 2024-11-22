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
# in plot window click broom

library(ggplot2)
library(vegan)

########################### Importing data

# NOTE: SEE WHICH PLOTS ARE REMOVED AND WHY IN THE SCRIPT WHERE THE DATA IS CLEANED
# Load envData and vegData from CSV files, setting the first column as row names
envData <- as.matrix(read.csv("3.TemporaryFiles/envDataWithShannonTransformed.csv", row.names = 1))
envData <- envData[, c("VerticalWaterDistanceLog", "SoilMoistureAvrg", "pHLog", "SalinityAdjustedLog", "BulkDensityIncRootsLog")]
vegData <- as.matrix(read.csv("3.TemporaryFiles/vegData.csv", row.names = 1))

# Loading the cluster data from the script "Clustering.R"
plotclusters <- read.csv("3.TemporaryFiles/Clusters.Plots.1ward.D.csv")
speciesClusters <- read.csv("3.TemporaryFiles/Clusters.Species.1ward.D.csv")


########################### Plotting function

plotNMMDS <- function(nmds, file_name, fit) {
  png(filename = paste0("4.Results/NMMDS.", file_name, ".png"), width = 1200, height = 1000)
  plot(nmds, type = "n", cex.axis = 1, cex.lab = 1)
  orditorp(nmds, display = "species", col = "blue", cex = 1)
  orditorp(nmds, display = "sites", col = "black", pch = "+", cex = 1)
  plot(fit, col = "darkgreen")
  dev.off()
}

plotNMMDSWithLines <- function(nmds, file_name, environData, env_variable, fit) {
  png(filename = paste0("4.Results/NMMDS.", file_name, ".png"), width = 1200, height = 1000)
  plot(nmds, type = "n", cex.axis = 1, cex.lab = 1)
  orditorp(nmds, display = "species", col = "blue", cex = 1)
  orditorp(nmds, display = "sites", col = "black", pch = "+", cex = 1)
  ordisurf(nmds, as.data.frame(environData)[[env_variable]], add = TRUE)
  plot(fit, col = "darkgreen")
  dev.off()
}

plotNMMDSWithSiteClusters <- function(nmds, file_name, fit, cColors) {
  png(filename = paste0("4.Results/NMMDS.", file_name, ".png"), width = 1200, height = 1000)
  plot(nmds, type = "n", cex.axis = 1, cex.lab = 1)
  # orditorp(nmds, display = "species", col = cColors, cex = 1)
  orditorp(nmds, display = "sites", col = cColors, pch = "+", cex = 1)
  plot(fit, col = "#313131")
  dev.off()
}

plotNMMDSWithSpeciesClusters <- function(nmds, file_name, fit, cColors) {
  png(filename = paste0("4.Results/NMMDS.", file_name, ".png"), width = 1200, height = 1000)
  plot(nmds, type = "n", cex.axis = 1, cex.lab = 1)
  orditorp(nmds, display = "species", col = cColors, cex = 1)
  # orditorp(nmds, display = "sites", col = cColors, pch = "+", cex = 1)
  plot(fit, col = "#313131")
  dev.off()
}


########################### The analysis itself

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
# subsets of the data) to determine the significance of associations between species and environmental data
ord.fit 


########################### Assigning colors from cluster data

Colors <- c("#34e0d9", "#34e067", "#e2be0e", "#6e23b7", "#b72323")

# Define specific colors for each cluster
plotColors <- as.factor(plotclusters$Cluster)
# Assign colors to the clusters based on their factor levels
plotColors <- Colors[plotColors]

# Define specific colors for each cluster
speciesColors <- as.factor(speciesClusters$Cluster)
# Assign colors to the clusters based on their factor levels
speciesColors <- Colors[speciesColors]


########################### Plots

# Regular NM-MDS plot
plotNMMDS(ord_nmds, "OrdPlot", ord.fit)

# Plot with colored clusters
plotNMMDSWithSiteClusters(ord_nmds, "OrdPlotColoredSiteClusters", ord.fit, plotColors)
plotNMMDSWithSpeciesClusters(ord_nmds, "OrdPlotColoredSpeciesClusters", ord.fit, speciesColors)

# Plotting NM-MDS with red lines for the variables
plotNMMDSWithLines(ord_nmds, "OrdPlotpH", envData, "pHLog", ord.fit)
plotNMMDSWithLines(ord_nmds, "OrdPlotSalinity", envData, "SalinityAdjustedLog", ord.fit)
plotNMMDSWithLines(ord_nmds, "OrdPlotSoilMoisture", envData, "SoilMoistureAvrg", ord.fit)
plotNMMDSWithLines(ord_nmds, "OrdPlotVerticalWaterDist", envData, "VerticalWaterDistanceLog", ord.fit)
plotNMMDSWithLines(ord_nmds, "OrdPlotBulkDensity", envData, "BulkDensityIncRootsLog", ord.fit)







