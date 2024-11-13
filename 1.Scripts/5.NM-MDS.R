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










rm(list = ls()) # Cleaning the environment
# ctrl + L in console will clear everything
# in plot window click broom

library(ggplot2)
library(vegan)

########################### Importing data

# Load envData and vegData from CSV files, setting the first column as row names
envData <- as.matrix(read.csv("2.Data/envDataForMVATransformed.csv", row.names = 1))
vegData <- as.matrix(read.csv("2.Data/vegDataForMVA.csv", row.names = 1))

# NOTE: SEE WHICH PLOTS ARE REMOVED AND WHY IN THE SCRIPT WHERE THE DATA IS CLEANED


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


########################### Plots

png(filename = "4.Results/NMMDS.OrdPlot.png", width = 1200, height = 1000)
plot(ord_nmds, type="n", cex.axis = 1, cex.lab = 1)
orditorp(ord_nmds, display="species", col="blue", cex = 1)
orditorp(ord_nmds, display = "sites", col = "black", pch="+", cex = 1)
dev.off()

# Plotting the species and adding the environmental gradients on top, using the data from the 
# envfit() function.
png(filename = "4.Results/NMMDS.OrdPlotEnvironmentalVariables.png", width = 1200, height = 1000)
plot(ord_nmds, dis="species", cex.axis = 1, cex.lab = 1, cex = 2)
plot(ord.fit)
dev.off()

# you can also plot an environmental factor as a surface onto an existing ordination
png(filename = "4.Results/NMMDS.OrdPlotpH.png", width = 1200, height = 1000)
plot(ord_nmds, type="n", cex.axis = 1, cex.lab = 1)
orditorp(ord_nmds, display="species", col="blue", cex = 1)
orditorp(ord_nmds, display = "sites", col = "black", pch="+", cex = 1)
ordisurf(ord_nmds, as.data.frame(envData)$pHLog, add=TRUE)
plot(ord.fit, col = "darkgreen")
dev.off()

png(filename = "4.Results/NMMDS.OrdPlotSalinity.png", width = 1200, height = 1000)
plot(ord_nmds, type="n", cex.axis = 1, cex.lab = 1)
orditorp(ord_nmds, display="species", col="blue", cex = 1)
orditorp(ord_nmds, display = "sites", col = "black", pch="+", cex = 1)
ordisurf(ord_nmds, as.data.frame(envData)$SalinityAdjustedLog, add=TRUE)
plot(ord.fit, col = "darkgreen")
dev.off()

png(filename = "4.Results/NMMDS.OrdPlotSoilMoisture.png", width = 1200, height = 1000)
plot(ord_nmds, type="n", cex.axis = 1, cex.lab = 1)
orditorp(ord_nmds, display="species", col="blue", cex = 1)
orditorp(ord_nmds, display = "sites", col = "black", pch="+", cex = 1)
ordisurf(ord_nmds, as.data.frame(envData)$SoilMoistureAvrg, add=TRUE)
plot(ord.fit, col = "darkgreen")
dev.off()

png(filename = "4.Results/NMMDS.OrdPlotVerticalWaterDist.png", width = 1200, height = 1000)
plot(ord_nmds, type="n", cex.axis = 1, cex.lab = 1)
orditorp(ord_nmds, display="species", col="blue", cex = 1)
orditorp(ord_nmds, display = "sites", col = "black", pch="+", cex = 1)
ordisurf(ord_nmds, as.data.frame(envData)$VerticalWaterDistanceLog, add=TRUE)
plot(ord.fit, col = "darkgreen")
dev.off()

png(filename = "4.Results/NMMDS.OrdPlotBulkDensity.png", width = 1200, height = 1000)
plot(ord_nmds, type="n", cex.axis = 1, cex.lab = 1)
orditorp(ord_nmds, display="species", col="blue", cex = 1)
orditorp(ord_nmds, display = "sites", col = "black", pch="+", cex = 1)
ordisurf(ord_nmds, as.data.frame(envData)$BulkDensityIncRootsLog, add=TRUE)
plot(ord.fit, col = "darkgreen")
dev.off()






