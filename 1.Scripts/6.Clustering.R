# Some clustering methods can use the raw vegetation data, instead of the CCA.
# This should probably be better, since the CCA axes explain relatively little of the variation.
# Though maybe if the non-metric MDS works better at explaining the variance, its axis can be used.



# https://youtu.be/7xHsRkOdVwo?si=lrpQew6NPLB13hli
# Hierarchical clustering and dendrograms: start by comparing individual samples with each other,
# and slowly builds bigger and bigger clusters. How these clusters got formed is depicted in a 
# dendrogram. If there is logically no such thing as nested clusters in your type of data, this 
# method doesn't make a lot of sense to use. All methods below are 'flat' clustering methods.

# https://youtu.be/HVXime0nQeI?si=OK8J6_aOJdBTy50m
# K-nearest neighbor classification: not applicable for the data, because it requires a lot of data
# of which categories/clusters are already known.

# https://youtu.be/4b5d3muPQmA?si=2p3mjdckOs3hliAr
# Another option is to do K-means clustering: plot the reduction of variation against the number of 
# clusters to see where the elbow point is, and use that as K. Raw data can be used.

# https://youtu.be/RDZUdRSDOok?si=aRRp0ssyc4LZN6jf
# When the clusters are more complex than 'circular-shaped', a standard method such as K-means might
# have difficulty to cluster everything correctly. 

# Additionally, with data that has more than two/three axes, it's not possible to visually identify 
# whether clusters are nested. There are clustering algorithms that can do this. One of these 
# methods is the DBSCAN.

# DBSCAN has two user defined parameters: radius of "close" circle, and number of close points for a
# core point. Core points extend a cluster with all points in close proximity. Non-core points can 
# only join a cluster, not extend it further.


# https://github.com/zdealveindy/twinspanR/

######################################################










rm(list = ls()) # Cleaning the environment
# ctrl + L will clear console
# in plot window click broom

library(ggplot2)
library(vegan)

########################### Importing data

# Load envData and vegData from CSV files, setting the first column as row names
envData <- as.matrix(read.csv("2.Data/envDataForMVATransformed.csv", row.names = 1))
vegData <- as.matrix(read.csv("2.Data/vegDataForMVA.csv", row.names = 1))

# NOTE: SEE WHICH PLOTS ARE REMOVED AND WHY IN THE SCRIPT WHERE THE DATA IS CLEANED


########################### Function for running the clustering

createClusters <- function(data, clusteringMethod) {
  samples_dist <- dist(data) 
  samples_hc <- hclust(samples_dist, method = clusteringMethod)
  plot(samples_hc)
}

createClustersWithTab <- function(data, clusteringMethod) {
  samples_dist <- dist(data) 
  samples_hc <- hclust(samples_dist, method = clusteringMethod)
  tabasco(data, samples_hc)
}

clusterAndSavePlot <- function(tab, fileNumber, data, clusterinMethod) {
  if (tab) {
    png(paste0("4.Results/Clust.tab.", fileNumber, clusterinMethod, ".png"), width = 1200, height = 1000)
    createClustersWithTab(data, clusterinMethod)
  } else {
    png(paste0("4.Results/Clust.", fileNumber, clusterinMethod, ".png"), width = 1200, height = 1000)
    createClusters(data, clusterinMethod)
  }
  dev.off()
}


########################### Clustering the plots based on the species

# Plotting the clusters themselves
clusterAndSavePlot(FALSE, "Plots.1", vegData, "ward.D")
clusterAndSavePlot(FALSE, "Plots.2", vegData, "ward.D2")
clusterAndSavePlot(FALSE, "Plots.3", vegData, "single")
clusterAndSavePlot(FALSE, "Plots.4", vegData, "complete")
clusterAndSavePlot(FALSE, "Plots.5", vegData, "average")
clusterAndSavePlot(FALSE, "Plots.6", vegData, "mcquitty")
clusterAndSavePlot(FALSE, "Plots.7", vegData, "median")
clusterAndSavePlot(FALSE, "Plots.8", vegData, "centroid")

# plotting the clusters with the tabasco function
clusterAndSavePlot(TRUE, "Plots.1", vegData, "ward.D")
clusterAndSavePlot(TRUE, "Plots.2", vegData, "ward.D2")
clusterAndSavePlot(TRUE, "Plots.3", vegData, "single")
clusterAndSavePlot(TRUE, "Plots.4", vegData, "complete")
clusterAndSavePlot(TRUE, "Plots.5", vegData, "average")
clusterAndSavePlot(TRUE, "Plots.6", vegData, "mcquitty")
clusterAndSavePlot(TRUE, "Plots.7", vegData, "median")
clusterAndSavePlot(TRUE, "Plots.8", vegData, "centroid")


########################### Clustering the species based on the plots

# Plotting the clusters themselves
clusterAndSavePlot(FALSE, "Species.1", t(vegData), "ward.D")
clusterAndSavePlot(FALSE, "Species.2", t(vegData), "ward.D2")
clusterAndSavePlot(FALSE, "Species.3", t(vegData), "single")
clusterAndSavePlot(FALSE, "Species.4", t(vegData), "complete")
clusterAndSavePlot(FALSE, "Species.5", t(vegData), "average")
clusterAndSavePlot(FALSE, "Species.6", t(vegData), "mcquitty")
clusterAndSavePlot(FALSE, "Species.7", t(vegData), "median")
clusterAndSavePlot(FALSE, "Species.8", t(vegData), "centroid")

# plotting the clusters with the tabasco function
clusterAndSavePlot(TRUE, "Species.1", t(vegData), "ward.D")
clusterAndSavePlot(TRUE, "Species.2", t(vegData), "ward.D2")
clusterAndSavePlot(TRUE, "Species.3", t(vegData), "single")
clusterAndSavePlot(TRUE, "Species.4", t(vegData), "complete")
clusterAndSavePlot(TRUE, "Species.5", t(vegData), "average")
clusterAndSavePlot(TRUE, "Species.6", t(vegData), "mcquitty")
clusterAndSavePlot(TRUE, "Species.7", t(vegData), "median")
clusterAndSavePlot(TRUE, "Species.8", t(vegData), "centroid")

