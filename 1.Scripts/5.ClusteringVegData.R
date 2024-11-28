# Some clustering methods can use the raw vegetation data, instead of the CCA.
# This should probably be better, since the CCA axes explain relatively little of the variation.
# Though maybe if the non-metric MDS works better at explaining the variance, its axis can be used.



# https://youtu.be/7xHsRkOdVwo?si=lrpQew6NPLB13hli
# Hierarchical clustering and dendrograms: start by comparing individual samples with each other,
# and slowly builds bigger and bigger clusters. How these clusters got formed is depicted in a 
# dendrogram. All methods below are 'flat' clustering methods.

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







### OUTPUT OF THIS SCRIPT:
# cluster dendograms images
# Two data sets: plots with cluster numbers, and species with cluster numbers


rm(list = ls()) # Cleaning the environment
# ctrl + L will clear console
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # Cleaning plot window (or click broom)

library(ggplot2)
library(vegan)
# library(ggdendro)
# library(dendextend)


########################### Importing data

# NOTE: SEE WHICH PLOTS ARE REMOVED AND WHY IN THE SCRIPT WHERE THE DATA IS CLEANED
# Load envData and vegData from CSV files, setting the first column as row names
vegData <- read.csv("3.TemporaryFiles/vegData.csv", row.names = 1)


########################### Function for running the clustering

# # [Not working correctly]
# createClustersGGplot <- function(data, clusteringMethod) {
#   dend <- as.dendrogram(hclust(dist(data), method = clusteringMethod))
#   ggd1 <- as.ggdend(dend)
#   p2 <- ggplot(ggd1, horiz = TRUE, theme = NULL)  +
#     theme(axis.text.x = element_text(size = 1))  # Adjust size as needed
# 
#   # p <- ggplot(segment(data)) +
#   #   geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
#   #   geom_text(data = label(data),
#   #             aes(x = x, y = y, label = label, hjust = 0),
#   #             size = 2) +
#   #   coord_flip() +
#   #   scale_y_reverse(expand = c(0.2, 0))
#   ggsave(filename = paste0("4.Results/Clust.Plots.",
#                            clusteringMethod,
#                            ".png"),
#          plot=p2, width=10, height=20, dpi=300)
# }

## Takes species abundance data and a clustering method and performs normal clustering
## Returns a hierarchical clustering object
createClusters <- function(data, clusteringMethod) {
  samples_dist <- dist(data) 
  samples_hc <- hclust(samples_dist, method = clusteringMethod)
  plot(samples_hc)
  return(samples_hc)
}

## Takes species abundance data and a clustering method and performs clustering with tabasco()
## Returns a hierarchical clustering object
createClustersWithTab <- function(data, clusteringMethod) {
  samples_dist <- dist(data) 
  samples_hc <- hclust(samples_dist, method = clusteringMethod)
  tabasco(data, samples_hc)
  return(samples_hc)
}

## Takes data, calls one of the clustering methods and saves the image
## Returns a hierarchical clustering object
clusterAndSavePlot <- function(tab, fileName, data, clusterinMethod) {
  if (tab) {
    png(paste0("4.Results/Clust.tab.", fileName, clusterinMethod, ".png"), width = 1200, height = 1000)
    clusters <- createClustersWithTab(data, clusterinMethod)
  } else {
    png(paste0("4.Results/Clust.", fileName, clusterinMethod, ".png"), width = 1200, height = 1000)
    clusters <- createClusters(data, clusterinMethod)
  }
  dev.off()
  return(clusters)
}

## Takes a clustering object and writes clusters to a csv file
## Returns none
saveClusters <- function(numClust, clusters, fileName, data, clusterinMethod) {
  clusters <- cutree(clusters, k = numClust)
  cluster_data <- data.frame(SampleID = rownames(data), Cluster = clusters)
  write.csv(cluster_data, paste0("3.TemporaryFiles/Clusters.", fileName, clusterinMethod, ".csv"), row.names = FALSE)
}


########################### Clustering the plots based on the species

# createClustersGGplot(vegData, "ward.D")

# Other clustering options: ward.D2, single, complete, average, mcquitty, median, centroid
clusters <- clusterAndSavePlot(FALSE, "Plots.1", vegData, "ward.D")
# plotting the clusters with the tabasco function
clusters <- clusters <- clusterAndSavePlot(TRUE, "Plots.1", vegData, "ward.D")
# Saving the clusters as a csv
saveClusters(5, clusters, "Plots.1", vegData, "ward.D")

########################### Clustering the species based on the plots

clusters <- clusterAndSavePlot(FALSE, "Species.1", t(vegData), "ward.D")
clusters <- clusterAndSavePlot(TRUE, "Species.1", t(vegData), "ward.D")
saveClusters(5, clusters, "Species.1", t(vegData), "ward.D")

