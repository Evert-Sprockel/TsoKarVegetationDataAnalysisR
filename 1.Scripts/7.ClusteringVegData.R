### PURPOSE OF THIS SCRIPT:
# Creating clusters of the plots and species based on the vegetation data

### OUTPUT OF THIS SCRIPT:
# cluster dendograms images
# Two data sets: plots with cluster numbers, and species with cluster numbers


rm(list = ls()) # Cleaning the environment
# ctrl + L will clear console
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # Cleaning plot window (or click broom)

library(ggplot2)
library(vegan)
library(cluster)


########################### Importing data

# NOTE: SEE WHICH PLOTS ARE REMOVED AND WHY IN THE SCRIPT WHERE THE DATA IS CLEANED
# Load envData and vegData from CSV files, setting the first column as row names
vegData <- read.csv("3.TemporaryFiles/vegData.csv", row.names = 1)


########################### Function for running the clustering

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
  silhouette_widths <- silhouette(clusters, dist(data))
  avg_silhouette <- mean(silhouette_widths[, 3])
  print(paste0(numClust, ", ", "avg_silhouette: ", avg_silhouette))
  cluster_data <- data.frame(SampleID = rownames(data), Cluster = clusters)
  write.csv(cluster_data, paste0("3.TemporaryFiles/Clusters.", fileName, clusterinMethod, ".csv"), row.names = FALSE)
}


########################### Clustering the plots based on the species

# Other clustering options: ward.D2, single, complete, average, mcquitty, median, centroid
clusters <- clusterAndSavePlot(FALSE, "Plots.1", vegData, "ward.D")
# plotting the clusters with the tabasco function
clusters <- clusters <- clusterAndSavePlot(TRUE, "Plots.1", vegData, "ward.D")
# Saving the clusters as a csv
saveClusters(5, clusters, "Plots.1", vegData, "ward.D")

# Mean silhouette scores (higher is better):
# 3 clusters: 0.31115
# 4 clusters: 0.28402
# 5 clusters: 0.30505
# 6 clusters: 0.31688
# 7 clusters: 0.29066


########################### Clustering the species based on the plots

clusters <- clusterAndSavePlot(FALSE, "Species.1", t(vegData), "ward.D")
clusters <- clusterAndSavePlot(TRUE, "Species.1", t(vegData), "ward.D")
saveClusters(5, clusters, "Species.1", t(vegData), "ward.D")

