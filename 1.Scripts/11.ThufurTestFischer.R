### PURPOSE OF THIS SCRIPT:
# Testing the proportion of thufur/hollow per site and cluster

### OUTPUT OF THIS SCRIPT:
# Nothing
# (Test results printed to terminal)


rm(list = ls()) # Cleaning the environment
# ctrl + L in console will clear everything
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # Cleaning plot window (or click broom)

library(dplyr)
library(rstatix)


########################### Importing data

# Load envData and vegData from CSV files, setting the first column as row names
envData <- read.csv("3.TemporaryFiles/envDataWithShannonTransformed.csv", row.names = 1)
# Add site numbers as a column
envData$Site <- as.factor(sub("s(\\d+).*", "\\1", rownames(envData)))
# Loading the cluster data from the script "Clustering.R"
envData$Cluster <- as.factor(read.csv("3.TemporaryFiles/Clusters.Plots.1ward.D.csv")$Cluster)
# envDataVars <- envData[, c("VerticalWaterDistanceLog",
#                            "SoilMoistureAvrg",
#                            "pHLog",
#                            "SalinityAdjustedLog",
#                            "ECLog",
#                            "BulkDensityIncRootsLog",
#                            "PlantBiomassLog",
#                            "ShannonIndex",
#                            "SpeciesRichness")]

envDataFiltered <- envData %>% filter(!is.na(ThufurHollowNA))

# Alternative: Use table() function for a quick cross-tabulation
site_table <- table(envDataFiltered$Site, envDataFiltered$ThufurHollowNA)
cluster_table <- table(envDataFiltered$Cluster, envDataFiltered$ThufurHollowNA)
print(site_table)
print(cluster_table)

fisher.test(site_table)
fisher.test(cluster_table)

# Perform pairwise Fisher’s Exact Test
cluster_matrix <- as.matrix(cluster_table)
pairwise_fisher_results <- pairwise_fisher_test(cluster_matrix, p.adjust.method = "fdr")
print(pairwise_fisher_results)



