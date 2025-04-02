### OUTPUT OF THIS SCRIPT:
# One data set (.csv): the list of plant species with true or false for presence per cluster


rm(list = ls()) # Cleaning the environment
# ctrl + L in console will clear everything
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # Cleaning plot window (or click broom)

library(dplyr)
library(tidyr)
library(tibble)

########################### Importing data

# NOTE: SEE WHICH PLOTS ARE REMOVED AND WHY IN THE SCRIPT WHERE THE DATA IS CLEANED
# Load envData and vegData from CSV files, setting the first column as row names
vegData <- read.csv("3.TemporaryFiles/vegData.csv", row.names = 1)

# Loading the cluster data from the script "Clustering.R"
plotClusters <- read.csv("3.TemporaryFiles/Clusters.Plots.1ward.D.csv", row.names = 1)


########################### Transforming data

# Ensure the plot codes are the row names in vegData
vegData <- vegData %>%
  rownames_to_column(var = "PlotCode")

# Ensure plotClusters has PlotCode as a column (if not already)
plotClusters <- plotClusters %>%
  rownames_to_column(var = "PlotCode")

# Join the two datasets by PlotCode
merged_data <- vegData %>%
  inner_join(plotClusters, by = "PlotCode")

# Remove PlotCode column after merging
merged_data <- merged_data %>%
  select(-PlotCode)

# Convert coverage values to presence/absence (TRUE/FALSE)
presence_absence <- merged_data %>%
  mutate(across(-Cluster, ~ . > 0))

# Reshape data: species as rows, clusters as columns
result <- presence_absence %>%
  pivot_longer(-Cluster, names_to = "Species", values_to = "Present") %>%
  group_by(Species, Cluster) %>%
  summarise(Present = any(Present), .groups = "drop") %>%
  pivot_wider(names_from = Cluster, values_from = Present, values_fill = FALSE)


################# Output

write.csv(result, "4.Results/SpeciesListPresencePerCluster.csv", row.names = TRUE)
