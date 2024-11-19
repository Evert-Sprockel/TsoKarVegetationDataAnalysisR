rm(list = ls()) # Cleaning the environment
# ctrl + L in console will clear everything
# in plot window click broom

library(ggplot2)
library(vegan)

########################### Importing data

# NOTE: SEE WHICH PLOTS ARE REMOVED AND WHY IN THE SCRIPT WHERE THE DATA IS CLEANED
# Load envData and vegData from CSV files, setting the first column as row names
vegData <- as.matrix(read.csv("2.Data/vegDataForMVA.csv", row.names = 1))


########################### Calculate species richness and Shannon diversity index and add to DF

speciesRichness <- specnumber(vegData)
shannonIndices <- diversity(vegData, index = "shannon")

# Combine the results into a data frame
diversityData <- data.frame(
  Sample = rownames(vegData),  # Use the row names as sample names
  ShannonIndex = shannonIndices,
  SpeciesRichness = speciesRichness
)

# Reset row names back to 1, 2, 3, etc.
rownames(diversityData) <- NULL

# Print the resulting data frame
print(diversityData)

########################### Saving the data

# Save the data frame to a CSV file
write.csv(diversityData, "2.Data/RichnessAndShannonIndex.csv", row.names = FALSE)
