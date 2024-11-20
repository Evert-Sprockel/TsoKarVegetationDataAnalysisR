### OUTPUT OF THIS SCRIPT:
# One data set (.csv): envData with the new columns Shannon index and species richness


rm(list = ls()) # Cleaning the environment
# ctrl + L in console will clear everything
# in plot window click broom

library(ggplot2)
library(vegan)

########################### Importing data

# NOTE: SEE WHICH PLOTS ARE REMOVED AND WHY IN THE SCRIPT WHERE THE DATA IS CLEANED
# Load envData and vegData from CSV files, setting the first column as row names
vegData <- read.csv("2.Data/vegData.csv", row.names = 1)
envData <- read.csv("2.Data/envData.csv", row.names = 1)


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


########################### Merging data frame with envData en saving the file

envData <- cbind(envData, diversityData[, -1])
write.csv(envData, "2.Data/envDataWithShannon.csv", row.names = TRUE)
