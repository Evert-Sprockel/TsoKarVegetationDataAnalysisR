### OUTPUT OF THIS SCRIPT:
# A .txt file with basic descriptive statistics for both data sets


rm(list = ls()) # Cleaning the environment
# ctrl + L in console will clear everything
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # Cleaning plot window (or click broom)


########################### Importing data

vegData <- read.csv("3.TemporaryFiles/vegData.csv", row.names = 1)
vegData <- as.data.frame(lapply(vegData, function(x) as.numeric(as.character(x))))
row.names(vegData) <- row.names(read.csv("3.TemporaryFiles/vegData.csv", row.names = 1))
envData <- read.csv("3.TemporaryFiles/envDataWithShannonTransformed.csv", row.names = 1)
envData$ThufurHollowNA <- as.factor(envData$ThufurHollowNA)
envData$AnimalActivity <- as.factor(envData$AnimalActivity)


########################### Basic descriptive statistics

output_file <- file("4.Results/BasicDescriptiveStatisticsForBothDataSheets.txt", open = "wt")
# Use sink() to redirect output to the file
sink(output_file)

cat("BASIC DESCRIPTIVE STATISTICS OF THE TWO DATA SETS")

cat("\n\nEnvironmental data summary:\n")
print(summary(envData))
cat("\n\nEnvironmental data structure:\n")
str(envData)

cat("\n\nVegetation data summary:\n")
print(summary(vegData))
cat("\n\nVegetation data structure:\n")
str(vegData)

column_nonzero_counts <- colSums(vegData != 0)
column_nonzero_means <- colMeans(vegData != 0)

cat("\n\nSorted column non-zero counts (number of plots where species were present):\n")
print(sort(column_nonzero_counts, decreasing = TRUE))
cat("\n\nSorted column non-zero means (mean abundance per species only for plots where they occurred):\n")
print(sort(column_nonzero_means, decreasing = TRUE))

row_nonzero_counts <- rowSums(vegData != 0)

cat("\n\nSorted row non-zero counts (number of species per plot):\n")
print(sort(row_nonzero_counts, decreasing = TRUE))

# Close the file connection
sink()
close(output_file)
