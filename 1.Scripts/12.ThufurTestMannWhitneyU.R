### PURPOSE OF THIS SCRIPT:
# Testing difference in means between the plots on thufurs/hollows

### OUTPUT OF THIS SCRIPT:
# Nothing
# (Test results printed to terminal)


rm(list = ls()) # Cleaning the environment
# ctrl + L in console will clear everything
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # Cleaning plot window (or click broom)

library(dplyr)
library(ggplot2)


########################### Importing data

# Load envData and vegData from CSV files, setting the first column as row names
envData <- read.csv("3.TemporaryFiles/envDataWithShannonTransformed.csv", row.names = 1)
envData <- envData[, c("ThufurHollowNA",
                           "VerticalWaterDistanceLog",
                           "SoilMoistureAvrg",
                           "pHLog",
                           "SalinityAdjustedLog",
                           "BulkDensityIncRootsLog",
                           "PlantBiomassLog",
                           "ShannonIndex",
                           "SpeciesRichness")]

envDataFiltered <- envData %>% filter(!is.na(ThufurHollowNA))
envDataFiltered$ThufurHollowNA <- as.factor(envDataFiltered$ThufurHollowNA)


########################### Running tests

# Extract numeric columns (exclude the grouping factor)
numeric_vars <- colnames(envDataFiltered)[colnames(envDataFiltered) != "ThufurHollowNA"]

# Initialize a list to store test results
test_results <- list()

# Loop through each numeric variable
for (var in numeric_vars) {
  values <- envDataFiltered[[var]]
  group <- envDataFiltered$ThufurHollowNA
  
  # Split the values into the two ThufurHollowNA groups
  group1 <- values[group == levels(group)[1]]
  group2 <- values[group == levels(group)[2]]
  
  # Run Shapiro-Wilk normality test for both groups
  shapiro_p1 <- shapiro.test(group1)$p.value
  shapiro_p2 <- shapiro.test(group2)$p.value
  
  # Decide which test to use
  if (shapiro_p1 > 0.05 & shapiro_p2 > 0.05) {
    test <- t.test(values ~ group)
    method <- "t-test"
  } else {
    test <- wilcox.test(values ~ group)
    method <- "Mann-Whitney U"
  }
  
  # Save results
  test_results[[var]] <- list(
    method = method,
    statistic = test$statistic,
    p.value = test$p.value
  )
}

# Optionally, print the results nicely
for (var in names(test_results)) {
  cat("\nVariable:", var,
      "\n  Test used:", test_results[[var]]$method,
      "\n  Statistic:", round(test_results[[var]]$statistic, 3),
      "\n  p-value:", round(test_results[[var]]$p.value, 4), "\n")
}


p <- ggplot(envDataFiltered, aes(x=ThufurHollowNA, y=ShannonIndex)) + 
  geom_boxplot()
p
