### OUTPUT OF THIS SCRIPT:
# One data set (.csv): envData with extra columns for transformed variables


rm(list = ls()) # Cleaning the environment
# ctrl + L in console will clear everything
# in plot window click broom

library(dplyr)


########################### Importing data

# Load envData and vegData from CSV files, setting the first column as row names
envData <- as.matrix(read.csv("2.Data/envDataWithShannon.csv", row.names = 1))


########################### Function for checking normality

# Produces a histogram for every variable and saves everything in one image with p-values
# Takes a transform type for the file name and a data frame
# Returns none
checkNormality <- function(transformType, data) {
  png(filename = paste0("4.Results/Transf.EnvironmentalGradients", transformType, ".png"), width = 1500, height = 1000)
  par(mfrow = c(2, ncol(data) %/% 2), cex.main = 1.5, cex.axis = 1.2, cex.lab = 1.4) # plot side by side
  
  for (colName in colnames(data)) {
    test <- shapiro.test(data[, colName])
    p_value <- test$p.value
    hist(data[, colName], main = paste("Histogram of", colName), xlab = paste0(colName, ", P-value: ", p_value))
  }
  dev.off()
}

# Transforms columns of a data frame either with a log or square root transformation, or none
# Takes a data frame, a vector with transform types (must be same as no. of cols)
# Returns a transformed data frame
transformData <- function(df, transformTypes) {
  # Ensure transformTypes has the same length as the number of columns in df
  if (length(transformTypes) != ncol(df)) {
    stop("The length of transformTypes must match the number of columns in the data frame.")
  }
  dfTransformed <- df  # Create a copy of the original data frame
  for (i in seq_along(colnames(df))) {
    transformType <- transformTypes[i]
    colName <- colnames(df)[i]
    
    # Get the minimum value in the column
    minValue <- min(df[, colName], na.rm = TRUE)
    # If the minimum value is 0 or lower, adjust by adding the absolute value of the minimum
    adjustment = 0
    if (minValue <= 0) {
      adjustment <- abs(minValue)  # Use the absolute value of the minimum
    }
    
    if (transformType == "log") {
      dfTransformed[, colName] <- log(df[, colName] + adjustment + 1)  # No need to adjust if min > 0
      colnames(dfTransformed)[i] <- paste(colName, "Log", sep = "")
    } else if (transformType == "sqrt") {
      dfTransformed[, colName] <- sqrt(df[, colName] + adjustment)
      colnames(dfTransformed)[i] <- paste(colName, "Sqrt", sep = "")
    } else if (transformType == "none") {
      # No transformation (keep the column as is)
      dfTransformed[, colName] <- df[, colName]
    } else {
      stop("Invalid transformation type: use 'log', 'sqrt', or 'none'.")
    }
  }
  return(dfTransformed)
}


################# Inspect normality

# Check without transformations
checkNormality("", envData)

# Log transform every variable
envDataLog <- transformData(envData, transformTypes = c("log", "log", "log", "log", "log", "log", "log", "log", "log", "log"))
checkNormality("Log", envDataLog)

# Square root transform every variable
envDataSqrt <- transformData(envData, transformTypes = c("sqrt", "sqrt", "sqrt", "sqrt", "sqrt", "sqrt", "sqrt", "sqrt", "sqrt", "sqrt"))
checkNormality("Sqrt", envDataSqrt)

# Choose the best transformations for each variable
envDataFinal <- transformData(envData, transformTypes = c("none", "log", "log", "none", "log", "log", "log", "log", "none", "none"))
checkNormality("Final", envDataFinal)


################# Merge data matrices into one big data frame

# Get unique column names from both matrices
unique_columns <- unique(c(colnames(envData), colnames(envDataFinal)))
# Create an empty data frame to store the combined data
envDataMerged <- data.frame(matrix(ncol = length(unique_columns), nrow = nrow(envData)))
# Preserve the row names from envData (or envDataFinal)
rownames(envDataMerged) <- rownames(envData)
# Set column names for the combined data
colnames(envDataMerged) <- unique_columns
# Add columns from envData
envDataMerged[colnames(envData)] <- envData
# Add columns from envDataFinal (only the unique ones)
envDataMerged[colnames(envDataFinal)] <- envDataFinal


write.csv(envDataMerged, "2.Data/envDataWithShannonTransformed.csv", row.names = TRUE)





