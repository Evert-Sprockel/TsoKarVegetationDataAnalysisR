### OUTPUT OF THIS SCRIPT:
# Scatter plots
# (No data set)

rm(list = ls()) # Cleaning the environment
# ctrl + L in console will clear everything
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # Cleaning plot window (or click broom)

library(ggplot2)
library(gridExtra)


########################### Importing data

envData <- read.csv("3.TemporaryFiles/envDataWithShannonTransformed.csv", row.names = 1)
envDataVars <- envData[, c("VerticalWaterDistanceLog",
                       "SoilMoistureAvrg",
                       "pHLog",
                       "SalinityAdjustedLog",
                       "ECLog",
                       "BulkDensityIncRootsLog")]


########################### Function for creating plots

# Creates a single scatter or box plot and saves the image
# Takes a type ('box' or 'scatter'), and two vectors
# Returns a ggplot2 object
createSinglePlot <- function(type, varY, varX, varX_name, testResult = "") {
  varY_name <- deparse(substitute(varY))
  varY_name <- sub(".*\\$", "", varY_name)
  varX_name <- paste0(varX_name, ", ", testResult)
  if (type == "scatter") {
    plot <- ggplot(data = NULL, mapping = aes(y= varY, x = varX)) +
      geom_point() +
      geom_smooth(method = "lm") +
      labs(x = varX_name, y = varY_name)
  } else if (type == "box") {
    plot <- ggplot(data = NULL, mapping = aes(y= varY, x = varX)) +
      geom_boxplot() +
      labs(x = varX_name, y = varY_name)
  } else {
    stop("Invalid plot type. Use 'scatter' or 'box'.")
  }
  return(ggplotGrob(plot))
}


# Runs a Pearson correlation test
# Takes two vectors
# Returns result
runPearson <- function(x, y) {
  if (length(x) != length(y)) {
    stop("Vectors x and y must have the same length.")
  }
  correlation_result <- cor.test(x, y, method = "pearson")
  print(correlation_result)
  return(correlation_result)
}


# Round a number with one of two methods
# Takes float/double
# Returns number as a string
rNum <- function(num) {
  if (num < 0.0001) {
    rndNum <- formatC(num, format = "e", digits = 3)
  } else {
    rndNum <- round(num, 4)
  }
  return(as.character(rndNum))
}


########################### Creating the plots

testResults <- list()
for (i in colnames(envDataVars)) {
  result <- runPearson(envDataVars[[i]], envData$GreennessIndex)
  # print(result)  # All significant
  testResults[[i]] <- paste0("p=",
                             rNum(result$p.value),
                             ", r^2=",
                             rNum(result$estimate^2))
}

plotList <- list()
for (i in colnames(envDataVars)) {
  p <- createSinglePlot("scatter", envData$GreennessIndex, envDataVars[[i]], i, testResults[[i]])
  plotList[[i]] <- p
  print(class(p))
}

# Arrange the plots in a grid (e.g., 2 rows, 3 columns)
combined_plot <- do.call(grid.arrange, c(plotList, ncol = 3))
ggsave("4.Results/PCor.GreennessEnv.png", 
       plot = combined_plot,
       dpi = 200,
       width = 12,
       height = 8,
       units = "in")

runPearson(envData$GreennessIndex, envData$Contrast)

