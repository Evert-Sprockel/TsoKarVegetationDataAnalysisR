### OUTPUT OF THIS SCRIPT:
# One pdf image with a correlation matrix


rm(list = ls()) # Cleaning the environment
# ctrl + L in console will clear everything
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # Cleaning plot window (or click broom)

library(ggplot2)
library(ggpubr)
library(ggcorrplot)


########################### Importing data

envData <- read.csv("3.TemporaryFiles/envDataWithShannonTransformed.csv", row.names = 1)


########################### Creating the matrix

# Select relevant columns
vars <- c("BulkDensityIncRootsLog", "SalinityAdjustedLog", "pHLog", 
          "VerticalWaterDistanceLog", "PlantBiomassLog", 
          "SoilMoistureAvrg", "SpeciesRichness", "ShannonIndex")

# Compute correlation matrix
cor_matrix <- cor(envData[, vars], use = "pairwise.complete.obs")

# Compute p-value matrix
pmatrix_cor <- cor_pmat(envData[, vars])

# Plot correlation matrix
p <- ggcorrplot(cor_matrix, type='lower', show.diag=FALSE,
                hc.order=TRUE, lab=TRUE, p.mat=pmatrix_cor,
                insig="blank") + 
                theme(text = element_text(family = "serif", size = 10),
                      plot.margin = unit(c(0, 0, 0, 0), "pt"))

# Save as a PDF
ggsave("4.Results/CorrelationPlot.pdf", plot = p, width = 6.4, height = 5)


