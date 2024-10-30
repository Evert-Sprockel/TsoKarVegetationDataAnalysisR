# tidyverse for plots, readxl for Excel files, vegan for clustering, car for analyses

rm(list = ls()) # Cleaning the environment
# ctrl + L in console will clear everything
# in plot window click broom

library(readxl)
library(ggplot2)

FData <- read_excel("2.Data/FieldData.xlsx") 
VData <- read_excel("2.Data/VegetationData.xlsx")
# glimpse(FData) in console to see summary of datasheet
# ?cor.test in console to see explanation
FData$SoilTexture <- as.factor(FData$SoilTexture)  # Change the data type of a column
FData$AnimalActivity <- as.factor(FData$AnimalActivity)  # Change the data type of a column
FData$ThufurHollowNA <- as.factor(FData$ThufurHollowNA)  # Change the data type of a column

str(VData)
head(VData)
dim(VData)

# Convert data to matrix
dataMatrix <- as.matrix(VData[, -c(1, 2)])
rownames(dataMatrix) <- VData[[1]]

str(dataMatrix)
head(dataMatrix)
dim(dataMatrix)


dataMatrix[is.na(dataMatrix)] <- 0
# zero_variance_cols <- apply(t(dataMatrix), 2, var) == 0
# if (any(zero_variance_cols)) {
#   cat("Constant columns found at indices:", which(zero_variance_cols), "\n")
#   
#   # Optionally, print the names of the constant columns
#   constant_columns <- colnames(dataMatrix)[zero_variance_cols]
#   print(constant_columns)
#   
#   # Remove constant columns from dataMatrix
#   dataMatrix <- dataMatrix[, !zero_variance_cols]
# }


####################################################### actual PCA


# Calling prcomp() to do a PCA on the data
# The goal is to draw a graph that shows how the samples are related
# By default, it expects samples to be rows and genes to be columns
# So the data is here transposed using the t() function
# If data isn't transposed, the graph will show how the
# genes are related to each other
# prcomp() returns three things:
#   x: contains pr. comp. for drawing a graph. The no. of samples = no. of PC's
#   sdev: standard deviation, gives information about amount of variation per PC
#   rotation: loading scores
pca = prcomp(t(dataMatrix), scale=TRUE)


########### Plots

# Using the first two PC's to draw a 2d plot using ggplot2
pcaData <- data.frame(Sample=rownames(pca$x),
                      X=pca$x[,1],
                      Y=pca$x[,2])
# Creating the plot
plot1 <- ggplot(data=pcaData, aes(x=X, y=Y, label=Sample)) + 
  geom_text() +
  xlab(paste("PC1 - ", pcaVarPer[1], "%", sep="")) +
  ylab(paste("PC2 - ", pcaVarPer[2], "%", sep="")) +
  theme_bw() +
  ggtitle("PCA Graph")
ggsave(filename = "4.Results/PCA.FirstTwoAxes.png",
       plot=plot1, width=8, height=6, dpi=300)


# To get a sense of how meaningful the clusters are, let's see how much variation they account for, 
# using the square of sdev.
pcaVar <- pca$sdev^2
# Percentages are more useful, so these are calculated and plotted
pcaVarPer <- round(pcaVar/sum(pcaVar)*100, 1)
# Creating plot
png(filename = "4.Results/PCA.ScreeVariationPCs.png", width = 500, height = 500)
barplot(pcaVarPer, main="Scree Plot", xlab="Principal component", ylab="Percent variation")
dev.off()





########### Checking which species are most responsible for the ordination

# To see which genes have the largest effect on where the samples are plotted
loadingScores <- pca$rotation[,1]
# Using the abs() function to sort values on magnitude instead of high-low
speciesScores <- abs(loadingScores)
speciesScoresRanked <- sort(speciesScores, decreasing=TRUE)
Top10species <- names(speciesScoresRanked[1:10])

# Showing the genes
Top10species
# Show the scores and +/- sign
# Positive genes push samples to the right, negative to the left
pca$rotation[Top10species,1] 
