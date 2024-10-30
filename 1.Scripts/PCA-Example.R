# https://youtu.be/0Jp4gsfOLMs?si=JkB6ruQhFoga3X8C

dataMatrix <- matrix(nrow=100, ncol=10)
# Naming the columns
colnames(dataMatrix) <- c(  
  paste("wt", 1:5, sep=""),
  paste("ko", 1:5, sep=""))
# Naming the rows
rownames(dataMatrix) <- paste("gene", 1:100, sep="")

# Fake read counts
for (i in 1:100) {
  wtValues <- rpois(5, lambda=sample(x=10:1000, size=1))
  koValues <- rpois(5, lambda=sample(x=10:1000, size=1))
  
  dataMatrix[i,] <- c(wtValues, koValues)
}

# First six rows of the data
head(dataMatrix)

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

# Using the first two PC's to draw a 2d plot
plot(pca$x[,1], pca$x[,2], )

# To get a sense of how meaningful the clusters are, let's see how much variation 
# they account for, using the square of sdev.
pcaVar <- pca$sdev^2

# Percentages are more useful, so these are calculated and plotted
pcaVarPer <- round(pcaVar/sum(pcaVar)*100, 1)
barplot(pcaVarPer, main="Scree Plot", xlab="Principal component", ylab="Percent variation")

# ggplot2 can be used to make a fancy PCA plot
library(ggplot2)
pcaData <- data.frame(Sample=rownames(pca$x),
                      X=pca$x[,1],
                      Y=pca$x[,2])
pcaData

# Creating the plot
ggplot(data=pcaData, aes(x=X, y=Y, label=Sample)) + 
  geom_text() +
  xlab(paste("PC1 - ", pcaVarPer[1], "%", sep="")) +
  ylab(paste("PC2 - ", pcaVarPer[2], "%", sep="")) +
  theme_bw() +
  ggtitle("PCA Graph")

# To see which genes have the largest effect on where the samples are plotted
loadingScores <- pca$rotation[,1]
# Using the abs() function to sort values on magnitude instead of high-low
geneScores <- abs(loadingScores)
geneScoresRanked <- sort(geneScores, decreasing=TRUE)
Top10Genes <- names(geneScoresRanked[1:10])

# Showing the genes
Top10Genes
# Show the scores and +/- sign
# Positive genes push samples to the right, negative to the left
pca$rotation[Top10Genes,1] 
