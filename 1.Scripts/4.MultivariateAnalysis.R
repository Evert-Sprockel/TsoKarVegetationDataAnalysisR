rm(list = ls()) # Cleaning the environment
# ctrl + L in console will clear everything
# in plot window click broom

library(ggplot2)
library(vegan)

# https://youtu.be/FgakZw6K1QQ?si=dW2n96Xhc8HEZhoO
# Explanation of PCA. It's similar to a DCA. Since the axes lengths in the data are large, a DCA is
# more suitable.




# Load envData and vegData from CSV files, setting the first column as row names
envData <- as.matrix(read.csv("2.Data/envDataForMVATransformed.csv", row.names = 1))
vegData <- as.matrix(read.csv("2.Data/vegDataForMVA.csv", row.names = 1))

# NOTE: SEE WHICH PLOTS ARE REMOVED AND WHY IN THE SCRIPT WHERE THE DATA IS CLEANED



################# DCA

# check axis length. If >3, PCA is not suitable. Use DCA and CCA
dca <- decorana(vegData)
png(filename = "4.Results/DCA2.OrdinationPlot.png", width = 1200, height = 1000)
plot(dca, main = "Unconstrained DCA Ordination", 
     cex.main = 1.5,    # Increase title font size
     cex.axis = 1.2,    # Increase axis tick label font size
     cex.lab = 1.3,     # Increase axis label font size
     cex = 1.2)         # General scaling for other text elements)
dev.off()



################# CCA

cca <- cca(vegData, envData)

png(filename = "4.Results/CCA2.OrdinationPlot.png", width = 1200, height = 1000)
plot(cca, 
     main = "Ordination Plot of CCA", 
     cex.main = 1.5,    # Increase title font size
     cex.axis = 1.2,    # Increase axis tick label font size
     cex.lab = 1.3,     # Increase axis label font size
     cex = 1.2)         # General scaling for other text elements
dev.off()

# Plot species and environmental variables together
png(filename = "4.Results/CCA2.SpeciesAndEnvironmentalVars.png", width = 1200, height = 1000)
plot(cca, type = "n", main = "CCA Ordination Plot with Labels", 
     cex.main = 1.5,    # Increase title font size
     cex.axis = 1.2,    # Increase axis tick label font size
     cex.lab = 1.3,     # Increase axis label font size
     cex = 1.2)         # General scaling for other text elements)
text(cca, display = "species", col = "blue") # Species in blue
text(cca, display = "bp", col = "red")       # Environmental variables in red
dev.off()

# Plot species scores only
png(filename = "4.Results/CCA2.Species.png", width = 1200, height = 1000)
plot(cca, display = "species", main = "Species Plot", 
     cex.main = 1.5,    # Increase title font size
     cex.axis = 1.2,    # Increase axis tick label font size
     cex.lab = 1.3,     # Increase axis label font size
     cex = 1.2)         # General scaling for other text elements)
dev.off()
# Plot environmental variables only
png(filename = "4.Results/CCA2.EnvironmentalVars.png", width = 1200, height = 1000)
plot(cca, display = "bp", main = "Environmental Variables Plot", 
     cex.main = 1.5,    # Increase title font size
     cex.axis = 1.2,    # Increase axis tick label font size
     cex.lab = 1.3,     # Increase axis label font size
     cex = 1.2)         # General scaling for other text elements)
dev.off()


# See the R^2 (amount of variation explained by the axes)
summary(eigenvals(cca))



########### Checking which species are most responsible for the ordination
speciesScores <- abs(cca$CCA$v[, 1]) # Extract the species scores for the first CCA axis
# Sort by magnitude to identify the top species influencing the first axis
speciesScoresRanked <- sort(speciesScores, decreasing = TRUE)
Top10species <- names(speciesScoresRanked[1:10])
# Display the names of the top 10 species
Top10species
# Show the scores for the top 10 species (with direction indicated by +/- sign)
cca$CCA$v[Top10species, 1]


