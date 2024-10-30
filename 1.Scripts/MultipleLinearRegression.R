# tidyverse for plots, readxl for Excel files, vegan for clustering, car for analyses

rm(list = ls()) # Cleaning the environment
# ctrl + L in console will clear everything
# in plot window click broom

library(readxl)
library(ggplot2)  # includes ggplot
library(gridExtra)
library(car)
library(emmeans)

FData <- read_excel("2.Data/FieldData.xlsx") 
VData <- read_excel("2.Data/VegetationData.xlsx")
# glimpse(FData) in console to see summary of datasheet
# ?cor.test in console to see explanation
FData$SoilTexture <- as.factor(FData$SoilTexture)  # Change the data type of a column
FData$AnimalActivity <- as.factor(FData$AnimalActivity)  # Change the data type of a column
FData$ThufurHollowNA <- as.factor(FData$ThufurHollowNA)  # Change the data type of a column




####################################################### MLR 1 Plant biomass


model <- lm(log(PlantBiomass) ~ VerticalWaterDistance +
              SoilMoistureAvrg + pH + EC + SalinityAdjusted +
              BulkDensityIncRoots + SoilLightness, data = FData)

# Check linearity and homoscedasticity
# Set up the file to save as PNG
png(filename = "4.Results/PlantBiomassModelDiagnostics.png", width = 800, height = 800)
par(mfrow = c(2, 2))  # Set up a 2x2 plotting area
plot(model)
# Close the device to save the file
dev.off()

# Check normality of residuals
shapiro.test(residuals(model))  # Shapiro-Wilk test

# Check for multicollinearity
vif_values <- vif(model)
print(vif_values)

par(mfrow = c(1, 2))
hist(residuals(model), main = "Residuals Histogram", xlab = "Residuals")
qqnorm(residuals(model))
qqline(residuals(model), col = "red")            



########### Removed collinear variable EC

modelWithoutEC <- lm(log(PlantBiomass) ~ VerticalWaterDistance +
                       SoilMoistureAvrg + pH + SalinityAdjusted +
                       BulkDensityIncRoots + SoilLightness, data = FData)            

# Check linearity and homoscedasticity
# Set up the file to save as PNG
png(filename = "4.Results/PlantBiomassModelDiagnosticsNoEC.png", width = 800, height = 800)
par(mfrow = c(2, 2))  # Set up a 2x2 plotting area
plot(modelWithoutEC)
# Close the device to save the file
dev.off()

# Check normality of residuals
shapiro.test(residuals(modelWithoutEC))  # Shapiro-Wilk test

# Check for multicollinearity
vif_values <- vif(modelWithoutEC)
print(vif_values)

png(filename = "4.Results/PlantBiomassModelResidualsQQ.png", width = 800, height = 500)
par(mfrow = c(1, 2))
hist(residuals(modelWithoutEC), main = "Residuals Histogram", xlab = "Residuals")
qqnorm(residuals(modelWithoutEC))
qqline(residuals(modelWithoutEC), col = "red")
dev.off()

summary(modelWithoutEC)




####################################################### MLR 2 Greenness

model2WithoutEC <- lm(log(GreennessIndex) ~ VerticalWaterDistance +
              SoilMoistureAvrg + pH + SalinityAdjusted +
              BulkDensityIncRoots + SoilLightness, data = FData)

# Check linearity and homoscedasticity
# Set up the file to save as PNG
png(filename = "4.Results/GreennessIndexModelDiagnostics.png", width = 800, height = 800)
par(mfrow = c(2, 2))  # Set up a 2x2 plotting area
plot(model2WithoutEC)
# Close the device to save the file
dev.off()

# Check normality of residuals
shapiro.test(residuals(model2WithoutEC))  # Shapiro-Wilk test

# Check for multicollinearity
vif_values <- vif(model2WithoutEC)
print(vif_values)

png(filename = "4.Results/GreennessIndexModelResidualsQQ.png", width = 800, height = 500)
par(mfrow = c(1, 2))
hist(residuals(modelWithoutEC), main = "Residuals Histogram", xlab = "Residuals")
qqnorm(residuals(modelWithoutEC))
qqline(residuals(modelWithoutEC), col = "red")
dev.off()

summary(model2WithoutEC)
