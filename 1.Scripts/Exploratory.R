# tidyverse for plots, readxl for Excel files, vegan for clustering, car for analyses

rm(list = ls()) # Cleaning the environment
# ctrl + L in console will clear everything
# in plot window click broom

library(readxl)
library(ggplot2)  # includes ggplot
library(gridExtra)
library(car)

FData <- read_excel("2.Data/FieldData.xlsx") 
VData <- read_excel("2.Data/VegetationData.xlsx")
# glimpse(FData) in console to see summary of datasheet
# ?cor.test in console to see explanation
FData$SoilTexture <- as.factor(FData$SoilTexture)  # Change the data type of a column
FData$AnimalActivity <- as.factor(FData$AnimalActivity)  # Change the data type of a column
FData$ThufurHollowNA <- as.factor(FData$ThufurHollowNA)  # Change the data type of a column




####################################################### scatter plots

########### greenness plant biomass
# TODO: maybe logarithmic transformation: test if residuals are normally distributed
#  couple it to plant cover
#  give all pictures with very bright condition different color dots in the graph
plot1 <- ggplot(data = FData, mapping = aes(y= GreennessIndex, x = PlantBiomass)) +
  geom_point() +
  geom_smooth(method = "lm")
plot1
ggsave(filename = "4.Results/CorrelationGreennessPlantBiomass.png",
       plot=plot1, width=8, height=6, dpi=300)
# t = 3.6812, df = 88, p-value = 0.0003998, cor 0.3652958, R^2 = 
cor.test(FData$PlantBiomass, FData$GreennessIndex, method = "pearson")


########### greenness vert. distance, plant biomass vert. distance
plot2 <- ggplot(data = FData, mapping = aes(y= GreennessIndex, x = VerticalWaterDistance)) +
  geom_point() +
  geom_smooth(method = "lm")
plot3 <- ggplot(data = FData, mapping = aes(y= PlantBiomass, x = VerticalWaterDistance)) +
  geom_point() +
  geom_smooth(method = "lm")
# Arrange the plots side by side
combinedPlot2.3 <- grid.arrange(plot2, plot3, ncol = 2)
ggsave(filename = "4.Results/GreennessBiomassVertDist.png", 
       plot = combinedPlot2.3, width = 12, height = 6, dpi = 300)
# t = -2.6022, df = 88, p-value = 0.01087, cor = -0.2673051, R^2 = 
cor.test(FData$GreennessIndex, FData$VerticalWaterDistance, method = "pearson")
# t = -2.9032, df = 88, p-value = 0.004669, cor = -0.2956433, R^2 = 
cor.test(FData$PlantBiomass, FData$VerticalWaterDistance, method = "pearson")


########### greenness soil moisture, plant biomass soil moisture
plot4 <- ggplot(data = FData, mapping = aes(y= GreennessIndex, x = SoilMoistureAvrg)) +
  geom_point() +
  geom_smooth(method = "lm")
plot5 <- ggplot(data = FData, mapping = aes(y= PlantBiomass, x = SoilMoistureAvrg)) +
  geom_point() +
  geom_smooth(method = "lm")
# Arrange the plots side by side
combinedPlot4.5 <- grid.arrange(plot4, plot5, ncol = 2)
ggsave(filename = "4.Results/GreennessBiomassSoilMoisture.png", 
       plot = combinedPlot4.5, width = 12, height = 6, dpi = 300)
# t = 6.7812, df = 88, p-value = 1.315e-09, cor = 0.5858382, R^2 = 
cor.test(FData$GreennessIndex, FData$SoilMoistureAvrg, method = "pearson")
# t = 6.8378, df = 88, p-value = 1.016e-09, cor = 0.5890397, R^2 = 
cor.test(FData$PlantBiomass, FData$SoilMoistureAvrg, method = "pearson")


########### greenness pH, plant biomass pH
plot6 <- ggplot(data = FData, mapping = aes(y= GreennessIndex, x = pH)) +
  geom_point() +
  geom_smooth(method = "lm")
plot7 <- ggplot(data = FData, mapping = aes(y= PlantBiomass, x = pH)) +
  geom_point() +
  geom_smooth(method = "lm")
# Arrange the plots side by side
combinedPlot6.7 <- grid.arrange(plot6, plot7, ncol = 2)
ggsave(filename = "4.Results/GreennessBiomasspH.png", 
       plot = combinedPlot6.7, width = 12, height = 6, dpi = 300)
# t = -6.0753, df = 87, p-value = 3.178e-08, cor = -0.5457774, R^2 = 
cor.test(FData$GreennessIndex, FData$pH, method = "pearson")
# t = -3.48, df = 87, p-value = 0.0007859, cor = -0.3495585, R^2 = 
cor.test(FData$PlantBiomass, FData$pH, method = "pearson")


########### greenness EC, plant biomass EC
plot8 <- ggplot(data = FData, mapping = aes(y= GreennessIndex, x = EC)) +
  geom_point() +
  geom_smooth(method = "lm")
plot9 <- ggplot(data = FData, mapping = aes(y= PlantBiomass, x = EC)) +
  geom_point() +
  geom_smooth(method = "lm")
# Arrange the plots side by side
combinedPlot8.9 <- grid.arrange(plot8, plot9, ncol = 2)
ggsave(filename = "4.Results/GreennessBiomassEC.png", 
       plot = combinedPlot8.9, width = 12, height = 6, dpi = 300)
# t = -3.4891, df = 87, p-value = 0.000763, cor = -0.3503567, R^2 = 
cor.test(FData$GreennessIndex, FData$EC, method = "pearson")
# t = -1.4104, df = 87, p-value = 0.162, cor = -0.1495116 , R^2 = 
cor.test(FData$PlantBiomass, FData$EC, method = "pearson")


########### greenness salinity, plant biomass salinity
plot10 <- ggplot(data = FData, mapping = aes(y= GreennessIndex, x = SalinityAdjusted)) +
  geom_point() +
  geom_smooth(method = "lm")
plot11 <- ggplot(data = FData, mapping = aes(y= PlantBiomass, x = SalinityAdjusted)) +
  geom_point() +
  geom_smooth(method = "lm")
# Arrange the plots side by side
combinedPlot10.11 <- grid.arrange(plot10, plot11, ncol = 2)
ggsave(filename = "4.Results/GreennessBiomassSalinity.png", 
       plot = combinedPlot10.11, width = 12, height = 6, dpi = 300)
# t = -3.9969, df = 87, p-value = 0.0001341, cor = -0.3938753, R^2 = 
cor.test(FData$GreennessIndex, FData$SalinityAdjusted, method = "pearson")
# t = -1.8175, df = 87, p-value = 0.07258, cor = -0.1912598, R^2 = 
cor.test(FData$PlantBiomass, FData$SalinityAdjusted, method = "pearson")


########### greenness bulk density, plant biomass bulk density
plot12 <- ggplot(data = FData, mapping = aes(y= GreennessIndex, x = BulkDensityIncRoots)) +
  geom_point() +
  geom_smooth(method = "lm")
plot13 <- ggplot(data = FData, mapping = aes(y= PlantBiomass, x = BulkDensityIncRoots)) +
  geom_point() +
  geom_smooth(method = "lm")
# Arrange the plots side by side
combinedPlot12.13 <- grid.arrange(plot12, plot13, ncol = 2)
ggsave(filename = "4.Results/GreennessBiomassBulkDensity.png", 
       plot = combinedPlot12.13, width = 12, height = 6, dpi = 300)
# t = -7.9333, df = 87, p-value = 6.761e-12, cor = -0.6478856, R^2 = 
cor.test(FData$GreennessIndex, FData$BulkDensityIncRoots, method = "pearson")
# t = -3.9533, df = 87, p-value = 0.0001565, cor = -0.3902383, R^2 = 
cor.test(FData$PlantBiomass, FData$BulkDensityIncRoots, method = "pearson")


########### greenness lightness, plant biomass lightness
plot14 <- ggplot(data = FData, mapping = aes(y= GreennessIndex, x = SoilLightness)) +
  geom_point() +
  geom_smooth(method = "lm")
plot15 <- ggplot(data = FData, mapping = aes(y= PlantBiomass, x = SoilLightness)) +
  geom_point() +
  geom_smooth(method = "lm")
# Arrange the plots side by side
combinedPlot14.15 <- grid.arrange(plot14, plot15, ncol = 2)
ggsave(filename = "4.Results/GreennessBiomassSoilLightness.png", 
       plot = combinedPlot14.15, width = 12, height = 6, dpi = 300)
# t = -5.5626, df = 87, p-value = 2.884e-07, cor = -0.5122055 , R^2 = 
cor.test(FData$GreennessIndex, FData$SoilLightness, method = "pearson")
# t = -2.8612, df = 87, p-value = 0.005284, cor = -0.2932669, R^2 = 
cor.test(FData$PlantBiomass, FData$SoilLightness, method = "pearson")




########### Vert. distance water soil moisture
plot1 <- ggplot(data = FData, mapping = aes(y= VerticalWaterDistance, x = SoilMoistureAvrg)) +
  geom_point() +
  geom_smooth(method = "lm")
plot1
ggsave(filename = "4.Results/CorrelationVertDistWaterSoilMoisture.png",
       plot=plot1, width=8, height=6, dpi=300)


########### Salinity EC
plot1 <- ggplot(data = FData, mapping = aes(y= SalinityAdjusted, x = EC)) +
  geom_point() +
  geom_smooth(method = "lm")
plot1
ggsave(filename = "4.Results/CorrelationSalinityEC.png",
       plot=plot1, width=8, height=6, dpi=300)
# t = 48.498, df = 87, p-value < 2.2e-16, cor = 0.982003 
cor.test(FData$SalinityAdjusted, FData$EC, method = "pearson")


########### Salinity pH
plot1 <- ggplot(data = FData, mapping = aes(y= SalinityAdjusted, x = pH)) +
  geom_point() +
  geom_smooth(method = "lm")
plot1
ggsave(filename = "4.Results/CorrelationSalinitypH.png",
       plot=plot1, width=8, height=6, dpi=300)

####################################################### box plots

########### greenness animal activity, plant biomass animal activity
plot16 <- ggplot(data = FData, mapping = aes(y= GreennessIndex, x = AnimalActivity)) +
  geom_boxplot()
plot17 <- ggplot(data = FData, mapping = aes(y= PlantBiomass, x = AnimalActivity)) +
  geom_boxplot()
# Arrange the plots side by side
combinedPlot16.17 <- grid.arrange(plot16, plot17, ncol = 2)
ggsave(filename = "4.Results/GreennessBiomassAnimalActivity.png", 
       plot = combinedPlot16.17, width = 12, height = 6, dpi = 300)


########### greenness thufur/hollow, plant biomass thufur/hollow
plot18 <- ggplot(data = FData, mapping = aes(y= GreennessIndex, x = ThufurHollowNA)) +
  geom_boxplot()
plot19 <- ggplot(data = FData, mapping = aes(y= PlantBiomass, x = ThufurHollowNA)) +
  geom_boxplot()
# Arrange the plots side by side
combinedPlot18.19 <- grid.arrange(plot18, plot19, ncol = 2)
ggsave(filename = "4.Results/GreennessBiomassThufurHollow.png", 
       plot = combinedPlot18.19, width = 12, height = 6, dpi = 300)