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