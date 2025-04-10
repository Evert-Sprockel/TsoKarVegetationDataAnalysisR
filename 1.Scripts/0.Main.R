
####################################################################################################
# This script will import all the source files in the project and run everything from start to finish. 

### DEPENDENCIES
# install.packages("readxl")
# install.packages("tidyverse")
# install.packages("vegan")
# install.packages("gridExtra")
# install.packages("GGally")
# install.packages("ggcorrplot")
# install.packages("ggpubr")
# install.packages("cluster")
# install.packages("rstatix")
# install.packages("MuMIn")
# install.packages("car")
# install.packages("patchwork")
# install.packages("devtools")
# library(devtools)
# install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")


### REQUIREMENTS
# This script needs to be together with the sub scripts and input data in a project with the 
# following folder structure:
# - Working directory
#   - 1.Scripts
#       - [all scripts]
#   - 2.Data
#       - EnvironmentalData.xlsx
#       - VegetationData.xlsx
#   - 3.TemporaryFiles (empty)
#   - 4.Results (empty)


### INPUT:
# This project needs two Excel data sheets: 
# - EnvironmentalData.xlsx: contains all measurements except for the vegetation data
# - VegetationData.xlsx: contains the species list per plot with their abundance in percentages


## OUTPUT:
# During execution, several versions of the Excel sheets will be produced and stored as csv files in 
# the TemporaryFiles folder. These can be removed later. In the Results folder all the graphs and 
# diagrams will be saved. Test outputs will be printed to the terminal



########################### Cleaning environment before starting

rm(list = ls()) # Cleaning the environment
# ctrl + L in console will clear everything
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # Cleaning plot window (or click broom)


########################### Running source files

source("1.Scripts/1.CleaningData.R")
source("1.Scripts/2.CalculatingRichnessAndShannon.R")
source("1.Scripts/3.TransformingEnvData.R")
source("1.Scripts/4.BasicDescriptiveStatistics.R")
source("1.Scripts/5.CorrelationTable.R")
source("1.Scripts/6.ExploratoryScatterBoxPlots.R")
source("1.Scripts/7.ClusteringVegData.R")
source("1.scripts/8.SpeciesListPerCluster.R")
source("1.Scripts/9.NMDS.R")
source("1.Scripts/10.TestingClustersAndSites.R")
source("1.Scripts/11.ThufurTest.R")
source("1.Scripts/12.GLMs.R")
source("1.Scripts/13.PearsonCorrelationGreenness.R")
source("1.Scripts/14.RangeFigures.R")
source("1.Scripts/15.RangeFigures-ColorCoded.R")


########################### Cleaning environment once finished

rm(list = ls()) # Cleaning the environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # Cleaning plot window (or click broom)

