
####################################################################################################
# This script will import all the source files in the project and run everything from start to finish. 

### DEPENDENCIES
# install.packages("readxl")
# install.packages("ggplot2")
# install.packages("vegan")
# install.packages("tidyverse")
# install.packages("gridExtra")
# install.packages("hrbrthemes")


### REQUIREMENTS OF THIS SCRIPT
# This script needs to be together with the subscripts and input data in a
# project with the following folder structure:
# - Working directory
#   - 1.Scripts
#       - [all scripts]
#   - 2.Data
#       - EnvironmentalData.xlsx
#       - VegetationData.xlsx
#   - 3.TemporaryFiles (empty)
#   - 4.Results (empty)


### INPUT OF THIS SCRIPT:
# This project needs two Excel data sheets: 
# - EnvironmentalData.xlsx
# - VegetationData.xlsx


## OUTPUT OF THIS SCRIPT:
# During execution, several version of the Excel sheets will be produced and
# stored in the TemporaryFiles folder. These can be removed later. In the 
# Results folder, all the graphs and diagrams will be saved


rm(list = ls()) # Cleaning the environment
# ctrl + L in console will clear everything
# in plot window click broom




########################### Running source files

source("1.Scripts/1.CleaningData.R")
source("1.Scripts/2.CalculatingRichnessAndShannon.R")
source("1.Scripts/3.TransformingEnvData.R")
source("1.Scripts/4.ExploratoryScatterBoxPlots.R")
source("1.Scripts/5.ClusteringVegData.R")
source("1.Scripts/6.NM-MDS.R")
source("1.Scripts/7.TestingPlotClusters.R")
source("1.Scripts/8.LM-ShannonIndex.R")
source("1.Scripts/10.RangeFigures.R")


########################### 

rm(list = ls()) # Cleaning the environment

