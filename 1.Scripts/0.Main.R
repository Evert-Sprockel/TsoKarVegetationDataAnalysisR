
####################################################################################################
# This script will import all the source files in the project and run everything from start to finish. 

### DEPENDENCIES
# install.packages("readxl")
# install.packages("ggplot2")
# install.packages("vegan")
# install.packages("tidyverse")
# install.packages("gridExtra")
# install.packages("hrbrthemes")

### OUTPUT OF THIS SCRIPT:
# A lot of graphs
# (No data set)


rm(list = ls()) # Cleaning the environment
# ctrl + L in console will clear everything
# in plot window click broom









# TODO: make the exploratory.R script work the cleaned data and add that to this file once finished

# Change the transforming.R script in such a way that it automatically selects the best transformation









########################### Running source files

source("1.Scripts/2.CleaningData.R")
source("1.Scripts/3.CalculatingRichnessAndShannon.R")
source("1.Scripts/4.TransformingEnvData.R")
source("1.Scripts/5.ClusteringVegData.R")
source("1.Scripts/6.NM-MDS.R")
source("1.Scripts/8.LM-ShannonIndex.R")
source("1.Scripts/10.RangeFigures.R")


########################### 

rm(list = ls()) # Cleaning the environment

