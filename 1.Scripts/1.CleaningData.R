### PURPOSE OF THIS SCRIPT:
# Importing raw Excel data, cleaning, output as usable csv

### OUTPUT OF THIS SCRIPT:
# Two cleaned data sets (.csv): envData and vegData


rm(list = ls()) # Cleaning the environment
# ctrl + L in console will clear everything
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # Cleaning plot window (or click broom)

library(readxl)


########################### Importing data

vData <- as.data.frame(read_excel("2.Data/VegetationData.xlsx"))
eData <- as.data.frame(read_excel("2.Data/EnvironmentalData.xlsx"))
# glimpse(FData) in console to see summary of datasheet
# ?cor.test in console to see explanation


rownames(vData) <- vData[[1]]  # Set the first column as row names
vData <- vData[, -1]  # Remove the first column from the dataframe
rownames(eData) <- tolower(eData[[1]])  # Set the first column as row names
eData <- eData[, -1]  # Remove the first column from the dataframe


########################### Removing incorrect or missing data

#  s1t2p5: missing environmental data
#  s2t4p6: inaccurate vegetation data
#  s3t5p3: inaccurate vegetation data'
# The species rows that contain no data after the removal of these plots, are also removed

vData <- vData[, !colnames(vData) %in% c("s1t2p5", "s2t4p6", "s3t5p3")]
vData <- vData[!rownames(vData) %in% c("Cyperaceae unknown", "Leymus secalinus"), ]
eData <- eData[!rownames(eData) %in% c("s1t2p5", "s2t4p6", "s3t5p3"), ]


########################### Tidying the data frames, removing unnecessary information

vegData <- vData[, -c(1, 1)]  # only select the columns with abundance data
vegData[is.na(vegData)] <- 0  # Fill empty cells with 0
vegData <- as.data.frame(t(vegData))

envData <- eData[, c("GreennessIndex",
                     "Contrast",
                     "PlantBiomass",
                     "VerticalWaterDistance", 
                     "SoilMoistureAvrg", 
                     "pH",
                     "EC",
                     "SalinityAdjusted", 
                     "BulkDensityIncRoots")]

envDataTAA <- eData[, c("GreennessIndex",
                        "Contrast",
                        "PlantBiomass",
                        "VerticalWaterDistance", 
                        "SoilMoistureAvrg", 
                        "pH",
                        "EC",
                        "SalinityAdjusted", 
                        "BulkDensityIncRoots",
                        "ThufurHollowNA",
                        "AnimalActivity")]


################# Summary of data

# str(vegData)
# head(vegData)
# dim(vegData)
# 
# str(envData)
# head(envData)
# dim(envData)


################# Exporting the data

write.csv(vegData, "3.TemporaryFiles/vegData.csv", row.names = TRUE)
write.csv(envDataTAA, "3.TemporaryFiles/envData.csv", row.names = TRUE)



