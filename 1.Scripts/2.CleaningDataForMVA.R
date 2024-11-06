rm(list = ls()) # Cleaning the environment
# ctrl + L in console will clear everything
# in plot window click broom

library(readxl)


eData <- read_excel("2.Data/FieldData.xlsx") 
vData <- read_excel("2.Data/VegetationData.xlsx")
# glimpse(FData) in console to see summary of datasheet
# ?cor.test in console to see explanation



# Convert vegetation data to matrix
vegData <- as.matrix(vData[, -c(1, 2)])
rownames(vegData) <- vData[[1]]  # set first column of cells as row names
vegData[is.na(vegData)] <- 0  # Fill empty cells with 0


# Convert environmental data to matrix and make a selection of the environmental gradients
envData <- as.matrix(eData[, c("VerticalWaterDistance", "SoilMoistureAvrg", "pH", "SalinityAdjusted", 
                         "BulkDensityIncRoots")])
rownames(envData) <- tolower(eData[[1]])  # set first column of cells as row names



################# THE FOLLOWING PLOTS ARE REMOVED: #################
#  s1t2p5: missing environmental data
#  s2t4p6: inaccurate vegetation data
#  s3t5p3: inaccurate vegetation data

# vData: remove column "s1t2p5", "s2t4p6", "s3t5p3" and row "Cyperaceae unknown"
vegData <- vegData[, !colnames(vegData) %in% c("s1t2p5", "s2t4p6", "s3t5p3")]
vegData <- vegData[!rownames(vegData) %in% c("Cyperaceae unknown"), ]

# fData: remove row "s1t2p5", "s2t4p6", "s3t5p3" 
envData <- envData[!rownames(envData) %in% c("s1t2p5", "s2t4p6", "s3t5p3"), ]
################# ################################ #################


################# Transpose

vegData <- t(vegData)


################# Summary of data

str(vegData)
head(vegData)
dim(vegData)

str(envData)
head(envData)
dim(envData)



################# Exporting the data

write.csv(envData, "2.Data/envDataForMVA.csv", row.names = TRUE)
write.csv(vegData, "2.Data/vegDataForMVA.csv", row.names = TRUE)


