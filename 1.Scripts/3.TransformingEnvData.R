rm(list = ls()) # Cleaning the environment
# ctrl + L in console will clear everything
# in plot window click broom



# Load envData and vegData from CSV files, setting the first column as row names
envData <- as.matrix(read.csv("2.Data/envDataForMVA.csv", row.names = 1))
# vegData <- as.matrix(read.csv("2.Data/vegDataForMVA.csv", row.names = 1))



################# Inspect normality
# visual check
png(filename = "4.Results/Transf.EnvironmentalGradients.png", width = 1500, height = 1000)
par(mfrow = c(2, 3), cex.main = 1.5, cex.axis = 1.2, cex.lab = 1.4) # plot side by side
hist(envData[, "VerticalWaterDistance"], xlab = "VerticalWaterDistance")
hist(envData[, "SoilMoistureAvrg"], xlab = "SoilMoistureAvrg")
hist(envData[, "pH"], xlab = "pH")
hist(envData[, "SalinityAdjusted"], xlab = "SalinityAdjusted")
hist(envData[, "BulkDensityIncRoots"], xlab = "BulkDensityIncRoots")
dev.off()

# statistical check with a shapiro test
shapiro.test(envData[, "VerticalWaterDistance"])  # not normal 6.64e-11
shapiro.test(envData[, "SoilMoistureAvrg"])       # normal 0.1232                 best
shapiro.test(envData[, "pH"])                     # not normal 0.04031
shapiro.test(envData[, "SalinityAdjusted"])       # not normal 5.246e-13
shapiro.test(envData[, "BulkDensityIncRoots"])    # not normal 0.0003235



################# Log transform
envDataLog <- envData
envDataLog[, "VerticalWaterDistance"] <- log(envData[, "VerticalWaterDistance"] + 3.5)
envDataLog[, c("pH",
               "SoilMoistureAvrg",
               "SalinityAdjusted",
               "BulkDensityIncRoots")] <- log(envData[, c("pH",
                                                          "SoilMoistureAvrg",
                                                          "SalinityAdjusted",
                                                          "BulkDensityIncRoots")])



################# Inspect normality
# visual check
png(filename = "4.Results/Transf.EnvironmentalGradientsLog.png", width = 1500, height = 1000)
par(mfrow = c(2, 3), cex.main = 1.5, cex.axis = 1.2, cex.lab = 1.4) # plot side by side
hist(envDataLog[, "VerticalWaterDistance"], xlab = "log(VerticalWaterDistance + 3.5)")
hist(envDataLog[, "SoilMoistureAvrg"], xlab = "log(SoilMoistureAvrg)")
hist(envDataLog[, "pH"], xlab = "log(pH)")
hist(envDataLog[, "SalinityAdjusted"], xlab = "log(SalinityAdjusted)")
hist(envDataLog[, "BulkDensityIncRoots"], xlab = "log(BulkDensityIncRoots)")
dev.off()

# statistical check with a shapiro test
shapiro.test(envDataLog[, "VerticalWaterDistance"])  # normal 0.2022              best
shapiro.test(envDataLog[, "SoilMoistureAvrg"])       # not normal 3.17e-06
shapiro.test(envDataLog[, "pH"])                     # normal 0.1229              best
shapiro.test(envDataLog[, "SalinityAdjusted"])       # not normal 0.02305
shapiro.test(envDataLog[, "BulkDensityIncRoots"])    # not normal 0.02153



################# square root transform
envDataSquare <- envData
envDataSquare[, "VerticalWaterDistance"] <- sqrt(envData[, "VerticalWaterDistance"] + 3.5)
envDataSquare[, "BulkDensityIncRoots"] <- sqrt(envData[, "BulkDensityIncRoots"])
envDataSquare[, c("pH",
                  "SalinityAdjusted")] <- sqrt(envData[, c("pH",
                                                           "SalinityAdjusted")])



################# Inspect normality
# visual check
png(filename = "4.Results/Transf.EnvironmentalGradientsSqrt.png", width = 1500, height = 1000)
par(mfrow = c(2, 3), cex.main = 1.5, cex.axis = 1.2, cex.lab = 1.4) # plot side by side
hist(envDataSquare[, "VerticalWaterDistance"], xlab = "sqrt(VerticalWaterDistance + 3.5)")
hist(envDataSquare[, "SoilMoistureAvrg"], xlab = "sqrt(SoilMoistureAvrg)")
hist(envDataSquare[, "pH"], xlab = "sqrt(pH)")
hist(envDataSquare[, "SalinityAdjusted"], xlab = "sqrt(SalinityAdjusted)")
hist(envDataSquare[, "BulkDensityIncRoots"], xlab = "sqrt(BulkDensityIncRoots)")
dev.off()

# statistical check with a shapiro test
shapiro.test(envDataSquare[, "VerticalWaterDistance"])  # not normal 2.511e-05
shapiro.test(envDataSquare[, "SoilMoistureAvrg"])       # normal 0.1232
shapiro.test(envDataSquare[, "pH"])                     # normal 0.07468
shapiro.test(envDataSquare[, "SalinityAdjusted"])       # not normal 3.453e-08
shapiro.test(envDataSquare[, "BulkDensityIncRoots"])    # not normal 0.01751



################# Box-Cox transformation check

# https://youtu.be/vGOpEpjz2Ks?si=mcGVeLnYw8hmf6WQ
# The box-cox is a very nice method, but it's mostly for linear models, where it helps identify 
# an optimal power transformation to normalize the residuals.
# I.e., it's not applicable here




################# Merge columns for best transformation
envDataFinal <- envData
envDataFinal[, c("VerticalWaterDistance",
            "pH",
            "SalinityAdjusted",
            "BulkDensityIncRoots")] <- envDataLog[, c("VerticalWaterDistance",
                                                      "pH",
                                                      "SalinityAdjusted",
                                                      "BulkDensityIncRoots")]
colnames(envDataFinal)[colnames(envDataFinal) 
                  %in% c("VerticalWaterDistance", 
                         "pH", 
                         "SalinityAdjusted",
                         "BulkDensityIncRoots")] <- paste0(c("VerticalWaterDistance", 
                                                             "pH", 
                                                             "SalinityAdjusted", 
                                                             "BulkDensityIncRoots"), "Log")



# visual check
png(filename = "4.Results/Transf.EnvironmentalGradientsFinal.png", width = 1500, height = 1000)
par(mfrow = c(2, 3), cex.main = 1.5, cex.axis = 1.2, cex.lab = 1.4) # plot side by side
hist(envDataFinal[, "VerticalWaterDistanceLog"], xlab = "log(VerticalWaterDistance + 3.5)")
hist(envDataFinal[, "SoilMoistureAvrg"], xlab = "SoilMoistureAvrg")
hist(envDataFinal[, "pHLog"], xlab = "log(pH)")
hist(envDataFinal[, "SalinityAdjustedLog"], xlab = "log(SalinityAdjusted)")
hist(envDataFinal[, "BulkDensityIncRootsLog"], xlab = "log(BulkDensityIncRoots)")
dev.off()

# statistical check with a shapiro test
shapiro.test(envDataFinal[, "VerticalWaterDistanceLog"])  # normal 0.2022
shapiro.test(envDataFinal[, "SoilMoistureAvrg"])          # normal 0.1232
shapiro.test(envDataFinal[, "pHLog"])                     # normal 0.1229
shapiro.test(envDataFinal[, "SalinityAdjustedLog"])       # not normal 0.02305
shapiro.test(envDataFinal[, "BulkDensityIncRootsLog"])    # not normal 0.02153




################# Export transformed data
write.csv(envDataFinal, "2.Data/envDataForMVATransformed.csv", row.names = TRUE)





