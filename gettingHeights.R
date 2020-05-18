library(tidyverse)
#library(DescTools)
#library(av)
#library(rgdal)
#library(rayshader)
library(raster)
setwd("C:/Users/Student/Documents/PLU/Spring 2020/Capstone B/FoodProject")
waData <- read.csv("waData.csv", header = TRUE, stringsAsFactors = FALSE)

# Loading geographic data
waCounties <- shapefile("WA_Counties.shp")

# Using the FARA data to generate a list of each county's average poverty rate
# in an order that matches the geographic data
povRate <- unlist(lapply(waCounties@data$NAME, function(c) mean(waData$PovertyRate[waData$County == c])))

povRateRound <- round(povRate, digits = 0)
rates <- cbind.data.frame(waCounties@data$NAME, povRate, povRateRound, povRate - povRateRound)


roundVar <- var(rates[4])

nTracts <- unlist(lapply(waCounties@data$NAME, function(c) length(waData$PovertyRate[waData$County == c])))
variances <- unlist(lapply(waCounties@data$NAME, function(c) var(waData$PovertyRate[waData$County == c])))
CountyFIPS <- as.numeric(waCounties@data$COUNTYFP)

comparisonData <- cbind.data.frame(rates[,1], CountyFIPS, nTracts, rates[,2:4], variances)

names(comparisonData) <- c("County", "CountyFIPS", "nTracts", "povRate", "povRateRound", "roundError", "tractVariance")

write.csv(comparisonData, file = "comparisonData.csv", row.names = FALSE)
