library(tidyverse)
#library(DescTools)
#library(av)
#library(rgdal)
#library(rayshader)
library(raster)
setwd("C:/Users/Student/Documents/PLU/Spring 2020/Capstone B/FoodProject")

waData <- read.csv("waData.csv", header = TRUE, stringsAsFactors = FALSE) # FARA data for WA
waCounties <- shapefile("WA_Counties.shp") # County line data

# -------- Low access rate for each census tract (1 entry per census tract) --------

waData$LAPOP1_10 <- round(waData$LAPOP1_10)          # Rounding LAPOP1_10 to integer (individual people)
LAPCT <- (waData$LAPOP1_10 / waData$POP2010) * 100   # Computing % of low access people in each tract

# Generating a list of FIPS codes with dimensions that match the waData frame
FIPS <- unlist(lapply(waData$County, function(c) waCounties@data$COUNTYFP[waCounties@data$NAME == c]))
rates <- cbind.data.frame(waData$County, FIPS, waData$POP2010, LAPCT) # cbinding into frame

# Computing LAPCT introduced NaNs where population = 0. Since we care about the
# accessibility of supermarkets to residents and not just the ubiquity of supermarkets, so
# we remove rows with NaN (0 population in these tracts) rather than calling it 0% low access:
waData <- waData[complete.cases(rates),]
rates <- rates[complete.cases(rates),]
names(rates) <- c("County", "CountyFIPS", "Population", "LApct") # Descriptive names

# -------- County-wise mean low access rate (1 entry per county) --------

# Helper function for weighted mean
weighted_mean <- function(name)
{
  x <- rates$LApct[rates$County == name]
  weights <- waData$POP2010[waData$County == name]
  return(weighted.mean(x, weights))
}

countyLApct <- waData$County %>%              # Take the county name
  unique() %>%                                # Remove duplicates
  lapply(function(c) weighted_mean(c)) %>%    # Take the mean of each county's census tracts, weighted by population
  unlist()                                    # Drop the outside nested list

roundLApct <- round(countyLApct, digits = 0)  # Low access percents rounded to integer
dispError <- roundLApct - countyLApct # Display error is rounded value - 'exact' value

shortFIPS <- unique(FIPS) # List of FIPS codes that matches the order of the low access rates

plottingHeights <- cbind.data.frame(shortFIPS, roundLApct)   # Bind FIPS codes and rounded low access rates into a data frame
names(plottingHeights) <- c("CountyFIPS", "roundLApct")      # Add desctiptive headers
write.csv(plottingHeights, file = "plottingHeights.csv", row.names = FALSE)     # Save as a CSV


errorID <- rep(0, length(dispError))                      # ID column to differentiate display error data from LApct data
dispDF <- cbind.data.frame(dispError, errorID)
leveneTop <- rates[,c(4, 2)]                              # LApct data, but just | LApct | FIPS |
names(dispDF) <- c("Quantity", "ID")                      # Renaming both frames so we can row bind them
names(leveneTop) <- c("Quantity", "ID")
leveneTop$ID <- as.numeric(as.character(leveneTop$ID))    # Fixing factor weirdness so I can row bind

leveneData <- rbind.data.frame(leveneTop, dispDF)
leveneData$ID <- as.factor(leveneData$ID)                 # Back to factor (I may regret this later)

write.csv(leveneData, file = "leveneData.csv", row.names = FALSE)     # Save as a CSV


