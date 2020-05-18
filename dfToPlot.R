library(rgdal)
library(raster)
library(tidyverse)
library(rayshader)
library(brickr)
library(RColorBrewer)
library(reshape2)
library(DescTools)
library(av)

wd = "C:/Users/Student/Documents/PLU/Spring 2020/Capstone B/FoodProject"
setwd(wd)

# Loading county line data
waCounties <- shapefile("WA_Counties.shp")

# Loading county height data
plottingHeights <- read.csv("plottingHeights.csv", header = TRUE)

raster <- raster(ncol = 90, nrow = 45, xmn = -124.5, xmx = -116.2, ymn = 45.5, ymx = 49.1) # Empty raster
waCounties@data$COUNTYFP <- as.numeric(waCounties@data$COUNTYFP) # FIPS was saved as string. Converting to numeric
countiesRaster <- rasterize(waCounties, raster, field = "COUNTYFP", background = NA) # Filling the raster
countiesMatrix <- as.matrix(countiesRaster) # Converting the raster to a matrix

# Loading in the first brick layer
fixedMatrix <- Rev(countiesMatrix, margin = 1) # Reversing the matrix so y points up
firstLayer <- melt(fixedMatrix, na.rm = TRUE, varnames = c("Y", "X")) # Matrix to data frame
firstLayer <- cbind.data.frame(firstLayer, rep(2, length(firstLayer$value))) # First layer of z coordinates
firstLayer <- firstLayer[,c(2, 1, 4, 3)] # Reordering to X, Y, Z, FIPS
names(firstLayer) <- c("x", "y", "z", "CountyFIPS") # Descriptive headers

# Replacing the FIPS code with low access rate in the plotting data frame
heights <- unlist(lapply(firstLayer$CountyFIPS,
                              function(FIPS) plottingHeights$roundLApct[plottingHeights$CountyFIPS == FIPS]))
firstLayer <- cbind.data.frame(firstLayer, heights)

# Building vertically on the first layer.
# Subsets and stacks rows of the first layer if the listed height is g/eq than the current
# iterable
everyLayer <- firstLayer
for(i in 3:max(as.vector(firstLayer$heights))){
  attach <- firstLayer[firstLayer$heights >= i,]
  if(length(attach) > 0){
    attach$z <- rep(i, nrow(attach))
    everyLayer <- rbind.data.frame(everyLayer, attach)
  }
}

Color <- rep("Bright blue", nrow(everyLayer)) # Generating a column of colors. Monochromatic for now
piece_type <- rep("p", nrow(everyLayer)) # Generating a column that specifies plates are to be used, not bricks

everyLayer <- cbind.data.frame(everyLayer[,1:3], Color) # Binding the color column

flatModel <- bricks_from_coords(everyLayer) # Generating a brickr model from the data frame
build_bricks(flatModel) # Rendering the model

