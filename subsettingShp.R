library(rgdal)
library(raster)

wd = "C:/Users/Student/Documents/PLU/Spring 2020/Capstone B/FoodProject"
setwd(wd)

usCounties <- shapefile("US_Counties.shp")

#Subsetting WA counties
# Washington state's FIPS code is 53
waCounties <- usCounties[usCounties@data$STATEFP == '53',]

# Truncating the 'county' field in geographic data to one word
waCounties@data$NAME <- unlist(lapply(strsplit(waCounties@data$NAME, " "), function(l) l[[1]]))

# Converting county names in the geographic data to lower case
waCounties@data$NAME <- tolower(waCounties@data$NAME)

# Saving the subsetted data
shapefile(waCounties, "WA_Counties.shp", overwrite = TRUE)