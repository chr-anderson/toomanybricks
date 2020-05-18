setwd("C:/Users/Student/Documents/PLU/Spring 2020/Capstone B/FoodProject")

# Loading the full dataset
faraData <- read.csv("faraData.csv", header = TRUE, stringsAsFactors = FALSE)

# Subsetting for Washington data
waData <- faraData[faraData$State == 'Washington',]

# Truncating the 'County' field to one word
waData$County <- unlist(lapply(strsplit(waData$County, " "), function(l) l[[1]]))

# Converting county names to lower case
waData$County <- tolower(waData$County)

# Saving the WA county data
write.csv(waData, file = "waData.csv", row.names = FALSE)

