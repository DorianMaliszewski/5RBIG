if (!require(tidyverse)) {
  install.packages("tidyverse")
}
if (!require(ggplot2)) {
  install.packages("ggplot2")
}

library(tidyverse)
library(readr)

# Load Data
initialcsv = as_tibble(read_csv(file="US Mass Shootings.csv"))

# Get only Location , Latitude, Longitude cols
lat_long = initialcsv %>% select(Location, Latitude, Longitude)
glimpse(lat_long)

# Get only rows without NA
missing_location = lat_long %>% filter(is.na(Location)) %>% distinct()
missing_lat_long = lat_long %>% filter(is.na(Latitude)) %>% distinct()


# Library to get cities or coords
if (!require("devtools")) {
  install.packages("devtools")
}
require(devtools)
if (!require(ggmap)) {
  install.packages("ggmap")
}
library(ggmap)

if (!require(data.table)) {
  install.packages("data.table")
}
library(data.table)

## KEEP CALM Please
register_google(key= "AIzaSyBvmBgdwG-0rxrdKkLaL8ZbdUORIv8gvwg")

##
# START : Retrieve lat lon for cities
##
missing_lat_long = as.data.frame(missing_lat_long %>% select(Location))


# Take some time to get response
missing_lat_long_completed = as_tibble(mutate_geocode(missing_lat_long, Location))
missing_lat_long_completed = missing_lat_long_completed %>% rename(Latitude = lat, Longitude = lon)
##
# END : Retrieve lat lon for cities
##


##
# START : Retrieve cities for lat lon
##
missing_location = as.data.frame(missing_location %>% select(Latitude, Longitude))
df_missing_location = as.data.frame(missing_location %>% rename(lat = Latitude, lon = Longitude))


# Create loop function
getCities = mapply(FUN = function(lon, lat) {
  revgeocode(c(lon, lat), output = "all") 
}, 
df_missing_location$lon, df_missing_location$lat
)


# Create a blank list and initialize our counter
list = c()
i=1

# Loop to get the city of the Google Geocode API
while (i < length(getCities) - 1) {
    obj = getCities[1,i]$plus_code$compound_code
    obj = strsplit(obj, " +", )[[1]][2]
    obj = substr(obj, 1, nchar(obj)-1)
    list[[i]] = obj
    i = i + 1
}

# Transform to tibble and rename default column
list = as_tibble(list)
list = list %>% rename(Location = value)


# Concatenate the two tibble
missing_location_completed = missing_location
missing_location_completed$Location = list$Location

##
# END : Retrieve cities for lat lon
##



