if (!require(tidyverse)) {
  install.packages("tidyverse")
}
if (!require(ggplot2)) {
  install.packages("ggplot2")
}

library(tidyverse)
library(readr)

# First letter uppercase function
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

# Load Data
initialcsv = as_tibble(read_csv(file="US Mass Shootings.csv"))

# Get only Location , Latitude, Longitude cols
lat_long = initialcsv %>% select(Location, Latitude, Longitude)

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
    obj = strsplit(obj, ",", )[[1]][1]
    obj = strsplit(obj, " +", )[[1]]
    obj = obj[-1]
    obj = paste(unlist(obj), collapse=" ")
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


##
# START : Put Location in initial data
##
formattedCsv = initialcsv

# Concatenate Location columns
formattedCsv = formattedCsv %>% left_join(missing_location_completed, by= c("Longitude", "Latitude"))
formattedCsv = formattedCsv %>% mutate_if(is.character, function(x) {replace_na(x, "")})
formattedCsv = formattedCsv %>% unite(Location, Location.x, Location.y, sep="")

# Concatenate Longitude and Latitude columns
formattedCsv = formattedCsv %>% left_join(missing_lat_long_completed, by= "Location")
formattedCsv = formattedCsv %>% mutate_if(is.numeric, function(x) {replace_na(x, 0)})
formattedCsv = formattedCsv %>% mutate(Longitude.x = replace_na(Longitude.x, 0)) %>% mutate(Longitude.y = replace_na(Longitude.y, 0)) %>% mutate(Longitude = Longitude.x + Longitude.y)
formattedCsv = formattedCsv %>% mutate(Latitude.x = replace_na(Latitude.x, 0)) %>% mutate(Latitude.y = replace_na(Latitude.y, 0)) %>% mutate(Latitude = Latitude.x + Latitude.y)
# Deleting old columns
formattedCsv = formattedCsv %>% select(-Latitude.x, -Latitude.y, -Longitude.x, -Longitude.y)
##
# END : Put Location in initial data
##


##
# START : Separate Date to Year, Month, Day
##
formattedCsv = formattedCsv %>% mutate(Date = as.Date(Date, "%m/%d/%y"))

formattedCsv$Day = weekdays(formattedCsv$Date)
formattedCsv$Month = months(formattedCsv$Date)
formattedCsv$Year = year(formattedCsv$Date)

formattedCsv = formattedCsv %>% select(-Date)
##
# END : Separate Date to Year, Month, Day
##


##
# START : Group by Gender, Race, Mental Health Issue, Open/Close Location, Cause, Target, Incident Area, Weapon Type
##

gByTribble = formattedCsv

# Gender
gByTribble = gByTribble %>% mutate(Gender = case_when(Gender == "Male" ~ "M", Gender == "Female" ~ "F", Gender == "Male/Female" ~ "M/F", Gender == "Unknown" ~ "M/F", TRUE ~ Gender))

#Race
gByTribble = gByTribble %>% mutate(Race = case_when(
    grepl("[Ww]hite", Race) ~ "White", 
    grepl("[Bb]lack", Race) ~ "Black",
    grepl("[Aa]sian", Race) ~ "Asian",
    grepl("[Nn]ative", Race) ~ "Native",
    Race == "Some other race" | Race == "Two or more races" | Race == "Other" | Race == "" ~ "Unknown",
    TRUE ~ Race
  )
)

#Mental Health Issue
gByTribble = gByTribble %>% mutate(`Mental Health Issues` = case_when(`Mental Health Issues` == "Unclear" ~ "Unknown", TRUE ~ `Mental Health Issues`))

#Open/Close Location
gByTribble = gByTribble %>% mutate(`Open/Close Location` = case_when(`Open/Close Location` == "Open+Close" ~ "Unknown", TRUE ~ `Open/Close Location`))

#Cause
gByTribble = gByTribble %>% mutate(Cause = case_when(Cause == "" ~ "Unknown", TRUE ~ firstup(Cause)))


#Target
gByTribble = gByTribble %>% mutate(Target = case_when(Target == "" ~ "Random", TRUE ~ firstup(Target)))


#Incident Area
gByTribble = gByTribble %>% mutate(`Incident Area` = case_when(`Incident Area` == "" ~ "Random", TRUE ~ firstup(`Incident Area`)))


#Weapon Type
gByTribble = gByTribble %>% mutate(`Weapon Type` = case_when(`Weapon Type` == "" ~ "Random", TRUE ~ firstup(`Weapon Type`)))




##
# END : Group by Gender, Race, Mental Health Issue, Open/Close Location, Cause, Target, Incident Area, Weapon Type
##

