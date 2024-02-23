#Joshua G. Smith
#jossmith@mbayaq.org

#this script formats the cleaned foraging data for SOFA.
#user-defined filtering should be applied at this stage

rm(list=ls())

#load packages
librarian::shelf(tidyverse, readxl, here, sp, rgdal)

#set directories 
datin <- "/Volumes/seaotterdb$/kelp_recovery/data/foraging_data/processed"
datout <- "/Volumes/seaotterdb$/kelp_recovery/data/sofa_data/intermediate"


for_dat <- read_csv(file.path(datin, "foraging_data_1997_2023.csv"))






# Create a SpatialPoints object with UTM coordinates
coordinates <- data.frame(easting=utm_easting, northing=utm_northing)
sp_points <- SpatialPoints(coords = coordinates, proj4string = CRS(paste0("+proj=utm +zone=", utm_zone, " +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))

# Convert UTM to lat/long
latlon_points <- spTransform(sp_points, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# Extract latitude and longitude
lat <- coordinates(latlon_points)[,2]  # Latitude
lon <- coordinates(latlon_points)[,1]  # Longitude

print(paste("Latitude:", lat, "Longitude:", lon))








