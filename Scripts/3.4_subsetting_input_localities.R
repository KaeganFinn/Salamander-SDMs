########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs 

### Author: Jayna Bergman & Kaegan Finn

### Goal of this Script: 

# Takes an input csv with all input localities for each species (separately) and subsets the localities for each study extent
# A new csv is saved for each study extent.  

### Notes:  

# Uses input localities "species_dupl_rm.csv"
# need locality records in csv format - double check names of columns. 

### Date: May 31, 2022

### Version of R:  R Version 4.0.3

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(tidyr)
library(tidyverse)
library(sf)

rm(list=ls())
########################### END SECTION ##############################

########################## DATA AND ANALYSIS #########################


## set working directory
getwd()
setwd()

################################# LTSA ################################

## Read in input localities and set them as a special feature 

LTSA_pts <- read.csv("./Input_localities/LTSA_dupl_rm.csv")

LTSA_pts <- st_as_sf(LTSA_pts, coords = c("Long_m", "Lat_m"), crs = "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
plot(st_geometry(LTSA_pts))


## Read in all study extents for the species as shapefiles

LTSA_genetic <- st_read("./Study_extents/Study_extents_aea/genetic_aea.shp")
LTSA_ecotone <- st_read("./Study_extents/Study_extents_aea/ecotone_aea.shp")
LTSA_political <- st_read("./Study_extents/Study_extents_aea/political_aea.shp")
LTSA_range <- st_read("./Study_extents/Study_extents_aea/range_aea.shp")

plot(st_geometry(LTSA_range))
plot(st_geometry(LTSA_genetic), add=TRUE)
plot(st_geometry(LTSA_ecotone), add=TRUE)
plot(st_geometry(LTSA_political), add=TRUE)


## Subset localities to study extents *** Need to have localities as a spatial vector (x argument) *** Need to have shapefiles with a spaital extent (y argument) ***

LTSA_genetic_pts <- st_join(LTSA_pts, LTSA_genetic, join = st_within, left = FALSE)
plot(st_geometry(LTSA_genetic_pts))

LTSA_ecotone_pts <- st_join(LTSA_pts, LTSA_ecotone, join = st_within, left = FALSE)
plot(st_geometry(LTSA_ecotone_pts))

LTSA_political_pts <- st_join(LTSA_pts, LTSA_political, join = st_within, left = FALSE)
plot(st_geometry(LTSA_political_pts))

LTSA_range_pts <- st_join(LTSA_pts, LTSA_range, join = st_within, left = FALSE)
plot(st_geometry(LTSA_range_pts))


## Dropping geometry from csv and set which columns to keep in the csv

## Genetic
LTSA_genetic_pts <- LTSA_genetic_pts %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

LTSA_genetic_pts <- LTSA_genetic_pts %>%
  st_drop_geometry()

LTSA_genetic_pts <- subset(LTSA_genetic_pts, select = c(Species, Long_m, Lat_m,  Longitude, Latitude, Datum, Location_Precision_meters, Year, State_Province, Source, Reference))

## Ecotone
LTSA_ecotone_pts <- LTSA_ecotone_pts %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

LTSA_ecotone_pts <- LTSA_ecotone_pts %>%
  st_drop_geometry()

LTSA_ecotone_pts <- subset(LTSA_ecotone_pts, select = c(Species, Long_m, Lat_m,  Longitude, Latitude, Datum, Location_Precision_meters, Year, State_Province, Source, Reference))

## Political
LTSA_political_pts <- LTSA_political_pts %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

LTSA_political_pts <- LTSA_political_pts %>%
  st_drop_geometry()

LTSA_political_pts <- subset(LTSA_political_pts, select = c(Species, Long_m, Lat_m,  Longitude, Latitude, Datum, Location_Precision_meters, Year, State_Province, Source, Reference))

## Range
LTSA_range_pts <- LTSA_range_pts %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

LTSA_range_pts <- LTSA_range_pts %>%
  st_drop_geometry()

LTSA_range_pts <- subset(LTSA_range_pts, select = c(Species, Long_m, Lat_m,  Longitude, Latitude, Datum, Location_Precision_meters, Year, State_Province, Source, Reference))

## Saving locality subsets as csv

write.csv(LTSA_genetic_pts, file = "./input_localities/model_subsets/LTSA/LTSA_genetic_locs.csv", row.names = FALSE)
write.csv(LTSA_ecotone_pts, file = "./input_localities/model_subsets/LTSA/LTSA_ecotone_locs.csv", row.names = FALSE)
write.csv(LTSA_political_pts, file = "./input_localities/model_subsets/LTSA/LTSA_political_locs.csv", row.names = FALSE)
write.csv(LTSA_range_pts, file = "./input_localities/model_subsets/LTSA/LTSA_range_locs.csv", row.names = FALSE)

########################### END SECTION ##############################

########################## FINAL COMMENTS ############################

########################### END SCRIPT ###############################