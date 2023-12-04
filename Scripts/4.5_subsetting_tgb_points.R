########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs 

### Author: Jayna Bergman & Kaegan Finn

### Goal of this Script: 

# Takes an input csv with all TGB localities for each species (separately) and subsets the localities for each study extent
# A new csv with TGB is saved for each study extent.  

### Notes:  

# Uses input localities "species_tgb_dupl_rm.csv"
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
setwd()

################################# LTSA ################################

## Read in input localities and set them as a special feature 

LTSA_tgb <- read.csv("./tgb/pre_processing/LTSA/LTSA_tgb_dupl_rm.csv")

LTSA_tgb <- st_as_sf(LTSA_tgb, coords = c("Long_m", "Lat_m"), crs = "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
plot(st_geometry(LTSA_tgb))

## Read in all study extents for the species as shapefiles

LTSA_genetic <- st_read("./study_extents/model_subsets/LTSA/LTSA_genetic.shp")
LTSA_ecotone <- st_read("./study_extents/model_subsets/LTSA/LTSA_ecotone.shp")
LTSA_political <- st_read("./study_extents/model_subsets/LTSA/LTSA_political.shp")
LTSA_range <- st_read("./study_extents/model_subsets/LTSA/LTSA_range.shp")

plot(st_geometry(LTSA_range))
plot(st_geometry(LTSA_genetic), add=TRUE)
plot(st_geometry(LTSA_ecotone), add=TRUE)
plot(st_geometry(LTSA_political), add=TRUE)


## Subset localities to study extents *** Need to have localities as a spatial vector (x argument) *** Need to have shapefiles with a spaital extent (y argument) ***

LTSA_genetic_pts_tgb <- st_join(LTSA_tgb, LTSA_genetic, join = st_within, left = FALSE)
plot(st_geometry(LTSA_genetic_pts_tgb))

LTSA_ecotone_pts_tgb <- st_join(LTSA_tgb, LTSA_ecotone, join = st_within, left = FALSE)
plot(st_geometry(LTSA_ecotone_pts_tgb))

LTSA_political_pts_tgb <- st_join(LTSA_tgb, LTSA_political, join = st_within, left = FALSE)
plot(st_geometry(LTSA_political_pts_tgb))

LTSA_range_pts_tgb <- st_join(LTSA_tgb,LTSA_range, join = st_within, left = FALSE)
 plot(st_geometry(LTSA_range_pts_tgb))


## Dropping geometry from csv and set which columns to keep in the csv

## Genetic
LTSA_genetic_pts_tgb <- LTSA_genetic_pts_tgb %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

LTSA_genetic_pts_tgb <- LTSA_genetic_pts_tgb %>%
  st_drop_geometry()

LTSA_genetic_pts_tgb <- subset(LTSA_genetic_pts_tgb, select = c(Scientific_Name, CommonName, Long_m, Lat_m,  Longitude, Latitude, Location_Precision_meters, Year, stateProvince, references))

## Ecotone
LTSA_ecotone_pts_tgb <- LTSA_ecotone_pts_tgb %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

LTSA_ecotone_pts_tgb <- LTSA_ecotone_pts_tgb %>%
  st_drop_geometry()

LTSA_ecotone_pts_tgb <- subset(LTSA_ecotone_pts_tgb, select = c(Scientific_Name, CommonName, Long_m, Lat_m,  Longitude, Latitude, Location_Precision_meters, Year, stateProvince, references))

## Political
LTSA_political_pts_tgb <- LTSA_political_pts_tgb %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

LTSA_political_pts_tgb <- LTSA_political_pts_tgb %>%
  st_drop_geometry()

LTSA_political_pts_tgb <- subset(LTSA_political_pts_tgb, select = c(Scientific_Name, CommonName, Long_m, Lat_m,  Longitude, Latitude, Location_Precision_meters, Year, stateProvince, references))

## Range
LTSA_range_pts_tgb <- LTSA_range_pts_tgb %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2]) 

LTSA_range_pts_tgb <- LTSA_range_pts_tgb %>%
  st_drop_geometry()

LTSA_range_pts_tgb <- subset(LTSA_range_pts_tgb, select = c(Scientific_Name, CommonName, Long_m, Lat_m,  Longitude, Latitude, Location_Precision_meters, Year, stateProvince, references))

## Saving locality subsets as csv

write.csv(LTSA_genetic_pts_tgb, file = "./tgb/model_subsets/LTSA/LTSA_genetic_tgb.csv", row.names = FALSE)
write.csv(LTSA_ecotone_pts_tgb, file = "./tgb/model_subsets/LTSA/LTSA_ecotone_tgb.csv", row.names = FALSE)
write.csv(LTSA_political_pts_tgb, file = "./tgb/model_subsets/LTSA/LTSA_political_tgb.csv", row.names = FALSE)
write.csv(LTSA_range_pts_tgb, file = "./tgb/model_subsets/LTSA/LTSA_range_tgb.csv", row.names = FALSE)

########################### END SECTION ##############################

########################## FINAL COMMENTS ############################

########################### END SCRIPT ###############################