########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Jayna Bergman & Kaegan Finn

### Goal of this Script:

# Converts localities from Lat/Long in datum WGS84 to Lat/Long in North America Albers Equal Area (AEA)

# for Target Group Background 

### Notes:  

# Make sure original projections are being defined correctly

### Date: May 12, 2022

### Version of R:  R version 4.0.3

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(sf)
library(dplyr)
library(tidyr)

rm(list=ls())
########################### END SECTION ##############################

###################### FUNCTION DEFINITIONS ##########################

## NA

########################### END SECTION ##############################

########################## DATA AND ANALYSIS #########################

## Set working directory

getwd()
setwd()

################################# LTSA ################################

## Read in filtered locality data

LTSA_tgb <- read.csv("./TGB/LTSA_tgb.csv")

## create a table with species name, latitude, and longitude columns to be able to use sf command

LTSA_pts_tgb <- LTSA_tgb[,c("Scientific_Name", "Longitude", "Latitude")]

## Converting to a special features and transforming to aea by defining the projection 

LTSA_pts_tgb <- st_as_sf(LTSA_pts_tgb, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84")
LTSA_pts_tgb_aea <- st_transform(LTSA_pts_tgb, crs= "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") 
LTSA_pts_tgb_aea


## creating Longitude (x) and Latitude (y) in meters, converting back to a data frame
LTSA_pts_tgb_aea <- LTSA_pts_tgb_aea %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2])

LTSA_pts_tgb_aea <- LTSA_pts_tgb_aea %>%
  st_drop_geometry()

LTSA_pts_tgb_aea <- as.data.frame(LTSA_pts_tgb_aea)
class(LTSA_pts_tgb_aea)


## creating final data frame to save as the new csv - binding "metadata" with the aea coordinates

LTSA_metadata <- cbind(LTSA_pts_tgb_aea, LTSA_tgb)
LTSA_metadata <- subset(LTSA_metadata, select= -c(Scientific_Name))


## Saving localities as new CSV with aea coordinates

write.csv(LTSA_metadata, file="./TGB/LTSA_tgb_aea.csv", row.names=FALSE)

########################### END SECTION ###############################

########################## FINAL COMMENTS ############################

########################### END SCRIPT ###############################