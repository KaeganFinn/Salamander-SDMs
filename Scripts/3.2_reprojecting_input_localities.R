########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Jayna Bergman & Kaegan Finn

### Goal of this Script:

# Converts localities from Lat/Long in datum WGS84 to Lat/Long in North America Albers Equal Area (AEA)

### Notes:  

# Make sure original projections are being defined correctly

### Date: May 12, 2022 & Oct 2023

### Version of R:  R version 4.0.3 & 4.2.1

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

KF_survey <- read.csv("./RestorationSites/ConservationPriority.csv")


## create a table with species name, latitude, and longitude columns to be able to use sf command

KF_pts <- KF_survey[,c("NAME", "LON", "LAT")]


## Converting to a special features and transforming to aea by defining the projection 

KF_pts <- st_as_sf(KF_pts, coords = c("LON", "LAT"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84")
KF_pts_aea <- st_transform(KF_pts, crs= "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs") 
KF_pts_aea
plot(KF_pts_aea)

## creating Longitude (x) and Latitude (y) in meters, converting back to a data frame
KF_pts_aea <- KF_pts_aea %>%
  dplyr::mutate(Long_m = sf::st_coordinates(.)[,1],
                Lat_m = sf::st_coordinates(.)[,2])
plot(st_geometry(KF_pts_aea))


KF_pts_aea <- KF_pts_aea %>%
  st_drop_geometry()

KF_pts_aea <- as.data.frame(KF_pts_aea)
class(KF_pts_aea)


## creating final data frame to save as the new csv - binding "metadata" with the aea coordinates

KF_pts_aea <- KF_pts_aea%>%
  rename(NAME = NAME)

KF_metadata <- cbind(KF_pts_aea, KF_survey)
KF_metadata <- subset(KF_metadata, select= -c(NAME))


## Saving localities as new CSV with aea coordinates

write.csv(KF_metadata, file="./RestorationSites/ConservationPriority_aea.csv", row.names=FALSE)

########################### END SECTION ###############################

########################## FINAL COMMENTS ############################

########################### END SCRIPT ###############################