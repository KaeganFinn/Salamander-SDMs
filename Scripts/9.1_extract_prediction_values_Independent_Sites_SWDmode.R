########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs 

### Author: Jayna Bergman, updated by Kaegan Finn Dec 5 2022

### Goal of this Script: 

# Extracts data from the predicted habitat suitability layers (from MaxEnt models) for the species that we have independent data for (to be used to do an occupancy analysis)

# One csv created with all species predictions at each site 

### Notes:  

### Date: September 12, 2022

### Version of R:  R version 4.0.3

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(dismo)
library(sf)
library(terra)
library(rgdal)
library(dplyr)

rm(list=ls())
########################### END SECTION ##############################

###################### FUNCTION DEFINITIONS ##########################

## NA

########################### END SECTION ##############################

########################## DATA AND ANALYSIS #########################

## Set working directory

setwd()

## read in csv for independent survey site locations


KF_survey <- read.csv("./Independent_data/KF_Surveys/KF_PresAbs.csv")
str(KF_survey)

KF_survey <- subset(KF_survey, select = c("ID", "NAME"))

################################# LTSA ################################

##### range #####

## read in prediction raster 

range_sdm <- rast("./Maxent_SWD_mode/LTSA/range/LTSA_range_all_locs/LTSA_range_prediction_surface.tif")
plot(LTSA_range_predict_rast)


## extract values from prediction layer under WLNP Sites

LTSA_range_predictions <- terra::extract(LTSA_range_predict_rast, WLNP_sites[,c("Long_m", "Lat_m")])


## cleaning csv 

LTSA_range_predictions <- LTSA_range_predictions %>%
  rename(LTSA_range = LTSA_range_prediction_surface)

SDM_predictions <- cbind(SDM_predictions, LTSA_range_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID))


##### political #####

## read in prediction raster 

LTSA_political_predict_rast <- rast("./Maxent_SWD_mode/LTSA/political/LTSA_political_all_locs/LTSA_political_prediction_surface.tif")
plot(LTSA_political_predict_rast)


## extract values from prediction layer under WLNP Sites

LTSA_political_predictions <- terra::extract(LTSA_political_predict_rast, WLNP_sites[,c("Long_m", "Lat_m")])


## cleaning csv 

LTSA_political_predictions <- LTSA_political_predictions %>%
  rename(LTSA_political = LTSA_political_prediction_surface)

SDM_predictions <- cbind(SDM_predictions, LTSA_political_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID))


##### ecotone #####

## read in prediction raster 

LTSA_ecotone_predict_rast <- rast("./Maxent_SWD_mode/LTSA/ecotone/LTSA_ecotone_all_locs/LTSA_ecotone_prediction_surface.tif")
plot(LTSA_ecotone_predict_rast)


## extract values from prediction layer under WLNP Sites

LTSA_ecotone_predictions <- terra::extract(LTSA_ecotone_predict_rast, WLNP_sites[,c("Long_m", "Lat_m")])


## cleaning csv 

LTSA_ecotone_predictions <- LTSA_ecotone_predictions %>%
  rename(LTSA_ecotone = LTSA_ecotone_prediction_surface)

SDM_predictions <- cbind(SDM_predictions, LTSA_ecotone_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID))


##### genetic #####

## read in prediction raster 

LTSA_genetic_predict_rast <- rast("./Maxent_SWD_mode/LTSA/genetic/LTSA_genetic_all_locs/LTSA_genetic_prediction_surface.tif")
plot(LTSA_genetic_predict_rast)


## extract values from prediction layer under WLNP Sites

LTSA_genetic_predictions <- terra::extract(LTSA_genetic_predict_rast, WLNP_sites[,c("Long_m", "Lat_m")])


## cleaning csv 

LTSA_genetic_predictions <- LTSA_genetic_predictions %>%
  rename(LTSA_genetic = LTSA_genetic_prediction_surface)

SDM_predictions <- cbind(SDM_predictions, LTSA_genetic_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID))

write.csv(SDM_predictions,"./SDM_presictions.csv")


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################


########################### END SCRIPT ###############################