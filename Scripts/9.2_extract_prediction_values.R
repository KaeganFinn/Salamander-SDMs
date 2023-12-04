########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs 

### Author: Jayna Bergman & Kaegan Finn

### Goal of this Script: 

# Extracts data from the predicted habitat suitability layers (from MaxEnt models made with random background points) for the species that we have independent data for (to be used to do an occupancy analysis)

# One csv created with all species predictions at each site 

### Notes:  

### Date: September 30, 2022, December 2022

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

KF_survey <- read.csv("./Independent_data/KF_Surveys/KF_PresAbs_aea.csv")
str(KF_survey)

KF_pts <- subset(KF_survey, select = c("NAME","LAT","LON"))

JB <- rast("./JaynasModels/LTSA_political_prediction_surface_random.tif")
plot(JB)

crs(JB) <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

plot(JB)


survey_suit <- terra::extract(JB, KF_pts[,c("LAT", "LON")])

################################# ClimOnly + RB  ################################

##### range #####

## read in prediction raster 
range_sdm <- rast("./Maxent/ClimateOnly/RB/Range/ClimOnly_RB_Range_log.tif")
plot(range_sdm)

## extract values from prediction layer under WLNP Sites

range_sdm_predictions <- terra::extract(range_sdm, KF_pts[,c("Long_m", "Lat_m")])

## cleaning csv 

range_sdm_predictions <- range_sdm_predictions %>%
  rename(ClimOnly_RB_Range_SDM = layer)

SDM_predictions <- cbind(KF_pts, range_sdm_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID,SITE_ID,Long_m,Lat_m))


##### political #####

## read in prediction raster 
pol_sdm <- rast("./Maxent/ClimateOnly/RB/political/ClimOnly_RB_political_log.tif")
plot(pol_sdm)


## extract values from prediction layer under WLNP Sites

pol_sdm_predictions <- terra::extract(pol_sdm, KF_pts[,c("Long_m", "Lat_m")])

## cleaning csv 
pol_sdm_predictions <- pol_sdm_predictions %>%
  rename(ClimOnly_RB_pol_SDM = layer)

write.csv(SDM_predictions,"./model2.csv")

SDM_predictions <- cbind(KF_pts, pol_sdm_predictions)



SDM_predictions <- subset(SDM_predictions, select = -c(ID))


##### ecotone #####

## read in prediction raster 
gen_sdm <- rast("./Maxent/ClimateOnly/RB/genetic/ClimOnly_RB_genetic_log.tif")
plot(gen_sdm)


## extract values from prediction layer under WLNP Sites

gen_sdm_predictions <- terra::extract(gen_sdm, KF_pts[,c("Long_m", "Lat_m")])

## cleaning csv 
gen_sdm_predictions <- gen_sdm_predictions %>%
  rename(ClimOnly_RB_gen_SDM = layer)

SDM_predictions <- cbind(SDM_predictions, gen_sdm_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID))

##### genetic #####

## read in prediction raster 
eco_sdm <- rast("./Maxent/ClimateOnly/RB/ecotone/ClimOnly_RB_ecotone_log.tif")
plot(eco_sdm)


## extract values from prediction layer under WLNP Sites

eco_sdm_predictions <- terra::extract(eco_sdm, KF_pts[,c("Long_m", "Lat_m")])

## cleaning csv 
eco_sdm_predictions <- eco_sdm_predictions %>%
  rename(ClimOnly_RB_eco_SDM = layer)

SDM_predictions <- cbind(SDM_predictions, eco_sdm_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID))


#### Saving csv with all species predictions at all study extents with site names

write.csv(SDM_predictions, "./Independent_data/KF_pred_ClimOnly_RB.csv", row.names = FALSE)



################################# NonClimOnly + RB  ################################
## read in prediction raster 
pol_sdm <- rast("./Maxent/NonClimOnly/RB/political/NonClimOnly_RB_political_log.tif")
plot(pol_sdm)


## extract values from prediction layer under WLNP Sites
KF_pts <- read.csv("./KF_pts_74.csv")
str(KF_survey)

KF_pts <- subset(KF_pts, select = c("NAME","Long_m","Lat_m"))


pol_sdm_predictions <- terra::extract(pol_sdm, KF_pts[,c("Long_m", "Lat_m")])

## cleaning csv 
pol_sdm_predictions <- pol_sdm_predictions %>%
  rename(ClimNonClim_RB_pol_SDM = layer)

write.csv(pol_sdm_predictions,"./NonClimOnly_74.csv")

SDM_predictions <- cbind(KF_pts, pol_sdm_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID))




##### range #####

## read in prediction raster 
range_sdm <- rast("./Maxent/ClimNonClim/RB/Range/ClimNonClim_RB_Range_log.tif")
plot(range_sdm)

## extract values from prediction layer under WLNP Sites

range_sdm_predictions <- terra::extract(range_sdm, KF_pts[,c("Long_m", "Lat_m")])

## cleaning csv 

range_sdm_predictions <- range_sdm_predictions %>%
  rename(ClimNonClim_RB_Range_SDM = layer)

SDM_predictions <- cbind(KF_pts, range_sdm_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID,SITE_ID,Long_m,Lat_m))


##### political #####

## read in prediction raster 
pol_sdm <- rast("./Maxent/ClimNonClim/RB/political/ClimNonClim_RB_political_log.tif")
plot(pol_sdm)


## extract values from prediction layer under WLNP Sites

pol_sdm_predictions <- terra::extract(pol_sdm, KF_pts[,c("Long_m", "Lat_m")])

## cleaning csv 
pol_sdm_predictions <- pol_sdm_predictions %>%
  rename(ClimNonClim_RB_pol_SDM = layer)

write.csv(SDM_predictions,"./model1.csv")

SDM_predictions <- cbind(KF_pts, pol_sdm_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID))


##### ecotone #####

## read in prediction raster 
gen_sdm <- rast("./Maxent/ClimNonClim/RB/genetic/ClimNonClim_RB_genetic_log.tif")
plot(gen_sdm)


## extract values from prediction layer under WLNP Sites

gen_sdm_predictions <- terra::extract(gen_sdm, KF_pts[,c("Long_m", "Lat_m")])

## cleaning csv 
gen_sdm_predictions <- gen_sdm_predictions %>%
  rename(ClimNonClim_RB_gen_SDM = layer)

SDM_predictions <- cbind(SDM_predictions, gen_sdm_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID))

##### genetic #####

## read in prediction raster 
eco_sdm <- rast("./Maxent/ClimNonClim/RB/ecotone/ClimNonClim_RB_ecotone_log.tif")
plot(eco_sdm)


## extract values from prediction layer under WLNP Sites

eco_sdm_predictions <- terra::extract(eco_sdm, KF_pts[,c("Long_m", "Lat_m")])

## cleaning csv 
eco_sdm_predictions <- eco_sdm_predictions %>%
  rename(ClimNonClim_RB_eco_SDM = layer)

SDM_predictions <- cbind(SDM_predictions, eco_sdm_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID))


#### Saving csv with all species predictions at all study extents with site names

write.csv(SDM_predictions, "./Independent_data/KF_pred_ClimNonClim_RB.csv", row.names = FALSE)




################################# ClimNonCLim + TGB ################################
rm(list=ls())

KF_survey <- read.csv("./Independent_data/KF_Surveys/KF_PresAbs_aea.csv")
str(KF_survey)

KF_pts <- subset(KF_survey, select = c("SITE_ID", "NAME","Long_m","Lat_m","LTSA" ))

##### range #####

## read in prediction raster 
range_sdm <- rast("./Maxent/ClimNonClim/TGB/Range/ClimNonClim_TGB_Range_log.tif")
plot(range_sdm)

## extract values from prediction layer under WLNP Sites

range_sdm_predictions <- terra::extract(range_sdm, KF_pts[,c("Long_m", "Lat_m")])

## cleaning csv 

range_sdm_predictions <- range_sdm_predictions %>%
  rename(ClimNonClim_TGB_Range_SDM = layer)

SDM_predictions <- cbind(KF_pts, range_sdm_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID,SITE_ID,Long_m,Lat_m))


##### political #####

## read in prediction raster 
pol_sdm <- rast("./Maxent/ClimNonClim/TGB/political/ClimNonClim_TGB_political_log.tif")
plot(pol_sdm)


## extract values from prediction layer under WLNP Sites

pol_sdm_predictions <- terra::extract(pol_sdm, KF_pts[,c("Long_m", "Lat_m")])

## cleaning csv 
pol_sdm_predictions <- pol_sdm_predictions %>%
  rename(ClimNonClim_TGB_pol_SDM = layer)

SDM_predictions <- cbind(SDM_predictions, pol_sdm_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID))


##### ecotone #####

## read in prediction raster 
gen_sdm <- rast("./Maxent/ClimNonClim/TGB/genetic/ClimNonClim_TGB_genetic_log.tif")
plot(gen_sdm)


## extract values from prediction layer under WLNP Sites

gen_sdm_predictions <- terra::extract(gen_sdm, KF_pts[,c("Long_m", "Lat_m")])

## cleaning csv 
gen_sdm_predictions <- gen_sdm_predictions %>%
  rename(ClimNonClim_TGB_gen_SDM = layer)

SDM_predictions <- cbind(SDM_predictions, gen_sdm_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID))

##### genetic #####

## read in prediction raster 
eco_sdm <- rast("./Maxent/ClimNonClim/TGB/ecotone/ClimNonClim_TGB_ecotone_log.tif")
plot(eco_sdm)


## extract values from prediction layer under WLNP Sites

eco_sdm_predictions <- terra::extract(eco_sdm, KF_pts[,c("Long_m", "Lat_m")])

## cleaning csv 
eco_sdm_predictions <- eco_sdm_predictions %>%
  rename(ClimNonClim_TGB_eco_SDM = layer)

SDM_predictions <- cbind(SDM_predictions, eco_sdm_predictions)


SDM_predictions <- subset(SDM_predictions, select = -c(ID))


#### Saving csv with all species predictions at all study extents with site names

write.csv(SDM_predictions, "./Independent_data/KF_pred_ClimNonClim_TGB.csv", row.names = FALSE)

########################### END SECTION ##############################

########################## FINAL COMMENTS ############################

########################### END SCRIPT ###############################