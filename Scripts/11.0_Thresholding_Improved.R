########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Kaegan Finn

### Goal of this Script: 

# Use input localities to determined a threshold to make the continuous prediction surfaces into binary surfaces 

# Using 10th percentile (Rosner-Katz et al. 2020) of the prediction values for the input localities

# Using prediction surfaces for the study extents 

### Notes:  

#Did not like how script 11.1 was written, so made this new one for myself. Seemed over complicated, Plus the percentiles weren't even correct. 

### Date: Jan 23 2023

### Version of R:  R version 4.2.1
########################### END SECTION ##############################

############################ LIBRARIES ###############################

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(dismo)
library(sf)
library(terra)
library(rgdal)
library(dplyr)
library(raster)


rm(list=ls())
########################### END SECTION ##############################

###################### FUNCTION DEFINITIONS ##########################

## NA

########################### END SECTION ##############################

########################## DATA AND ANALYSIS #########################


## Set working directory

getwd()
setwd()


### Binary surfaces Future predictions ####
###### Range study extent ######

## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

ClimOnly_RB_ecotone <- raster("./Maxent/ClimateOnly/RB/ecotone/ClimOnly_RB_ecotone_log.tif")
ClimOnly_RB_ecotone_ssp245 <- raster("./Maxent/ClimateOnly/RB/ecotone/ClimOnly_RB_ecotone_ssp245.tif")
ClimOnly_RB_ecotone_ssp585 <- raster("./Maxent/ClimateOnly/RB/ecotone/ClimOnly_RB_ecotone_ssp585.tif")

plot(ClimOnly_RB_ecotone_ssp245)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

locs <- read.csv("./Input_localities/model_subsets/LTSA_ecotone_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

values <- terra::extract(ClimOnly_RB_ecotone,locs[,c("Long_m", "Lat_m")])
values <- cbind(locs, values)

## create 10 percentile threshold (to get rid of possible error/sink populations)

Threshold <- quantile(values$values, 0.1, na.rm = TRUE)
Threshold

## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

LTSA_ecotone_m <- c(0, Threshold, 0, Threshold, 1, 1)

LTSA_ecotone_matrix <- matrix(LTSA_ecotone_m, ncol = 3, byrow =TRUE)
LTSA_ecotone_matrix

LTSA_ecotone_binary <- reclassify(ClimOnly_RB_ecotone_ssp585, LTSA_ecotone_matrix)
plot(LTSA_ecotone_binary)

writeRaster(LTSA_ecotone_binary, "./Binary_surfaces/ClimOnly_RB_ecotone_binary_ssp585.tif", overwrite = TRUE)








##### REGION OF INTEREST PROJECTIONS (GENETIC AND ECOTONE) #####
### ########### ClimOnly + RB ##################
###### ecoregion study extent ######
## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

ClimOnly_RB_ecotone <- raster("./Maxent/ClimateOnly/RB/ecotone/ClimOnly_RB_ecotone_log.tif")
plot(ClimOnly_RB_ecotone)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

locs <- read.csv("./Input_localities/model_subsets/LTSA_ecotone_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

values <- terra::extract(ClimOnly_RB_ecotone,locs[,c("Long_m", "Lat_m")])
values <- cbind(locs, values)

## create 10 percentile threshold (to get rid of possible error/sink populations)

Threshold <- quantile(values$values, 0.1, na.rm = TRUE)
Threshold

## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

LTSA_ecotone_m <- c(0, Threshold, 0, Threshold, 1, 1)

LTSA_ecotone_matrix <- matrix(LTSA_ecotone_m, ncol = 3, byrow =TRUE)
LTSA_ecotone_matrix

#RECLASSIFY THE REGION OF INTEREST RASTER BASED ON THE FULL EXTENT THRESHOLD!
ClimOnly_RB_ecotone_ROI <- raster("./Maxent/Region of Interest Surfaces/ClimOnly_RB_ecotone_log_ROI.tif")
plot(ClimOnly_RB_ecotone_ROI)
ClimOnly_RB_ecotone_ROI

LTSA_ecotone_ROI_binary <- reclassify(ClimOnly_RB_ecotone_ROI, LTSA_ecotone_matrix)
plot(LTSA_ecotone_ROI_binary)

writeRaster(LTSA_ecotone_ROI_binary, "./Binary_surfaces/ClimOnly_RB_ecotone_binary_ROI.tif", overwrite = TRUE)

###### genetic study extent ######

## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

ClimOnly_RB_genetic <- raster("./Maxent/ClimateOnly/RB/genetic/ClimOnly_RB_genetic_log.tif")
plot(ClimOnly_RB_genetic)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

locs <- read.csv("./Input_localities/model_subsets/LTSA_genetic_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

values <- terra::extract(ClimOnly_RB_genetic,locs[,c("Long_m", "Lat_m")])
values <- cbind(locs, values)

## create 10 percentile threshold (to get rid of possible error/sink populations)

Threshold <- quantile(values$values, 0.1, na.rm = TRUE)
Threshold

## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

LTSA_genetic_m <- c(0, Threshold, 0, Threshold, 1, 1)

LTSA_genetic_matrix <- matrix(LTSA_genetic_m, ncol = 3, byrow =TRUE)
LTSA_genetic_matrix

#RECLASSIFY THE REGION OF INTEREST RASTER BASED ON THE FULL EXTENT THRESHOLD!
ClimOnly_RB_genetic_ROI <- raster("./Maxent/Region of Interest Surfaces/ClimOnly_RB_genetic_log_ROI.tif")
plot(ClimOnly_RB_genetic_ROI)

LTSA_genetic_ROI_binary <- reclassify(ClimOnly_RB_genetic_ROI, LTSA_genetic_matrix)
plot(LTSA_genetic_ROI_binary)

writeRaster(LTSA_genetic_ROI_binary, "./Binary_surfaces/ClimOnly_RB_genetic_binary_ROI.tif", overwrite = TRUE)





### ########### ClimNonClim + RB ##################
###### ecoregion study extent ######
## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

ClimNonClim_RB_ecotone <- raster("./Maxent/ClimNonClim/RB/ecotone/ClimNonClim_RB_ecotone_log.tif")
plot(ClimNonClim_RB_ecotone)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

locs <- read.csv("./Input_localities/model_subsets/LTSA_ecotone_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

values <- terra::extract(ClimNonClim_RB_ecotone,locs[,c("Long_m", "Lat_m")])
values <- cbind(locs, values)

## create 10 percentile threshold (to get rid of possible error/sink populations)

Threshold <- quantile(values$values, 0.1, na.rm = TRUE)
Threshold

## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

LTSA_ecotone_m <- c(0, Threshold, 0, Threshold, 1, 1)

LTSA_ecotone_matrix <- matrix(LTSA_ecotone_m, ncol = 3, byrow =TRUE)
LTSA_ecotone_matrix

#RECLASSIFY THE REGION OF INTEREST RASTER BASED ON THE FULL EXTENT THRESHOLD!
ClimNonClim_RB_ecotone_ROI <- raster("./Maxent/Region of Interest Surfaces/ClimNonClim_RB_ecotone_log_ROI.tif")
plot(ClimNonClim_RB_ecotone_ROI)

LTSA_ecotone_ROI_binary <- reclassify(ClimNonClim_RB_ecotone_ROI, LTSA_ecotone_matrix)
plot(LTSA_ecotone_ROI_binary)

writeRaster(LTSA_ecotone_ROI_binary, "./Binary_surfaces/ClimNonClim_RB_ecotone_binary_ROI.tif", overwrite = TRUE)

###### genetic study extent ######

## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

ClimNonClim_RB_genetic <- raster("./Maxent/ClimNonClim/RB/genetic/ClimNonClim_RB_genetic_log.tif")
plot(ClimNonClim_RB_genetic)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

locs <- read.csv("./Input_localities/model_subsets/LTSA_genetic_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

values <- terra::extract(ClimNonClim_RB_genetic,locs[,c("Long_m", "Lat_m")])
values <- cbind(locs, values)

## create 10 percentile threshold (to get rid of possible error/sink populations)

Threshold <- quantile(values$values, 0.1, na.rm = TRUE)
Threshold

## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

LTSA_genetic_m <- c(0, Threshold, 0, Threshold, 1, 1)

LTSA_genetic_matrix <- matrix(LTSA_genetic_m, ncol = 3, byrow =TRUE)
LTSA_genetic_matrix

#RECLASSIFY THE REGION OF INTEREST RASTER BASED ON THE FULL EXTENT THRESHOLD!
ClimNonClim_RB_genetic_ROI <- raster("./Maxent/Region of Interest Surfaces/ClimNonClim_RB_genetic_log_ROI.tif")
plot(ClimNonClim_RB_genetic_ROI)

LTSA_genetic_ROI_binary <- reclassify(ClimNonClim_RB_genetic_ROI, LTSA_genetic_matrix)
plot(LTSA_genetic_ROI_binary)

writeRaster(LTSA_genetic_ROI_binary, "./Binary_surfaces/ClimNonClim_RB_genetic_binary_ROI.tif", overwrite = TRUE)

### ########### ClimOnly + TGB ##################
###### ecoregion study extent ######
## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

ClimOnly_TGB_ecotone <- raster("./Maxent/ClimateOnly/TGB/ecotone/ClimOnly_TGB_ecotone_log.tif")
plot(ClimOnly_TGB_ecotone)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

locs <- read.csv("./Input_localities/model_subsets/LTSA_ecotone_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

values <- terra::extract(ClimOnly_TGB_ecotone,locs[,c("Long_m", "Lat_m")])
values <- cbind(locs, values)

## create 10 percentile threshold (to get rid of possible error/sink populations)

Threshold <- quantile(values$values, 0.1, na.rm = TRUE)
Threshold

## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

LTSA_ecotone_m <- c(0, Threshold, 0, Threshold, 1, 1)

LTSA_ecotone_matrix <- matrix(LTSA_ecotone_m, ncol = 3, byrow =TRUE)
LTSA_ecotone_matrix

#RECLASSIFY THE REGION OF INTEREST RASTER BASED ON THE FULL EXTENT THRESHOLD!
ClimOnly_TGB_ecotone_ROI <- raster("./Maxent/Region of Interest Surfaces/ClimOnly_TGB_ecotone_log_ROI.tif")
plot(ClimOnly_TGB_ecotone_ROI)

LTSA_ecotone_ROI_binary <- reclassify(ClimOnly_TGB_ecotone_ROI, LTSA_ecotone_matrix)
plot(LTSA_ecotone_ROI_binary)

writeRaster(LTSA_ecotone_ROI_binary, "./Binary_surfaces/ClimOnly_TGB_ecotone_binary_ROI.tif", overwrite = TRUE)

###### genetic study extent ######

## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

ClimOnly_TGB_genetic <- raster("./Maxent/ClimateOnly/TGB/genetic/ClimOnly_TGB_genetic_log.tif")
plot(ClimOnly_TGB_genetic)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

locs <- read.csv("./Input_localities/model_subsets/LTSA_genetic_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

values <- terra::extract(ClimOnly_TGB_genetic,locs[,c("Long_m", "Lat_m")])
values <- cbind(locs, values)

## create 10 percentile threshold (to get rid of possible error/sink populations)

Threshold <- quantile(values$values, 0.1, na.rm = TRUE)
Threshold

## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

LTSA_genetic_m <- c(0, Threshold, 0, Threshold, 1, 1)

LTSA_genetic_matrix <- matrix(LTSA_genetic_m, ncol = 3, byrow =TRUE)
LTSA_genetic_matrix

#RECLASSIFY THE REGION OF INTEREST RASTER BASED ON THE FULL EXTENT THRESHOLD!
ClimOnly_TGB_genetic_ROI <- raster("./Maxent/Region of Interest Surfaces/ClimOnly_TGB_genetic_log_ROI.tif")
plot(ClimOnly_TGB_genetic_ROI)

LTSA_genetic_ROI_binary <- reclassify(ClimOnly_TGB_genetic_ROI, LTSA_genetic_matrix)
plot(LTSA_genetic_ROI_binary)

writeRaster(LTSA_genetic_ROI_binary, "./Binary_surfaces/ClimOnly_TGB_genetic_binary_ROI.tif", overwrite = TRUE)





### ########### ClimNonClim + TGB ##################
###### ecoregion study extent ######
## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

ClimNonClim_TGB_ecotone <- raster("./Maxent/ClimNonClim/TGB/ecotone/ClimNonClim_TGB_ecotone_log.tif")
plot(ClimNonClim_TGB_ecotone)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

locs <- read.csv("./Input_localities/model_subsets/LTSA_ecotone_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

values <- terra::extract(ClimNonClim_TGB_ecotone,locs[,c("Long_m", "Lat_m")])
values <- cbind(locs, values)

## create 10 percentile threshold (to get rid of possible error/sink populations)

Threshold <- quantile(values$values, 0.1, na.rm = TRUE)
Threshold

## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

LTSA_ecotone_m <- c(0, Threshold, 0, Threshold, 1, 1)

LTSA_ecotone_matrix <- matrix(LTSA_ecotone_m, ncol = 3, byrow =TRUE)
LTSA_ecotone_matrix

#RECLASSIFY THE REGION OF INTEREST RASTER BASED ON THE FULL EXTENT THRESHOLD!
ClimNonClim_TGB_ecotone_ROI <- raster("./Maxent/Region of Interest Surfaces/ClimNonClim_TGB_ecotone_log_ROI.tif")
plot(ClimNonClim_TGB_ecotone_ROI)

LTSA_ecotone_ROI_binary <- reclassify(ClimNonClim_TGB_ecotone_ROI, LTSA_ecotone_matrix)
plot(LTSA_ecotone_ROI_binary)

writeRaster(LTSA_ecotone_ROI_binary, "./Binary_surfaces/ClimNonClim_TGB_ecotone_binary_ROI.tif", overwrite = TRUE)

###### genetic study extent ######

## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

ClimNonClim_TGB_genetic <- raster("./Maxent/ClimNonClim/TGB/genetic/ClimNonClim_TGB_genetic_log.tif")
plot(ClimNonClim_TGB_genetic)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

locs <- read.csv("./Input_localities/model_subsets/LTSA_genetic_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

values <- terra::extract(ClimNonClim_TGB_genetic,locs[,c("Long_m", "Lat_m")])
values <- cbind(locs, values)

## create 10 percentile threshold (to get rid of possible error/sink populations)

Threshold <- quantile(values$values, 0.1, na.rm = TRUE)
Threshold

## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

LTSA_genetic_m <- c(0, Threshold, 0, Threshold, 1, 1)

LTSA_genetic_matrix <- matrix(LTSA_genetic_m, ncol = 3, byrow =TRUE)
LTSA_genetic_matrix

#RECLASSIFY THE REGION OF INTEREST RASTER BASED ON THE FULL EXTENT THRESHOLD!
ClimNonClim_TGB_genetic_ROI <- raster("./Maxent/Region of Interest Surfaces/ClimNonClim_TGB_genetic_log_ROI.tif")
plot(ClimNonClim_TGB_genetic_ROI)

LTSA_genetic_ROI_binary <- reclassify(ClimNonClim_TGB_genetic_ROI, LTSA_genetic_matrix)
plot(LTSA_genetic_ROI_binary)

writeRaster(LTSA_genetic_ROI_binary, "./Binary_surfaces/ClimNonClim_TGB_genetic_binary_ROI.tif", overwrite = TRUE)






###### FULL MODELLING EXTENTS #######
### ########### ClimOnly + RB ##################
###### range study extent ######

## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

ClimOnly_RB_Range <- raster("./Maxent/ClimateOnly/RB/Range/ClimOnly_RB_Range_log.tif")
plot(ClimOnly_RB_Range)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

locs <- read.csv("./Input_localities/model_subsets/LTSA_range_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

values <- terra::extract(ClimOnly_RB_Range,locs[,c("Long_m", "Lat_m")])
values <- cbind(locs, values)

## create 10 percentile threshold (to get rid of possible error/sink populations)

Threshold <- quantile(values$values, 0.1, na.rm = TRUE)
Threshold

## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

LTSA_range_m <- c(0, Threshold, 0, Threshold, 1, 1)

LTSA_range_matrix <- matrix(LTSA_range_m, ncol = 3, byrow =TRUE)
LTSA_range_matrix

LTSA_range_binary <- reclassify(ClimOnly_RB_Range, LTSA_range_matrix)
plot(LTSA_range_binary)

writeRaster(LTSA_range_binary, "./Binary_surfaces/ClimOnly_RB_Range_binary.tif", overwrite = TRUE)

###### Political study extent ######

## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

ClimOnly_RB_political <- raster("./Maxent/ClimateOnly/RB/political/ClimOnly_RB_political_log.tif")
plot(ClimOnly_RB_political)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

locs <- read.csv("./Input_localities/model_subsets/LTSA_political_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

values <- terra::extract(ClimOnly_RB_political,locs[,c("Long_m", "Lat_m")])
values <- cbind(locs, values)

## create 10 percentile threshold (to get rid of possible error/sink populations)

Threshold <- quantile(values$values, 0.1, na.rm = TRUE)
Threshold

## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

LTSA_political_m <- c(0, Threshold, 0, Threshold, 1, 1)

LTSA_political_matrix <- matrix(LTSA_political_m, ncol = 3, byrow =TRUE)
LTSA_political_matrix

LTSA_political_binary <- reclassify(ClimOnly_RB_political, LTSA_political_matrix)
plot(LTSA_political_binary)

writeRaster(LTSA_political_binary, "./Binary_surfaces/ClimOnly_RB_political_binary.tif", overwrite = TRUE)

###### ecotone study extent ######

## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

ClimOnly_RB_ecotone <- raster("./Maxent/ClimateOnly/RB/ecotone/ClimOnly_RB_ecotone_log.tif")
plot(ClimOnly_RB_ecotone)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

locs <- read.csv("./Input_localities/model_subsets/LTSA_ecotone_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

values <- terra::extract(ClimOnly_RB_ecotone,locs[,c("Long_m", "Lat_m")])
values <- cbind(locs, values)

## create 10 percentile threshold (to get rid of possible error/sink populations)

Threshold <- quantile(values$values, 0.1, na.rm = TRUE)
Threshold

## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

LTSA_ecotone_m <- c(0, Threshold, 0, Threshold, 1, 1)

LTSA_ecotone_matrix <- matrix(LTSA_ecotone_m, ncol = 3, byrow =TRUE)
LTSA_ecotone_matrix

LTSA_ecotone_binary <- reclassify(ClimOnly_RB_ecotone, LTSA_ecotone_matrix)
plot(LTSA_ecotone_binary)

writeRaster(LTSA_ecotone_binary, "./Binary_surfaces/ClimOnly_RB_ecotone_binary.tif", overwrite = TRUE)

###### genetic study extent ######

## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

ClimOnly_RB_genetic <- raster("./Maxent/ClimateOnly/RB/genetic/ClimOnly_RB_genetic_log.tif")
plot(ClimOnly_RB_genetic)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

locs <- read.csv("./Input_localities/model_subsets/LTSA_genetic_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

values <- terra::extract(ClimOnly_RB_genetic,locs[,c("Long_m", "Lat_m")])
values <- cbind(locs, values)

## create 10 percentile threshold (to get rid of possible error/sink populations)

Threshold <- quantile(values$values, 0.1, na.rm = TRUE)
Threshold

## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

LTSA_genetic_m <- c(0, Threshold, 0, Threshold, 1, 1)

LTSA_genetic_matrix <- matrix(LTSA_genetic_m, ncol = 3, byrow =TRUE)
LTSA_genetic_matrix

LTSA_genetic_binary <- reclassify(ClimOnly_RB_genetic, LTSA_genetic_matrix)
plot(LTSA_genetic_binary)

writeRaster(LTSA_genetic_binary, "./Binary_surfaces/ClimOnly_RB_genetic_binary.tif", overwrite = TRUE)




### ########### ClimNonClim + RB ##################
###### range study extent ######

## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

ClimNonClim_RB_Range <- raster("./Maxent/ClimNonClim/RB/Range/ClimNonClim_RB_Range_log.tif")
plot(ClimNonClim_RB_Range)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

locs <- read.csv("./Input_localities/model_subsets/LTSA_range_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

values <- terra::extract(ClimNonClim_RB_Range,locs[,c("Long_m", "Lat_m")])
values <- cbind(locs, values)

## create 10 percentile threshold (to get rid of possible error/sink populations)

Threshold <- quantile(values$values, 0.1, na.rm = TRUE)
Threshold

## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

LTSA_range_m <- c(0, Threshold, 0, Threshold, 1, 1)

LTSA_range_matrix <- matrix(LTSA_range_m, ncol = 3, byrow =TRUE)
LTSA_range_matrix

LTSA_range_binary <- reclassify(ClimNonClim_RB_Range, LTSA_range_matrix)
plot(LTSA_range_binary)

writeRaster(LTSA_range_binary, "./Binary_surfaces/ClimNonClim_RB_Range_binary.tif", overwrite = TRUE)

###### Political study extent ######

## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

ClimNonClim_RB_political <- raster("./Maxent/ClimNonClim/RB/political/ClimNonClim_RB_political_log.tif")
plot(ClimNonClim_RB_political)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

locs <- read.csv("./Input_localities/model_subsets/LTSA_political_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

values <- terra::extract(ClimNonClim_RB_political,locs[,c("Long_m", "Lat_m")])
values <- cbind(locs, values)

## create 10 percentile threshold (to get rid of possible error/sink populations)

Threshold <- quantile(values$values, 0.1, na.rm = TRUE)
Threshold

## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

LTSA_political_m <- c(0, Threshold, 0, Threshold, 1, 1)

LTSA_political_matrix <- matrix(LTSA_political_m, ncol = 3, byrow =TRUE)
LTSA_political_matrix

LTSA_political_binary <- reclassify(ClimNonClim_RB_political, LTSA_political_matrix)
plot(LTSA_political_binary)

writeRaster(LTSA_political_binary, "./Binary_surfaces/ClimNonClim_RB_political_binary.tif", overwrite = TRUE)

###### ecotone study extent ######

## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

ClimNonClim_RB_ecotone <- raster("./Maxent/ClimNonClim/RB/ecotone/ClimNonClim_RB_ecotone_log.tif")
plot(ClimNonClim_RB_ecotone)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

locs <- read.csv("./Input_localities/model_subsets/LTSA_ecotone_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

values <- terra::extract(ClimNonClim_RB_ecotone,locs[,c("Long_m", "Lat_m")])
values <- cbind(locs, values)

## create 10 percentile threshold (to get rid of possible error/sink populations)

Threshold <- quantile(values$values, 0.1, na.rm = TRUE)
Threshold

## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

LTSA_ecotone_m <- c(0, Threshold, 0, Threshold, 1, 1)

LTSA_ecotone_matrix <- matrix(LTSA_ecotone_m, ncol = 3, byrow =TRUE)
LTSA_ecotone_matrix

LTSA_ecotone_binary <- reclassify(ClimNonClim_RB_ecotone, LTSA_ecotone_matrix)
plot(LTSA_ecotone_binary)

writeRaster(LTSA_ecotone_binary, "./Binary_surfaces/ClimNonClim_RB_ecotone_binary.tif", overwrite = TRUE)

###### genetic study extent ######

## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

ClimNonClim_RB_genetic <- raster("./Maxent/ClimNonClim/RB/genetic/ClimNonClim_RB_genetic_log.tif")
plot(ClimNonClim_RB_genetic)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

locs <- read.csv("./Input_localities/model_subsets/LTSA_genetic_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

values <- terra::extract(ClimNonClim_RB_genetic,locs[,c("Long_m", "Lat_m")])
values <- cbind(locs, values)

## create 10 percentile threshold (to get rid of possible error/sink populations)

Threshold <- quantile(values$values, 0.1, na.rm = TRUE)
Threshold

## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

LTSA_genetic_m <- c(0, Threshold, 0, Threshold, 1, 1)

LTSA_genetic_matrix <- matrix(LTSA_genetic_m, ncol = 3, byrow =TRUE)
LTSA_genetic_matrix

LTSA_genetic_binary <- reclassify(ClimNonClim_RB_genetic, LTSA_genetic_matrix)
plot(LTSA_genetic_binary)

writeRaster(LTSA_genetic_binary, "./Binary_surfaces/ClimNonClim_RB_genetic_binary.tif", overwrite = TRUE)


### ########### ClimOnly + TGB ##################
###### range study extent ######

## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

ClimOnly_TGB_Range <- raster("./Maxent/ClimateOnly/TGB/Range/ClimOnly_TGB_Range_log.tif")
plot(ClimOnly_TGB_Range)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

locs <- read.csv("./Input_localities/model_subsets/LTSA_range_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

values <- terra::extract(ClimOnly_TGB_Range,locs[,c("Long_m", "Lat_m")])
values <- cbind(locs, values)

## create 10 percentile threshold (to get rid of possible error/sink populations)

Threshold <- quantile(values$values, 0.1, na.rm = TRUE)
Threshold

## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

LTSA_range_m <- c(0, Threshold, 0, Threshold, 1, 1)

LTSA_range_matrix <- matrix(LTSA_range_m, ncol = 3, byrow =TRUE)
LTSA_range_matrix

LTSA_range_binary <- reclassify(ClimOnly_TGB_Range, LTSA_range_matrix)
plot(LTSA_range_binary)

writeRaster(LTSA_range_binary, "./Binary_surfaces/ClimOnly_TGB_Range_binary.tif", overwrite = TRUE)

###### Political study extent ######

## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

ClimOnly_TGB_political <- raster("./Maxent/ClimateOnly/TGB/political/ClimOnly_TGB_political_log.tif")
plot(ClimOnly_TGB_political)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

locs <- read.csv("./Input_localities/model_subsets/LTSA_political_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

values <- terra::extract(ClimOnly_TGB_political,locs[,c("Long_m", "Lat_m")])
values <- cbind(locs, values)

## create 10 percentile threshold (to get rid of possible error/sink populations)

Threshold <- quantile(values$values, 0.1, na.rm = TRUE)
Threshold

## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

LTSA_political_m <- c(0, Threshold, 0, Threshold, 1, 1)

LTSA_political_matrix <- matrix(LTSA_political_m, ncol = 3, byrow =TRUE)
LTSA_political_matrix

LTSA_political_binary <- reclassify(ClimOnly_TGB_political, LTSA_political_matrix)
plot(LTSA_political_binary)

writeRaster(LTSA_political_binary, "./Binary_surfaces/ClimOnly_TGB_political_binary.tif", overwrite = TRUE)

###### ecotone study extent ######

## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

ClimOnly_TGB_ecotone <- raster("./Maxent/ClimateOnly/TGB/ecotone/ClimOnly_TGB_ecotone_log.tif")
plot(ClimOnly_TGB_ecotone)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

locs <- read.csv("./Input_localities/model_subsets/LTSA_ecotone_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

values <- terra::extract(ClimOnly_TGB_ecotone,locs[,c("Long_m", "Lat_m")])
values <- cbind(locs, values)

## create 10 percentile threshold (to get rid of possible error/sink populations)

Threshold <- quantile(values$values, 0.1, na.rm = TRUE)
Threshold

## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

LTSA_ecotone_m <- c(0, Threshold, 0, Threshold, 1, 1)

LTSA_ecotone_matrix <- matrix(LTSA_ecotone_m, ncol = 3, byrow =TRUE)
LTSA_ecotone_matrix

LTSA_ecotone_binary <- reclassify(ClimOnly_TGB_ecotone, LTSA_ecotone_matrix)
plot(LTSA_ecotone_binary)

writeRaster(LTSA_ecotone_binary, "./Binary_surfaces/ClimOnly_TGB_ecotone_binary.tif", overwrite = TRUE)

###### genetic study extent ######

## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

ClimOnly_TGB_genetic <- raster("./Maxent/ClimateOnly/TGB/genetic/ClimOnly_TGB_genetic_log.tif")
plot(ClimOnly_TGB_genetic)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

locs <- read.csv("./Input_localities/model_subsets/LTSA_genetic_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

values <- terra::extract(ClimOnly_TGB_genetic,locs[,c("Long_m", "Lat_m")])
values <- cbind(locs, values)

## create 10 percentile threshold (to get rid of possible error/sink populations)

Threshold <- quantile(values$values, 0.1, na.rm = TRUE)
Threshold

## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

LTSA_genetic_m <- c(0, Threshold, 0, Threshold, 1, 1)

LTSA_genetic_matrix <- matrix(LTSA_genetic_m, ncol = 3, byrow =TRUE)
LTSA_genetic_matrix

LTSA_genetic_binary <- reclassify(ClimOnly_TGB_genetic, LTSA_genetic_matrix)
plot(LTSA_genetic_binary)

writeRaster(LTSA_genetic_binary, "./Binary_surfaces/ClimOnly_TGB_genetic_binary.tif", overwrite = TRUE)





### ########### ClimNonClim + TGB ##################
###### range study extent ######

## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

ClimNonClim_TGB_Range <- raster("./Maxent/ClimNonClim/TGB/Range/ClimNonClim_TGB_Range_log.tif")
plot(ClimNonClim_TGB_Range)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

locs <- read.csv("./Input_localities/model_subsets/LTSA_range_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

values <- terra::extract(ClimNonClim_TGB_Range,locs[,c("Long_m", "Lat_m")])
values <- cbind(locs, values)

## create 10 percentile threshold (to get rid of possible error/sink populations)

Threshold <- quantile(values$values, 0.1, na.rm = TRUE)
Threshold

## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

LTSA_range_m <- c(0, Threshold, 0, Threshold, 1, 1)

LTSA_range_matrix <- matrix(LTSA_range_m, ncol = 3, byrow =TRUE)
LTSA_range_matrix

LTSA_range_binary <- reclassify(ClimNonClim_TGB_Range, LTSA_range_matrix)
plot(LTSA_range_binary)

writeRaster(LTSA_range_binary, "./Binary_surfaces/ClimNonClim_TGB_Range_binary.tif", overwrite = TRUE)

###### Political study extent ######

## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

ClimNonClim_TGB_political <- raster("./Maxent/ClimNonClim/TGB/political/ClimNonClim_TGB_political_log.tif")
plot(ClimNonClim_TGB_political)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

locs <- read.csv("./Input_localities/model_subsets/LTSA_political_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

values <- terra::extract(ClimNonClim_TGB_political,locs[,c("Long_m", "Lat_m")])
values <- cbind(locs, values)

## create 10 percentile threshold (to get rid of possible error/sink populations)

Threshold <- quantile(values$values, 0.1, na.rm = TRUE)
Threshold

## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

LTSA_political_m <- c(0, Threshold, 0, Threshold, 1, 1)

LTSA_political_matrix <- matrix(LTSA_political_m, ncol = 3, byrow =TRUE)
LTSA_political_matrix

LTSA_political_binary <- reclassify(ClimNonClim_TGB_political, LTSA_political_matrix)
plot(LTSA_political_binary)

writeRaster(LTSA_political_binary, "./Binary_surfaces/ClimNonClim_TGB_political_binary.tif", overwrite = TRUE)

###### ecotone study extent ######

## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

ClimNonClim_TGB_ecotone <- raster("./Maxent/ClimNonClim/TGB/ecotone/ClimNonClim_TGB_ecotone_log.tif")
plot(ClimNonClim_TGB_ecotone)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

locs <- read.csv("./Input_localities/model_subsets/LTSA_ecotone_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

values <- terra::extract(ClimNonClim_TGB_ecotone,locs[,c("Long_m", "Lat_m")])
values <- cbind(locs, values)

## create 10 percentile threshold (to get rid of possible error/sink populations)

Threshold <- quantile(values$values, 0.1, na.rm = TRUE)
Threshold

## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

LTSA_ecotone_m <- c(0, Threshold, 0, Threshold, 1, 1)

LTSA_ecotone_matrix <- matrix(LTSA_ecotone_m, ncol = 3, byrow =TRUE)
LTSA_ecotone_matrix

LTSA_ecotone_binary <- reclassify(ClimNonClim_TGB_ecotone, LTSA_ecotone_matrix)
plot(LTSA_ecotone_binary)

writeRaster(LTSA_ecotone_binary, "./Binary_surfaces/ClimNonClim_TGB_ecotone_binary.tif", overwrite = TRUE)

###### genetic study extent ######

## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

ClimNonClim_TGB_genetic <- raster("./Maxent/ClimNonClim/TGB/genetic/ClimNonClim_TGB_genetic_log.tif")
plot(ClimNonClim_TGB_genetic)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

locs <- read.csv("./Input_localities/model_subsets/LTSA_genetic_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

values <- terra::extract(ClimNonClim_TGB_genetic,locs[,c("Long_m", "Lat_m")])
values <- cbind(locs, values)

## create 10 percentile threshold (to get rid of possible error/sink populations)

Threshold <- quantile(values$values, 0.1, na.rm = TRUE)
Threshold

## Creating a binary prediction surface using the <=10th percentile as 0 and saving as new Raster 

LTSA_genetic_m <- c(0, Threshold, 0, Threshold, 1, 1)

LTSA_genetic_matrix <- matrix(LTSA_genetic_m, ncol = 3, byrow =TRUE)
LTSA_genetic_matrix

LTSA_genetic_binary <- reclassify(ClimNonClim_TGB_genetic, LTSA_genetic_matrix)
plot(LTSA_genetic_binary)

writeRaster(LTSA_genetic_binary, "./Binary_surfaces/ClimNonClim_TGB_genetic_binary.tif", overwrite = TRUE)


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################

########################### END SCRIPT ###############################