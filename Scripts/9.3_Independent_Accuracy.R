########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs 

### Author: Jayna Bergman  Updated by Kaegan Finn, Dec 5, 2022

### Goal of this Script: 

# 1) calculates AUC values for independent (presence/absence) dataset  
# 2) saves: average prediction raster made with random pts, input locality predictions, independent locality predictions, independent data stats

### Notes:  

# Loops through different background extents 
# Updated version of script Julie Lee-Yaw wrote for Independent study (spring 2021)

### Date: November 7, 2022

### Version of R:  R version 4.0.3

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(ggpubr)
library(ggplot2)
library(terra)
library(raster)
library(sf)
library(PresenceAbsence)
library(dplyr)

rm(list=ls())
########################### END SECTION ##############################

###################### FUNCTION DEFINITIONS ##########################

## NA

########################### END SECTION ##############################

########################## DATA AND ANALYSIS #########################


## Set working directory

getwd()
setwd()


### ########### ClimOnly + RB ##################

## read in csv with prediction values extracted from prediction surfaces From Script 9.3

predictions <- read.csv("./Independent_data/Subsets/DupRem/KF_pred_ClimOnly_RB.csv")

###### range study extent ######

## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

range_sdm <- rast("./Maxent/ClimateOnly/RB/Range/ClimOnly_RB_Range_log.tif")
plot(range_sdm)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

LTSA_range_locs <- read.csv("./Input_localities/model_subsets/LTSA_range_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

LTSA_range_input_predict <- terra::extract(range_sdm, LTSA_range_locs[,c("Long_m", "Lat_m")])
LTSA_range_input_predict <- cbind(LTSA_range_locs, LTSA_range_input_predict)

LTSA_range_input_predict <- subset(LTSA_range_input_predict, select = -c(ID))


## create 10 percentile threshold (to get rid of possible error/sink populations)

LTSA_range_Threshold <- quantile(LTSA_range_input_predict$layer, 0.1, na.rm = TRUE)
LTSA_range_Threshold


## Get independent data into usable form for PresenceAbsence package 
## Columns required: 1) ID, 2) Observed status in binary format (0 or 1), 3) prediction values
  
LTSA_range_data <- predictions[,c("NAME","LTSA", "ClimOnly_RB_Range_SDM")]
  
## Test predictions with the Independent data using calculated threshold
  
LTSA_range_values <- presence.absence.accuracy(LTSA_range_data, threshold=LTSA_range_Threshold)
LTSA_range_values


## saving csv 
write.csv(LTSA_range_values, "./Maxent/ClimateOnly/RB/Accuracy/Subsets/DupRem/ClimOnly_RB_Range_Accuracy.csv", row.names=FALSE)


###### political study extent ######

## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

pol_sdm <- rast("./Maxent/ClimateOnly/RB/political/ClimOnly_RB_political_log.tif")
plot(pol_sdm)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

LTSA_pol_locs <- read.csv("./Input_localities/model_subsets/LTSA_political_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

LTSA_pol_input_predict <- terra::extract(pol_sdm, LTSA_pol_locs[,c("Long_m", "Lat_m")])
LTSA_pol_input_predict <- cbind(LTSA_pol_locs, LTSA_pol_input_predict)

LTSA_pol_input_predict <- subset(LTSA_pol_input_predict, select = -c(ID))


## create 5th percentile threshold (to get rid of possible error/sink populations)

LTSA_pol_Threshold <- quantile(LTSA_pol_input_predict$layer, 0.1, na.rm = TRUE)


## Get independent data into usable form for PresenceAbsence package 
## Columns required: 1) ID, 2) Observed status in binary format (0 or 1), 3) prediction values

LTSA_pol_data <- predictions[,c("NAME","LTSA", "ClimOnly_RB_pol_SDM")]

# Remove NAs Note that this will boot out tmm3 upper and lower

LTSA_pol_data <- na.omit(LTSA_pol_data) 

## Test predictions with the Independent data using calculated threshold

LTSA_pol_values <- presence.absence.accuracy(LTSA_pol_data, threshold=LTSA_pol_Threshold)
LTSA_pol_values

## saving csv 
write.csv(LTSA_pol_values, "./Maxent/ClimateOnly/RB/Accuracy/Subsets/DupRem/ClimOnly_RB_pol_Accuracy.csv", row.names=FALSE)


###### ecotone study extent ######

## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

eco_sdm <- rast("./Maxent/ClimateOnly/RB/ecotone/ClimOnly_RB_ecotone_log.tif")
plot(eco_sdm)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

LTSA_eco_locs <- read.csv("./Input_localities/model_subsets/LTSA_ecotone_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

LTSA_eco_input_predict <- terra::extract(eco_sdm, LTSA_eco_locs[,c("Long_m", "Lat_m")])
LTSA_eco_input_predict <- cbind(LTSA_eco_locs, LTSA_eco_input_predict)

LTSA_eco_input_predict <- subset(LTSA_eco_input_predict, select = -c(ID))


## create 5th percentile threshold (to get rid of possible error/sink populations)

LTSA_eco_Threshold <- quantile(LTSA_eco_input_predict$layer, 0.1, na.rm = TRUE)


## Get independent data into usable form for PresenceAbsence package 
## Columns required: 1) ID, 2) Observed status in binary format (0 or 1), 3) prediction values

LTSA_eco_data <- predictions[,c("NAME","LTSA", "ClimOnly_RB_eco_SDM")]

## Test predictions with the Independent data using calculated threshold

LTSA_eco_values <- presence.absence.accuracy(LTSA_eco_data, threshold=LTSA_eco_Threshold)
LTSA_eco_values


## saving csv 
write.csv(LTSA_eco_values, "./Maxent/ClimateOnly/RB/Accuracy/Subsets/DupRem/ClimOnly_RB_eco_Accuracy.csv", row.names=FALSE)

###### genetic study extent ######

## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

gen_sdm <- rast("./Maxent/ClimateOnly/RB/genetic/ClimOnly_RB_genetic_log.tif")
plot(gen_sdm)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

LTSA_gen_locs <- read.csv("./Input_localities/model_subsets/LTSA_genetic_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

LTSA_gen_input_predict <- terra::extract(gen_sdm, LTSA_gen_locs[,c("Long_m", "Lat_m")])
LTSA_gen_input_predict <- cbind(LTSA_gen_locs, LTSA_gen_input_predict)

LTSA_gen_input_predict <- subset(LTSA_gen_input_predict, select = -c(ID))


## create 5th percentile threshold (to get rid of possible error/sink populations)

LTSA_gen_Threshold <- quantile(LTSA_gen_input_predict$layer, 0.1, na.rm = TRUE)


## Get independent data into usable form for PresenceAbsence package 
## Columns required: 1) ID, 2) Observed status in binary format (0 or 1), 3) prediction values

LTSA_gen_data <- predictions[,c("NAME","LTSA", "ClimOnly_RB_gen_SDM")]

## Test predictions with the Independent data using calculated threshold

LTSA_gen_values <- presence.absence.accuracy(LTSA_gen_data, threshold=LTSA_gen_Threshold)
LTSA_gen_values <- as.data.frame(LTSA_gen_values)


## saving csv 
write.csv(LTSA_gen_values, "./Maxent/ClimateOnly/RB/Accuracy/Subsets/DupRem/ClimOnly_RB_gen_Accuracy.csv", row.names=FALSE)

rm(list=ls())

##############################

### ########### ClimNonClim + RB ##################

## read in csv with prediction values extracted from prediction surfaces From Script 9.3

predictions <- read.csv("./Independent_data/Subsets/DupRem/KF_pred_ClimNonClim_RB.csv")

###### range study extent ######

## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

range_sdm <- rast("./Maxent/ClimNonClim/RB/Range/ClimNonClim_RB_Range_log.tif")
plot(range_sdm)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

LTSA_range_locs <- read.csv("./Input_localities/model_subsets/LTSA_range_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

LTSA_range_input_predict <- terra::extract(range_sdm, LTSA_range_locs[,c("Long_m", "Lat_m")])
LTSA_range_input_predict <- cbind(LTSA_range_locs, LTSA_range_input_predict)

LTSA_range_input_predict <- subset(LTSA_range_input_predict, select = -c(ID))


## create 5th percentile threshold (to get rid of possible error/sink populations)

LTSA_range_Threshold <- quantile(LTSA_range_input_predict$layer, 0.1, na.rm = TRUE)


## Get independent data into usable form for PresenceAbsence package 
## Columns required: 1) ID, 2) Observed status in binary format (0 or 1), 3) prediction values

LTSA_range_data <- predictions[,c("NAME","LTSA", "ClimNonClim_RB_Range_SDM")]

# Remove NAs Note that this will boot out bellevue pond

LTSA_range_data <- na.omit(LTSA_range_data) 

## Test predictions with the Independent data using calculated threshold

LTSA_range_values <- presence.absence.accuracy(LTSA_range_data, threshold=LTSA_range_Threshold)
LTSA_range_values


## saving csv 
write.csv(LTSA_range_values, "./Maxent/ClimNonClim/RB/Accuracy/Subsets/DupRem/ClimNonClim_RB_Range_Accuracy.csv", row.names=FALSE)


###### political study extent ######

## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

pol_sdm <- rast("./Maxent/ClimNonClim/RB/political/ClimNonClim_RB_political_log.tif")
plot(pol_sdm)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

LTSA_pol_locs <- read.csv("./Input_localities/model_subsets/LTSA_political_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

LTSA_pol_input_predict <- terra::extract(pol_sdm, LTSA_pol_locs[,c("Long_m", "Lat_m")])
LTSA_pol_input_predict <- cbind(LTSA_pol_locs, LTSA_pol_input_predict)

LTSA_pol_input_predict <- subset(LTSA_pol_input_predict, select = -c(ID))


## create 5th percentile threshold (to get rid of possible error/sink populations)

LTSA_pol_Threshold <- quantile(LTSA_pol_input_predict$layer, 0.1, na.rm = TRUE)


## Get independent data into usable form for PresenceAbsence package 
## Columns required: 1) ID, 2) Observed status in binary format (0 or 1), 3) prediction values

LTSA_pol_data <- predictions[,c("NAME","LTSA", "ClimNonClim_RB_pol_SDM")]

# Remove NAs Note that this will boot out bellevue pond, and tmm3 upper and lower

LTSA_pol_data <- na.omit(LTSA_pol_data) 

## Test predictions with the Independent data using calculated threshold

LTSA_pol_values <- presence.absence.accuracy(LTSA_pol_data, threshold=LTSA_pol_Threshold)
LTSA_pol_values

## saving csv 
write.csv(LTSA_pol_values, "./Maxent/ClimNonClim/RB/Accuracy/Subsets/DupRem/ClimNonClim_RB_pol_Accuracy.csv", row.names=FALSE)



###### ecotone study extent ######

## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

eco_sdm <- rast("./Maxent/ClimNonClim/RB/ecotone/ClimNonClim_RB_ecotone_log.tif")
plot(eco_sdm)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

LTSA_eco_locs <- read.csv("./Input_localities/model_subsets/LTSA_ecotone_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

LTSA_eco_input_predict <- terra::extract(eco_sdm, LTSA_eco_locs[,c("Long_m", "Lat_m")])
LTSA_eco_input_predict <- cbind(LTSA_eco_locs, LTSA_eco_input_predict)

LTSA_eco_input_predict <- subset(LTSA_eco_input_predict, select = -c(ID))


## create 5th percentile threshold (to get rid of possible error/sink populations)

LTSA_eco_Threshold <- quantile(LTSA_eco_input_predict$layer, 0.1, na.rm = TRUE)


## Get independent data into usable form for PresenceAbsence package 
## Columns required: 1) ID, 2) Observed status in binary format (0 or 1), 3) prediction values

LTSA_eco_data <- predictions[,c("NAME","LTSA", "ClimNonClim_RB_eco_SDM")]

# Remove NAs Note that this will boot out bellevue pond

LTSA_eco_data <- na.omit(LTSA_eco_data) 

## Test predictions with the Independent data using calculated threshold

LTSA_eco_values <- presence.absence.accuracy(LTSA_eco_data, threshold=LTSA_eco_Threshold)
LTSA_eco_values


## saving csv 
write.csv(LTSA_eco_values, "./Maxent/ClimNonClim/RB/Accuracy/Subsets/DupRem/ClimNonClim_RB_eco_Accuracy.csv", row.names=FALSE)

###### genetic study extent ######

## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

gen_sdm <- rast("./Maxent/ClimNonClim/RB/genetic/ClimNonClim_RB_genetic_log.tif")
plot(gen_sdm)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

LTSA_gen_locs <- read.csv("./Input_localities/model_subsets/LTSA_genetic_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

LTSA_gen_input_predict <- terra::extract(gen_sdm, LTSA_gen_locs[,c("Long_m", "Lat_m")])
LTSA_gen_input_predict <- cbind(LTSA_gen_locs, LTSA_gen_input_predict)

LTSA_gen_input_predict <- subset(LTSA_gen_input_predict, select = -c(ID))


## create 5th percentile threshold (to get rid of possible error/sink populations)

LTSA_gen_Threshold <- quantile(LTSA_gen_input_predict$layer, 0.1, na.rm = TRUE)


## Get independent data into usable form for PresenceAbsence package 
## Columns required: 1) ID, 2) Observed status in binary format (0 or 1), 3) prediction values

LTSA_gen_data <- predictions[,c("NAME","LTSA", "ClimNonClim_RB_gen_SDM")]

# Remove NAs Note that this will boot out bellevue pond

LTSA_gen_data <- na.omit(LTSA_gen_data) 

## Test predictions with the Independent data using calculated threshold

LTSA_gen_values <- presence.absence.accuracy(LTSA_gen_data, threshold=LTSA_gen_Threshold)
LTSA_gen_values <- as.data.frame(LTSA_gen_values)
LTSA_gen_values


## saving csv 
write.csv(LTSA_gen_values, "./Maxent/ClimNonClim/RB/Accuracy/Subsets/DupRem/ClimNonClim_RB_gen_Accuracy.csv", row.names=FALSE)

rm(list=ls())

###########################################

### ########### ClimOnly + TGB ##################

## read in csv with prediction values extracted from prediction surfaces From Script 9.3

predictions <- read.csv("./Independent_data/Subsets/DupRem/KF_pred_ClimOnly_TGB.csv")

###### range study extent ######

## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

range_sdm <- rast("./Maxent/ClimateOnly/TGB/Range/ClimOnly_TGB_Range_log.tif")
plot(range_sdm)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

LTSA_range_locs <- read.csv("./Input_localities/model_subsets/LTSA_range_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

LTSA_range_input_predict <- terra::extract(range_sdm, LTSA_range_locs[,c("Long_m", "Lat_m")])
LTSA_range_input_predict <- cbind(LTSA_range_locs, LTSA_range_input_predict)

LTSA_range_input_predict <- subset(LTSA_range_input_predict, select = -c(ID))


## create 5th percentile threshold (to get rid of possible error/sink populations)

LTSA_range_Threshold <- quantile(LTSA_range_input_predict$layer, 0.1, na.rm = TRUE)


## Get independent data into usable form for PresenceAbsence package 
## Columns required: 1) ID, 2) Observed status in binary format (0 or 1), 3) prediction values

LTSA_range_data <- predictions[,c("NAME","LTSA", "ClimOnly_TGB_Range_SDM")]

## Test predictions with the Independent data using calculated threshold

LTSA_range_values <- presence.absence.accuracy(LTSA_range_data, threshold=LTSA_range_Threshold)
LTSA_range_values


## saving csv 
write.csv(LTSA_range_values, "./Maxent/ClimateOnly/TGB/Accuracy/Subsets/DupRem/ClimOnly_TGB_Range_Accuracy.csv", row.names=FALSE)


###### political study extent ######

## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

pol_sdm <- rast("./Maxent/ClimateOnly/TGB/political/ClimOnly_TGB_political_log.tif")
plot(pol_sdm)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

LTSA_pol_locs <- read.csv("./Input_localities/model_subsets/LTSA_political_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

LTSA_pol_input_predict <- terra::extract(pol_sdm, LTSA_pol_locs[,c("Long_m", "Lat_m")])
LTSA_pol_input_predict <- cbind(LTSA_pol_locs, LTSA_pol_input_predict)

LTSA_pol_input_predict <- subset(LTSA_pol_input_predict, select = -c(ID))


## create 5th percentile threshold (to get rid of possible error/sink populations)

LTSA_pol_Threshold <- quantile(LTSA_pol_input_predict$layer, 0.1, na.rm = TRUE)


## Get independent data into usable form for PresenceAbsence package 
## Columns required: 1) ID, 2) Observed status in binary format (0 or 1), 3) prediction values

LTSA_pol_data <- predictions[,c("NAME","LTSA", "ClimOnly_TGB_pol_SDM")]

# Remove NAs Note that this will boot out tmm3 upper and lower

LTSA_pol_data <- na.omit(LTSA_pol_data) 

## Test predictions with the Independent data using calculated threshold

LTSA_pol_values <- presence.absence.accuracy(LTSA_pol_data, threshold=LTSA_pol_Threshold)
LTSA_pol_values

## saving csv 
write.csv(LTSA_pol_values, "./Maxent/ClimateOnly/TGB/Accuracy/Subsets/DupRem//ClimOnly_TGB_pol_Accuracy.csv", row.names=FALSE)



###### ecotone study extent ######

## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

eco_sdm <- rast("./Maxent/ClimateOnly/TGB/ecotone/ClimOnly_TGB_ecotone_log.tif")
plot(eco_sdm)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

LTSA_eco_locs <- read.csv("./Input_localities/model_subsets/LTSA_ecotone_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

LTSA_eco_input_predict <- terra::extract(eco_sdm, LTSA_eco_locs[,c("Long_m", "Lat_m")])
LTSA_eco_input_predict <- cbind(LTSA_eco_locs, LTSA_eco_input_predict)

LTSA_eco_input_predict <- subset(LTSA_eco_input_predict, select = -c(ID))


## create 5th percentile threshold (to get rid of possible error/sink populations)

LTSA_eco_Threshold <- quantile(LTSA_eco_input_predict$layer, 0.1, na.rm = TRUE)


## Get independent data into usable form for PresenceAbsence package 
## Columns required: 1) ID, 2) Observed status in binary format (0 or 1), 3) prediction values

LTSA_eco_data <- predictions[,c("NAME","LTSA", "ClimOnly_TGB_eco_SDM")]

## Test predictions with the Independent data using calculated threshold

LTSA_eco_values <- presence.absence.accuracy(LTSA_eco_data, threshold=LTSA_eco_Threshold)
LTSA_eco_values


## saving csv 
write.csv(LTSA_eco_values, "./Maxent/ClimateOnly/TGB/Accuracy/Subsets/DupRem/ClimOnly_TGB_eco_Accuracy.csv", row.names=FALSE)

###### genetic study extent ######

## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

gen_sdm <- rast("./Maxent/ClimateOnly/TGB/genetic/ClimOnly_TGB_genetic_log.tif")
plot(gen_sdm)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

LTSA_gen_locs <- read.csv("./Input_localities/model_subsets/LTSA_genetic_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

LTSA_gen_input_predict <- terra::extract(gen_sdm, LTSA_gen_locs[,c("Long_m", "Lat_m")])
LTSA_gen_input_predict <- cbind(LTSA_gen_locs, LTSA_gen_input_predict)

LTSA_gen_input_predict <- subset(LTSA_gen_input_predict, select = -c(ID))


## create 5th percentile threshold (to get rid of possible error/sink populations)

LTSA_gen_Threshold <- quantile(LTSA_gen_input_predict$layer, 0.1, na.rm = TRUE)


## Get independent data into usable form for PresenceAbsence package 
## Columns required: 1) ID, 2) Observed status in binary format (0 or 1), 3) prediction values

LTSA_gen_data <- predictions[,c("NAME","LTSA", "ClimOnly_TGB_gen_SDM")]

## Test predictions with the Independent data using calculated threshold

LTSA_gen_values <- presence.absence.accuracy(LTSA_gen_data, threshold=LTSA_gen_Threshold)
LTSA_gen_values <- as.data.frame(LTSA_gen_values)
LTSA_gen_values

## saving csv 
write.csv(LTSA_gen_values, "./Maxent/ClimateOnly/TGB/Accuracy/Subsets/DupRem/ClimOnly_TGB_gen_Accuracy.csv", row.names=FALSE)

rm(list=ls())

##############################

### ########### ClimNonClim + TGB ##################

## read in csv with prediction values extracted from prediction surfaces From Script 9.3

predictions <- read.csv("./Independent_data/Subsets/DupRem/KF_pred_ClimNonClim_TGB.csv")

###### range study extent ######

## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

range_sdm <- rast("./Maxent/ClimNonClim/TGB/Range/ClimNonClim_TGB_Range_log.tif")
plot(range_sdm)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

LTSA_range_locs <- read.csv("./Input_localities/model_subsets/LTSA_range_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

LTSA_range_input_predict <- terra::extract(range_sdm, LTSA_range_locs[,c("Long_m", "Lat_m")])
LTSA_range_input_predict <- cbind(LTSA_range_locs, LTSA_range_input_predict)

LTSA_range_input_predict <- subset(LTSA_range_input_predict, select = -c(ID))


## create 5th percentile threshold (to get rid of possible error/sink populations)

LTSA_range_Threshold <- quantile(LTSA_range_input_predict$layer, 0.1, na.rm = TRUE)


## Get independent data into usable form for PresenceAbsence package 
## Columns required: 1) ID, 2) Observed status in binary format (0 or 1), 3) prediction values

LTSA_range_data <- predictions[,c("NAME","LTSA", "ClimNonClim_TGB_Range_SDM")]

# Remove NAs Note that this will boot out bellevue pond

LTSA_range_data <- na.omit(LTSA_range_data) 

## Test predictions with the Independent data using calculated threshold

LTSA_range_values <- presence.absence.accuracy(LTSA_range_data, threshold=LTSA_range_Threshold)
LTSA_range_values


## saving csv 
write.csv(LTSA_range_values, "./Maxent/ClimNonClim/TGB/Accuracy/Subsets/DupRem/ClimNonClim_TGB_Range_Accuracy.csv", row.names=FALSE)


###### political study extent ######

## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

pol_sdm <- rast("./Maxent/ClimNonClim/TGB/political/ClimNonClim_TGB_political_log.tif")
plot(pol_sdm)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

LTSA_pol_locs <- read.csv("./Input_localities/model_subsets/LTSA_political_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

LTSA_pol_input_predict <- terra::extract(pol_sdm, LTSA_pol_locs[,c("Long_m", "Lat_m")])
LTSA_pol_input_predict <- cbind(LTSA_pol_locs, LTSA_pol_input_predict)

LTSA_pol_input_predict <- subset(LTSA_pol_input_predict, select = -c(ID))


## create 5th percentile threshold (to get rid of possible error/sink populations)

LTSA_pol_Threshold <- quantile(LTSA_pol_input_predict$layer, 0.1, na.rm = TRUE)


## Get independent data into usable form for PresenceAbsence package 
## Columns required: 1) ID, 2) Observed status in binary format (0 or 1), 3) prediction values

LTSA_pol_data <- predictions[,c("NAME","LTSA", "ClimNonClim_TGB_pol_SDM")]

# Remove NAs Note that this will boot out tmm3 upper and lower

LTSA_pol_data <- na.omit(LTSA_pol_data) 

## Test predictions with the Independent data using calculated threshold

LTSA_pol_values <- presence.absence.accuracy(LTSA_pol_data, threshold=LTSA_pol_Threshold)
LTSA_pol_values

## saving csv 
write.csv(LTSA_pol_values, "./Maxent/ClimNonClim/TGB/Accuracy/Subsets/DupRem/ClimNonClim_TGB_pol_Accuracy.csv", row.names=FALSE)



###### ecotone study extent ######

## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

eco_sdm <- rast("./Maxent/ClimNonClim/TGB/ecotone/ClimNonClim_TGB_ecotone_log.tif")
plot(eco_sdm)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

LTSA_eco_locs <- read.csv("./Input_localities/model_subsets/LTSA_ecotone_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

LTSA_eco_input_predict <- terra::extract(eco_sdm, LTSA_eco_locs[,c("Long_m", "Lat_m")])
LTSA_eco_input_predict <- cbind(LTSA_eco_locs, LTSA_eco_input_predict)

LTSA_eco_input_predict <- subset(LTSA_eco_input_predict, select = -c(ID))


## create 5th percentile threshold (to get rid of possible error/sink populations)

LTSA_eco_Threshold <- quantile(LTSA_eco_input_predict$layer, 0.1, na.rm = TRUE)


## Get independent data into usable form for PresenceAbsence package 
## Columns required: 1) ID, 2) Observed status in binary format (0 or 1), 3) prediction values

LTSA_eco_data <- predictions[,c("NAME","LTSA", "ClimNonClim_TGB_eco_SDM")]

# Remove NAs Note that this will boot out bellevue pond

LTSA_eco_data <- na.omit(LTSA_eco_data) 

## Test predictions with the Independent data using calculated threshold

LTSA_eco_values <- presence.absence.accuracy(LTSA_eco_data, threshold=LTSA_eco_Threshold)
LTSA_eco_values


## saving csv 
write.csv(LTSA_eco_values, "./Maxent/ClimNonClim/TGB/Accuracy/Subsets/DupRem/ClimNonClim_TGB_eco_Accuracy.csv", row.names=FALSE)

###### genetic study extent ######

## To determine the threshold: 

## Read in prediction raster
## Raster generated from "all input localities" in Script 8.2_Maxent_SWD_all_locs

gen_sdm <- rast("./Maxent/ClimNonClim/TGB/genetic/ClimNonClim_TGB_genetic_log.tif")
plot(gen_sdm)

## Read in input localities (used to determine threshold value - predictions for input localities will also be save)

LTSA_gen_locs <- read.csv("./Input_localities/model_subsets/LTSA_genetic_locs.csv")

## Extract prediction values from the prediction raster and cbind to get "metadata" (for input localities)

LTSA_gen_input_predict <- terra::extract(gen_sdm, LTSA_gen_locs[,c("Long_m", "Lat_m")])
LTSA_gen_input_predict <- cbind(LTSA_gen_locs, LTSA_gen_input_predict)

LTSA_gen_input_predict <- subset(LTSA_gen_input_predict, select = -c(ID))


## create 5th percentile threshold (to get rid of possible error/sink populations)

LTSA_gen_Threshold <- quantile(LTSA_gen_input_predict$layer, 0.1, na.rm = TRUE)


## Get independent data into usable form for PresenceAbsence package 
## Columns required: 1) ID, 2) Observed status in binary format (0 or 1), 3) prediction values

LTSA_gen_data <- predictions[,c("NAME","LTSA", "ClimNonClim_TGB_gen_SDM")]

# Remove NAs Note that this will boot out bellevue pond

LTSA_gen_data <- na.omit(LTSA_gen_data) 

## Test predictions with the Independent data using calculated threshold

LTSA_gen_values <- presence.absence.accuracy(LTSA_gen_data, threshold=LTSA_gen_Threshold)
LTSA_gen_values <- as.data.frame(LTSA_gen_values)
LTSA_gen_values

## saving csv 
write.csv(LTSA_gen_values, "./Maxent/ClimNonClim/TGB/Accuracy/Subsets/DupRem//ClimNonClim_TGB_gen_Accuracy.csv", row.names=FALSE)

rm(list=ls())

##############################


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################


########################### END SCRIPT ###############################


