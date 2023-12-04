########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs 

### Author: Kaegan Finn

### Goal of this Script: 

# Count number of cells predicted to be suitable in each Class

### Notes:  

# outputs are: csv with prediction values for all input localities, csv with quantile (threshold values), binary prediction surface using 10th quantile

### Date: October 2022

### Version of R:  R version 4.0.3

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

## Read in raster  


ClimOnly_RB_Range_binary <- rast("./Binary_surfaces/binary_final/ClimOnly_RB_Range_binary.tif")
ClimOnly_RB_political_binary <- rast("./Binary_surfaces/binary_final/ClimOnly_RB_political_binary.tif")
ClimOnly_RB_ecotone_binary <- rast("./Binary_surfaces/binary_final/ClimOnly_RB_ecotone_binary.tif")
ClimOnly_RB_genetic_binary <- rast("./Binary_surfaces/binary_final/ClimOnly_RB_genetic_binary.tif")

ClimNonClim_RB_Range_binary <- rast("./Binary_surfaces/binary_final/ClimNonClim_RB_Range_binary.tif")
ClimNonClim_RB_political_binary <- rast("./Binary_surfaces/binary_final/ClimNonClim_RB_political_binary.tif")
ClimNonClim_RB_ecotone_binary <- rast("./Binary_surfaces/binary_final/ClimNonClim_RB_ecotone_binary.tif")
ClimNonClim_RB_genetic_binary <- rast("./Binary_surfaces/binary_final/ClimNonClim_RB_genetic_binary.tif")

ClimOnly_TGB_Range_binary <- rast("./Binary_surfaces/binary_final/ClimOnly_TGB_Range_binary.tif")
ClimOnly_TGB_political_binary <- rast("./Binary_surfaces/binary_final/ClimOnly_TGB_political_binary.tif")
ClimOnly_TGB_ecotone_binary <- rast("./Binary_surfaces/binary_final/ClimOnly_TGB_ecotone_binary.tif")
ClimOnly_TGB_genetic_binary <- rast("./Binary_surfaces/binary_final/ClimOnly_TGB_genetic_binary.tif")

ClimNonClim_TGB_Range_binary <- rast("./Binary_surfaces/binary_final/ClimNonClim_TGB_Range_binary.tif")
ClimNonClim_TGB_political_binary <- rast("./Binary_surfaces/binary_final/ClimNonClim_TGB_political_binary.tif")
ClimNonClim_TGB_ecotone_binary <- rast("./Binary_surfaces/binary_final/ClimNonClim_TGB_ecotone_binary.tif")
ClimNonClim_TGB_genetic_binary <- rast("./Binary_surfaces/binary_final/ClimNonClim_TGB_genetic_binary.tif")


## Making frequency table (value and count)
  
count <- terra::freq(ClimOnly_RB_Range_binary, bylayer=FALSE)
count
count <- terra::freq(ClimOnly_RB_political_binary, bylayer=FALSE)
count
count <- terra::freq(ClimOnly_RB_ecotone_binary, bylayer=FALSE)
count
count <- terra::freq(ClimOnly_RB_genetic_binary, bylayer=FALSE)
count
count <- terra::freq(ClimNonClim_RB_Range_binary, bylayer=FALSE)
count
count <- terra::freq(ClimNonClim_RB_political_binary, bylayer=FALSE)
count
count <- terra::freq(ClimNonClim_RB_ecotone_binary, bylayer=FALSE)
count
count <- terra::freq(ClimNonClim_RB_genetic_binary, bylayer=FALSE)
count
count <- terra::freq(ClimOnly_TGB_Range_binary, bylayer=FALSE)
count
count <- terra::freq(ClimOnly_TGB_political_binary, bylayer=FALSE)
count
count <- terra::freq(ClimOnly_TGB_ecotone_binary, bylayer=FALSE)
count
count <- terra::freq(ClimOnly_TGB_genetic_binary, bylayer=FALSE)
count
count <- terra::freq(ClimNonClim_TGB_Range_binary, bylayer=FALSE)
count
count <- terra::freq(ClimNonClim_TGB_political_binary, bylayer=FALSE)
count
count <- terra::freq(ClimNonClim_TGB_ecotone_binary, bylayer=FALSE)
count
count <- terra::freq(ClimNonClim_TGB_genetic_binary, bylayer=FALSE)
count


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################

########################### END SCRIPT ###############################




