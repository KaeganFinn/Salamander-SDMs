########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Jayna Bergman 

### Goal of this Script: 

# Crop biologically relevant / literature relevant environmental variables to the range study extent for each species 

### Notes:  

#

### Date: May 22, 2022

### Version of R: R version 4.0.3

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(terra)
library(sf)
library(dplyr)
library(raster)   

rm(list=ls())
########################### END SECTION ##############################

########################## DATA AND ANALYSIS #########################

## Set working directory

setwd()


## Read in rasters as a list from a file and stack them (same for all species)

rast_list <- list.files("./Environmental_Variables/Range_aea", pattern='.tif$', all.files=TRUE, full.names=TRUE)
rast_list

rstack <- stack(rast_list)
plot(rstack)


## Create a list of the raster names in the file and properly name variables 

names_list <- gsub(pattern=".tif", replacement="", list.files("./envi_variables/ClimateNA_variables/aea", pattern='.tif$', all.files=TRUE, full.names=FALSE))

names(rstack) <- names_list


################################# LTSA ################################

## Read in range shapefile

LTSA_range <- st_read("./study_extents/model_subsets/LTSA/LTSA_range.shp")
plot(st_geometry(LTSA_range))


## crop rasters to the range study extent and save Rasters in the specific species folder 

LTSA_rstack_crop <- crop(rstack, LTSA_range)
plot(LTSA_rstack_crop[[1]])


setwd()

writeRaster(stack(LTSA_rstack_crop), names(LTSA_rstack_crop), bylayer = TRUE, format= "GTiff", overwrite = TRUE)


########################### END SECTION ###############################


########################## FINAL COMMENTS ############################


########################### END SCRIPT ###############################