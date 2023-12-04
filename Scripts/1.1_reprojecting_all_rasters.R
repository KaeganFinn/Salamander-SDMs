########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Jayna Bergman 

### Goal of this Script: 

# This script reprojects rasters into Albers Equal Area projection by using a list of raster names within a file and looping through reprojecting each raster individually by the names list.

### Notes:  

# Variables in the file were previously narrowed down (from the total 33 climate NA variables) by biological relevance - 16 variables in this case.
# These variables are not separated for the single species but rather all the variables that will be needed for all species. 

### Date: May 20, 2022

### Version of R: R version 4.0.3

########################### END SECTION ##############################

############################ LIBRARIES ###############################

install.packages("raster")
install.packages("terra")
install.packages("sf")
install.packages("rgdal")

library(raster)
library(terra)
library(sf)
library(rgdal)

rm(list=ls())
########################### END SECTION ##############################

###################### FUNCTION DEFINITIONS ##########################

## NA

########################### END SECTION ##############################

########################## DATA AND ANALYSIS #########################

## Set working directory

getwd()
setwd()


## Read in environmental variable rasters (previously cropped to range limits) and stack them 

LTSA_rast_list <- list.files("./Environmental_Variables/Range/Future_Scenarios/Original_Projection/SSP5_8.5", pattern='.tif$', all.files=TRUE, full.names=TRUE)
LTSA_rast_list

LTSA_range_env <- stack(LTSA_rast_list)
plot(LTSA_range_env)


## Create a list of the raster names in the file and properly name variables 

LTSA_names_list <- gsub(pattern=".tif", replacement="", list.files("./Environmental_Variables/Range/Future_Scenarios/Original_Projection/SSP5_8.5", pattern='.tif$', all.files=TRUE, full.names=FALSE))

names(LTSA_range_env) <- LTSA_names_list

## Define the new crs (projection) - in this case North America Albers equal area conic 

crs <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"


## loop to read in one raster at a time, reproject it to aea, and save a new raster file 

for(i in 1:length(LTSA_names_list)){
  r <- rast(paste("./Environmental_Variables/Range/Future_Scenarios/Original_Projection/SSP5_8.5/",LTSA_names_list[i],".tif", sep=""))
  r_aea <- terra::project(r, crs)
  writeRaster(r_aea, paste("./Environmental_Variables/Range/Future_Scenarios/aea",LTSA_names_list[i],"_aea.tif", sep=""))
}


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################

########################### END SCRIPT ###############################
