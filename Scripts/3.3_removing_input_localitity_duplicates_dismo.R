########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Jayna Bergman & Kaegan Finn

### Goal of this Script: 

# Read filtered input localities and remove duplicates from within the same raster grid cell using dismo 

### Notes:  

# Raster is for all of NA and at 1km resolution 

### Date: May 22, 2022

### Version of R: R version 4.0.3

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(dismo)
library(sf)
library(terra)
library(tidyverse)
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


## Read in Raster to define the study extent and the resolution (using all of NA and 1km resolution)

r <- raster("./Environmental_Variables/Range/ClimateNonClimate/MAP.tif")
plot(r)


################################# LTSA ################################

## Read in all LTSA sites & get into a two column Matrix

LTSAsites <- read.csv("./input_localities/LTSA_filtered_aea.csv")

LTSApts <- LTSAsites[,c("Long_m", "Lat_m")]


## Thin for localities in the same cell

LTSA_thinned <- gridSample(LTSApts, r, n=1)
plot(LTSA_thinned)


## Adding other columns back onto thinned coordinates

LTSA_thinned_pts <- LTSAsites[row.names(LTSA_thinned),]

## saving a new CSV

write.csv(LTSA_thinned_pts,file="./Input_localities/LTSA_dupl_rm.csv",row.names=FALSE)


########################### END SECTION ###############################

########################## FINAL COMMENTS ############################

########################### END SCRIPT ###############################