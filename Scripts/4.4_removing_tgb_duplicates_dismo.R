########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Jayna Bergman & Kaegan Finn

### Goal of this Script: 

# Read filtered tgb localities and remove duplicates from within the same raster grid cell using dismo 

### Notes:  

# Raster is at 1km resolution - separate rasters read in for each species

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

################################# LTSA ################################

## Read in raster to define study extent (using range wide extents)

LTSA_r <- raster("./Environmental_Variables/Range/ClimateOnly/MAP.tif")
plot(LTSA_r)

## Read in all LTSA sites & get into a two column Matrix

LTSA_tgb <- read.csv("./TGB//LTSA_tgb_aea_presence_rm.csv")

LTSApts_tgb <- LTSA_tgb[,c("Long_m", "Lat_m")]


## Thin for localities in the same cell

LTSA_thinned_tgb <- gridSample(LTSApts_tgb, LTSA_r, n=1)
plot(LTSA_thinned_tgb)

## Adding other columns back onto thinned coordinates

LTSA_thinned_pts_tgb <- LTSA_tgb[row.names(LTSA_thinned_tgb),]

## saving a new CSV

write.csv(LTSA_thinned_pts_tgb,file="./TGB/LTSA_tgb_dupl_rm.csv",row.names=FALSE)

########################### END SECTION ##############################

########################## FINAL COMMENTS ############################

########################### END SCRIPT ###############################