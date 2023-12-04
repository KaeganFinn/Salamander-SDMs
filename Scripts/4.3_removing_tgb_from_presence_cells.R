########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Jayna Bergman & Kaegan Finn

### Goal of this Script:

# removes target group background points from the same cell as an input locality for each species 

### Notes:  

# For input localities use "species_dupl_rm.csv"

### Date: May 30, 2022

### Version of R:  R version 4.0.3

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

## Read in range study extent raster

LTSA_r <- rast("./Environmental_Variables/Range/ClimateOnly/MAP.tif")
plot(LTSA_r)

## Read in csv for the tgb and input localities

LTSA_tgb <- read.csv("./TGB/LTSA_tgb_aea.csv")

LTSA_sites <- read.csv("./Input_localities/LTSA_dupl_rm.csv")

## Extract raster values for the tgb points, cbind extracted values with original tgb file, remove rows with NA.

LTSA_cellnumstgb <- terra::extract(LTSA_r, LTSA_tgb[c("Long_m", "Lat_m")], cells=TRUE)

LTSA_tgb_bind <- cbind(LTSA_tgb, LTSA_cellnumstgb)

LTSA_tgb_bind <- LTSA_tgb_bind %>%
  filter(!is.na(cell))

## Extract raster values for the input localities points, remove rows with NA (no need to bind because not saving input localities)

LTSA_cellnumssites <- terra::extract(LTSA_r, LTSA_sites[c("Long_m", "Lat_m")], cells=TRUE)

LTSA_cellnumssites <- LTSA_cellnumssites %>%
  filter(!is.na(cell))

## Remove tgb points that fall in the same cell as the input localities

LTSA_final_tgb <- LTSA_tgb_bind[!(LTSA_tgb_bind$cell%in%LTSA_cellnumssites$cell),]

## Remove extra columns

LTSA_final_tgb <- subset(LTSA_final_tgb, select= -c(ID, Normal_1991_2020_MAP_aea, cell))

## Save new tbg csv

write.csv(LTSA_final_tgb, file="./TGB/LTSA_tgb_aea_presence_rm.csv", row.names=FALSE)

########################### END SECTION ##############################

########################## FINAL COMMENTS ############################

########################### END SCRIPT ###############################