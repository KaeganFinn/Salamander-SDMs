########################## GOALS AND NOTES ###########################

### Project: Kaegan Finn MSc. Thesis: Long-toed salamander surveys

### Author: Kaegan Finn

### Goal of this Script: 

# extract suitability values based on all models for restoration sites

### Notes:  

### Date: March 13, 2023

### Version of R:  R version 4.2.1

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(ggpubr)
library(ggplot2)
library(terra)
library(raster)
library(sf)
library(dplyr)
library(broom)
library(car)
library(visreg)


rm(list=ls())
########################### END SECTION ##############################

###################### FUNCTION DEFINITIONS ##########################

## NA

########################### END SECTION ##############################

########################## DATA AND ANALYSIS #########################


## Set working directory

getwd()
setwd()

####### Get restoration sites


sites <- read.csv("./RestorationSites/ConservationPriority_aea.csv")
str(sites)

## Read in prediction surfaces

LTSA_rast_list <- list.files("./cont_final", pattern='.tif$', all.files=TRUE, full.names=TRUE)
LTSA_rast_list

predictions <- stack(LTSA_rast_list)
plot(predictions)

## Create a list of the raster names in the file and properly name variables 

LTSA_names_list <- gsub(pattern=".tif", replacement="", list.files("./cont_final", pattern='.tif$', all.files=TRUE, full.names=FALSE))

names(predictions) <- LTSA_names_list
plot(predictions)

# Extract 

scores <- terra::extract(predictions, sites[,c("Long_m", "Lat_m")])
sites_scores <- cbind (sites, scores)

# write csv 

write.csv (sites_scores, "./RestorationSites/ConservationPriority_Scores.csv")


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################

########################### END SCRIPT ###############################

