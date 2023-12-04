########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Kaegan Finn

### Goal of this Script: 

# Determine which of the previously selected environmental variables are highly correlated with each other using Pearson correlation

### Notes:  

# Using previously cropped biologically relevant rasters (from script 3.2_cropping_rasters_range_study_extents.R)

### Date: Nov 28, 2022

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

getwd()
setwd("/Users/kaegan/Dropbox/Mac/Desktop/_/MSc/Thesis/Models")

################################# LTSA ################################

## Read in rasters as a list from a file and stack them 

rast_list <- list.files("./Environmental_Variables/Range/ClimateNonClimate", pattern='.tif$', all.files=TRUE, full.names=TRUE)
rast_list

rstack <- rast(rast_list)

## Create a list of the raster names in the file and properly name variables 

names_list <- gsub(pattern=".tif", replacement="", list.files("./Environmental_Variables/Range/ClimateNonClimate", pattern='.tif$', all.files=TRUE, full.names=FALSE))

names(rstack) <- names_list
plot(rstack)

#layerCor in terra package ensure to set na.rm = TRUE

Pearson <- layerCor(rstack, "pearson", na.rm = TRUE)
Pearson <- as.data.frame(Pearson)

## Save csv with pearson correlation values

write.csv(Pearson, "./Environmental_Variables/Pearson_cor.csv")

########################### END SECTION ##############################

########################## FINAL COMMENTS ############################

########################### END SCRIPT ###############################