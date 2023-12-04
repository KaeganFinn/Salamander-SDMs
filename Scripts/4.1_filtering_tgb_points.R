########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs 

### Author: Jayna Bergman & Kaegan Finn

### Goal of this Script: 

# Takes an input csv with multiple amphibian species and filters by user set columns
# For each species a separte csv is saved that excludes the target species. 

### Notes:  

#need locality records in csv format - double check names of columns. 

### Date: May 28, 2022

### Version of R:  R Version 4.0.3

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(tidyr)
library(tidyverse)
library(sf)

rm(list=ls())
########################### END SECTION ##############################

########################## DATA AND ANALYSIS #########################


## set working directory
getwd()
setwd()


## read in unfiltered csv of all target-group background localities
tgb <- read.csv("./TGB/TGB_amphibs_unfiltered.csv")
View(tgb)


## removing rows with missing coordinates (lat & long), missing years, missing accuracy
## filtering the data to have years >= 1990
## filtering the data to have accuracy <=1000m
## removing exact duplicate records based on lat&long

tgb_filtered <- tgb %>% 
  filter(!is.na(Longitude)) %>%
  filter(!is.na(Latitude)) %>%
  filter(!is.na(Year)) %>% 
  filter(!is.na(Location_Precision_meters)) %>%
  filter(Year >= 1990) %>%
  filter(Location_Precision_meters <= 1000) %>%
  distinct(Longitude, Latitude, .keep_all = TRUE)


## write new csv for filtered tgb points with all Amphibians

write.csv(tgb_filtered, file = "./TGB/tgb_filtered.csv", row.names = FALSE)


################################# LTSA ################################

## filtering out target species (LTSA)

LTSA_tgb <- tgb_filtered %>%
  filter(Scientific_Name != 'Ambystoma macrodactylum')
  

## write new csv for filtered tgb points without focal species 

write.csv(LTSA_tgb, file = "./TGB/LTSA_tgb.csv", row.names = FALSE)


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################

########################### END SCRIPT ###############################
