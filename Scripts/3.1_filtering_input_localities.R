########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs 

### Author: Jayna Bergman & Kaegan Finn

### Goal of this Script: 

# Takes an input csv of locality records and filters them according to rules set by user
# Output is a csv 

### Notes:  

#need locality records in csv format - double check names of columns. 

### Date: May 12, 2022

### Version of R:  R Version 4.0.3

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(tidyr)
library(tidyverse)

rm(list=ls())
########################### END SECTION ##############################

########################## DATA AND ANALYSIS #########################


## set working directory
getwd()
setwd()


################################# LTSA ################################

## Read in unfiltered locality data
LTSA_unfiltered <- read.csv("./Input_localities/LTSA_unfiltered.csv")
View(LTSA_unfiltered)


## removing rows with missing coordinates (lat & long), missing years, missing accuracy
## filtering the data to have years >= 1990
## filtering the data to have accuracy <=1000m 
LTSA_filtered <- LTSA_unfiltered %>% 
  filter(!is.na(Longitude)) %>%
  filter(!is.na(Latitude)) %>%
  filter(!is.na(Year)) %>% 
  filter(!is.na(Location_Precision_meters)) %>%
  filter(Year >= 1990) %>%
  filter(Location_Precision_meters <= 1000)


## removing exact duplicate records based on lat&long
LTSA_filtered <- LTSA_filtered %>%
  distinct(Longitude, Latitude, .keep_all = TRUE)


## write new csv for filtered input localities
write.csv(LTSA_filtered, file = "./Input_localities/LTSA_filtered.csv", row.names = FALSE)


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################

########################### END SCRIPT ###############################
