########################## GOALS AND NOTES ###########################

### Project: Salamdander SDMs

### Author: Kaegan Finn

### Goal of this Script: 

# Check if independent survey sites span the range of environental spce using pca

### Date: April 13, 2023

### Version of R:  R version 4.2.1

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(dismo)
library(sf)
library(terra)
library(rgdal)
library(dplyr)
library(rJava)
library(ggplot2)
library(ggmosaic)


rm(list=ls())
########################### END SECTION ##############################

## Set working directory

getwd()
setwd("/Users/kaegan/Dropbox/Mac/Desktop/_/MSc/Thesis/Models")


############################

## MESS NEW AND IMPROVED, OCT 2023 
#First eleemtn (x) needs to be future raster stack of clim Only vars for only the Political region
# Second elemenet should be the env vars at both presence and absence locations used to build each model based on current condish 
# If all the names match, it should work? 


################################# CLIMATE ONLY _FUTURE SCENARIO SSP2_4.5 ################################

## Read in environmental variable rasters (previously cropped to range limits) and stack them 

LTSA_rast_list <- list.files("./Environmental_Variables/Range/Future_Scenarios/SSP2_4.5", pattern='.tif$', all.files=TRUE, full.names=TRUE)
LTSA_rast_list

LTSA_range_env_ssp245 <- stack(LTSA_rast_list)
plot(LTSA_range_env_ssp245)


## Create a list of the raster names in the file and properly name variables 

LTSA_names_list <- gsub(pattern=".tif", replacement="", list.files("./Environmental_Variables/Range/Future_Scenarios/SSP2_4.5", pattern='.tif$', all.files=TRUE, full.names=FALSE))

names(LTSA_range_env_ssp245) <- LTSA_names_list

################################# CLIMATE ONLY _FUTURE SCENARIO SSP5_8.5 ################################

## Read in environmental variable rasters (previously cropped to range limits) and stack them 

LTSA_rast_list <- list.files("./Environmental_Variables/Range/Future_Scenarios/SSP5_8.5", pattern='.tif$', all.files=TRUE, full.names=TRUE)
LTSA_rast_list

LTSA_range_env_ssp585 <- stack(LTSA_rast_list)
plot(LTSA_range_env_ssp585)


## Create a list of the raster names in the file and properly name variables 

LTSA_names_list <- gsub(pattern=".tif", replacement="", list.files("./Environmental_Variables/Range/Future_Scenarios/SSP5_8.5", pattern='.tif$', all.files=TRUE, full.names=FALSE))

names(LTSA_range_env_ssp585) <- LTSA_names_list

# MASK IT 
LTSA_political <- st_read("./Study_extents/Study_extents_aea/political_aea.shp")

LTSA_pol_env_ssp245 <- mask(LTSA_range_env_ssp245, LTSA_political) # Only need to use this one
LTSA_pol_env_ssp585 <- mask(LTSA_range_env_ssp585, LTSA_political)


plot(LTSA_pol_env_ssp245)
plot(LTSA_pol_env_ssp585)

#### READ IN REFERENCE POINTS!!

range_pts <- read.csv("./MESS_Oct23/ClimOnly_RB_range_PresAbsValues.csv")
pol_pts <- read.csv("./MESS_Oct23/ClimOnly_RB_pol_PresAbsValues.csv")
eco_pts <- read.csv("./MESS_Oct23/ClimOnly_RB_eco_PresAbsValues.csv")

## Keep Only the columns that matter (the env values)

range_pts <-  subset(range_pts, select = -c(species,x,y))
pol_pts <-  subset(pol_pts, select = -c(species,x,y))
eco_pts <-  subset(eco_pts, select = -c(species,x,y))


### RUN THE MESS FUNCTION

# Range
#585
ssp585_range_mess <- mess(LTSA_pol_env_ssp585,range_pts, full = FALSE, overwrite=TRUE,
                     filename = "./MESS_Oct23/ssp585_range_mess",sep="",format="GTiff")

plot(ssp585_range_mess)

#245
ssp245_range_mess <- mess(LTSA_pol_env_ssp245,range_pts, full = FALSE, overwrite=TRUE,
                          filename = "./MESS_Oct23/ssp245_range_mess",sep="",format="GTiff")

plot(ssp245_range_mess)

# NO NEGATIVE VALUES IN REGION OF INTEREST! NO CHANGES

# ECO 
#585
ssp585_eco_mess <- mess(LTSA_pol_env_ssp585,eco_pts, full = FALSE, overwrite=TRUE,
                          filename = "./MESS_Oct23/ssp585_eco_mess",sep="",format="GTiff")

plot(ssp585_eco_mess)

#245
ssp245_eco_mess <- mess(LTSA_pol_env_ssp245,eco_pts, full = FALSE, overwrite=TRUE,
                          filename = "./MESS_Oct23/ssp245_eco_mess",sep="",format="GTiff")

plot(ssp245_eco_mess)

#SOME NEGATIVE VALUES... LOOKS SIMILAIR? Hard to Say for sure 

# POL
#585
ssp585_pol_mess <- mess(LTSA_pol_env_ssp585,pol_pts, full = FALSE, overwrite=TRUE,
                        filename = "./MESS_Oct23/ssp585_pol_mess",sep="",format="GTiff")

plot(ssp585_pol_mess)

#245
ssp245_pol_mess <- mess(LTSA_pol_env_ssp245,pol_pts, full = FALSE, overwrite=TRUE,
                        filename = "./MESS_Oct23/ssp245_pol_mess",sep="",format="GTiff")

plot(ssp245_pol_mess)

# 585 look like total extrapolation, 245 might have some salvageable points.. 

##NEXT STEP 
#EXTRACT values from each MESS MAP at potential Translocation Sites. 

# Step 1: Stack MESS Rasters 

mess_stack <- stack(ssp585_range_mess,
                    ssp245_range_mess,
                    ssp585_eco_mess,
                    ssp245_eco_mess,
                    ssp585_pol_mess,
                    ssp245_pol_mess)

plot(mess_stack)


# Step 2: Bring in potential translocation sites 

sites <- read.csv("./MESS_Oct23/TranslocationSites_aea.csv")


# Extract
sites_messvalues <- terra::extract(mess_stack, sites[,c("Long_m", "Lat_m")])

# change to dataframe 
sites_messvalues <- as.data.frame(sites_messvalues)

# cbind metadata back onto dataframe
site_data <- cbind(sites,sites_messvalues)

#Rename columns 5-10 their original names 

site_data <- site_data %>%
  rename("ssp585_range_mess" = 5,
         "ssp245_range_mess" = 6,
         "ssp585_eco_mess" = 7,
         "ssp245_eco_mess" = 8,
         "ssp585_pol_mess" = 9,
         "ssp245_pol_mess" = 10)

#write CSV 
write.csv(site_data,"./MESS_Oct23/site_data.csv")



### EXTRACT SUITABILITY OF SITES FROM SSP 585 ECO MODEL TO DETERMINE SUITABILITY  


## Read in raster 

ClimOnly_RB_eco_ssp585 <- raster("./MESS_Oct23/EcoModel/ClimOnly_RB_ecotone_ssp585.tif")

sites_modelvalues <- terra::extract(ClimOnly_RB_eco_ssp585, sites[,c("Long_m", "Lat_m")])

sites_modelvalues <- as.data.frame(sites_modelvalues)

site_data2 <- cbind(sites,sites_modelvalues)


write.csv(site_data2,"./MESS_Oct23/ClimOnly_RB_ecotone_ssp585_values.csv")







