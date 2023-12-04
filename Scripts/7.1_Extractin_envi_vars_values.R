########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Jayna Bergman, updated by Kaegan Finn, Dec 9 2022

### Goal of this Script: 

# Extract data (using terra package) from GIS layers underlying presences and tgb points for each species and save as csv files 

### Notes:  

# Adds "Type" (0 or 1) Column 

### Date: June 6, 2022

### Version of R:  R version 4.0.3

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(dismo)
library(sf)
library(terra)
library(rgdal)
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

################################# ClimateOnly ################################

## Read in environmental layers for the species

## Read in environmental variable rasters (previously cropped to range limits) and stack them 

LTSA_rast_list <- list.files("./Environmental_Variables/Range/ClimateOnly", pattern='.tif$', all.files=TRUE, full.names=TRUE)
LTSA_rast_list

LTSA_range_env <- stack(LTSA_rast_list)

## Create a list of the raster names in the file and properly name variables 

LTSA_names_list <- gsub(pattern=".tif", replacement="", list.files("./Environmental_Variables/Range/ClimateOnly", pattern='.tif$', all.files=TRUE, full.names=FALSE))

names(LTSA_range_env) <- LTSA_names_list
plot(LTSA_range_env)

## Read study extent shapefiles in (to be able to crop rasters)
LTSA_genetic <- st_read("./Study_extents/Study_extents_aea/genetic_aea.shp")
LTSA_ecotone <- st_read("./Study_extents/Study_extents_aea/ecotone_aea.shp")
LTSA_political <- st_read("./Study_extents/Study_extents_aea/political_aea.shp")
LTSA_range <- st_read("./Study_extents/Study_extents_aea/range_aea.shp")

## mask rasters to shapefiles (input rasters are at range - dont need to crop again)
LTSA_genetic_env <- mask(LTSA_range_env, LTSA_genetic)
LTSA_ecotone_env <- mask(LTSA_range_env, LTSA_ecotone)
LTSA_political_env <- mask(LTSA_range_env, LTSA_political)

plot(LTSA_ecotone_env)

## Read input localities for each study extent 
LTSA_genetic_locs <- read.csv("./Input_localities/model_subsets/LTSA_genetic_locs.csv")
LTSA_ecotone_locs <- read.csv("./Input_localities/model_subsets/LTSA_ecotone_locs.csv")
LTSA_political_locs <- read.csv("./Input_localities/model_subsets/LTSA_political_locs.csv")
LTSA_range_locs <- read.csv("./Input_localities/model_subsets/LTSA_range_locs.csv")

## Get input localities into a two column matrix
LTSA_genetic_pres <- subset(LTSA_genetic_locs, select=c(Long_m, Lat_m))
LTSA_ecotone_pres <- subset(LTSA_ecotone_locs, select=c(Long_m, Lat_m))
LTSA_political_pres <- subset(LTSA_political_locs, select=c(Long_m, Lat_m))
LTSA_range_pres <- subset(LTSA_range_locs, select=c(Long_m, Lat_m))

# add column of 1s for presences
LTSA_genetic_pres$Type <- 1
LTSA_ecotone_pres$Type <- 1
LTSA_political_pres$Type <- 1
LTSA_range_pres$Type <- 1

## Read in TGB localities (grab proper columns & add a Type column: 0 for absences) 
LTSA_genetic_TGB <- read.csv("./TGB/model_subsets/TGB_genetic.csv")
LTSA_ecotone_TGB <- read.csv("./TGB/model_subsets/TGB_ecotone.csv")
LTSA_political_TGB <- read.csv("./TGB/model_subsets/TGB_political.csv")
LTSA_range_TGB <- read.csv("./TGB/model_subsets/TGB_range.csv")

## Get TGB localities into a two column matrix 
LTSA_genetic_abs <- LTSA_genetic_TGB[,c("CommonName", "Long_m", "Lat_m")]
LTSA_ecotone_abs <- LTSA_ecotone_TGB[,c("CommonName", "Long_m", "Lat_m")]
LTSA_political_abs <- LTSA_political_TGB[,c("CommonName", "Long_m", "Lat_m")]
LTSA_range_abs <- LTSA_range_TGB[,c("CommonName", "Long_m", "Lat_m")]

# add column of 0s for absences
LTSA_genetic_abs$Type <- 0
LTSA_ecotone_abs$Type <- 0
LTSA_political_abs$Type <- 0
LTSA_range_abs$Type <- 0

##################################################
#Extract presences

LTSA_genetic_presenvi <- terra::extract(LTSA_genetic_env, LTSA_genetic_pres[,c("Long_m", "Lat_m")])
LTSA_genetic_presdat <- cbind (LTSA_genetic_pres, LTSA_genetic_presenvi)

LTSA_ecotone_presenvi <- terra::extract(LTSA_ecotone_env, LTSA_ecotone_pres[,c("Long_m", "Lat_m")])
LTSA_ecotone_presdat <- cbind (LTSA_ecotone_pres, LTSA_ecotone_presenvi)

LTSA_political_presenvi <- terra::extract(LTSA_political_env, LTSA_political_pres[,c("Long_m", "Lat_m")])
LTSA_political_presdat <- cbind (LTSA_political_pres, LTSA_political_presenvi)

LTSA_range_presenvi <- terra::extract(LTSA_range_env, LTSA_range_pres[,c("Long_m", "Lat_m")])
LTSA_range_presdat <- cbind (LTSA_range_pres, LTSA_range_presenvi)

#Extract TGB
  
LTSA_genetic_tgbenvi <- terra::extract(LTSA_genetic_env, LTSA_genetic_abs[,c("Long_m", "Lat_m")])
LTSA_genetic_tgbdat <- cbind (LTSA_genetic_abs, LTSA_genetic_tgbenvi)

LTSA_ecotone_tgbenvi <- terra::extract(LTSA_ecotone_env, LTSA_ecotone_abs[,c("Long_m", "Lat_m")])
LTSA_ecotone_tgbdat <- cbind (LTSA_ecotone_abs, LTSA_ecotone_tgbenvi)

LTSA_political_tgbenvi <- terra::extract(LTSA_political_env, LTSA_political_abs[,c("Long_m", "Lat_m")])
LTSA_political_tgbdat <- cbind (LTSA_political_abs, LTSA_political_tgbenvi)

LTSA_range_tgbenvi <- terra::extract(LTSA_range_env, LTSA_range_abs[,c("Long_m", "Lat_m")])
LTSA_range_tgbdat <- cbind (LTSA_range_abs, LTSA_range_tgbenvi)


## Save csvs
# Presences

write.csv(LTSA_genetic_presdat, "./TGB/extracted/genetic/genetic_presdat.csv", row.names=FALSE)
write.csv(LTSA_ecotone_presdat, "./TGB/extracted/ecotone/ecotone_presdat.csv", row.names=FALSE)
write.csv(LTSA_political_presdat, "./TGB/extracted/political/political_presdat.csv", row.names=FALSE)
write.csv(LTSA_range_presdat, "./TGB/extracted/range/range_presdat.csv", row.names=FALSE)

# tgb

write.csv(LTSA_genetic_tgbdat, "./TGB/extracted/genetic/genetic_tgbdat.csv", row.names=FALSE)
write.csv(LTSA_ecotone_tgbdat, "./TGB/extracted/ecotone/ecotone_tgbdat.csv", row.names=FALSE)
write.csv(LTSA_political_tgbdat, "./TGB/extracted/political/political_tgbdat.csv", row.names=FALSE)
write.csv(LTSA_range_tgbdat, "./TGB/extracted/range/range_tgbdat.csv", row.names=FALSE)
  

########################### END SECTION ###############################

################################# Climate + Non Climate ################################
rm(list=ls())

################################# LTSA CLIMATE+NON CLIMATE, RandBackground BE SURE TO note which variables are categorical (Landcover) ################################

## READ IN MANNUALLY SO THAT LANDCOVER CAN BE DEFINED AS FACTOR

#Range
Landcover_R <-raster("./Environmental_Variables/model_subsets/LandCover/Landcover_R.tif")
Landcover_R <- as.factor(Landcover_R)
levels(Landcover_R)
Landcover_R
plot(Landcover_R)

#political
Landcover_P <-raster("./Environmental_Variables/model_subsets/LandCover/Landcover_P.tif")
Landcover_P <- as.factor(Landcover_P)
levels(Landcover_P)
Landcover_P
plot(Landcover_P)

#ecoregion
Landcover_E <-raster("./Environmental_Variables/model_subsets/LandCover/Landcover_E.tif")
Landcover_E <- as.factor(Landcover_E)
levels(Landcover_E)
Landcover_E
plot(Landcover_E)

#genetic
Landcover_G <-raster("./Environmental_Variables/model_subsets/LandCover/Landcover_G.tif")
Landcover_G <- as.factor(Landcover_G)
levels(Landcover_G)
Landcover_G
plot(Landcover_G)


#writeRaster(Landcover, filename="./Environmental_Variables/Range/ClimateNonClimate_NOLC/Landcover.tif", format="GTiff",overwrite = TRUE)

#px <- ratify(Landcover, count = TRUE)

#write.dbf(levels(px)[[1]], file = "./Environmental_Variables/Range/ClimateNonClimate_NOLC/Landcover2.tif.vat.dbf")

#double check that levels are preserved
#Landcover_check<-raster("./Environmental_Variables/Range/ClimateNonClimate_NOLC/Landcover.tif")
#levels(Landcover_check)
#Landcover_check

## Read in environmental variable rasters (previously cropped to range limits) and stack them 

#Range
LTSA_rast_list <- list.files("./Environmental_Variables/Range/ClimateNonClimate_nosoil", pattern='.tif$', all.files=TRUE, full.names=TRUE)
LTSA_rast_list

LTSA_range_env <- stack(LTSA_rast_list,Landcover_R)

LTSA_range_env

plot(LTSA_range_env)

#Political
LTSA_rast_list <- list.files("./Environmental_Variables/model_subsets/political_nosoil", pattern='.tif$', all.files=TRUE, full.names=TRUE)
LTSA_rast_list

LTSA_pol_env <- stack(LTSA_rast_list,Landcover_P)

LTSA_pol_env

plot(LTSA_pol_env)

#ecoregion
LTSA_rast_list <- list.files("./Environmental_Variables/model_subsets/ecotone_nosoil", pattern='.tif$', all.files=TRUE, full.names=TRUE)
LTSA_rast_list

LTSA_eco_env <- stack(LTSA_rast_list,Landcover_E)

LTSA_eco_env

plot(LTSA_eco_env)

#genetic
LTSA_rast_list <- list.files("./Environmental_Variables/model_subsets/genetic_nosoil", pattern='.tif$', all.files=TRUE, full.names=TRUE)
LTSA_rast_list

LTSA_gen_env <- stack(LTSA_rast_list,Landcover_G)

LTSA_gen_env

plot(LTSA_gen_env)


## Create a list of the raster names in the file and properly name variables 

LTSA_names_list <- c("MAP","NFFD","NPP","PPT_sm","RH","SHM","TD","Wetness","Landcover")

names(LTSA_range_env) <- LTSA_names_list
names(LTSA_pol_env) <- LTSA_names_list
names(LTSA_eco_env) <- LTSA_names_list
names(LTSA_gen_env) <- LTSA_names_list

plot(LTSA_range_env)

## Read input localities for each study extent 
LTSA_genetic_locs <- read.csv("./Input_localities/model_subsets/LTSA_genetic_locs.csv")
LTSA_ecotone_locs <- read.csv("./Input_localities/model_subsets/LTSA_ecotone_locs.csv")
LTSA_political_locs <- read.csv("./Input_localities/model_subsets/LTSA_political_locs.csv")
LTSA_range_locs <- read.csv("./Input_localities/model_subsets/LTSA_range_locs.csv")

## Get input localities into a two column matrix
LTSA_genetic_pres <- subset(LTSA_genetic_locs, select=c(Long_m, Lat_m))
LTSA_ecotone_pres <- subset(LTSA_ecotone_locs, select=c(Long_m, Lat_m))
LTSA_political_pres <- subset(LTSA_political_locs, select=c(Long_m, Lat_m))
LTSA_range_pres <- subset(LTSA_range_locs, select=c(Long_m, Lat_m))

# add column of 1s for presences
LTSA_genetic_pres$Type <- 1
LTSA_ecotone_pres$Type <- 1
LTSA_political_pres$Type <- 1
LTSA_range_pres$Type <- 1

## Read in TGB localities (grab proper columns & add a Type column: 0 for absences) 
LTSA_genetic_TGB <- read.csv("./TGB/model_subsets/TGB_genetic.csv")
LTSA_ecotone_TGB <- read.csv("./TGB/model_subsets/TGB_ecotone.csv")
LTSA_political_TGB <- read.csv("./TGB/model_subsets/TGB_political.csv")
LTSA_range_TGB <- read.csv("./TGB/model_subsets/TGB_range.csv")

## Get TGB localities into a two column matrix 
LTSA_genetic_abs <- LTSA_genetic_TGB[,c("CommonName", "Long_m", "Lat_m")]
LTSA_ecotone_abs <- LTSA_ecotone_TGB[,c("CommonName", "Long_m", "Lat_m")]
LTSA_political_abs <- LTSA_political_TGB[,c("CommonName", "Long_m", "Lat_m")]
LTSA_range_abs <- LTSA_range_TGB[,c("CommonName", "Long_m", "Lat_m")]

# add column of 0s for absences
LTSA_genetic_abs$Type <- 0
LTSA_ecotone_abs$Type <- 0
LTSA_political_abs$Type <- 0
LTSA_range_abs$Type <- 0

##################################################
#Extract presences

LTSA_genetic_presenvi <- terra::extract(LTSA_gen_env, LTSA_genetic_pres[,c("Long_m", "Lat_m")])
LTSA_genetic_presdat <- cbind (LTSA_genetic_pres, LTSA_genetic_presenvi)

LTSA_ecotone_presenvi <- terra::extract(LTSA_eco_env, LTSA_ecotone_pres[,c("Long_m", "Lat_m")])
LTSA_ecotone_presdat <- cbind (LTSA_ecotone_pres, LTSA_ecotone_presenvi)

LTSA_political_presenvi <- terra::extract(LTSA_pol_env, LTSA_political_pres[,c("Long_m", "Lat_m")])
LTSA_political_presdat <- cbind (LTSA_political_pres, LTSA_political_presenvi)

LTSA_range_presenvi <- terra::extract(LTSA_range_env, LTSA_range_pres[,c("Long_m", "Lat_m")])
LTSA_range_presdat <- cbind (LTSA_range_pres, LTSA_range_presenvi)

#Extract TGB

LTSA_genetic_tgbenvi <- terra::extract(LTSA_gen_env, LTSA_genetic_abs[,c("Long_m", "Lat_m")])
LTSA_genetic_tgbdat <- cbind (LTSA_genetic_abs, LTSA_genetic_tgbenvi)

LTSA_ecotone_tgbenvi <- terra::extract(LTSA_eco_env, LTSA_ecotone_abs[,c("Long_m", "Lat_m")])
LTSA_ecotone_tgbdat <- cbind (LTSA_ecotone_abs, LTSA_ecotone_tgbenvi)

LTSA_political_tgbenvi <- terra::extract(LTSA_pol_env, LTSA_political_abs[,c("Long_m", "Lat_m")])
LTSA_political_tgbdat <- cbind (LTSA_political_abs, LTSA_political_tgbenvi)

LTSA_range_tgbenvi <- terra::extract(LTSA_range_env, LTSA_range_abs[,c("Long_m", "Lat_m")])
LTSA_range_tgbdat <- cbind (LTSA_range_abs, LTSA_range_tgbenvi)


## Save csvs
# Presences

write.csv(LTSA_genetic_presdat, "./TGB/extracted/ClimNonClim/genetic/genetic_presdat.csv", row.names=FALSE)
write.csv(LTSA_ecotone_presdat, "./TGB/extracted/ClimNonClim/ecotone/ecotone_presdat.csv", row.names=FALSE)
write.csv(LTSA_political_presdat, "./TGB/extracted/ClimNonClim/political/political_presdat.csv", row.names=FALSE)
write.csv(LTSA_range_presdat, "./TGB/extracted/ClimNonClim/range/range_presdat.csv", row.names=FALSE)

# tgb

write.csv(LTSA_genetic_tgbdat, "./TGB/extracted/ClimNonClim/genetic/genetic_tgbdat.csv", row.names=FALSE)
write.csv(LTSA_ecotone_tgbdat, "./TGB/extracted/ClimNonClim/ecotone/ecotone_tgbdat.csv", row.names=FALSE)
write.csv(LTSA_political_tgbdat, "./TGB/extracted/ClimNonClim/political/political_tgbdat.csv", row.names=FALSE)
write.csv(LTSA_range_tgbdat, "./TGB/extracted/ClimNonClim/range/range_tgbdat.csv", row.names=FALSE)

########################### END SECTION ##############################

########################## FINAL COMMENTS ############################

## 

########################### END SCRIPT ###############################