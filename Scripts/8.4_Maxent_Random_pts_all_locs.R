########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Jayna Bergman , updated by Kaegan Finn Dec 5

### Goal of this Script: 

# Runs maxent using random background points and all the input localities 

### Notes: Uses uncorrelated variables (from Pearson Correlation)

# Creates a prediction surface raster 

### Date: June 9, 2022 , Nov 30 2022

### Version of R:  R version 4.0.3

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(dismo)
library(sf)
library(terra)
library(rgdal)
library(dplyr)

########################### END SECTION ##############################

###################### CLEAR ENVIRONMENTS ##########################

rm(list=ls())

########################### END SECTION ##############################

########################## DATA AND ANALYSIS #########################

## Set working directory

getwd()
setwd()

################################# CLIMATE ONLY ################################

## Read in environmental variable rasters (previously cropped to range limits) and stack them 

LTSA_rast_list <- list.files("./Environmental_Variables/Range/ClimateOnly", pattern='.tif$', all.files=TRUE, full.names=TRUE)
LTSA_rast_list

LTSA_range_env <- stack(LTSA_rast_list)
plot(LTSA_range_env)


## Create a list of the raster names in the file and properly name variables 

LTSA_names_list <- gsub(pattern=".tif", replacement="", list.files("./Environmental_Variables/Range/ClimateOnly", pattern='.tif$', all.files=TRUE, full.names=FALSE))

names(LTSA_range_env) <- LTSA_names_list


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


## Read input localities for each study extent 

LTSA_genetic_locs <- read.csv("./Input_localities/model_subsets/LTSA_genetic_locs.csv")
LTSA_ecotone_locs <- read.csv("./Input_localities/model_subsets/LTSA_ecotone_locs.csv")
LTSA_political_locs <- read.csv("./Input_localities/model_subsets/LTSA_political_locs.csv")
LTSA_range_locs <- read.csv("./Input_localities/model_subsets/LTSA_range_locs.csv")


## Get input localities into a two column matrix

LTSA_genetic_locs <- subset(LTSA_genetic_locs, select=c(Long_m, Lat_m))
LTSA_ecotone_locs <- subset(LTSA_ecotone_locs, select=c(Long_m, Lat_m))
LTSA_political_locs <- subset(LTSA_political_locs, select=c(Long_m, Lat_m))
LTSA_range_locs <- subset(LTSA_range_locs, select=c(Long_m, Lat_m))

## Read study extent shapefiles in (to be able to crop rasters)

LTSA_genetic <- st_read("./Study_extents/Study_extents_aea/genetic_aea.shp")
LTSA_ecotone <- st_read("./Study_extents/Study_extents_aea/ecotone_aea.shp")
LTSA_political <- st_read("./Study_extents/Study_extents_aea/political_aea.shp")
LTSA_range <- st_read("./Study_extents/Study_extents_aea/range_aea.shp")

## mask rasters to shapefiles (input rasters are at range - dont need to crop again)

LTSA_genetic_env <- mask(LTSA_range_env, LTSA_genetic)
LTSA_ecotone_env <- mask(LTSA_range_env, LTSA_ecotone)
LTSA_political_env <- mask(LTSA_range_env, LTSA_political)
LTSA_eco_env_ssp245 <- mask(LTSA_range_env_ssp245, LTSA_ecotone)
LTSA_eco_env_ssp585 <- mask(LTSA_range_env_ssp585, LTSA_ecotone)
LTSA_gen_env_ssp245 <- mask(LTSA_range_env_ssp245, LTSA_genetic)
LTSA_gen_env_ssp585 <- mask(LTSA_range_env_ssp585, LTSA_genetic)
LTSA_pol_env_ssp245 <- mask(LTSA_range_env_ssp245, LTSA_political)
LTSA_pol_env_ssp585 <- mask(LTSA_range_env_ssp585, LTSA_political)


###### range study extent ######

## Setting parameters from 6.1_Tuning script

#LTSA_range_features <- c("") #All features used
LTSA_range_reg <- 0.5

#featureOmission <- get(paste("LTSA_range_features",sep="")) 
r <- get(paste("LTSA_range_reg",sep="")) # regularization parameter

## Setting maxent arguments 

basicargs <- c("-J","-P","writebackgroundpredictions","maximumiterations=5000")
LTSA_range_max_args <- c(basicargs, features=c("Threshold"), paste("betamultiplier=",r,sep=""))

## Create new directory for output files 

Range <- paste("./Maxent/ClimateOnly/RB/Range")

## Maxent command and save

ClimOnly_RB_Range <- maxent(x=LTSA_range_env, p=LTSA_range_locs[,c("Long_m", "Lat_m")], nbg= 10000, removeDuplicates = FALSE, args = LTSA_range_max_args, path=Range)

saveRDS(ClimOnly_RB_Range, "./Maxent/ClimateOnly/RB/Range/ClimOnly_RB_Range")

ClimOnly_RB_Range <- readRDS("./Maxent/ClimateOnly/RB/Range/ClimOnly_RB_Range")

## Creating prediction surface  raw, cumulative, logistic and cloglog
#logistic output
ClimOnly_RB_Range_log <- predict(ClimOnly_RB_Range, LTSA_range_env_ssp585, 
                              args = "outputformat=logistic", progress="", 
                              defaultprevalence = 0.58,
                              overwrite=TRUE,
                              filename=paste(Range,"/ClimOnly_RB_Range_log_ssp585",sep=""),format="GTiff")

#cumulative output
ClimOnly_RB_Range_cum <- predict(ClimOnly_RB_Range, LTSA_range_env, 
                                 args = "outputformat=cumulative", progress="", 
                                 defaultprevalence = 0.58,
                                 overwrite=TRUE,
                                 filename=paste(Range,"/ClimOnly_RB_Range_cum",sep=""),format="GTiff")

#raw output
ClimOnly_RB_Range_raw <- predict(ClimOnly_RB_Range, LTSA_range_env, 
                                 args = "outputformat=raw", progress="", 
                                 defaultprevalence = 0.58,
                                 overwrite=TRUE,
                                 filename=paste(Range,"/ClimOnly_RB_Range_raw",sep=""),format="GTiff")

#cloglog output
ClimOnly_RB_Range_cloglog <- predict(ClimOnly_RB_Range, LTSA_range_env, 
                                 args = "outputformat=cloglog", progress="", 
                                 defaultprevalence = 0.58,
                                 overwrite=TRUE,
                                 filename=paste(Range,"/ClimOnly_RB_Range_cloglog",sep=""),format="GTiff")

plot(ClimOnly_RB_Range_log)


#Try to pull each of these up in ARC tomorrow to make sure they worked# They did! Except Cumulative. This must be derived from raw

###### political study extent ######

#Setting parameters from 6.1_Tuning script

#LTSA_political_features <- c("") #All features used
LTSA_political_reg <- 1.5

#featureOmission <- get(paste("LTSA_political_features",sep="")) 
r <- get(paste("LTSA_political_reg",sep="")) # regularization parameter

## Setting maxent arguments 

basicargs <- c("-J","-P","writebackgroundpredictions","maximumiterations=5000")
LTSA_political_max_args <- c(basicargs, features=c("Threshold"), paste("betamultiplier=",r,sep=""))

## Create new directory for output files 

political <- paste("./Maxent/ClimateOnly/RB/political")

## Maxent command and save

ClimOnly_RB_political <- maxent(x=LTSA_political_env, p=LTSA_political_locs[,c("Long_m", "Lat_m")], nbg= 10000, removeDuplicates = FALSE, args = LTSA_political_max_args, path=political)

saveRDS(ClimOnly_RB_political, "./Maxent/ClimateOnly/RB/political/ClimOnly_RB_political")

ClimOnly_RB_political <- readRDS("./Maxent/ClimateOnly/RB/political/ClimOnly_RB_political")


## Creating prediction surface  raw, cumulative, logistic and cloglog
#logistic output
ClimOnly_RB_political_log <- predict(ClimOnly_RB_political, LTSA_political_env_ssp585, 
                                 args = "outputformat=logistic", progress="", 
                                 defaultprevalence = 0.58,
                                 overwrite=TRUE,
                                 filename=paste(political,"/ClimOnly_RB_political_log_ssp585",sep=""),format="GTiff")

#cumulative output
ClimOnly_RB_political_cum <- predict(ClimOnly_RB_political, LTSA_political_env, 
                                 args = "outputformat=cumulative", progress="", 
                                 defaultprevalence = 0.58,
                                 overwrite=TRUE,
                                 filename=paste(political,"/ClimOnly_RB_political_cum",sep=""),format="GTiff")

#raw output
ClimOnly_RB_political_raw <- predict(ClimOnly_RB_political, LTSA_political_env, 
                                 args = "outputformat=raw", progress="", 
                                 defaultprevalence = 0.58,
                                 overwrite=TRUE,
                                 filename=paste(political,"/ClimOnly_RB_political_raw",sep=""),format="GTiff")

#cloglog output
ClimOnly_RB_political_cloglog <- predict(ClimOnly_RB_political, LTSA_political_env, 
                                     args = "outputformat=cloglog", progress="", 
                                     defaultprevalence = 0.58,
                                     overwrite=TRUE,
                                     filename=paste(political,"/ClimOnly_RB_political_cloglog",sep=""),format="GTiff")

plot(ClimOnly_RB_political_log)

###### genetic study extent ######

#Setting parameters from 6.1_Tuning script

#LTSA_genetic_features <- c("") #All features used
LTSA_genetic_reg <- 0.5

#featureOmission <- get(paste("LTSA_genetic_features",sep="")) 
r <- get(paste("LTSA_genetic_reg",sep="")) # regularization parameter

## Setting maxent arguments 

basicargs <- c("-J","-P","writebackgroundpredictions","maximumiterations=5000")
LTSA_genetic_max_args <- c(basicargs, features=c("Threshold"), paste("betamultiplier=",r,sep=""))

## Create new directory for output files 

genetic <- paste("./Maxent/ClimateOnly/RB/genetic")

## Maxent command and save

ClimOnly_RB_genetic <- maxent(x=LTSA_genetic_env, p=LTSA_genetic_locs[,c("Long_m", "Lat_m")], nbg= 10000, removeDuplicates = FALSE, args = LTSA_genetic_max_args, path=genetic)

saveRDS(ClimOnly_RB_genetic, "./Maxent/ClimateOnly/RB/genetic/ClimOnly_RB_genetic")

ClimOnly_RB_genetic <- readRDS("./Maxent/ClimateOnly/RB/genetic/ClimOnly_RB_genetic")


## Creating prediction surface  raw, cumulative, logistic and cloglog
#logistic output
ClimOnly_RB_genetic_log <- predict(ClimOnly_RB_genetic, LTSA_pol_env_ssp245, 
                                     args = "outputformat=logistic", progress="", 
                                     defaultprevalence = 0.58,
                                     overwrite=TRUE,
                                     filename=paste(genetic,"/LTSA_pol_env_ssp245",sep=""),format="GTiff")

#cumulative output
ClimOnly_RB_genetic_cum <- predict(ClimOnly_RB_genetic, LTSA_genetic_env, 
                                     args = "outputformat=cumulative", progress="", 
                                     defaultprevalence = 0.58,
                                     overwrite=TRUE,
                                     filename=paste(genetic,"/ClimOnly_RB_genetic_cum",sep=""),format="GTiff")

#raw output
ClimOnly_RB_genetic_raw <- predict(ClimOnly_RB_genetic, LTSA_genetic_env, 
                                     args = "outputformat=raw", progress="", 
                                     defaultprevalence = 0.58,
                                     overwrite=TRUE,
                                     filename=paste(genetic,"/ClimOnly_RB_genetic_raw",sep=""),format="GTiff")

#cloglog output
ClimOnly_RB_genetic_cloglog <- predict(ClimOnly_RB_genetic, LTSA_genetic_env, 
                                         args = "outputformat=cloglog", progress="", 
                                         defaultprevalence = 0.58,
                                         overwrite=TRUE,
                                         filename=paste(genetic,"/ClimOnly_RB_genetic_cloglog",sep=""),format="GTiff")

plot(ClimOnly_RB_genetic_log)


###### ecoregion study extent ######

#Setting parameters from 6.1_Tuning script

#LTSA_ecotone_features <- c("") #All features used
LTSA_ecotone_reg <- 0.5

#featureOmission <- get(paste("LTSA_ecotone_features",sep="")) 
r <- get(paste("LTSA_ecotone_reg",sep="")) # regularization parameter

## Setting maxent arguments 

basicargs <- c("-J","-P","writebackgroundpredictions","maximumiterations=5000")
LTSA_ecotone_max_args <- c(basicargs, paste("betamultiplier=",r,sep=""))

## Create new directory for output files 

ecotone <- paste("./Maxent/ClimateOnly/RB/ecotone")

## Maxent command and save

ClimOnly_RB_ecotone <- maxent(x=LTSA_ecotone_env, p=LTSA_ecotone_locs[,c("Long_m", "Lat_m")], nbg= 10000, removeDuplicates = FALSE, args = LTSA_ecotone_max_args, path=ecotone)

saveRDS(ClimOnly_RB_ecotone, "./Maxent/ClimateOnly/RB/ecotone/ClimOnly_RB_ecotone")

ClimOnly_RB_ecotone <- readRDS("./Maxent/ClimateOnly/RB/ecotone/ClimOnly_RB_ecotone")

## Creating prediction surface  raw, cumulative, logistic and cloglog
#logistic output
ClimOnly_RB_ecotone_log <- predict(ClimOnly_RB_ecotone, LTSA_pol_env_ssp245, 
                                     args = "outputformat=logistic", progress="", 
                                     defaultprevalence = 0.58,
                                     overwrite=TRUE,
                                     filename=paste(ecotone,"/LTSA_pol_env_ssp245",sep=""),format="GTiff")

#cumulative output
ClimOnly_RB_ecotone_cum <- predict(ClimOnly_RB_ecotone, LTSA_ecotone_env, 
                                     args = "outputformat=cumulative", progress="",
                                    defaultprevalence = 0.58,
                                     overwrite=TRUE,
                                     filename=paste(ecotone,"/ClimOnly_RB_ecotone_cum",sep=""),format="GTiff")

#raw output
ClimOnly_RB_ecotone_raw <- predict(ClimOnly_RB_ecotone, LTSA_ecotone_env, 
                                     args = "outputformat=raw", progress="", 
                                     defaultprevalence = 0.58,
                                     overwrite=TRUE,
                                     filename=paste(ecotone,"/ClimOnly_RB_ecotone_raw",sep=""),format="GTiff")

#cloglog output
ClimOnly_RB_ecotone_cloglog <- predict(ClimOnly_RB_ecotone, LTSA_ecotone_env, 
                                         args = "outputformat=cloglog", progress="", 
                                         defaultprevalence = 0.58,
                                         overwrite=TRUE,
                                         filename=paste(ecotone,"/ClimOnly_RB_ecotone_cloglog",sep=""),format="GTiff")

plot(ClimOnly_RB_ecotone_log)






################################# CLIMATE + NONCLIMATE ################################
## READ IN MANNUALLY SO THAT LANDCOVER CAN BE DEFINED AS FACTOR

#Range
Landcover_R <-raster("./Environmental_Variables/model_subsets/LandCover/Landcover_R.tif")
Landcover_R <- as.factor(Landcover_R)
levels(Landcover_R)
Landcover_R

#political
Landcover_P <-raster("./Environmental_Variables/model_subsets/LandCover/Landcover_P.tif")
Landcover_P <- as.factor(Landcover_P)
levels(Landcover_P)
Landcover_P

#ecoregion
Landcover_E <-raster("./Environmental_Variables/model_subsets/LandCover/Landcover_E.tif")
Landcover_E <- as.factor(Landcover_E)
levels(Landcover_E)
Landcover_E

#genetic
Landcover_G <-raster("./Environmental_Variables/model_subsets/LandCover/Landcover_G.tif")
Landcover_G <- as.factor(Landcover_G)
levels(Landcover_G)
Landcover_G

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

#Political
LTSA_rast_list <- list.files("./Environmental_Variables/model_subsets/political_nosoil", pattern='.tif$', all.files=TRUE, full.names=TRUE)
LTSA_rast_list

LTSA_pol_env <- stack(LTSA_rast_list,Landcover_P)

plot(LTSA_pol_env)

#ecoregion
LTSA_rast_list <- list.files("./Environmental_Variables/model_subsets/ecotone_nosoil", pattern='.tif$', all.files=TRUE, full.names=TRUE)
LTSA_rast_list

LTSA_eco_env <- stack(LTSA_rast_list,Landcover_E)

LTSA_eco_env

#genetic
LTSA_rast_list <- list.files("./Environmental_Variables/model_subsets/genetic_nosoil", pattern='.tif$', all.files=TRUE, full.names=TRUE)
LTSA_rast_list

LTSA_gen_env <- stack(LTSA_rast_list,Landcover_G)

LTSA_gen_env

## Create a list of the raster names in the file and properly name variables 

LTSA_names_list <- c("MAP","NFFD","NPP","PPT_sm","RH","SHM","TD","Wetness","Landcover")

names(LTSA_range_env) <- LTSA_names_list
names(LTSA_pol_env) <- LTSA_names_list
names(LTSA_eco_env) <- LTSA_names_list
names(LTSA_gen_env) <- LTSA_names_list

plot(LTSA_pol_env)


## Read input localities for each study extent 

LTSA_genetic_locs <- read.csv("./Input_localities/model_subsets/LTSA_genetic_locs.csv")
LTSA_ecotone_locs <- read.csv("./Input_localities/model_subsets//LTSA_ecotone_locs.csv")
LTSA_political_locs <- read.csv("./Input_localities/model_subsets/LTSA_political_locs.csv")
LTSA_range_locs <- read.csv("./Input_localities/model_subsets/LTSA_range_locs.csv")


## Get input localities into a two column matrix

LTSA_genetic_locs <- subset(LTSA_genetic_locs, select=c(Long_m, Lat_m))
LTSA_ecotone_locs <- subset(LTSA_ecotone_locs, select=c(Long_m, Lat_m))
LTSA_political_locs <- subset(LTSA_political_locs, select=c(Long_m, Lat_m))
LTSA_range_locs <- subset(LTSA_range_locs, select=c(Long_m, Lat_m))

###### range study extent ######

## Setting parameters from 6.1_Tuning script

#LTSA_range_features <- c("") #All features used
LTSA_range_reg <- 0.5

#featureOmission <- get(paste("LTSA_range_features",sep="")) 
r <- get(paste("LTSA_range_reg",sep="")) # regularization parameter

## Setting maxent arguments 

basicargs <- c("-J","-P","writebackgroundpredictions","maximumiterations=5000")
LTSA_range_max_args <- c(basicargs, features=c("Threshold"), paste("betamultiplier=",r,sep=""))

## Create new directory for output files 

Range <- paste("./Maxent/ClimNonClim/RB/Range")

## Maxent command and save

ClimNonClim_RB_Range <- maxent(x=LTSA_range_env, p=LTSA_range_locs[,c("Long_m", "Lat_m")], factors = "Landcover", nbg= 10000, removeDuplicates = FALSE, args = LTSA_range_max_args, path=Range)

saveRDS(ClimNonClim_RB_Range, "./Maxent/ClimNonClim/RB/range/ClimNonClim_RB_Range")

## Creating prediction surface  raw, cumulative, logistic and cloglog
#logistic output
ClimNonClim_RB_Range_log <- predict(ClimNonClim_RB_Range, LTSA_range_env, 
                                 args = "outputformat=logistic", progress="", 
                                 defaultprevalence = 0.58,
                                 overwrite=TRUE,
                                 filename=paste(Range,"/ClimNonClim_RB_Range_log",sep=""),format="GTiff")

#cumulative output
ClimNonClim_RB_Range_cum <- predict(ClimNonClim_RB_Range, LTSA_range_env, 
                                 args = "outputformat=cumulative", progress="", 
                                 defaultprevalence = 0.58,
                                 overwrite=TRUE,
                                 filename=paste(Range,"/ClimNonClim_RB_Range_cum",sep=""),format="GTiff")

#raw output
ClimNonClim_RB_Range_raw <- predict(ClimNonClim_RB_Range, LTSA_range_env, 
                                 args = "outputformat=raw", progress="", 
                                 defaultprevalence = 0.58,
                                 overwrite=TRUE,
                                 filename=paste(Range,"/ClimNonClim_RB_Range_raw",sep=""),format="GTiff")

#cloglog output
ClimNonClim_RB_Range_cloglog <- predict(ClimNonClim_RB_Range, LTSA_range_env, 
                                     args = "outputformat=cloglog", progress="", 
                                     defaultprevalence = 0.58,
                                     overwrite=TRUE,
                                     filename=paste(Range,"/ClimNonClim_RB_Range_cloglog",sep=""),format="GTiff")

plot(ClimNonClim_RB_Range_log)


#Try to pull each of these up in ARC tomorrow to make sure they worked

###### political study extent ######

#Setting parameters from 6.1_Tuning script

#LTSA_political_features <- c("") #All features used
LTSA_political_reg <- 0.5

#featureOmission <- get(paste("LTSA_political_features",sep="")) 
r <- get(paste("LTSA_political_reg",sep="")) # regularization parameter

## Setting maxent arguments 

basicargs <- c("-J","-P","writebackgroundpredictions","maximumiterations=5000")
LTSA_political_max_args <- c(basicargs, features=c("nohinge","noproduct"), paste("betamultiplier=",r,sep=""))

## Create new directory for output files 

political <- paste("./Maxent/ClimNonClim/RB/political")

## Maxent command and save

ClimNonClim_RB_political <- maxent(x=LTSA_pol_env, p=LTSA_political_locs[,c("Long_m", "Lat_m")],factors = "Landcover", nbg= 10000, removeDuplicates = FALSE, args = LTSA_political_max_args, path=political)

saveRDS(ClimNonClim_RB_political, "./Maxent/ClimNonClim/RB/political/ClimNonClim_RB_political")

## Creating prediction surface  raw, cumulative, logistic and cloglog
#logistic output
ClimNonClim_RB_political_log <- predict(ClimNonClim_RB_political, LTSA_pol_env, 
                                     args = "outputformat=logistic", progress="", 
                                     defaultprevalence = 0.58,
                                     overwrite=TRUE,
                                     filename=paste(political,"/ClimNonClim_RB_political_log",sep=""),format="GTiff")

#cumulative output
ClimNonClim_RB_political_cum <- predict(ClimNonClim_RB_political, LTSA_pol_env, 
                                     args = "outputformat=cumulative", progress="", 
                                     defaultprevalence = 0.58,
                                     overwrite=TRUE,
                                     filename=paste(political,"/ClimNonClim_RB_political_cum",sep=""),format="GTiff")

#raw output
ClimNonClim_RB_political_raw <- predict(ClimNonClim_RB_political, LTSA_pol_env, 
                                     args = "outputformat=raw", progress="", 
                                     defaultprevalence = 0.58,
                                     overwrite=TRUE,
                                     filename=paste(political,"/ClimNonClim_RB_political_raw",sep=""),format="GTiff")

#cloglog output
ClimNonClim_RB_political_cloglog <- predict(ClimNonClim_RB_political, LTSA_pol_env, 
                                         args = "outputformat=cloglog", progress="", 
                                         defaultprevalence = 0.58,
                                         overwrite=TRUE,
                                         filename=paste(political,"/ClimNonClim_RB_political_cloglog",sep=""),format="GTiff")

plot(ClimNonClim_RB_political_log)

###### genetic study extent ######

#Setting parameters from 6.1_Tuning script

#LTSA_genetic_features <- c("") #All features used
LTSA_genetic_reg <- 0.5

#featureOmission <- get(paste("LTSA_genetic_features",sep="")) 
r <- get(paste("LTSA_genetic_reg",sep="")) # regularization parameter

## Setting maxent arguments 

basicargs <- c("-J","-P","writebackgroundpredictions","maximumiterations=5000")
LTSA_genetic_max_args <- c(basicargs, features=c("Threshold"), paste("betamultiplier=",r,sep=""))

## Create new directory for output files 

genetic <- paste("./Maxent/ClimNonClim/RB/genetic")

## Maxent command and save

ClimNonClim_RB_genetic <- maxent(x=LTSA_gen_env, p=LTSA_genetic_locs[,c("Long_m", "Lat_m")],factors = "Landcover", nbg= 10000, removeDuplicates = FALSE, args = LTSA_genetic_max_args, path=genetic)

saveRDS(ClimNonClim_RB_genetic, "./Maxent/ClimNonClim/RB/genetic/ClimNonClim_RB_genetic")

ClimNonClim_RB_genetic <- readRDS("./Maxent/ClimNonClim/RB/genetic/ClimNonClim_RB_genetic")

## Creating prediction surface  raw, cumulative, logistic and cloglog
#logistic output
ClimNonClim_RB_genetic_log <- predict(ClimNonClim_RB_genetic, LTSA_pol_env, 
                                   args = "outputformat=logistic", progress="", 
                                   defaultprevalence = 0.58,
                                   overwrite=TRUE,
                                   filename=paste(genetic,"/ClimNonClim_RB_genetic_log_ROI",sep=""),format="GTiff")

#cumulative output
ClimNonClim_RB_genetic_cum <- predict(ClimNonClim_RB_genetic, LTSA_gen_env, 
                                   args = "outputformat=cumulative", progress="", 
                                   defaultprevalence = 0.58,
                                   overwrite=TRUE,
                                   filename=paste(genetic,"/ClimNonClim_RB_genetic_cum",sep=""),format="GTiff")

#raw output
ClimNonClim_RB_genetic_raw <- predict(ClimNonClim_RB_genetic, LTSA_gen_env, 
                                   args = "outputformat=raw", progress="", 
                                   defaultprevalence = 0.58,
                                   overwrite=TRUE,
                                   filename=paste(genetic,"/ClimNonClim_RB_genetic_raw",sep=""),format="GTiff")

#cloglog output
ClimNonClim_RB_genetic_cloglog <- predict(ClimNonClim_RB_genetic, LTSA_gen_env, 
                                       args = "outputformat=cloglog", progress="", 
                                       defaultprevalence = 0.58,
                                       overwrite=TRUE,
                                       filename=paste(genetic,"/ClimNonClim_RB_genetic_cloglog",sep=""),format="GTiff")

plot(ClimNonClim_RB_genetic_log)


###### ecoregion study extent ######

#Setting parameters from 6.1_Tuning script

#LTSA_ecotone_features <- c("") #All features used
LTSA_ecotone_reg <- 0.5

#featureOmission <- get(paste("LTSA_ecotone_features",sep="")) 
r <- get(paste("LTSA_ecotone_reg",sep="")) # regularization parameter

## Setting maxent arguments 

basicargs <- c("-J","-P","writebackgroundpredictions","maximumiterations=5000")
LTSA_ecotone_max_args <- c(basicargs, features=c(""), paste("betamultiplier=",r,sep=""))

## Create new directory for output files 

ecotone <- paste("./Maxent/ClimNonClim/RB/ecotone")

## Maxent command and save

ClimNonClim_RB_ecotone <- maxent(x=LTSA_eco_env, p=LTSA_ecotone_locs[,c("Long_m", "Lat_m")],factors = "Landcover", nbg= 10000, removeDuplicates = FALSE, args = LTSA_ecotone_max_args, path=ecotone)

saveRDS(ClimNonClim_RB_ecotone, "./Maxent/ClimNonClim/RB/ecotone/ClimNonClim_RB_ecotone")

ClimNonClim_RB_ecotone <- readRDS("./Maxent/ClimNonClim/RB/ecotone/ClimNonClim_RB_ecotone")


## Creating prediction surface  raw, cumulative, logistic and cloglog
#logistic output
ClimNonClim_RB_ecotone_log <- predict(ClimNonClim_RB_ecotone, LTSA_pol_env, 
                                   args = "outputformat=logistic", progress="", 
                                   defaultprevalence = 0.58,
                                   overwrite=TRUE,
                                   filename=paste(ecotone,"/ClimNonClim_RB_ecotone_log_ROI",sep=""),format="GTiff")

#cumulative output
ClimNonClim_RB_ecotone_cum <- predict(ClimNonClim_RB_ecotone, LTSA_eco_env, 
                                   args = "outputformat=cumulative", progress="", 
                                   defaultprevalence = 0.58,
                                   overwrite=TRUE,
                                   filename=paste(ecotone,"/ClimNonClim_RB_ecotone_cum",sep=""),format="GTiff")

#raw output
ClimNonClim_RB_ecotone_raw <- predict(ClimNonClim_RB_ecotone, LTSA_eco_env, 
                                   args = "outputformat=raw", progress="", 
                                   defaultprevalence = 0.58,
                                   overwrite=TRUE,
                                   filename=paste(ecotone,"/ClimNonClim_RB_ecotone_raw",sep=""),format="GTiff")

#cloglog output
ClimNonClim_RB_ecotone_cloglog <- predict(ClimNonClim_RB_ecotone, LTSA_eco_env, 
                                       args = "outputformat=cloglog", progress="", 
                                       defaultprevalence = 0.58,
                                       overwrite=TRUE,
                                       filename=paste(ecotone,"/ClimNonClim_RB_ecotone_cloglog",sep=""),format="GTiff")

plot(ClimNonClim_RB_ecotone_log)



########################## FINAL COMMENTS ############################

##

########################### END SCRIPT ###############################
