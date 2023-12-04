########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Kaegan Finn, Oct 20th, 2023

### Goal of this Script: 

# Runs maxent using only non-climate variables. In response to JAE Review sent on October 18, 2023

# Creates a prediction surface raster 

### Version of R:  R version 4.2.

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

## START WITH ONLY THE BEST MODEL, (RANDOM + CLIM+NONCLIM + POLITICAL)

## READ IN MANNUALLY SO THAT LANDCOVER CAN BE DEFINED AS FACTOR

#political
Landcover_P <-raster("./Environmental_Variables/model_subsets/LandCover/Landcover_P.tif")
Landcover_P <- as.factor(Landcover_P)
levels(Landcover_P)
Landcover_P

#writeRaster(Landcover, filename="./Environmental_Variables/Range/ClimateNonClimate_NOLC/Landcover.tif", format="GTiff",overwrite = TRUE)

#px <- ratify(Landcover, count = TRUE)

#write.dbf(levels(px)[[1]], file = "./Environmental_Variables/Range/ClimateNonClimate_NOLC/Landcover2.tif.vat.dbf")

#double check that levels are preserved
#Landcover_check<-raster("./Environmental_Variables/Range/ClimateNonClimate_NOLC/Landcover.tif")
#levels(Landcover_check)
#Landcover_check

## Read in environmental variable rasters (previously cropped to range limits) and stack them 

#Political
LTSA_rast_list <- list.files("./Environmental_Variables/model_subsets/political_nonclim", pattern='.tif$', all.files=TRUE, full.names=TRUE)
LTSA_rast_list

LTSA_pol_env <- stack(LTSA_rast_list,Landcover_P)

plot(LTSA_pol_env)

## Create a list of the raster names in the file and properly name variables 

LTSA_names_list <- c("NPP","Wetness","Landcover")

names(LTSA_pol_env) <- LTSA_names_list

plot(LTSA_pol_env)

## Read input localities for each study extent 

LTSA_political_locs <- read.csv("./Input_localities/model_subsets/LTSA_political_locs.csv")

## Get input localities into a two column matrix

LTSA_political_locs <- subset(LTSA_political_locs, select=c(Long_m, Lat_m))


###
### MODEL TIME ####
###
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

political <- paste("./Maxent/NonClimOnly/RB/political")

## Maxent command and save

NonClimOnly_RB_political <- maxent(x=LTSA_pol_env, p=LTSA_political_locs[,c("Long_m", "Lat_m")],factors = "Landcover", nbg= 10000, removeDuplicates = FALSE, args = LTSA_political_max_args, path=political)

saveRDS(NonClimOnly_RB_political, "./Maxent/NonClimOnly/RB/political/NonClimOnly_RB_political")

## Creating prediction surface  raw, cumulative, logistic and cloglog
#logistic output
NonClimOnly_RB_political_log <- predict(NonClimOnly_RB_political, LTSA_pol_env, 
                                        args = "outputformat=logistic", progress="", 
                                        defaultprevalence = 0.58,
                                        overwrite=TRUE,
                                        filename=paste(political,"/NonClimOnly_RB_political_log",sep=""),format="GTiff")

#cumulative output
NonClimOnly_RB_political_cum <- predict(NonClimOnly_RB_political, LTSA_pol_env, 
                                        args = "outputformat=cumulative", progress="", 
                                        defaultprevalence = 0.58,
                                        overwrite=TRUE,
                                        filename=paste(political,"/NonClimOnly_RB_political_cum",sep=""),format="GTiff")

#raw output
NonClimOnly_RB_political_raw <- predict(NonClimOnly_RB_political, LTSA_pol_env, 
                                        args = "outputformat=raw", progress="", 
                                        defaultprevalence = 0.58,
                                        overwrite=TRUE,
                                        filename=paste(political,"/NonClimOnly_RB_political_raw",sep=""),format="GTiff")

#cloglog output
NonClimOnly_RB_political_cloglog <- predict(NonClimOnly_RB_political, LTSA_pol_env, 
                                            args = "outputformat=cloglog", progress="", 
                                            defaultprevalence = 0.58,
                                            overwrite=TRUE,
                                            filename=paste(political,"/NonClimOnly_RB_political_cloglog ",sep=""),format="GTiff")

plot(NonClimOnly_RB_political_log)


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################

########################### END SCRIPT ###############################


