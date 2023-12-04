########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Jayna Bergman, updated by Kaegan Finn Dec 12, 2022

### Goal of this Script: 

# Runs maxent in SWD mode using all the input localities and tgb points 

### Notes: Uses uncorrelated variables (from Pearson Correlation)

# Creates a prediction surface raster 

### Date: June 9, 2022

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


################################# CLIMATE ONLY ################################

## Read in environmental variable rasters (previously cropped to range limits) and stack them 

LTSA_rast_list <- list.files("./Environmental_Variables/Range/ClimateOnly", pattern='.tif$', all.files=TRUE, full.names=TRUE)
LTSA_rast_list

LTSA_range_env <- stack(LTSA_rast_list)
plot(LTSA_range_env)

## Create a list of the raster names in the file and properly name variables 

LTSA_names_list <- gsub(pattern=".tif", replacement="", list.files("./Environmental_Variables/Range/ClimateOnly", pattern='.tif$', all.files=TRUE, full.names=FALSE))
index <- "Type"

names(LTSA_range_env) <- LTSA_names_list
plot(LTSA_range_env)

## Read in input localities and tgb points 
LTSA_range_occ <- read.csv("./TGB/extracted/ClimateOnly/range/range_presdat.csv")
LTSA_range_tgb <- read.csv("./TGB/extracted/ClimateOnly/range/range_tgbdat.csv")
LTSA_range_dat <- rbind(LTSA_range_occ, LTSA_range_tgb)

LTSA_political_occ <- read.csv("./TGB/extracted/ClimateOnly/political/political_presdat.csv")
LTSA_political_tgb <- read.csv("./TGB/extracted/ClimateOnly/political/political_tgbdat.csv")
LTSA_political_dat <- rbind(LTSA_political_occ, LTSA_political_tgb)

LTSA_ecotone_occ <- read.csv("./TGB/extracted/ClimateOnly/ecotone/ecotone_presdat.csv")
LTSA_ecotone_tgb <- read.csv("./TGB/extracted/ClimateOnly/ecotone/ecotone_tgbdat.csv")
LTSA_ecotone_dat <- rbind(LTSA_ecotone_occ, LTSA_ecotone_tgb)

LTSA_genetic_occ <- read.csv("./TGB/extracted/ClimateOnly/genetic/genetic_presdat.csv")
LTSA_genetic_tgb <- read.csv("./TGB/extracted/ClimateOnly/genetic/genetic_tgbdat.csv")
LTSA_genetic_dat <- rbind(LTSA_genetic_occ, LTSA_genetic_tgb)

## Read study extent shapefiles in (to be able to crop rasters)

LTSA_genetic <- st_read("./Study_extents/Study_extents_aea/genetic_aea.shp")
LTSA_ecotone <- st_read("./Study_extents/Study_extents_aea/ecotone_aea.shp")
LTSA_political <- st_read("./Study_extents/Study_extents_aea/political_aea.shp")
LTSA_range <- st_read("./Study_extents/Study_extents_aea/range_aea.shp")

## mask rasters to shapefiles (input rasters are at range - dont need to crop again)

LTSA_genetic_env <- mask(LTSA_range_env, LTSA_genetic)
LTSA_ecotone_env <- mask(LTSA_range_env, LTSA_ecotone)
LTSA_political_env <- mask(LTSA_range_env, LTSA_political)

plot(LTSA_political_env,1)


###### range study extent ######
## Setting parameters from 6.1_Tuning script
LTSA_range_reg <- 0.5
r <- get(paste("LTSA_range_reg",sep="")) # regularization parameter

## Setting maxent arguments 

basicargs <- c("-J","-P","writebackgroundpredictions","maximumiterations=5000")
LTSA_range_max_args <- c(basicargs, features=c("Threshold"), paste("betamultiplier=",r,sep=""))

## Create new directory for output files 

Range <- paste("./Maxent/ClimateOnly/TGB/range")

## Maxent command and save

ClimOnly_TGB_range_ <- maxent(x=LTSA_range_dat[,LTSA_names_list], p=LTSA_range_dat[,index], removeDuplicates = FALSE, args = LTSA_range_max_args, path=Range)
saveRDS(ClimOnly_TGB_range_, "./Maxent/ClimateOnly/TGB/range/ClimOnly_TGB_Range")

## Creating prediction surface  raw, cumulative, logistic and cloglog
#logistic output
ClimOnly_TGB_Range_log <- predict(ClimOnly_TGB_range_, LTSA_range_env, 
                                 args = "outputformat=logistic", progress="", 
                                 defaultprevalence = 0.58,
                                 overwrite=TRUE,
                                 filename=paste(Range,"/ClimOnly_TGB_Range_log",sep=""),format="GTiff")

#cumulative output
ClimOnly_TGB_Range_cum <- predict(ClimOnly_TGB_range_, LTSA_range_env, 
                                 args = "outputformat=cumulative", progress="", 
                                 defaultprevalence = 0.58,
                                 overwrite=TRUE,
                                 filename=paste(Range,"/ClimOnly_TGB_Range_cum",sep=""),format="GTiff")

#raw output
ClimOnly_TGB_Range_raw <- predict(ClimOnly_TGB_range_, LTSA_range_env, 
                                 args = "outputformat=raw", progress="", 
                                 defaultprevalence = 0.58,
                                 overwrite=TRUE,
                                 filename=paste(Range,"/ClimOnly_TGB_Range_raw",sep=""),format="GTiff")

#cloglog output
ClimOnly_TGB_Range_cloglog <- predict(ClimOnly_TGB_range_, LTSA_range_env, 
                                     args = "outputformat=cloglog", progress="", 
                                     defaultprevalence = 0.58,
                                     overwrite=TRUE,
                                     filename=paste(Range,"/ClimOnly_TGB_Range_cloglog",sep=""),format="GTiff")

plot(ClimOnly_TGB_Range_log)


###### political study extent ######
## Setting parameters from 6.1_Tuning script
LTSA_political_reg <- 0.5
r <- get(paste("LTSA_political_reg",sep="")) # regularization parameter

## Setting maxent arguments 

basicargs <- c("-J","-P","writebackgroundpredictions","maximumiterations=5000")
LTSA_political_max_args <- c(basicargs, features=c("nohinge","noproduct"), paste("betamultiplier=",r,sep=""))

## Create new directory for output files 

political <- paste("./Maxent/ClimateOnly/TGB/political")

## Maxent command and save

ClimOnly_TGB_political <- maxent(x=LTSA_political_dat[,LTSA_names_list], p=LTSA_political_dat[,index], removeDuplicates = FALSE, args = LTSA_political_max_args, path=political)
saveRDS(ClimOnly_TGB_political, "./Maxent/ClimateOnly/TGB/political/ClimOnly_TGB_political")

## Creating prediction surface  raw, cumulative, logistic and cloglog
#logistic output
ClimOnly_TGB_political_log <- predict(ClimOnly_TGB_political, LTSA_political_env, 
                                  args = "outputformat=logistic", progress="", 
                                  defaultprevalence = 0.58,
                                  overwrite=TRUE,
                                  filename=paste(political,"/ClimOnly_TGB_political_log",sep=""),format="GTiff")

#cumulative output
ClimOnly_TGB_political_cum <- predict(ClimOnly_TGB_political, LTSA_political_env, 
                                  args = "outputformat=cumulative", progress="", 
                                  defaultprevalence = 0.58,
                                  overwrite=TRUE,
                                  filename=paste(political,"/ClimOnly_TGB_political_cum",sep=""),format="GTiff")

#raw output
ClimOnly_TGB_political_raw <- predict(ClimOnly_TGB_political, LTSA_political_env, 
                                  args = "outputformat=raw", progress="", 
                                  defaultprevalence = 0.58,
                                  overwrite=TRUE,
                                  filename=paste(political,"/ClimOnly_TGB_political_raw",sep=""),format="GTiff")

#cloglog output
ClimOnly_TGB_political_cloglog <- predict(ClimOnly_TGB_political, LTSA_political_env, 
                                      args = "outputformat=cloglog", progress="", 
                                      defaultprevalence = 0.58,
                                      overwrite=TRUE,
                                      filename=paste(political,"/ClimOnly_TGB_political_cloglog",sep=""),format="GTiff")

plot(ClimOnly_TGB_political_log)


###### ecotone study extent ######
## Setting parameters from 6.1_Tuning script
LTSA_ecotone_reg <- 0.5
r <- get(paste("LTSA_ecotone_reg",sep="")) # regularization parameter

## Setting maxent arguments 

basicargs <- c("-J","-P","writebackgroundpredictions","maximumiterations=5000")
LTSA_ecotone_max_args <- c(basicargs, features=c(""), paste("betamultiplier=",r,sep=""))

## Create new directory for output files 

ecotone <- paste("./Maxent/ClimateOnly/TGB/ecotone")

## Maxent command and save

ClimOnly_TGB_ecotone <- maxent(x=LTSA_ecotone_dat[,LTSA_names_list], p=LTSA_ecotone_dat[,index], removeDuplicates = FALSE, args = LTSA_ecotone_max_args, path=ecotone)

saveRDS(ClimOnly_TGB_ecotone, "./Maxent/ClimateOnly/TGB/ecotone/ClimOnly_TGB_ecotone")

ClimOnly_TGB_ecotone <- readRDS("./Maxent/ClimateOnly/TGB/ecotone/ClimOnly_TGB_ecotone")

## Creating prediction surface  raw, cumulative, logistic and cloglog
#logistic output
ClimOnly_TGB_ecotone_log <- predict(ClimOnly_TGB_ecotone, LTSA_political_env, 
                                      args = "outputformat=logistic", progress="", 
                                      defaultprevalence = 0.58,
                                      overwrite=TRUE,
                                      filename=paste(ecotone,"/ClimOnly_TGB_ecotone_log_ROI",sep=""),format="GTiff")

#cumulative output
ClimOnly_TGB_ecotone_cum <- predict(ClimOnly_TGB_ecotone, LTSA_ecotone_env, 
                                      args = "outputformat=cumulative", progress="", 
                                      defaultprevalence = 0.58,
                                      overwrite=TRUE,
                                      filename=paste(ecotone,"/ClimOnly_TGB_ecotone_cum",sep=""),format="GTiff")

#raw output
ClimOnly_TGB_ecotone_raw <- predict(ClimOnly_TGB_ecotone, LTSA_ecotone_env, 
                                      args = "outputformat=raw", progress="", 
                                      defaultprevalence = 0.58,
                                      overwrite=TRUE,
                                      filename=paste(ecotone,"/ClimOnly_TGB_ecotone_raw",sep=""),format="GTiff")

#cloglog output
ClimOnly_TGB_ecotone_cloglog <- predict(ClimOnly_TGB_ecotone, LTSA_ecotone_env, 
                                          args = "outputformat=cloglog", progress="", 
                                          defaultprevalence = 0.58,
                                          overwrite=TRUE,
                                          filename=paste(ecotone,"/ClimOnly_TGB_ecotone_cloglog",sep=""),format="GTiff")

plot(ClimOnly_TGB_ecotone_log)


###### genetic study extent ######
## Setting parameters from 6.1_Tuning script
LTSA_genetic_reg <- 0.5
r <- get(paste("LTSA_genetic_reg",sep="")) # regularization parameter

## Setting maxent arguments 

basicargs <- c("-J","-P","writebackgroundpredictions","maximumiterations=5000")
LTSA_genetic_max_args <- c(basicargs, features=c("Threshold"), paste("betamultiplier=",r,sep=""))

## Create new directory for output files 

genetic <- paste("./Maxent/ClimateOnly/TGB/genetic")

## Maxent command and save

ClimOnly_TGB_genetic <- maxent(x=LTSA_genetic_dat[,LTSA_names_list], p=LTSA_genetic_dat[,index], removeDuplicates = FALSE, args = LTSA_genetic_max_args, path=genetic)
saveRDS(ClimOnly_TGB_genetic, "./Maxent/ClimateOnly/TGB/genetic/ClimOnly_TGB_genetic")

ClimOnly_TGB_genetic <- readRDS("./Maxent/ClimateOnly/TGB/genetic/ClimOnly_TGB_genetic")

## Creating prediction surface  raw, cumulative, logistic and cloglog
#logistic output
ClimOnly_TGB_genetic_log <- predict(ClimOnly_TGB_genetic, LTSA_political_env, 
                                    args = "outputformat=logistic", progress="", 
                                    defaultprevalence = 0.58,
                                    overwrite=TRUE,
                                    filename=paste(genetic,"/ClimOnly_TGB_genetic_log_ROI",sep=""),format="GTiff")

#cumulative output
ClimOnly_TGB_genetic_cum <- predict(ClimOnly_TGB_genetic, LTSA_genetic_env, 
                                    args = "outputformat=cumulative", progress="", 
                                    defaultprevalence = 0.58,
                                    overwrite=TRUE,
                                    filename=paste(genetic,"/ClimOnly_TGB_genetic_cum",sep=""),format="GTiff")

#raw output
ClimOnly_TGB_genetic_raw <- predict(ClimOnly_TGB_genetic, LTSA_genetic_env, 
                                    args = "outputformat=raw", progress="", 
                                    defaultprevalence = 0.58,
                                    overwrite=TRUE,
                                    filename=paste(genetic,"/ClimOnly_TGB_genetic_raw",sep=""),format="GTiff")

#cloglog output
ClimOnly_TGB_genetic_cloglog <- predict(ClimOnly_TGB_genetic, LTSA_genetic_env, 
                                        args = "outputformat=cloglog", progress="", 
                                        defaultprevalence = 0.58,
                                        overwrite=TRUE,
                                        filename=paste(genetic,"/ClimOnly_TGB_genetic_cloglog",sep=""),format="GTiff")

plot(ClimOnly_TGB_genetic_log)

##################################################################################
##################################################################################
rm(list=ls())
################################# CLIMATE + NON CLIMATE ################################
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

LTSA_pol_env

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

plot(LTSA_range_env)

index <- "Type"

## Read in input localities and tgb points 
LTSA_range_occ <- read.csv("./TGB/extracted/ClimNonClim/range/range_presdat.csv")
LTSA_range_tgb <- read.csv("./TGB/extracted/ClimNonClim/range/range_tgbdat.csv")
LTSA_range_tgb <- LTSA_range_tgb[,-1]
LTSA_range_dat <- rbind(LTSA_range_occ, LTSA_range_tgb)

LTSA_political_occ <- read.csv("./TGB/extracted/ClimNonClim/political/political_presdat.csv")
LTSA_political_tgb <- read.csv("./TGB/extracted/ClimNonClim/political/political_tgbdat.csv")
LTSA_political_tgb <- LTSA_political_tgb[,-1]
LTSA_political_dat <- rbind(LTSA_political_occ, LTSA_political_tgb)

LTSA_ecotone_occ <- read.csv("./TGB/extracted/ClimNonClim/ecotone/ecotone_presdat.csv")
LTSA_ecotone_tgb <- read.csv("./TGB/extracted/ClimNonClim/ecotone/ecotone_tgbdat.csv")
LTSA_ecotone_tgb <- LTSA_ecotone_tgb[,-1]
LTSA_ecotone_dat <- rbind(LTSA_ecotone_occ, LTSA_ecotone_tgb)

LTSA_genetic_occ <- read.csv("./TGB/extracted/ClimNonClim/genetic/genetic_presdat.csv")
LTSA_genetic_tgb <- read.csv("./TGB/extracted/ClimNonClim/genetic/genetic_tgbdat.csv")
LTSA_genetic_tgb <- LTSA_genetic_tgb[,-1]
LTSA_genetic_dat <- rbind(LTSA_genetic_occ, LTSA_genetic_tgb)

##Specify Landcover as factor
LTSA_range_dat$Landcover <- as.factor(LTSA_range_dat$Landcover)
LTSA_political_dat$Landcover <- as.factor(LTSA_political_dat$Landcover)
LTSA_ecotone_dat$Landcover <- as.factor(LTSA_ecotone_dat$Landcover)
LTSA_genetic_dat$Landcover <- as.factor(LTSA_genetic_dat$Landcover)

###### range study extent ######
## Setting parameters from 6.1_Tuning script
LTSA_range_reg <- 0.5
r <- get(paste("LTSA_range_reg",sep="")) # regularization parameter

## Setting maxent arguments 

basicargs <- c("-J","-P","writebackgroundpredictions","maximumiterations=5000")
LTSA_range_max_args <- c(basicargs, features=c("Threshold"), paste("betamultiplier=",r,sep=""))

## Create new directory for output files 

Range <- paste("./Maxent/ClimNonClim/TGB/range")

## Maxent command and save

ClimNonClim_TGB_range <- maxent(x=LTSA_range_dat[,LTSA_names_list], p=LTSA_range_dat[,index], removeDuplicates = FALSE, args = LTSA_range_max_args, path=Range)
saveRDS(ClimNonClim_TGB_range, "./Maxent/ClimNonClim/TGB/range/ClimNonClim_TGB_Range")

## Creating prediction surface  raw, cumulative, logistic and cloglog
#logistic output
ClimNonClim_TGB_Range_log <- predict(ClimNonClim_TGB_range, LTSA_range_env, 
                                  args = "outputformat=logistic", progress="", 
                                  defaultprevalence = 0.58,
                                  overwrite=TRUE,
                                  filename=paste(Range,"/ClimNonClim_TGB_Range_log",sep=""),format="GTiff")

#cumulative output
ClimNonClim_TGB_Range_cum <- predict(ClimNonClim_TGB_range, LTSA_range_env, 
                                  args = "outputformat=cumulative", progress="", 
                                  defaultprevalence = 0.58,
                                  overwrite=TRUE,
                                  filename=paste(Range,"/ClimNonClim_TGB_Range_cum",sep=""),format="GTiff")

#raw output
ClimNonClim_TGB_Range_raw <- predict(ClimNonClim_TGB_range, LTSA_range_env, 
                                  args = "outputformat=raw", progress="", 
                                  defaultprevalence = 0.58,
                                  overwrite=TRUE,
                                  filename=paste(Range,"/ClimNonClim_TGB_Range_raw",sep=""),format="GTiff")

#cloglog output
ClimNonClim_TGB_Range_cloglog <- predict(ClimNonClim_TGB_range, LTSA_range_env, 
                                      args = "outputformat=cloglog", progress="", 
                                      defaultprevalence = 0.58,
                                      overwrite=TRUE,
                                      filename=paste(Range,"/ClimNonClim_TGB_Range_cloglog",sep=""),format="GTiff")

plot(ClimNonClim_TGB_Range_log)

###### political study extent ######
## Setting parameters from 6.1_Tuning script
LTSA_political_reg <- 0.5
r <- get(paste("LTSA_political_reg",sep="")) # regularization parameter

## Setting maxent arguments 

basicargs <- c("-J","-P","writebackgroundpredictions","maximumiterations=5000")
LTSA_political_max_args <- c(basicargs, features=c("noquadratic","nohinge","noproduct"), paste("betamultiplier=",r,sep=""))

## Create new directory for output files 

political <- paste("./Maxent/ClimNonClim/TGB/political")

## Maxent command and save

ClimNonClim_TGB_political <- maxent(x=LTSA_political_dat[,LTSA_names_list], p=LTSA_political_dat[,index], removeDuplicates = FALSE, args = LTSA_political_max_args, path=political)
saveRDS(ClimNonClim_TGB_political, "./Maxent/ClimNonClim/TGB/political/ClimNonClim_TGB_political")

## Creating prediction surface  raw, cumulative, logistic and cloglog
#logistic output
ClimNonClim_TGB_political_log <- predict(ClimNonClim_TGB_political, LTSA_pol_env, 
                                     args = "outputformat=logistic", progress="", 
                                     defaultprevalence = 0.58,
                                     overwrite=TRUE,
                                     filename=paste(political,"/ClimNonClim_TGB_political_log",sep=""),format="GTiff")

#cumulative output
ClimNonClim_TGB_political_cum <- predict(ClimNonClim_TGB_political, LTSA_pol_env, 
                                     args = "outputformat=cumulative", progress="", 
                                     defaultprevalence = 0.58,
                                     overwrite=TRUE,
                                     filename=paste(political,"/ClimNonClim_TGB_political_cum",sep=""),format="GTiff")

#raw output
ClimNonClim_TGB_political_raw <- predict(ClimNonClim_TGB_political, LTSA_pol_env, 
                                     args = "outputformat=raw", progress="", 
                                     defaultprevalence = 0.58,
                                     overwrite=TRUE,
                                     filename=paste(political,"/ClimNonClim_TGB_political_raw",sep=""),format="GTiff")

#cloglog output
ClimNonClim_TGB_political_cloglog <- predict(ClimNonClim_TGB_political, LTSA_pol_env, 
                                         args = "outputformat=cloglog", progress="", 
                                         defaultprevalence = 0.58,
                                         overwrite=TRUE,
                                         filename=paste(political,"/ClimNonClim_TGB_political_cloglog",sep=""),format="GTiff")

plot(ClimNonClim_TGB_political_log)

###### ecotone study extent ######
## Setting parameters from 6.1_Tuning script
LTSA_ecotone_reg <- 0.5
r <- get(paste("LTSA_ecotone_reg",sep="")) # regularization parameter

## Setting maxent arguments 

basicargs <- c("-J","-P","writebackgroundpredictions","maximumiterations=5000")
LTSA_ecotone_max_args <- c(basicargs, features=c("Threshold"), paste("betamultiplier=",r,sep=""))

## Create new directory for output files 

ecotone <- paste("./Maxent/ClimNonClim/TGB/ecotone")

## Maxent command and save

ClimNonClim_TGB_ecotone <- maxent(x=LTSA_ecotone_dat[,LTSA_names_list], p=LTSA_ecotone_dat[,index], removeDuplicates = FALSE, args = LTSA_ecotone_max_args, path=ecotone)
saveRDS(ClimNonClim_TGB_ecotone, "./Maxent/ClimNonClim/TGB/ecotone/ClimNonClim_TGB_ecotone")

ClimNonClim_TGB_ecotone <- readRDS("./Maxent/ClimNonClim/TGB/ecotone/ClimNonClim_TGB_ecotone")

## Creating prediction surface  raw, cumulative, logistic and cloglog
#logistic output
ClimNonClim_TGB_ecotone_log <- predict(ClimNonClim_TGB_ecotone, LTSA_pol_env, 
                                     args = "outputformat=logistic", progress="", 
                                     defaultprevalence = 0.58,
                                     overwrite=TRUE,
                                     filename=paste(ecotone,"/ClimNonClim_TGB_ecotone_log_ROI",sep=""),format="GTiff")

#cumulative output
ClimNonClim_TGB_ecotone_cum <- predict(ClimNonClim_TGB_ecotone, LTSA_eco_env, 
                                     args = "outputformat=cumulative", progress="", 
                                     defaultprevalence = 0.58,
                                     overwrite=TRUE,
                                     filename=paste(ecotone,"/ClimNonClim_TGB_ecotone_cum",sep=""),format="GTiff")

#raw output
ClimNonClim_TGB_ecotone_raw <- predict(ClimNonClim_TGB_ecotone, LTSA_eco_env, 
                                     args = "outputformat=raw", progress="", 
                                     defaultprevalence = 0.58,
                                     overwrite=TRUE,
                                     filename=paste(ecotone,"/ClimNonClim_TGB_ecotone_raw",sep=""),format="GTiff")

#cloglog output
ClimNonClim_TGB_ecotone_cloglog <- predict(ClimNonClim_TGB_ecotone, LTSA_eco_env, 
                                         args = "outputformat=cloglog", progress="", 
                                         defaultprevalence = 0.58,
                                         overwrite=TRUE,
                                         filename=paste(ecotone,"/ClimNonClim_TGB_ecotone_cloglog",sep=""),format="GTiff")

plot(ClimNonClim_TGB_ecotone_log)

###### genetic study extent ######
## Setting parameters from 6.1_Tuning script
LTSA_genetic_reg <- 0.5
r <- get(paste("LTSA_genetic_reg",sep="")) # regularization parameter

## Setting maxent arguments 

basicargs <- c("-J","-P","writebackgroundpredictions","maximumiterations=5000")
LTSA_genetic_max_args <- c(basicargs, features=c("Threshold"), paste("betamultiplier=",r,sep=""))

## Create new directory for output files 

genetic <- paste("./Maxent/ClimNonClim/TGB/genetic")

## Maxent command and save

ClimNonClim_TGB_genetic <- maxent(x=LTSA_genetic_dat[,LTSA_names_list], p=LTSA_genetic_dat[,index], removeDuplicates = FALSE, args = LTSA_genetic_max_args, path=genetic)
saveRDS(ClimNonClim_TGB_genetic, "./Maxent/ClimNonClim/TGB/genetic/ClimNonClim_TGB_genetic")

ClimNonClim_TGB_genetic <- readRDS("./Maxent/ClimNonClim/TGB/genetic/ClimNonClim_TGB_genetic")


## Creating prediction surface  raw, cumulative, logistic and cloglog
#logistic output
ClimNonClim_TGB_genetic_log <- predict(ClimNonClim_TGB_genetic, LTSA_pol_env, 
                                     args = "outputformat=logistic", progress="", 
                                     defaultprevalence = 0.58,
                                     overwrite=TRUE,
                                     filename=paste(genetic,"/ClimNonClim_TGB_genetic_log_ROI",sep=""),format="GTiff")

#cumulative output
ClimNonClim_TGB_genetic_cum <- predict(ClimNonClim_TGB_genetic, LTSA_gen_env, 
                                     args = "outputformat=cumulative", progress="", 
                                     defaultprevalence = 0.58,
                                     overwrite=TRUE,
                                     filename=paste(genetic,"/ClimNonClim_TGB_genetic_cum",sep=""),format="GTiff")

#raw output
ClimNonClim_TGB_genetic_raw <- predict(ClimNonClim_TGB_genetic, LTSA_gen_env, 
                                     args = "outputformat=raw", progress="", 
                                     defaultprevalence = 0.58,
                                     overwrite=TRUE,
                                     filename=paste(genetic,"/ClimNonClim_TGB_genetic_raw",sep=""),format="GTiff")

#cloglog output
ClimNonClim_TGB_genetic_cloglog <- predict(ClimNonClim_TGB_genetic, LTSA_gen_env, 
                                         args = "outputformat=cloglog", progress="", 
                                         defaultprevalence = 0.58,
                                         overwrite=TRUE,
                                         filename=paste(genetic,"/ClimNonClim_TGB_genetic_cloglog",sep=""),format="GTiff")

plot(ClimNonClim_TGB_genetic_log)


########################### END SECTION ###############################

########################## FINAL COMMENTS ############################

##

########################### END SCRIPT ###############################
