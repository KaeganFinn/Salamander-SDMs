########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Kaegan Finn

### Goal of this Script: Determine optimal features and regularization parameters using ENMeval. "Tuning" SDM parameters. 

### Notes:  Based on tutorial from Josh Banta: https://www.youtube.com/watch?v=G_xTGXUvYXo
# Everything in this script is Easting & Northing in Albers Equal Area Conic

### Date: Nov 29, 2022

### Version of R:  R Version 4.0.3

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(dismo)
library(devtools)
library(ENMeval)
library(raster)
library(MASS)
library(sf)

rm(list=ls())

########################### END SECTION ##############################

########################## DATA AND ANALYSIS #########################

## set working directory

getwd()
setwd("/Users/kaegan/Dropbox/Mac/Desktop/_/MSc/Thesis/Models")


################################# LTSA, CLIMATE ONLY, Random Background  ################################

## Read in environmental variable rasters (previously cropped to range limits) and stack them 

LTSA_rast_list <- list.files("./Environmental_Variables/Range/ClimateOnly", pattern='.tif$', all.files=TRUE, full.names=TRUE)
LTSA_rast_list

LTSA_range_env <- stack(LTSA_rast_list)
plot(LTSA_range_env)


## Create a list of the raster names in the file and properly name variables 

LTSA_names_list <- gsub(pattern=".tif", replacement="", list.files("./Environmental_Variables/Range/ClimateOnly", pattern='.tif$', all.files=TRUE, full.names=FALSE))

names(LTSA_range_env) <- LTSA_names_list


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

## BIAS FILES SHOULD NOT BE CREATED FOR RANDOM POINT MODELS. They are RANDOM! Bias correction is being tested w TGB ##
##
##


## Generating Background Point ## 
#If this number is far in excess of 10,000, then use 10,000 background points.
#If this number is comprable to, or smaller than 10,000, then use 5,000, 1,000, 500,
#or even 100 background points. The number of available non-NA spaces should 
#be well in excess of the number of background points used.

#The code is set up to sample 5,000 background points. It would be better if we
#could sample 10,000 background points, but there are not enough places available.
#If we could change it to 10,000 background points we would change the ", 5000," to ",10000,"

length(which(!is.na(values(subset(LTSA_genetic_env, 1)))))

#This random background point method does not generate points in cells where input localities occur
LTSA_genetic_bg<- randomPoints(LTSA_genetic_env, 10000, LTSA_genetic_locs, excludep=TRUE, 
             cellnumbers=FALSE, warn=2, lonlatCorrection=TRUE)

LTSA_genetic_bg <- as.data.frame(LTSA_genetic_bg)
colnames(LTSA_genetic_bg)<- c("Long_m","Lat_m")

plot(LTSA_genetic_bg)

# ecotone
length(which(!is.na(values(subset(LTSA_ecotone_env, 1)))))

LTSA_ecotone_bg<- randomPoints(LTSA_ecotone_env, 10000, LTSA_ecotone_locs, excludep=TRUE, 
                               cellnumbers=FALSE, warn=2, lonlatCorrection=TRUE)

LTSA_ecotone_bg <- as.data.frame(LTSA_ecotone_bg)
colnames(LTSA_ecotone_bg)<- c("Long_m","Lat_m")

plot(LTSA_ecotone_bg)

# political
length(which(!is.na(values(subset(LTSA_political_env, 1)))))

LTSA_political_bg<- randomPoints(LTSA_political_env, 5000, LTSA_political_locs, excludep=TRUE, 
                               cellnumbers=FALSE, warn=2, lonlatCorrection=TRUE)

LTSA_political_bg <- as.data.frame(LTSA_political_bg)
colnames(LTSA_political_bg)<- c("Long_m","Lat_m")

plot(LTSA_political_bg)


# range
length(which(!is.na(values(subset(LTSA_range_env, 1)))))

LTSA_range_bg<- randomPoints(LTSA_range_env, 10000, LTSA_range_locs, excludep=TRUE, 
                               cellnumbers=FALSE, warn=2, lonlatCorrection=TRUE)

LTSA_range_bg <- as.data.frame(LTSA_range_bg)
colnames(LTSA_range_bg)<- c("Long_m","Lat_m")

plot(LTSA_range_bg)

## RUNNING ENMeval ## 

##run the evaluation
##This run uses the "randomkfold" method of cross-validation, with a set of background points
##sampled based on the bias file, and 10 cross-validation folds.

LTSA_genetic_results <- ENMevaluate(LTSA_genetic_locs, LTSA_genetic_env, LTSA_genetic_bg, 
                                    tune.args = list(fc = c("L","LQ","H", "LQH", "LQHP", "LQHPT"), 
                                                     rm = c(0.5,1,1.5,2,2.5,3,3.5,4)), partitions = "randomkfold", partition.settings = list(kfolds = 2), algorithm = "maxnet")

LTSA_ecotone_results <- ENMevaluate(LTSA_ecotone_locs, LTSA_ecotone_env, LTSA_ecotone_bg, 
                                    tune.args = list(fc = c("L","LQ","H", "LQH", "LQHP", "LQHPT"), 
                                                     rm = c(0.5,1,1.5,2,2.5,3,3.5,4)), partitions = "randomkfold", partition.settings = list(kfolds = 2), algorithm = "maxnet")

LTSA_political_results <- ENMevaluate(LTSA_political_locs, LTSA_political_env, LTSA_political_bg, 
                                      tune.args = list(fc = c("L","LQ","H", "LQH", "LQHP", "LQHPT"), 
                                                       rm = c(0.5,1,1.5,2,2.5,3,3.5,4)), partitions = "randomkfold", partition.settings = list(kfolds = 2), algorithm = "maxnet")

LTSA_range_results <- ENMevaluate(LTSA_range_locs, LTSA_range_env, LTSA_range_bg, 
                                  tune.args = list(fc = c("L","LQ","H", "LQH", "LQHP", "LQHPT"), 
                                                   rm = c(0.5,1,1.5,2,2.5,3,3.5,4)), partitions = "randomkfold", partition.settings = list(kfolds = 2), algorithm = "maxnet")



# Create results table

LTSA_genetic_results@results
write.csv(LTSA_genetic_results@results, "./Tuning/Results/Genetic_ClimateOnly_RB_2folds.csv", row.names = FALSE)

LTSA_ecotone_results@results
write.csv(LTSA_ecotone_results@results, "./Tuning/Results/Ecotone_ClimateOnly_RB_2folds.csv", row.names = FALSE)

LTSA_political_results@results
write.csv(LTSA_political_results@results, "./Tuning/Results/Political_ClimateOnly_RB_2folds.csv", row.names = FALSE)

LTSA_range_results@results
write.csv(LTSA_range_results@results, "./Tuning/Results/Range_ClimateOnly_RB_2folds.csv", row.names = FALSE)


########################### END SECTION ###############################

################################# LTSA CLIMATE+NON CLIMATE, RandBackground BE SURE TO note which variables are categorical (Landcover) ################################


## Read in environmental variable rasters (previously cropped to range limits) and stack them 

LTSA_rast_list <- list.files("./Environmental_Variables/Range/ClimateNonClimate", pattern='.tif$', all.files=TRUE, full.names=TRUE)
LTSA_rast_list

LTSA_range_env <- stack(LTSA_rast_list)
plot(LTSA_range_env)

plot(LTSA_range_env)


## Create a list of the raster names in the file and properly name variables 

LTSA_names_list <- gsub(pattern=".tif", replacement="", list.files("./Environmental_Variables/Range/ClimateNonClimate", pattern='.tif$', all.files=TRUE, full.names=FALSE))

names(LTSA_range_env) <- LTSA_names_list


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

##


## Generating Background Point ## 
#If this number is far in excess of 10,000, then use 10,000 background points.
#If this number is comprable to, or smaller than 10,000, then use 5,000, 1,000, 500,
#or even 100 background points. The number of available non-NA spaces should 
#be well in excess of the number of background points used.

#The code is set up to sample 5,000 background points. It would be better if we
#could sample 10,000 background points, but there are not enough places available.
#If we could change it to 10,000 background points we would change the ", 5000," to ",10000,"

length(which(!is.na(values(subset(LTSA_genetic_env, 1)))))

#This random background point method does not generate points in cells where input localities occur
LTSA_genetic_bg<- randomPoints(LTSA_genetic_env, 10000, LTSA_genetic_locs, excludep=TRUE, 
                               cellnumbers=FALSE, warn=2, lonlatCorrection=TRUE)

LTSA_genetic_bg <- as.data.frame(LTSA_genetic_bg)
colnames(LTSA_genetic_bg)<- c("Long_m","Lat_m")

plot(LTSA_genetic_bg)

# ecotone
length(which(!is.na(values(subset(LTSA_ecotone_env, 1)))))

LTSA_ecotone_bg<- randomPoints(LTSA_ecotone_env, 10000, LTSA_ecotone_locs, excludep=TRUE, 
                               cellnumbers=FALSE, warn=2, lonlatCorrection=TRUE)

LTSA_ecotone_bg <- as.data.frame(LTSA_ecotone_bg)
colnames(LTSA_ecotone_bg)<- c("Long_m","Lat_m")

plot(LTSA_ecotone_bg)

# political
length(which(!is.na(values(subset(LTSA_political_env, 1)))))

LTSA_political_bg<- randomPoints(LTSA_political_env, 5000, LTSA_political_locs, excludep=TRUE, 
                                 cellnumbers=FALSE, warn=2, lonlatCorrection=TRUE)

LTSA_political_bg <- as.data.frame(LTSA_political_bg)
colnames(LTSA_political_bg)<- c("Long_m","Lat_m")

plot(LTSA_political_bg)


# range
length(which(!is.na(values(subset(LTSA_range_env, 1)))))

LTSA_range_bg<- randomPoints(LTSA_range_env, 10000, LTSA_range_locs, excludep=TRUE, 
                             cellnumbers=FALSE, warn=2, lonlatCorrection=TRUE)

LTSA_range_bg <- as.data.frame(LTSA_range_bg)
colnames(LTSA_range_bg)<- c("Long_m","Lat_m")

plot(LTSA_range_bg)


## RUNNING ENMeval ## 

##run the evaluation
##This run uses the "randomkfold" method of cross-validation, with a set of background points
##sampled based on the bias file, and 10 cross-validation folds.

LTSA_genetic_results <- ENMevaluate(LTSA_genetic_locs, LTSA_genetic_env, LTSA_genetic_bg,categoricals = "Landcover",
                                    tune.args = list(fc = c("L","LQ","H", "LQH", "LQHP", "LQHPT"),
                                                     rm = c(0.5,1,1.5,2,2.5,3,3.5,4)), partitions = "randomkfold", partition.settings = list(kfolds = 2), algorithm = "maxent.jar")

LTSA_ecotone_results <- ENMevaluate(LTSA_ecotone_locs, LTSA_ecotone_env, LTSA_ecotone_bg, categoricals = "Landcover",
                                    tune.args = list(fc = c("L","LQ","H", "LQH", "LQHP", "LQHPT"), 
                                                     rm = c(0.5,1,1.5,2,2.5,3,3.5,4)), partitions = "randomkfold", partition.settings = list(kfolds = 2), algorithm = "maxent.jar")

LTSA_political_results <- ENMevaluate(LTSA_political_locs, LTSA_political_env, LTSA_political_bg, categoricals = "Landcover",
                                      tune.args = list(fc = c("L","LQ","H", "LQH", "LQHP", "LQHPT"), 
                                                       rm = c(0.5,1,1.5,2,2.5,3,3.5,4)), partitions = "randomkfold", partition.settings = list(kfolds = 5), algorithm = "maxent.jar")

LTSA_range_results <- ENMevaluate(LTSA_range_locs, LTSA_range_env, LTSA_range_bg, categoricals = "Landcover",
                                  tune.args = list(fc = c("L","LQ","H", "LQH", "LQHP", "LQHPT"),
                                                   rm = c(0.5,1,1.5,2,2.5,3,3.5,4)), partitions = "randomkfold", partition.settings = list(kfolds = 2), algorithm = "maxent.jar")



# Create results table

LTSA_genetic_results@results
write.csv(LTSA_genetic_results@results, "./Tuning/Results/Genetic_ClimateNonClimate_RB_2folds_jar.csv", row.names = FALSE)

LTSA_ecotone_results@results
write.csv(LTSA_ecotone_results@results, "./Tuning/Results/Ecotone_ClimateNonClimate_RB_2folds_jar.csv", row.names = FALSE)

LTSA_political_results@results
write.csv(LTSA_political_results@results, "./Tuning/Results/Political_ClimateNonClimate_RB_5folds_MAXENT.JAR.csv", row.names = FALSE)

LTSA_range_results@results
write.csv(LTSA_range_results@results, "./Tuning/Results/Range_ClimateNonClimate_RB_2folds.csv", row.names = FALSE)

###################################################################################################################
###################################################################################################################
###################################################################################################################



################################# LTSA, CLIMATE ONLY, TGB  ################################

## Read in environmental variable rasters (previously cropped to range limits) and stack them 

LTSA_rast_list <- list.files("./Environmental_Variables/Range/ClimateOnly", pattern='.tif$', all.files=TRUE, full.names=TRUE)
LTSA_rast_list

LTSA_range_env <- stack(LTSA_rast_list)
plot(LTSA_range_env)


## Create a list of the raster names in the file and properly name variables 

LTSA_names_list <- gsub(pattern=".tif", replacement="", list.files("./Environmental_Variables/Range/ClimateOnly", pattern='.tif$', all.files=TRUE, full.names=FALSE))

names(LTSA_range_env) <- LTSA_names_list


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

## Getting TGB points

## Read TGB localities for each study extent 

LTSA_genetic_TGB <- read.csv("./TGB/model_subsets/TGB_genetic.csv")
LTSA_ecotone_TGB <- read.csv("./TGB/model_subsets/TGB_ecotone.csv")
LTSA_political_TGB <- read.csv("./TGB/model_subsets/TGB_political.csv")
LTSA_range_TGB <- read.csv("./TGB/model_subsets/TGB_range.csv")


## Get input localities into a two column matrix

LTSA_genetic_TGB <- subset(LTSA_genetic_TGB, select=c(Long_m, Lat_m))
LTSA_ecotone_TGB <- subset(LTSA_ecotone_TGB, select=c(Long_m, Lat_m))
LTSA_political_TGB <- subset(LTSA_political_TGB, select=c(Long_m, Lat_m))
LTSA_range_TGB <- subset(LTSA_range_TGB, select=c(Long_m, Lat_m))


## RUNNING ENMeval ## 

##run the evaluation
##This run uses the "randomkfold" method of cross-validation, with a set of background points
##sampled based on the bias file, and 10 cross-validation folds.

plot(LTSA_genetic_TGB)
plot(LTSA_genetic_locs)
plot(LTSA_genetic_env)

LTSA_genetic_results <- ENMevaluate(LTSA_genetic_locs, LTSA_genetic_env, LTSA_genetic_TGB, 
                                    tune.args = list(fc = c("L","LQ","H", "LQH", "LQHP", "LQHPT"), 
                                                     rm = c(0.5,1,1.5,2,2.5,3,3.5,4)), partitions = "randomkfold", partition.settings = list(kfolds = 2), algorithm = "maxnet")

LTSA_ecotone_results <- ENMevaluate(LTSA_ecotone_locs, LTSA_ecotone_env, LTSA_ecotone_TGB, 
                                    tune.args = list(fc = c("L","LQ","H", "LQH", "LQHP", "LQHPT"), 
                                                     rm = c(0.5,1,1.5,2,2.5,3,3.5,4)), partitions = "randomkfold", partition.settings = list(kfolds = 2), algorithm = "maxnet")

LTSA_political_results <- ENMevaluate(LTSA_political_locs, LTSA_political_env, LTSA_political_TGB, 
                                      tune.args = list(fc = c("L","LQ","H", "LQH", "LQHP", "LQHPT"), 
                                                       rm = c(0.5,1,1.5,2,2.5,3,3.5,4)), partitions = "randomkfold", partition.settings = list(kfolds = 2), algorithm = "maxent.jar")

LTSA_range_results <- ENMevaluate(LTSA_range_locs, LTSA_range_env, LTSA_range_TGB, 
                                  tune.args = list(fc = c("L","LQ","H", "LQH", "LQHP", "LQHPT"), 
                                                   rm = c(0.5,1,1.5,2,2.5,3,3.5,4)), partitions = "randomkfold", partition.settings = list(kfolds = 2), algorithm = "maxnet")

# Create results table

LTSA_genetic_results@results
write.csv(LTSA_genetic_results@results, "./Tuning/Results/Genetic_ClimateOnly_TGB_2folds.csv", row.names = FALSE)

LTSA_ecotone_results@results
write.csv(LTSA_ecotone_results@results, "./Tuning/Results/Ecotone_ClimateOnly_TGB_2folds.csv", row.names = FALSE)

LTSA_political_results@results
write.csv(LTSA_political_results@results, "./Tuning/Results/Political_ClimateOnly_TGB_2folds_jar.csv", row.names = FALSE)

LTSA_range_results@results
write.csv(LTSA_range_results@results, "./Tuning/Results/Range_ClimateOnly_TGB_2folds.csv", row.names = FALSE)


################################# LTSA, CLIMATE + NON CLIMATE, TGB  ################################

## Read in environmental variable rasters (previously cropped to range limits) and stack them 

LTSA_rast_list <- list.files("./Environmental_Variables/Range/ClimateNonClimate", pattern='.tif$', all.files=TRUE, full.names=TRUE)
LTSA_rast_list

LTSA_range_env <- stack(LTSA_rast_list)
plot(LTSA_range_env)


## Create a list of the raster names in the file and properly name variables 

LTSA_names_list <- gsub(pattern=".tif", replacement="", list.files("./Environmental_Variables/Range/ClimateNonClimate", pattern='.tif$', all.files=TRUE, full.names=FALSE))

names(LTSA_range_env) <- LTSA_names_list


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

## Getting TGB points

## Read TGB localities for each study extent 

LTSA_genetic_TGB <- read.csv("./TGB/model_subsets/TGB_genetic.csv")
LTSA_ecotone_TGB <- read.csv("./TGB/model_subsets/TGB_ecotone.csv")
LTSA_political_TGB <- read.csv("./TGB/model_subsets/TGB_political.csv")
LTSA_range_TGB <- read.csv("./TGB/model_subsets/TGB_range.csv")

plot(LTSA_political_TGB)
plot(LTSA_political_locs)

## Get input localities into a two column matrix

LTSA_genetic_TGB <- subset(LTSA_genetic_TGB, select=c(Long_m, Lat_m))
LTSA_ecotone_TGB <- subset(LTSA_ecotone_TGB, select=c(Long_m, Lat_m))
LTSA_political_TGB <- subset(LTSA_political_TGB, select=c(Long_m, Lat_m))
LTSA_range_TGB <- subset(LTSA_range_TGB, select=c(Long_m, Lat_m))


## RUNNING ENMeval ## 

##run the evaluation
##This run uses the "randomkfold" method of cross-validation, with a set of background points
##sampled based on the bias file, and 10 cross-validation folds.

LTSA_genetic_results <- ENMevaluate(LTSA_genetic_locs, LTSA_genetic_env, LTSA_genetic_TGB,categoricals = "Landcover",
                                    tune.args = list(fc = c("L","LQ","H", "LQH", "LQHP", "LQHPT"),
                                                     rm = c(0.5,1,1.5,2,2.5,3,3.5,4)), partitions = "randomkfold", partition.settings = list(kfolds = 2), algorithm = "maxent.jar")

LTSA_ecotone_results <- ENMevaluate(LTSA_ecotone_locs, LTSA_ecotone_env, LTSA_ecotone_TGB, categoricals = "Landcover",
                                    tune.args = list(fc = c("L","LQ","H", "LQH", "LQHP", "LQHPT"), 
                                                     rm = c(0.5,1,1.5,2,2.5,3,3.5,4)), partitions = "randomkfold", partition.settings = list(kfolds = 2), algorithm = "maxent.jar")

LTSA_political_results <- ENMevaluate(LTSA_political_locs, LTSA_political_env, LTSA_political_TGB, categoricals = "Landcover",
                                      tune.args = list(fc = c("L","LQ","H", "LQH", "LQHP", "LQHPT"), 
                                                       rm = c(0.5,1,1.5,2,2.5,3,3.5,4)), partitions = "randomkfold", partition.settings = list(kfolds = 2), algorithm = "maxent.jar")

LTSA_range_results <- ENMevaluate(LTSA_range_locs, LTSA_range_env, LTSA_range_TGB, categoricals = "Landcover",
                                  tune.args = list(fc = c("L","LQ","H", "LQH", "LQHP", "LQHPT"),
                                                   rm = c(0.5,1,1.5,2,2.5,3,3.5,4)), partitions = "randomkfold", partition.settings = list(kfolds = 2), algorithm = "maxent.jar")


# Create results table

LTSA_genetic_results@results
write.csv(LTSA_genetic_results@results, "./Tuning/Results/Genetic_ClimateNonClimate_TGB_2folds_jar.csv", row.names = FALSE)

LTSA_ecotone_results@results
write.csv(LTSA_ecotone_results@results, "./Tuning/Results/Ecotone_ClimateNonClimate_TGB_2folds_jar.csv", row.names = FALSE)

LTSA_political_results@results
write.csv(LTSA_political_results@results, "./Tuning/Results/Political_ClimateNonClimate_TGB_2folds_jar.csv", row.names = FALSE)

LTSA_range_results@results
write.csv(LTSA_range_results@results, "./Tuning/Results/Range_ClimateNonClimate_RB_2folds_jar.csv", row.names = FALSE)



########################### END SECTION ##############################

########################## FINAL COMMENTS ############################

#NOTES: 

#If you were to use the block method, you would replace:
#partitions = "randomkfold", partition.settings = list(kfolds = 10)
#with:
#partitions = "block"

########################### END SCRIPT ###############################
