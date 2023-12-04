########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Kaegan Finn

### Goal of this Script: 

# 1) Run Maxent with random background points for each kfold (training localities)
# 2) Evaluate each model with kfold (testing localities)

### Notes:  

# 

### Date: December 13, 2022

### Version of R:  R version 4.0.3

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(dismo)
library(sf)
library(terra)
library(rgdal)
library(dplyr)
library(rJava)


rm(list=ls())
########################### END SECTION ##############################

###################### FUNCTION DEFINITIONS ##########################

# NA 

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


###### range study extent ######
## Setting parameters from 6.1_Tuning script
LTSA_range_reg <- 0.5
r <- get(paste("LTSA_range_reg",sep="")) # regularization parameter

## Setting maxent arguments 

basicargs <- c("-J","-P","writebackgroundpredictions","maximumiterations=5000")
LTSA_range_max_args <- c(basicargs, features=c("Threshold"), paste("betamultiplier=",r,sep=""))

### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
LTSA_range_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  LTSA_range_training_locs <- read.csv(file = paste("./kfolds/Range/Range_trainingdat",i,".csv",sep=""))
  LTSA_range_testing_locs <- read.csv(file = paste("./kfolds/Range/Range_testingdat",i,".csv", sep=""))
  
  # Directory 
  
  range <- paste("./Maxent/ClimateOnly/RB/kfolds/range/range_kfold_",i,sep="")
  dir.create(range)

  ## Maxent command and save
  
  ClimOnly_RB_Range_kfolds <- maxent(x=LTSA_range_env, p=LTSA_range_training_locs[,c("Long_m", "Lat_m")], nbg= 10000, removeDuplicates = FALSE, args = LTSA_range_max_args, path=range)
  
  saveRDS(ClimOnly_RB_Range_kfolds, "./Maxent/ClimateOnly/RB/kfolds/range/ClimOnly_RB_Range_kfolds")
  
  ## Evaluate by creating random points 
  
  eval_background_pts <- randomPoints(LTSA_range_env, n = 10000, p = LTSA_range_testing_locs[,c("Long_m", "Lat_m")])
  
  ClimOnly_RB_Range_kfolds_e <- evaluate(p=LTSA_range_testing_locs[,c("Long_m", "Lat_m")], a=eval_background_pts, model=ClimOnly_RB_Range_kfolds, x = LTSA_range_env)
  print(ClimOnly_RB_Range_kfolds_e)
  
  LTSA_range_internal_auc[i] <- ClimOnly_RB_Range_kfolds_e@auc
  
  dput(ClimOnly_RB_Range_kfolds_e, file=paste("./Maxent/ClimateOnly/RB/kfolds/range/range_kfold_",i,"/internal_eval_",i,sep=""))
  
}

## Saving auc scores as a csv
range_aucdat <- as.data.frame(LTSA_range_internal_auc)
range_aucdat$fold <- row.names(range_aucdat)

write.csv(range_aucdat[,c(2,1)], file = "./Maxent/ClimateOnly/RB/kfolds/range/ClimOnly_RB_Range_Internalauc.csv", row.names = FALSE)

##############################################################################################################################################################
#trying to figure out why TGB would be k-folded in SWD mode, when It appears RB are not.... 

LTSA_range_testing_locs <- read.csv("./kfolds/Range/Range_testingdat1.csv")
eval_background_pts <- randomPoints(LTSA_range_env, n = 10000, p = LTSA_range_testing_locs[,c("Long_m", "Lat_m")])
plot(eval_background_pts)

##############################################################################################################################################################

##### political study extent #######
## Setting parameters from 6.1_Tuning script
LTSA_political_reg <- 1.5
r <- get(paste("LTSA_political_reg",sep="")) # regularization parameter

## Setting maxent arguments 

basicargs <- c("-J","-P","writebackgroundpredictions","maximumiterations=5000")
LTSA_political_max_args <- c(basicargs, features=c("Threshold"), paste("betamultiplier=",r,sep=""))

### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
LTSA_political_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  LTSA_political_training_locs <- read.csv(file = paste("./kfolds/political/political_trainingdat",i,".csv",sep=""))
  LTSA_political_testing_locs <- read.csv(file = paste("./kfolds/political/political_testingdat",i,".csv", sep=""))
  
  # Directory 
  
  political <- paste("./Maxent/ClimateOnly/RB/kfolds/political/political_kfold_",i,sep="")
  dir.create(political)
  
  ## Maxent command and save
  
  ClimOnly_RB_political_kfolds <- maxent(x=LTSA_political_env, p=LTSA_political_training_locs[,c("Long_m", "Lat_m")], nbg= 10000, removeDuplicates = FALSE, args = LTSA_political_max_args, path=political)
  
  saveRDS(ClimOnly_RB_political_kfolds, "./Maxent/ClimateOnly/RB/kfolds/political/ClimOnly_RB_political_kfolds")
  
  ## Evaluate by creating random points 
  
  eval_background_pts <- randomPoints(LTSA_political_env, n = 10000, p = LTSA_political_testing_locs[,c("Long_m", "Lat_m")])
  
  ClimOnly_RB_political_kfolds_e <- evaluate(p=LTSA_political_testing_locs[,c("Long_m", "Lat_m")], a=eval_background_pts, model=ClimOnly_RB_political_kfolds, x = LTSA_political_env)
  print(ClimOnly_RB_political_kfolds_e)
  
  LTSA_political_internal_auc[i] <- ClimOnly_RB_political_kfolds_e@auc
  
  dput(ClimOnly_RB_political_kfolds_e, file=paste("./Maxent/ClimateOnly/RB/kfolds/political/political_kfold_",i,"/internal_eval_",i,sep=""))
  
}

## Saving auc scores as a csv
political_aucdat <- as.data.frame(LTSA_political_internal_auc)
political_aucdat$fold <- row.names(political_aucdat)

write.csv(political_aucdat[,c(2,1)], file = "./Maxent/ClimateOnly/RB/kfolds/political/ClimOnly_RB_political_Internalauc.csv", row.names = FALSE)


##############################################################################################################################################################


##### ecotone study extent #######
## Setting parameters from 6.1_Tuning script
LTSA_ecotone_reg <- 0.5
r <- get(paste("LTSA_ecotone_reg",sep="")) # regularization parameter

## Setting maxent arguments 

basicargs <- c("-J","-P","writebackgroundpredictions","maximumiterations=5000")
LTSA_ecotone_max_args <- c(basicargs, paste("betamultiplier=",r,sep=""))

### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
LTSA_ecotone_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  LTSA_ecotone_training_locs <- read.csv(file = paste("./kfolds/ecotone/ecotone_trainingdat",i,".csv",sep=""))
  LTSA_ecotone_testing_locs <- read.csv(file = paste("./kfolds/ecotone/ecotone_testingdat",i,".csv", sep=""))
  
  # Directory 
  
  ecotone <- paste("./Maxent/ClimateOnly/RB/kfolds/ecotone/ecotone_kfold_",i,sep="")
  dir.create(ecotone)
  
  ## Maxent command and save
  
  ClimOnly_RB_ecotone_kfolds <- maxent(x=LTSA_ecotone_env, p=LTSA_ecotone_training_locs[,c("Long_m", "Lat_m")], nbg= 10000, removeDuplicates = FALSE, args = LTSA_ecotone_max_args, path=ecotone)
  
  saveRDS(ClimOnly_RB_ecotone_kfolds, "./Maxent/ClimateOnly/RB/kfolds/ecotone/ClimOnly_RB_ecotone_kfolds")
  
  ## Evaluate by creating random points 
  
  eval_background_pts <- randomPoints(LTSA_ecotone_env, n = 10000, p = LTSA_ecotone_testing_locs[,c("Long_m", "Lat_m")])
  
  ClimOnly_RB_ecotone_kfolds_e <- evaluate(p=LTSA_ecotone_testing_locs[,c("Long_m", "Lat_m")], a=eval_background_pts, model=ClimOnly_RB_ecotone_kfolds, x = LTSA_ecotone_env)
  print(ClimOnly_RB_ecotone_kfolds_e)
  
  LTSA_ecotone_internal_auc[i] <- ClimOnly_RB_ecotone_kfolds_e@auc
  
  dput(ClimOnly_RB_ecotone_kfolds_e, file=paste("./Maxent/ClimateOnly/RB/kfolds/ecotone/ecotone_kfold_",i,"/internal_eval_",i,sep=""))
  
}

## Saving auc scores as a csv
ecotone_aucdat <- as.data.frame(LTSA_ecotone_internal_auc)
ecotone_aucdat$fold <- row.names(ecotone_aucdat)

write.csv(ecotone_aucdat[,c(2,1)], file = "./Maxent/ClimateOnly/RB/kfolds/ecotone/ClimOnly_RB_ecotone_Internalauc.csv", row.names = FALSE)


##############################################################################################################################################################


##### genetic study extent #######
## Setting parameters from 6.1_Tuning script
LTSA_genetic_reg <- 0.5
r <- get(paste("LTSA_genetic_reg",sep="")) # regularization parameter

## Setting maxent arguments 

basicargs <- c("-J","-P","writebackgroundpredictions","maximumiterations=5000")
LTSA_genetic_max_args <- c(basicargs, features=c("Threshold"), paste("betamultiplier=",r,sep=""))

### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
LTSA_genetic_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  LTSA_genetic_training_locs <- read.csv(file = paste("./kfolds/genetic/genetic_trainingdat",i,".csv",sep=""))
  LTSA_genetic_testing_locs <- read.csv(file = paste("./kfolds/genetic/genetic_testingdat",i,".csv", sep=""))
  
  # Directory 
  
  genetic <- paste("./Maxent/ClimateOnly/RB/kfolds/genetic/genetic_kfold_",i,sep="")
  dir.create(genetic)
  
  ## Maxent command and save
  
  ClimOnly_RB_genetic_kfolds <- maxent(x=LTSA_genetic_env, p=LTSA_genetic_training_locs[,c("Long_m", "Lat_m")], nbg= 10000, removeDuplicates = FALSE, args = LTSA_genetic_max_args, path=genetic)
  
  saveRDS(ClimOnly_RB_genetic_kfolds, "./Maxent/ClimateOnly/RB/kfolds/genetic/ClimOnly_RB_genetic_kfolds")
  
  ## Evaluate by creating random points 
  
  eval_background_pts <- randomPoints(LTSA_genetic_env, n = 10000, p = LTSA_genetic_testing_locs[,c("Long_m", "Lat_m")])
  
  ClimOnly_RB_genetic_kfolds_e <- evaluate(p=LTSA_genetic_testing_locs[,c("Long_m", "Lat_m")], a=eval_background_pts, model=ClimOnly_RB_genetic_kfolds, x = LTSA_genetic_env)
  print(ClimOnly_RB_genetic_kfolds_e)
  
  LTSA_genetic_internal_auc[i] <- ClimOnly_RB_genetic_kfolds_e@auc
  
  dput(ClimOnly_RB_genetic_kfolds_e, file=paste("./Maxent/ClimateOnly/RB/kfolds/genetic/genetic_kfold_",i,"/internal_eval_",i,sep=""))
  
}

## Saving auc scores as a csv
genetic_aucdat <- as.data.frame(LTSA_genetic_internal_auc)
genetic_aucdat$fold <- row.names(genetic_aucdat)

write.csv(genetic_aucdat[,c(2,1)], file = "./Maxent/ClimateOnly/RB/kfolds/genetic/ClimOnly_RB_genetic_Internalauc.csv", row.names = FALSE)

##############################################################################################################################################################
##############################################################################################################################################################
##############################################################################################################################################################

##### CLIMATE + NON CLIMATE ##### 
rm(list=ls())
####

##### GET ENVIRO-VARS #####

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

###### range study extent ######
## Setting parameters from 6.1_Tuning script
LTSA_range_reg <- 0.5
r <- get(paste("LTSA_range_reg",sep="")) # regularization parameter

## Setting maxent arguments 

basicargs <- c("-J","-P","writebackgroundpredictions","maximumiterations=5000")
LTSA_range_max_args <- c(basicargs, features=c("Threshold"), paste("betamultiplier=",r,sep=""))

### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
LTSA_range_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  LTSA_range_training_locs <- read.csv(file = paste("./kfolds/ClimNonClim/RB/Range/Range_trainingdat",i,".csv",sep=""))
  LTSA_range_testing_locs <- read.csv(file = paste("./kfolds/ClimNonClim/RB/Range/Range_testingdat",i,".csv", sep=""))
  
  # Directory 
  
  range <- paste("./Maxent/ClimNonClim/RB/kfolds/range/range_kfold_",i,sep="")
  dir.create(range)
  
  ## Maxent command and save
  
  ClimNonClim_RB_Range_kfolds <- maxent(x=LTSA_range_env, p=LTSA_range_training_locs[,c("Long_m", "Lat_m")], factors = "Landcover", nbg= 10000, removeDuplicates = FALSE, args = LTSA_range_max_args, path=range)
  
  saveRDS(ClimNonClim_RB_Range_kfolds, "./Maxent/ClimNonClim/RB/kfolds/range/ClimNonClim_RB_Range_kfolds")
  
  ## Evaluate by creating random points 
  
  eval_background_pts <- randomPoints(LTSA_range_env, n = 10000, p = LTSA_range_testing_locs[,c("Long_m", "Lat_m")])
  
  ClimNonClim_RB_Range_kfolds_e <- evaluate(p=LTSA_range_testing_locs[,c("Long_m", "Lat_m")], a=eval_background_pts, model=ClimNonClim_RB_Range_kfolds, x = LTSA_range_env)
  print(ClimNonClim_RB_Range_kfolds_e)
  
  LTSA_range_internal_auc[i] <- ClimNonClim_RB_Range_kfolds_e@auc
  
  dput(ClimNonClim_RB_Range_kfolds_e, file=paste("./Maxent/ClimNonClim/RB/kfolds/range/range_kfold_",i,"/internal_eval_",i,sep=""))
  
}

## Saving auc scores as a csv
range_aucdat <- as.data.frame(LTSA_range_internal_auc)
range_aucdat$fold <- row.names(range_aucdat)

write.csv(range_aucdat[,c(2,1)], file = "./Maxent/ClimNonClim/RB/kfolds/range/ClimNonClim_RB_Range_Internalauc.csv", row.names = FALSE)

##############################################################################################################################################################
#trying to figure out why TGB would be k-folded in SWD mode, when It appears RB are not.... 

LTSA_range_testing_locs <- read.csv("./kfolds/Range/Range_testingdat1.csv")
eval_background_pts <- randomPoints(LTSA_range_env, n = 10000, p = LTSA_range_testing_locs[,c("Long_m", "Lat_m")])
plot(eval_background_pts)

##############################################################################################################################################################

##### political study extent #######
## Setting parameters from 6.1_Tuning script
LTSA_political_reg <- 0.5
r <- get(paste("LTSA_political_reg",sep="")) # regularization parameter

## Setting maxent arguments 

basicargs <- c("-J","-P","writebackgroundpredictions","maximumiterations=5000")
LTSA_political_max_args <- c(basicargs, features=c("nohinge","noproduct"), paste("betamultiplier=",r,sep=""))

### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
LTSA_political_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  LTSA_political_training_locs <- read.csv(file = paste("./kfolds/ClimNonClim/RB/political/political_trainingdat",i,".csv",sep=""))
  LTSA_political_testing_locs <- read.csv(file = paste("./kfolds/ClimNonClim/RB/political/political_testingdat",i,".csv", sep=""))
  
  # Directory 
  
  political <- paste("./Maxent/ClimNonClim/RB/kfolds/political/political_kfold_",i,sep="")
  dir.create(political)
  
  ## Maxent command and save
  
  ClimNonClim_RB_political_kfolds <- maxent(x=LTSA_pol_env, p=LTSA_political_training_locs[,c("Long_m", "Lat_m")],factors = "Landcover", nbg= 10000, removeDuplicates = FALSE, args = LTSA_political_max_args, path=political)
  
  saveRDS(ClimNonClim_RB_political_kfolds, "./Maxent/ClimNonClim/RB/kfolds/political/ClimNonClim_RB_political_kfolds")
  
  ## Evaluate by creating random points 
  
  eval_background_pts <- randomPoints(LTSA_pol_env, n = 10000, p = LTSA_political_testing_locs[,c("Long_m", "Lat_m")])
  
  ClimNonClim_RB_political_kfolds_e <- evaluate(p=LTSA_political_testing_locs[,c("Long_m", "Lat_m")], a=eval_background_pts, model=ClimNonClim_RB_political_kfolds, x = LTSA_pol_env)
  print(ClimNonClim_RB_political_kfolds_e)
  
  LTSA_political_internal_auc[i] <- ClimNonClim_RB_political_kfolds_e@auc
  
  dput(ClimNonClim_RB_political_kfolds_e, file=paste("./Maxent/ClimNonClim/RB/kfolds/political/political_kfold_",i,"/internal_eval_",i,sep=""))
  
}

## Saving auc scores as a csv
political_aucdat <- as.data.frame(LTSA_political_internal_auc)
political_aucdat$fold <- row.names(political_aucdat)

write.csv(political_aucdat[,c(2,1)], file = "./Maxent/ClimNonClim/RB/kfolds/political/ClimNonClim_RB_political_Internalauc.csv", row.names = FALSE)


##############################################################################################################################################################


##### ecotone study extent #######
## Setting parameters from 6.1_Tuning script
LTSA_ecotone_reg <- 0.5
r <- get(paste("LTSA_ecotone_reg",sep="")) # regularization parameter

## Setting maxent arguments 

basicargs <- c("-J","-P","writebackgroundpredictions","maximumiterations=5000")
LTSA_ecotone_max_args <- c(basicargs, features=c(""), paste("betamultiplier=",r,sep=""))

### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
LTSA_ecotone_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  LTSA_ecotone_training_locs <- read.csv(file = paste("./kfolds/ClimNonClim/RB/ecotone/ecotone_trainingdat",i,".csv",sep=""))
  LTSA_ecotone_testing_locs <- read.csv(file = paste("./kfolds/ClimNonClim/RB/ecotone/ecotone_testingdat",i,".csv", sep=""))
  
  # Directory 
  
  ecotone <- paste("./Maxent/ClimNonClim/RB/kfolds/ecotone/ecotone_kfold_",i,sep="")
  dir.create(ecotone)
  
  ## Maxent command and save
  
  ClimNonClim_RB_ecotone_kfolds <- maxent(x=LTSA_eco_env, p=LTSA_ecotone_training_locs[,c("Long_m", "Lat_m")],factors = "Landcover", nbg= 10000, removeDuplicates = FALSE, args = LTSA_ecotone_max_args, path=ecotone)
  
  saveRDS(ClimNonClim_RB_ecotone_kfolds, "./Maxent/ClimNonClim/RB/kfolds/ecotone/ClimNonClim_RB_ecotone_kfolds")
  
  ## Evaluate by creating random points 
  
  eval_background_pts <- randomPoints(LTSA_eco_env, n = 10000, p = LTSA_ecotone_testing_locs[,c("Long_m", "Lat_m")])
  
  ClimNonClim_RB_ecotone_kfolds_e <- evaluate(p=LTSA_ecotone_testing_locs[,c("Long_m", "Lat_m")], a=eval_background_pts, model=ClimNonClim_RB_ecotone_kfolds, x = LTSA_eco_env)
  print(ClimNonClim_RB_ecotone_kfolds_e)
  
  LTSA_ecotone_internal_auc[i] <- ClimNonClim_RB_ecotone_kfolds_e@auc
  
  dput(ClimNonClim_RB_ecotone_kfolds_e, file=paste("./Maxent/ClimNonClim/RB/kfolds/ecotone/ecotone_kfold_",i,"/internal_eval_",i,sep=""))
  
}

## Saving auc scores as a csv
ecotone_aucdat <- as.data.frame(LTSA_ecotone_internal_auc)
ecotone_aucdat$fold <- row.names(ecotone_aucdat)

write.csv(ecotone_aucdat[,c(2,1)], file = "./Maxent/ClimNonClim/RB/kfolds/ecotone/ClimNonClim_RB_ecotone_Internalauc.csv", row.names = FALSE)


##############################################################################################################################################################


##### genetic study extent #######
## Setting parameters from 6.1_Tuning script
LTSA_genetic_reg <- 0.5
r <- get(paste("LTSA_genetic_reg",sep="")) # regularization parameter

## Setting maxent arguments 

basicargs <- c("-J","-P","writebackgroundpredictions","maximumiterations=5000")
LTSA_genetic_max_args <- c(basicargs, features=c("Threshold"), paste("betamultiplier=",r,sep=""))

### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
LTSA_genetic_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  LTSA_genetic_training_locs <- read.csv(file = paste("./kfolds/ClimNonClim/RB/genetic/genetic_trainingdat",i,".csv",sep=""))
  LTSA_genetic_testing_locs <- read.csv(file = paste("./kfolds/ClimNonClim/RB/genetic/genetic_testingdat",i,".csv", sep=""))
  
  # Directory 
  
  genetic <- paste("./Maxent/ClimNonClim/RB/kfolds/genetic/genetic_kfold_",i,sep="")
  dir.create(genetic)
  
  ## Maxent command and save
  
  ClimNonClim_RB_genetic_kfolds <- maxent(x=LTSA_gen_env, p=LTSA_genetic_training_locs[,c("Long_m", "Lat_m")],factors = "Landcover", nbg= 10000, removeDuplicates = FALSE, args = LTSA_genetic_max_args, path=genetic)
  
  saveRDS(ClimNonClim_RB_genetic_kfolds, "./Maxent/ClimNonClim/RB/kfolds/genetic/ClimNonClim_RB_genetic_kfolds")
  
  ## Evaluate by creating random points 
  
  eval_background_pts <- randomPoints(LTSA_gen_env, n = 10000, p = LTSA_genetic_testing_locs[,c("Long_m", "Lat_m")])
  
  ClimNonClim_RB_genetic_kfolds_e <- evaluate(p=LTSA_genetic_testing_locs[,c("Long_m", "Lat_m")], a=eval_background_pts, model=ClimNonClim_RB_genetic_kfolds, x = LTSA_gen_env)
  print(ClimNonClim_RB_genetic_kfolds_e)
  
  LTSA_genetic_internal_auc[i] <- ClimNonClim_RB_genetic_kfolds_e@auc
  
  dput(ClimNonClim_RB_genetic_kfolds_e, file=paste("./Maxent/ClimNonClim/RB/kfolds/genetic/genetic_kfold_",i,"/internal_eval_",i,sep=""))
  
}

## Saving auc scores as a csv
genetic_aucdat <- as.data.frame(LTSA_genetic_internal_auc)
genetic_aucdat$fold <- row.names(genetic_aucdat)

write.csv(genetic_aucdat[,c(2,1)], file = "./Maxent/ClimNonClim/RB/kfolds/genetic/ClimNonClim_RB_genetic_Internalauc.csv", row.names = FALSE)

########################### END SECTION ##############################

########################## FINAL COMMENTS ############################

########################### END SCRIPT ###############################
