########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Jayna Bergman, Updated by Kaegan Finn, Dec 9 2022

### Goal of this Script: 

# 1) Run Maxent in SWD mode for each kfold (training localities)
# 2) Evaluate each model with kfold (testing localities)

### Notes:  

# For loop for each kfold
# Possibly loop for each background??

### Date: April 11, 2022

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

## Create a list of the raster names in the file and properly name variables 

LTSA_names_list <- gsub(pattern=".tif", replacement="", list.files("./Environmental_Variables/Range/ClimateOnly", pattern='.tif$', all.files=TRUE, full.names=FALSE))
index <- "Type"

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
  
  LTSA_range_training_locs <- read.csv(file = paste("./kfolds/ClimateOnly/TGB/range/LTSA_fulltrainingdat",i,".csv",sep=""))
  LTSA_range_testing_locs <- read.csv(file = paste("./kfolds/ClimateOnly/TGB/range/LTSA_fulltestingdat",i,".csv", sep=""))
  
  ## Create new directory 
  
  range <- paste("./Maxent/ClimateOnly/TGB/kfolds/range/range_kfold_",i,sep="")
  dir.create(range)
  
  ## Maxent command and save
  
  ClimOnly_TGB_range_kfolds <- maxent(x=LTSA_range_training_locs[,LTSA_names_list], p=LTSA_range_training_locs[,index], removeDuplicates = FALSE, args = LTSA_range_max_args, path=range)
  
  saveRDS(ClimOnly_TGB_range_kfolds, "./Maxent/ClimateOnly/TGB/kfolds/range/ClimOnly_TGB_Range_kfolds")
  
  ## Evaluate
  
  ClimOnly_TGB_range_kfolds_e <- evaluate(p=LTSA_range_testing_locs[which(LTSA_range_testing_locs$Type == 1), LTSA_names_list], a=LTSA_range_testing_locs[which(LTSA_range_testing_locs$Type == 0), LTSA_names_list], model=ClimOnly_TGB_range_kfolds)
  print(ClimOnly_TGB_range_kfolds_e)
  
  LTSA_range_internal_auc[i] <- ClimOnly_TGB_range_kfolds_e@auc
  
  dput(ClimOnly_TGB_range_kfolds_e, file=paste("./Maxent/ClimateOnly/TGB/kfolds/range/range_kfold_",i,"/internal_eval_",i,sep=""))
  
}


## Saving auc scores as a csv
LTSA_range_aucdat <- as.data.frame(LTSA_range_internal_auc)
LTSA_range_aucdat$fold <- row.names(LTSA_range_aucdat)

write.csv(LTSA_range_aucdat[,c(2,1)], file = "./Maxent/ClimateOnly/TGB/kfolds/range/ClimOnly_TGB_Range_Internalauc.csv", row.names = FALSE)


###### political study extent ######

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
  
  LTSA_political_training_locs <- read.csv(file = paste("./kfolds/ClimateOnly/TGB/political/LTSA_fulltrainingdat",i,".csv",sep=""))
  LTSA_political_testing_locs <- read.csv(file = paste("./kfolds/ClimateOnly/TGB/political/LTSA_fulltestingdat",i,".csv", sep=""))
  
  ## Create new directory 
  
  political <- paste("./Maxent/ClimateOnly/TGB/kfolds/political/political_kfold_",i,sep="")
  dir.create(political)
  
  ## Maxent command and save
  
  ClimOnly_TGB_political_kfolds <- maxent(x=LTSA_political_training_locs[,LTSA_names_list], p=LTSA_political_training_locs[,index], removeDuplicates = FALSE, args = LTSA_political_max_args, path=political)
  
  saveRDS(ClimOnly_TGB_political_kfolds, "./Maxent/ClimateOnly/TGB/kfolds/political/ClimOnly_TGB_political_kfolds")
  
  ## Evaluate
  
  ClimOnly_TGB_political_kfolds_e <- evaluate(p=LTSA_political_testing_locs[which(LTSA_political_testing_locs$Type == 1), LTSA_names_list], a=LTSA_political_testing_locs[which(LTSA_political_testing_locs$Type == 0), LTSA_names_list], model=ClimOnly_TGB_political_kfolds)
  print(ClimOnly_TGB_political_kfolds_e)
  
  LTSA_political_internal_auc[i] <- ClimOnly_TGB_political_kfolds_e@auc
  
  dput(ClimOnly_TGB_political_kfolds_e, file=paste("./Maxent/ClimateOnly/TGB/kfolds/political/political_kfold_",i,"/internal_eval_",i,sep=""))
  
}


## Saving auc scores as a csv
LTSA_political_aucdat <- as.data.frame(LTSA_political_internal_auc)
LTSA_political_aucdat$fold <- row.names(LTSA_political_aucdat)

write.csv(LTSA_political_aucdat[,c(2,1)], file = "./Maxent/ClimateOnly/TGB/kfolds/political/ClimOnly_TGB_political_Internalauc.csv", row.names = FALSE)


###### ecotone study extent ######

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
  
  LTSA_ecotone_training_locs <- read.csv(file = paste("./kfolds/ClimateOnly/TGB/ecotone/LTSA_fulltrainingdat",i,".csv",sep=""))
  LTSA_ecotone_testing_locs <- read.csv(file = paste("./kfolds/ClimateOnly/TGB/ecotone/LTSA_fulltestingdat",i,".csv", sep=""))
  
  ## Create new directory 
  
  ecotone <- paste("./Maxent/ClimateOnly/TGB/kfolds/ecotone/ecotone_kfold_",i,sep="")
  dir.create(ecotone)
  
  ## Maxent command and save
  
  ClimOnly_TGB_ecotone_kfolds <- maxent(x=LTSA_ecotone_training_locs[,LTSA_names_list], p=LTSA_ecotone_training_locs[,index], removeDuplicates = FALSE, args = LTSA_ecotone_max_args, path=ecotone)
  
  saveRDS(ClimOnly_TGB_ecotone_kfolds, "./Maxent/ClimateOnly/TGB/kfolds/ecotone/ClimOnly_TGB_ecotone_kfolds")
  
  ## Evaluate
  
  ClimOnly_TGB_ecotone_kfolds_e <- evaluate(p=LTSA_ecotone_testing_locs[which(LTSA_ecotone_testing_locs$Type == 1), LTSA_names_list], a=LTSA_ecotone_testing_locs[which(LTSA_ecotone_testing_locs$Type == 0), LTSA_names_list], model=ClimOnly_TGB_ecotone_kfolds)
  print(ClimOnly_TGB_ecotone_kfolds_e)
  
  LTSA_ecotone_internal_auc[i] <- ClimOnly_TGB_ecotone_kfolds_e@auc
  
  dput(ClimOnly_TGB_ecotone_kfolds_e, file=paste("./Maxent/ClimateOnly/TGB/kfolds/ecotone/ecotone_kfold_",i,"/internal_eval_",i,sep=""))
  
}


## Saving auc scores as a csv
LTSA_ecotone_aucdat <- as.data.frame(LTSA_ecotone_internal_auc)
LTSA_ecotone_aucdat$fold <- row.names(LTSA_ecotone_aucdat)

write.csv(LTSA_ecotone_aucdat[,c(2,1)], file = "./Maxent/ClimateOnly/TGB/kfolds/ecotone/ClimOnly_TGB_ecotone_Internalauc.csv", row.names = FALSE)

###### genetic study extent ######

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
  
  LTSA_genetic_training_locs <- read.csv(file = paste("./kfolds/ClimateOnly/TGB/genetic/LTSA_fulltrainingdat",i,".csv",sep=""))
  LTSA_genetic_testing_locs <- read.csv(file = paste("./kfolds/ClimateOnly/TGB/genetic/LTSA_fulltestingdat",i,".csv", sep=""))
  
  ## Create new directory 
  
  genetic <- paste("./Maxent/ClimateOnly/TGB/kfolds/genetic/genetic_kfold_",i,sep="")
  dir.create(genetic)
  
  ## Maxent command and save
  
  ClimOnly_TGB_genetic_kfolds <- maxent(x=LTSA_genetic_training_locs[,LTSA_names_list], p=LTSA_genetic_training_locs[,index], removeDuplicates = FALSE, args = LTSA_genetic_max_args, path=genetic)
  
  saveRDS(ClimOnly_TGB_genetic_kfolds, "./Maxent/ClimateOnly/TGB/kfolds/genetic/ClimOnly_TGB_genetic_kfolds")
  
  ## Evaluate
  
  ClimOnly_TGB_genetic_kfolds_e <- evaluate(p=LTSA_genetic_testing_locs[which(LTSA_genetic_testing_locs$Type == 1), LTSA_names_list], a=LTSA_genetic_testing_locs[which(LTSA_genetic_testing_locs$Type == 0), LTSA_names_list], model=ClimOnly_TGB_genetic_kfolds)
  print(ClimOnly_TGB_genetic_kfolds_e)
  
  LTSA_genetic_internal_auc[i] <- ClimOnly_TGB_genetic_kfolds_e@auc
  
  dput(ClimOnly_TGB_genetic_kfolds_e, file=paste("./Maxent/ClimateOnly/TGB/kfolds/genetic/genetic_kfold_",i,"/internal_eval_",i,sep=""))
  
}


## Saving auc scores as a csv
LTSA_genetic_aucdat <- as.data.frame(LTSA_genetic_internal_auc)
LTSA_genetic_aucdat$fold <- row.names(LTSA_genetic_aucdat)

write.csv(LTSA_genetic_aucdat[,c(2,1)], file = "./Maxent/ClimateOnly/TGB/kfolds/genetic/ClimOnly_TGB_genetic_Internalauc.csv", row.names = FALSE)




################################# CLIMATE + NON CLIMATE  ################################
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
  
  LTSA_range_training_locs <- read.csv(file = paste("./kfolds/ClimNonClim/TGB/range/LTSA_fulltrainingdat",i,".csv",sep=""))
  LTSA_range_testing_locs <- read.csv(file = paste("./kfolds/ClimNonClim/TGB/range/LTSA_fulltestingdat",i,".csv", sep=""))
  
  ## Specify Landcover as factor
  LTSA_range_training_locs$Landcover <- as.factor(LTSA_range_training_locs$Landcover)
  LTSA_range_testing_locs$Landcover <- as.factor(LTSA_range_testing_locs$Landcover)
  
  ## Create new directory 
  
  range <- paste("./Maxent/ClimNonClim/TGB/kfolds/range/range_kfold_",i,sep="")
  
  ## Maxent command and save
  
  ClimOnly_TGB_range_kfolds <- maxent(x=LTSA_range_training_locs[,LTSA_names_list], p=LTSA_range_training_locs[,index], removeDuplicates = FALSE, args = LTSA_range_max_args, path=range)
  
  saveRDS(ClimOnly_TGB_range_kfolds, "./Maxent/ClimNonClim/TGB/kfolds/range/ClimOnly_TGB_range_kfolds")
  
  ## Evaluate
  
  ClimOnly_TGB_range_kfolds_e <- evaluate(p=LTSA_range_testing_locs[which(LTSA_range_testing_locs$Type == 1), LTSA_names_list], a=LTSA_range_testing_locs[which(LTSA_range_testing_locs$Type == 0), LTSA_names_list], model=ClimOnly_TGB_range_kfolds)
  print(ClimOnly_TGB_range_kfolds_e)
  
  LTSA_range_internal_auc[i] <- ClimOnly_TGB_range_kfolds_e@auc
  
  dput(ClimOnly_TGB_range_kfolds_e, file=paste("./Maxent/ClimNonClim/TGB/kfolds/range/range_kfold_",i,"/internal_eval_",i,sep=""))
  
}


## Saving auc scores as a csv
LTSA_range_aucdat <- as.data.frame(LTSA_range_internal_auc)
LTSA_range_aucdat$fold <- row.names(LTSA_range_aucdat)

write.csv(LTSA_range_aucdat[,c(2,1)], file = "./Maxent/ClimNonClim/TGB/kfolds/range/ClimOnly_TGB_range_Internalauc.csv", row.names = FALSE)

######################################
###### political study extent ######

## Setting parameters from 6.1_Tuning script
LTSA_political_reg <- 0.5
r <- get(paste("LTSA_political_reg",sep="")) # regularization parameter

## Setting maxent arguments 

basicargs <- c("-J","-P","writebackgroundpredictions","maximumiterations=5000")
LTSA_political_max_args <- c(basicargs, features=c("noquadratic","nohinge","noproduct"), paste("betamultiplier=",r,sep=""))


### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
LTSA_political_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  LTSA_political_training_locs <- read.csv(file = paste("./kfolds/ClimNonClim/TGB/political/LTSA_fulltrainingdat",i,".csv",sep=""))
  LTSA_political_testing_locs <- read.csv(file = paste("./kfolds/ClimNonClim/TGB/political/LTSA_fulltestingdat",i,".csv", sep=""))
  
  ## Specify Landcover as factor
  LTSA_political_training_locs$Landcover <- as.factor(LTSA_political_training_locs$Landcover)
  LTSA_political_testing_locs$Landcover <- as.factor(LTSA_political_testing_locs$Landcover)
  
  ## Create new directory 
  
  political <- paste("./Maxent/ClimNonClim/TGB/kfolds/political/political_kfold_",i,sep="")
  
  ## Maxent command and save
  
  ClimOnly_TGB_political_kfolds <- maxent(x=LTSA_political_training_locs[,LTSA_names_list], p=LTSA_political_training_locs[,index], removeDuplicates = FALSE, args = LTSA_political_max_args, path=political)
  
  saveRDS(ClimOnly_TGB_political_kfolds, "./Maxent/ClimNonClim/TGB/kfolds/political/ClimOnly_TGB_political_kfolds")
  
  ## Evaluate
  
  ClimOnly_TGB_political_kfolds_e <- evaluate(p=LTSA_political_testing_locs[which(LTSA_political_testing_locs$Type == 1), LTSA_names_list], a=LTSA_political_testing_locs[which(LTSA_political_testing_locs$Type == 0), LTSA_names_list], model=ClimOnly_TGB_political_kfolds)
  print(ClimOnly_TGB_political_kfolds_e)
  
  LTSA_political_internal_auc[i] <- ClimOnly_TGB_political_kfolds_e@auc
  
  dput(ClimOnly_TGB_political_kfolds_e, file=paste("./Maxent/ClimNonClim/TGB/kfolds/political/political_kfold_",i,"/internal_eval_",i,sep=""))
  
}


## Saving auc scores as a csv
LTSA_political_aucdat <- as.data.frame(LTSA_political_internal_auc)
LTSA_political_aucdat$fold <- row.names(LTSA_political_aucdat)

write.csv(LTSA_political_aucdat[,c(2,1)], file = "./Maxent/ClimNonClim/TGB/kfolds/political/ClimOnly_TGB_political_Internalauc.csv", row.names = FALSE)

###########################
###### ecotone study extent ######

## Setting parameters from 6.1_Tuning script
LTSA_ecotone_reg <- 0.5
r <- get(paste("LTSA_ecotone_reg",sep="")) # regularization parameter

## Setting maxent arguments 

basicargs <- c("-J","-P","writebackgroundpredictions","maximumiterations=5000")
LTSA_ecotone_max_args <- c(basicargs, features=c("Threshold"), paste("betamultiplier=",r,sep=""))

### For loop to loop through kfolds ###

x <- c(1,2,3,4,5)
LTSA_ecotone_internal_auc <- rep(NA, length(x))

for (i in 1:length(x)){
  
  ## Read in Training localities and testing localities
  
  LTSA_ecotone_training_locs <- read.csv(file = paste("./kfolds/ClimNonClim/TGB/ecotone/LTSA_fulltrainingdat",i,".csv",sep=""))
  LTSA_ecotone_testing_locs <- read.csv(file = paste("./kfolds/ClimNonClim/TGB/ecotone/LTSA_fulltestingdat",i,".csv", sep=""))
  
  ## Specify Landcover as factor
  LTSA_ecotone_training_locs$Landcover <- as.factor(LTSA_ecotone_training_locs$Landcover)
  LTSA_ecotone_testing_locs$Landcover <- as.factor(LTSA_ecotone_testing_locs$Landcover)
  
  ## Create new directory 
  
  ecotone <- paste("./Maxent/ClimNonClim/TGB/kfolds/ecotone/ecotone_kfold_",i,sep="")
  
  ## Maxent command and save
  
  ClimOnly_TGB_ecotone_kfolds <- maxent(x=LTSA_ecotone_training_locs[,LTSA_names_list], p=LTSA_ecotone_training_locs[,index], removeDuplicates = FALSE, args = LTSA_ecotone_max_args, path=ecotone)
  
  saveRDS(ClimOnly_TGB_ecotone_kfolds, "./Maxent/ClimNonClim/TGB/kfolds/ecotone/ClimOnly_TGB_ecotone_kfolds")
  
  ## Evaluate
  
  ClimOnly_TGB_ecotone_kfolds_e <- evaluate(p=LTSA_ecotone_testing_locs[which(LTSA_ecotone_testing_locs$Type == 1), LTSA_names_list], a=LTSA_ecotone_testing_locs[which(LTSA_ecotone_testing_locs$Type == 0), LTSA_names_list], model=ClimOnly_TGB_ecotone_kfolds)
  print(ClimOnly_TGB_ecotone_kfolds_e)
  
  LTSA_ecotone_internal_auc[i] <- ClimOnly_TGB_ecotone_kfolds_e@auc
  
  dput(ClimOnly_TGB_ecotone_kfolds_e, file=paste("./Maxent/ClimNonClim/TGB/kfolds/ecotone/ecotone_kfold_",i,"/internal_eval_",i,sep=""))
  
}


## Saving auc scores as a csv
LTSA_ecotone_aucdat <- as.data.frame(LTSA_ecotone_internal_auc)
LTSA_ecotone_aucdat$fold <- row.names(LTSA_ecotone_aucdat)

write.csv(LTSA_ecotone_aucdat[,c(2,1)], file = "./Maxent/ClimNonClim/TGB/kfolds/ecotone/ClimOnly_TGB_ecotone_Internalauc.csv", row.names = FALSE)

#################

###### genetic study extent ######

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
  
  LTSA_genetic_training_locs <- read.csv(file = paste("./kfolds/ClimNonClim/TGB/genetic/LTSA_fulltrainingdat",i,".csv",sep=""))
  LTSA_genetic_testing_locs <- read.csv(file = paste("./kfolds/ClimNonClim/TGB/genetic/LTSA_fulltestingdat",i,".csv", sep=""))
  
  ## Specify Landcover as factor
  LTSA_genetic_training_locs$Landcover <- as.factor(LTSA_genetic_training_locs$Landcover)
  LTSA_genetic_testing_locs$Landcover <- as.factor(LTSA_genetic_testing_locs$Landcover)
  
  ## Create new directory 
  
  genetic <- paste("./Maxent/ClimNonClim/TGB/kfolds/genetic/genetic_kfold_",i,sep="")
  
  ## Maxent command and save
  
  ClimOnly_TGB_genetic_kfolds <- maxent(x=LTSA_genetic_training_locs[,LTSA_names_list], p=LTSA_genetic_training_locs[,index], removeDuplicates = FALSE, args = LTSA_genetic_max_args, path=genetic)
  
  saveRDS(ClimOnly_TGB_genetic_kfolds, "./Maxent/ClimNonClim/TGB/kfolds/genetic/ClimOnly_TGB_genetic_kfolds")
  
  ## Evaluate
  
  ClimOnly_TGB_genetic_kfolds_e <- evaluate(p=LTSA_genetic_testing_locs[which(LTSA_genetic_testing_locs$Type == 1), LTSA_names_list], a=LTSA_genetic_testing_locs[which(LTSA_genetic_testing_locs$Type == 0), LTSA_names_list], model=ClimOnly_TGB_genetic_kfolds)
  print(ClimOnly_TGB_genetic_kfolds_e)
  
  LTSA_genetic_internal_auc[i] <- ClimOnly_TGB_genetic_kfolds_e@auc
  
  dput(ClimOnly_TGB_genetic_kfolds_e, file=paste("./Maxent/ClimNonClim/TGB/kfolds/genetic/genetic_kfold_",i,"/internal_eval_",i,sep=""))
  
}


## Saving auc scores as a csv
LTSA_genetic_aucdat <- as.data.frame(LTSA_genetic_internal_auc)
LTSA_genetic_aucdat$fold <- row.names(LTSA_genetic_aucdat)

write.csv(LTSA_genetic_aucdat[,c(2,1)], file = "./Maxent/ClimNonClim/TGB/kfolds/genetic/ClimOnly_TGB_genetic_Internalauc.csv", row.names = FALSE)


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################

########################### END SCRIPT ###############################
