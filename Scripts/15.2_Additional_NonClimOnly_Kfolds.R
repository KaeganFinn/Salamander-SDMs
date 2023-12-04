########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Kaegan Finn, Oct 20th, 2023

### Goal of this Script: 

# Runs maxent k folds using only non-climate variables. In response to JAE Review sent on October 18, 2023

# Creates a prediction surface rasters for 5 kfolds, gives you ability to calcluate average Internal AUC 

### Version of R:  R version 4.2.

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(dismo)
library(sf)
library(terra)
library(rgdal)
library(dplyr)
library(rJava)

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
  
  political <- paste("./Maxent/NonClimOnly/RB/kfolds/political/political_kfold_",i,sep="")
  dir.create(political)
  
  ## Maxent command and save
  
  NonClimOnly_RB_political_kfolds <- maxent(x=LTSA_pol_env, p=LTSA_political_training_locs[,c("Long_m", "Lat_m")],factors = "Landcover", nbg= 10000, removeDuplicates = FALSE, args = LTSA_political_max_args, path=political)
  
  saveRDS(NonClimOnly_RB_political_kfolds, "./Maxent/NonClimOnly/RB/kfolds/political/NonClimOnly_RB_political_kfolds")
  
  ## Evaluate by creating random points 
  
  eval_background_pts <- randomPoints(LTSA_pol_env, n = 10000, p = LTSA_political_testing_locs[,c("Long_m", "Lat_m")])
  
  NonClimOnly_RB_political_kfolds_e <- evaluate(p=LTSA_political_testing_locs[,c("Long_m", "Lat_m")], a=eval_background_pts, model=NonClimOnly_RB_political_kfolds, x = LTSA_pol_env)
  print(NonClimOnly_RB_political_kfolds_e)
  
  LTSA_political_internal_auc[i] <- NonClimOnly_RB_political_kfolds_e@auc
  
  dput(NonClimOnly_RB_political_kfolds_e, file=paste("./Maxent/NonClimOnly/RB/kfolds/political/political_kfold_",i,"/internal_eval_",i,sep=""))
  
}

## Saving auc scores as a csv
political_aucdat <- as.data.frame(LTSA_political_internal_auc)
political_aucdat$fold <- row.names(political_aucdat)

write.csv(political_aucdat[,c(2,1)], file = "./Maxent/NonClimOnly/RB/kfolds/political/NonClimOnly_RB_political_Internalauc.csv", row.names = FALSE)


##############################################################################################################################################################


########################## FINAL COMMENTS ############################

##

########################### END SCRIPT ###############################
