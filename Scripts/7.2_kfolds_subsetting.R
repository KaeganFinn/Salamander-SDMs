########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Jayna Bergman, Edited by Kaegan Finn Dec 7, 2022

### Goal of this Script: 

# reads in localities with extracted environmental variable values, creates x amount of kfolds, saves as separate csvs

### Notes:  

# 

### Date: June 6, 2022

### Version of R:  R version 4.0.3

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(dismo)
library(sf)

rm(list=ls())

## Set working directory

getwd()
setwd()

############### ClimateOnly + RB  ############

## Read input localities for each study extent 

LTSA_genetic_locs <- read.csv("./Input_localities/model_subsets/LTSA_genetic_locs.csv")
LTSA_ecotone_locs <- read.csv("./Input_localities/model_subsets//LTSA_ecotone_locs.csv")
LTSA_political_locs <- read.csv("./Input_localities/model_subsets/LTSA_political_locs.csv")
LTSA_range_locs <- read.csv("./Input_localities/model_subsets/LTSA_range_locs.csv")

################################# Range ################################

LTSA_range_fold <- kfold(LTSA_range_locs, k=5)

### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  LTSA_range_occtest <- LTSA_range_locs[LTSA_range_fold == i, ]
  LTSA_range_occtrain <- LTSA_range_locs[LTSA_range_fold != i, ]
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(LTSA_range_occtest, file=paste("./kfolds/Range/Range_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(LTSA_range_occtrain, file=paste("./kfolds/Range/Range_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
}
################################# Political ################################

LTSA_political_fold <- kfold(LTSA_political_locs, k=5)

### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  LTSA_political_occtest <- LTSA_political_locs[LTSA_political_fold == i, ]
  LTSA_political_occtrain <- LTSA_political_locs[LTSA_political_fold != i, ]
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(LTSA_political_occtest, file=paste("./kfolds/political/political_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(LTSA_political_occtrain, file=paste("./kfolds/political/political_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
}
################################# Genetic ################################

LTSA_genetic_fold <- kfold(LTSA_genetic_locs, k=5)

### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  LTSA_genetic_occtest <- LTSA_genetic_locs[LTSA_genetic_fold == i, ]
  LTSA_genetic_occtrain <- LTSA_genetic_locs[LTSA_genetic_fold != i, ]
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(LTSA_genetic_occtest, file=paste("./kfolds/genetic/genetic_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(LTSA_genetic_occtrain, file=paste("./kfolds/genetic/genetic_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
}
################################# Ecoregion ################################

LTSA_ecotone_fold <- kfold(LTSA_ecotone_locs, k=5)

### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  LTSA_ecotone_occtest <- LTSA_ecotone_locs[LTSA_ecotone_fold == i, ]
  LTSA_ecotone_occtrain <- LTSA_ecotone_locs[LTSA_ecotone_fold != i, ]
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(LTSA_ecotone_occtest, file=paste("./kfolds/ecotone/ecotone_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(LTSA_ecotone_occtrain, file=paste("./kfolds/ecotone/ecotone_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
}

############################
rm(list=ls())
##########################

############### ClimateNonClimate + RB  ############

## Read input localities for each study extent 

LTSA_genetic_locs <- read.csv("./Input_localities/model_subsets/LTSA_genetic_locs.csv")
LTSA_ecotone_locs <- read.csv("./Input_localities/model_subsets//LTSA_ecotone_locs.csv")
LTSA_political_locs <- read.csv("./Input_localities/model_subsets/LTSA_political_locs.csv")
LTSA_range_locs <- read.csv("./Input_localities/model_subsets/LTSA_range_locs.csv")

################################# Range ################################

LTSA_range_fold <- kfold(LTSA_range_locs, k=5)

### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  LTSA_range_occtest <- LTSA_range_locs[LTSA_range_fold == i, ]
  LTSA_range_occtrain <- LTSA_range_locs[LTSA_range_fold != i, ]
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(LTSA_range_occtest, file=paste("./kfolds/ClimNonClim/RB/Range/Range_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(LTSA_range_occtrain, file=paste("./kfolds/ClimNonClim/RB/Range/Range_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
}
################################# Political ################################

LTSA_political_fold <- kfold(LTSA_political_locs, k=5)

### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  LTSA_political_occtest <- LTSA_political_locs[LTSA_political_fold == i, ]
  LTSA_political_occtrain <- LTSA_political_locs[LTSA_political_fold != i, ]
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(LTSA_political_occtest, file=paste("./kfolds/ClimNonClim/RB/political/political_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(LTSA_political_occtrain, file=paste("./kfolds/ClimNonClim/RB/political/political_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
}
################################# Genetic ################################

LTSA_genetic_fold <- kfold(LTSA_genetic_locs, k=5)

### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  LTSA_genetic_occtest <- LTSA_genetic_locs[LTSA_genetic_fold == i, ]
  LTSA_genetic_occtrain <- LTSA_genetic_locs[LTSA_genetic_fold != i, ]
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(LTSA_genetic_occtest, file=paste("./kfolds/ClimNonClim/RB/genetic/genetic_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(LTSA_genetic_occtrain, file=paste("./kfolds/ClimNonClim/RB/genetic/genetic_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
}
################################# Ecoregion ################################

LTSA_ecotone_fold <- kfold(LTSA_ecotone_locs, k=5)

### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  LTSA_ecotone_occtest <- LTSA_ecotone_locs[LTSA_ecotone_fold == i, ]
  LTSA_ecotone_occtrain <- LTSA_ecotone_locs[LTSA_ecotone_fold != i, ]
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(LTSA_ecotone_occtest, file=paste("./kfolds/ClimNonClim/RB/ecotone/ecotone_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(LTSA_ecotone_occtrain, file=paste("./kfolds/ClimNonClim/RB/ecotone/ecotone_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
}


################################# ClimateOnly + TGB ################################

## Range study extent 

## Read input localities and tgb points with extracted envi data
  
LTSA_range_occ <- read.csv("./TGB/extracted/range/range_presdat.csv")
LTSA_range_tgb <- read.csv("./TGB/extracted/range/range_tgbdat.csv")
  
  
## creating kfolds (occ & tgb)
  
LTSA_range_fold <- kfold(LTSA_range_occ, k=5)
  
  
### For loop to run through multiple kfolds ###
  
x <- c(1,2,3,4,5)
  
for (i in 1:length(x)){
    
  ## creating testing and training sets (holding 20%)
    
  LTSA_range_occtest <- LTSA_range_occ[LTSA_range_fold == i, ]
  LTSA_range_occtrain <- LTSA_range_occ[LTSA_range_fold != i, ] 
    
  ## combining occ and tgb to be used in SWD mode ("full" means it has occurrences and tgb points)
    
  LTSA_range_fulltestingdat <- rbind(LTSA_range_occtest, LTSA_range_tgb)
  LTSA_range_fulltrainingdat <- rbind(LTSA_range_occtrain, LTSA_range_tgb)
    
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
    
  write.csv(LTSA_range_occtest, file=paste("./kfolds/ClimateOnly/TGB/range/LTSA_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(LTSA_range_occtrain, file=paste("./kfolds/ClimateOnly/TGB/range/LTSA_trainingdat",i,".csv",sep=""), row.names= FALSE)
    
  write.csv(LTSA_range_fulltestingdat, file=paste("./kfolds/ClimateOnly/TGB/range/LTSA_fulltestingdat",i,".csv", sep=""), row.names= FALSE)
  write.csv(LTSA_range_fulltrainingdat, file=paste("./kfolds/ClimateOnly/TGB/range/LTSA_fulltrainingdat",i,".csv", sep=""), row.names= FALSE)
    
}


## political study extent 

## Read input localities and tgb points with extracted envi data

LTSA_political_occ <- read.csv("./TGB/extracted/political/political_presdat.csv")
LTSA_political_tgb <- read.csv("./TGB/extracted/political/political_tgbdat.csv")


## creating kfolds (occ & tgb)

LTSA_political_fold <- kfold(LTSA_political_occ, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  LTSA_political_occtest <- LTSA_political_occ[LTSA_political_fold == i, ]
  LTSA_political_occtrain <- LTSA_political_occ[LTSA_political_fold != i, ] 
  
  ## combining occ and tgb to be used in SWD mode ("full" means it has occurrences and tgb points)
  
  LTSA_political_fulltestingdat <- rbind(LTSA_political_occtest, LTSA_political_tgb)
  LTSA_political_fulltrainingdat <- rbind(LTSA_political_occtrain, LTSA_political_tgb)
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(LTSA_political_occtest, file=paste("./kfolds/ClimateOnly/TGB/political/LTSA_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(LTSA_political_occtrain, file=paste("./kfolds/ClimateOnly/TGB/political/LTSA_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
  write.csv(LTSA_political_fulltestingdat, file=paste("./kfolds/ClimateOnly/TGB/political/LTSA_fulltestingdat",i,".csv", sep=""), row.names= FALSE)
  write.csv(LTSA_political_fulltrainingdat, file=paste("./kfolds/ClimateOnly/TGB/political/LTSA_fulltrainingdat",i,".csv", sep=""), row.names= FALSE)
  
}



## ecotone study extent 

## Read input localities and tgb points with extracted envi data

LTSA_ecotone_occ <- read.csv("./TGB/extracted/ecotone/ecotone_presdat.csv")
LTSA_ecotone_tgb <- read.csv("./TGB/extracted/ecotone/ecotone_tgbdat.csv")


## creating kfolds (occ & tgb)

LTSA_ecotone_fold <- kfold(LTSA_ecotone_occ, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  LTSA_ecotone_occtest <- LTSA_ecotone_occ[LTSA_ecotone_fold == i, ]
  LTSA_ecotone_occtrain <- LTSA_ecotone_occ[LTSA_ecotone_fold != i, ] 
  
  ## combining occ and tgb to be used in SWD mode ("full" means it has occurrences and tgb points)
  
  LTSA_ecotone_fulltestingdat <- rbind(LTSA_ecotone_occtest, LTSA_ecotone_tgb)
  LTSA_ecotone_fulltrainingdat <- rbind(LTSA_ecotone_occtrain, LTSA_ecotone_tgb)
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(LTSA_ecotone_occtest, file=paste("./kfolds/ClimateOnly/TGB/ecotone/LTSA_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(LTSA_ecotone_occtrain, file=paste("./kfolds/ClimateOnly/TGB/ecotone/LTSA_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
  write.csv(LTSA_ecotone_fulltestingdat, file=paste("./kfolds/ClimateOnly/TGB/ecotone/LTSA_fulltestingdat",i,".csv", sep=""), row.names= FALSE)
  write.csv(LTSA_ecotone_fulltrainingdat, file=paste("./kfolds/ClimateOnly/TGB/ecotone/LTSA_fulltrainingdat",i,".csv", sep=""), row.names= FALSE)
  
}


## genetic study extent 

## Read input localities and tgb points with extracted envi data

LTSA_genetic_occ <- read.csv("./TGB/extracted/genetic/genetic_presdat.csv")
LTSA_genetic_tgb <- read.csv("./TGB/extracted/genetic/genetic_tgbdat.csv")


## creating kfolds (occ & tgb)

LTSA_genetic_fold <- kfold(LTSA_genetic_occ, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  LTSA_genetic_occtest <- LTSA_genetic_occ[LTSA_genetic_fold == i, ]
  LTSA_genetic_occtrain <- LTSA_genetic_occ[LTSA_genetic_fold != i, ] 
  
  ## combining occ and tgb to be used in SWD mode ("full" means it has occurrences and tgb points)
  
  LTSA_genetic_fulltestingdat <- rbind(LTSA_genetic_occtest, LTSA_genetic_tgb)
  LTSA_genetic_fulltrainingdat <- rbind(LTSA_genetic_occtrain, LTSA_genetic_tgb)
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(LTSA_genetic_occtest, file=paste("./kfolds/ClimateOnly/TGB/genetic/LTSA_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(LTSA_genetic_occtrain, file=paste("./kfolds/ClimateOnly/TGB/genetic/LTSA_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
  write.csv(LTSA_genetic_fulltestingdat, file=paste("./kfolds/ClimateOnly/TGB/genetic/LTSA_fulltestingdat",i,".csv", sep=""), row.names= FALSE)
  write.csv(LTSA_genetic_fulltrainingdat, file=paste("./kfolds/ClimateOnly/TGB/genetic/LTSA_fulltrainingdat",i,".csv", sep=""), row.names= FALSE)
  
}





################################# ClimNonClim + TGB ################################

## Range study extent 

## Read input localities and tgb points with extracted envi data

LTSA_range_occ <- read.csv("./TGB/extracted/ClimNonClim/range/range_presdat.csv")
LTSA_range_tgb <- read.csv("./TGB/extracted/ClimNonClim/range/range_tgbdat.csv")
LTSA_range_tgb <- LTSA_range_tgb[,-1] #remove commoname
  

## creating kfolds (occ & tgb)

LTSA_range_fold <- kfold(LTSA_range_occ, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  LTSA_range_occtest <- LTSA_range_occ[LTSA_range_fold == i, ]
  LTSA_range_occtrain <- LTSA_range_occ[LTSA_range_fold != i, ] 
  
  ## combining occ and tgb to be used in SWD mode ("full" means it has occurrences and tgb points)
  
  LTSA_range_fulltestingdat <- rbind(LTSA_range_occtest, LTSA_range_tgb)
  LTSA_range_fulltrainingdat <- rbind(LTSA_range_occtrain, LTSA_range_tgb)
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(LTSA_range_occtest, file=paste("./kfolds/ClimNonClim/TGB/range/LTSA_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(LTSA_range_occtrain, file=paste("./kfolds/ClimNonClim/TGB/range/LTSA_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
  write.csv(LTSA_range_fulltestingdat, file=paste("./kfolds/ClimNonClim/TGB/range/LTSA_fulltestingdat",i,".csv", sep=""), row.names= FALSE)
  write.csv(LTSA_range_fulltrainingdat, file=paste("./kfolds/ClimNonClim/TGB/range/LTSA_fulltrainingdat",i,".csv", sep=""), row.names= FALSE)
  
}


## political study extent 

## Read input localities and tgb points with extracted envi data

LTSA_political_occ <- read.csv("./TGB/extracted/ClimNonClim/political/political_presdat.csv")
LTSA_political_tgb <- read.csv("./TGB/extracted/ClimNonClim/political/political_tgbdat.csv")
LTSA_political_tgb <- LTSA_political_tgb[,-1] #remove commoname

## creating kfolds (occ & tgb)

LTSA_political_fold <- kfold(LTSA_political_occ, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  LTSA_political_occtest <- LTSA_political_occ[LTSA_political_fold == i, ]
  LTSA_political_occtrain <- LTSA_political_occ[LTSA_political_fold != i, ] 
  
  ## combining occ and tgb to be used in SWD mode ("full" means it has occurrences and tgb points)
  
  LTSA_political_fulltestingdat <- rbind(LTSA_political_occtest, LTSA_political_tgb)
  LTSA_political_fulltrainingdat <- rbind(LTSA_political_occtrain, LTSA_political_tgb)
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(LTSA_political_occtest, file=paste("./kfolds/ClimNonClim/TGB/political/LTSA_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(LTSA_political_occtrain, file=paste("./kfolds/ClimNonClim/TGB/political/LTSA_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
  write.csv(LTSA_political_fulltestingdat, file=paste("./kfolds/ClimNonClim/TGB/political/LTSA_fulltestingdat",i,".csv", sep=""), row.names= FALSE)
  write.csv(LTSA_political_fulltrainingdat, file=paste("./kfolds/ClimNonClim/TGB/political/LTSA_fulltrainingdat",i,".csv", sep=""), row.names= FALSE)
  
}



## ecotone study extent 

## Read input localities and tgb points with extracted envi data

LTSA_ecotone_occ <- read.csv("./TGB/extracted/ClimNonClim/ecotone/ecotone_presdat.csv")
LTSA_ecotone_tgb <- read.csv("./TGB/extracted/ClimNonClim/ecotone/ecotone_tgbdat.csv")
LTSA_ecotone_tgb <- LTSA_ecotone_tgb[,-1] #remove commoname

## creating kfolds (occ & tgb)

LTSA_ecotone_fold <- kfold(LTSA_ecotone_occ, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  LTSA_ecotone_occtest <- LTSA_ecotone_occ[LTSA_ecotone_fold == i, ]
  LTSA_ecotone_occtrain <- LTSA_ecotone_occ[LTSA_ecotone_fold != i, ] 
  
  ## combining occ and tgb to be used in SWD mode ("full" means it has occurrences and tgb points)
  
  LTSA_ecotone_fulltestingdat <- rbind(LTSA_ecotone_occtest, LTSA_ecotone_tgb)
  LTSA_ecotone_fulltrainingdat <- rbind(LTSA_ecotone_occtrain, LTSA_ecotone_tgb)
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(LTSA_ecotone_occtest, file=paste("./kfolds/ClimNonClim/TGB/ecotone/LTSA_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(LTSA_ecotone_occtrain, file=paste("./kfolds/ClimNonClim/TGB/ecotone/LTSA_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
  write.csv(LTSA_ecotone_fulltestingdat, file=paste("./kfolds/ClimNonClim/TGB/ecotone/LTSA_fulltestingdat",i,".csv", sep=""), row.names= FALSE)
  write.csv(LTSA_ecotone_fulltrainingdat, file=paste("./kfolds/ClimNonClim/TGB/ecotone/LTSA_fulltrainingdat",i,".csv", sep=""), row.names= FALSE)
  
}


## genetic study extent 

## Read input localities and tgb points with extracted envi data

LTSA_genetic_occ <- read.csv("./TGB/extracted/ClimNonClim/genetic/genetic_presdat.csv")
LTSA_genetic_tgb <- read.csv("./TGB/extracted/ClimNonClim/genetic/genetic_tgbdat.csv")
LTSA_genetic_tgb <- LTSA_genetic_tgb[,-1] #remove commoname

## creating kfolds (occ & tgb)

LTSA_genetic_fold <- kfold(LTSA_genetic_occ, k=5)


### For loop to run through multiple kfolds ###

x <- c(1,2,3,4,5)

for (i in 1:length(x)){
  
  ## creating testing and training sets (holding 20%)
  
  LTSA_genetic_occtest <- LTSA_genetic_occ[LTSA_genetic_fold == i, ]
  LTSA_genetic_occtrain <- LTSA_genetic_occ[LTSA_genetic_fold != i, ] 
  
  ## combining occ and tgb to be used in SWD mode ("full" means it has occurrences and tgb points)
  
  LTSA_genetic_fulltestingdat <- rbind(LTSA_genetic_occtest, LTSA_genetic_tgb)
  LTSA_genetic_fulltrainingdat <- rbind(LTSA_genetic_occtrain, LTSA_genetic_tgb)
  
  ## saving csvs (species_background_occtest is for random background points fulltestingdat is for SWD mode with tgb)
  
  write.csv(LTSA_genetic_occtest, file=paste("./kfolds/ClimNonClim/TGB/genetic/LTSA_testingdat",i,".csv",sep=""), row.names= FALSE)
  write.csv(LTSA_genetic_occtrain, file=paste("./kfolds/ClimNonClim/TGB/genetic/LTSA_trainingdat",i,".csv",sep=""), row.names= FALSE)
  
  write.csv(LTSA_genetic_fulltestingdat, file=paste("./kfolds/ClimNonClim/TGB/genetic/LTSA_fulltestingdat",i,".csv", sep=""), row.names= FALSE)
  write.csv(LTSA_genetic_fulltrainingdat, file=paste("./kfolds/ClimNonClim/TGB/genetic/LTSA_fulltrainingdat",i,".csv", sep=""), row.names= FALSE)
  
}


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################

### Next steps
# run maxent using the fulltrainingdat1
# then run evaluate function using the resulting model and the fulltestingdat1
# repeat all steps above, creating a new "fulltesting" and "fulltraining" by setting the fold== and fold != values above to 2, then 3, then 4...etc.


########################### END SCRIPT ###############################
