########################## GOALS AND NOTES ###########################

### Project: Kaegan Finn MSc. Thesis: Long-toed salamander surveys

### Author: Kaegan Finn

### Goal of this Script: 

# test model calibration of LTSA presence as a function of SDM predicitons for all models. 
# Test the significance of this relationship (Gogol-Prokurat 2011).

### Notes:  

### Date: Jan 17, 2023

### Version of R:  R version 4.2.1

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(ggpubr)
library(ggplot2)
library(terra)
library(raster)
library(sf)
library(dplyr)
library(broom)
library(car)
library(visreg)


rm(list=ls())
########################### END SECTION ##############################

########################## DATA AND ANALYSIS #########################

## Set working directory

getwd()
setwd()

## read in csv 
predictions <-  read.csv("./predictions.csv")

####  MODELS #####

# ClimOnly + RB
ClimOnly_RB_Range_glm <- glm(LTSA ~ ClimOnly_RB_Range_SDM, family = binomial, data = predictions)
ClimOnly_RB_pol_glm <- glm(LTSA ~ ClimOnly_RB_pol_SDM, family = binomial, data = predictions)
ClimOnly_RB_eco_glm <- glm(LTSA ~ ClimOnly_RB_eco_SDM, family = binomial, data = predictions)
ClimOnly_RB_gen_glm <- glm(LTSA ~ ClimOnly_RB_gen_SDM, family = binomial, data = predictions)
# ClimNonClim + RB
ClimNonClim_RB_Range_glm <- glm(LTSA ~ ClimNonClim_RB_Range_SDM, family = binomial, data = predictions)
ClimNonClim_RB_pol_glm <- glm(LTSA ~ ClimNonClim_RB_pol_SDM, family = binomial, data = predictions)
ClimNonClim_RB_eco_glm <- glm(LTSA ~ ClimNonClim_RB_eco_SDM, family = binomial, data = predictions)
ClimNonClim_RB_gen_glm <- glm(LTSA ~ ClimNonClim_RB_gen_SDM, family = binomial, data = predictions)
# ClimOnly + TGB
ClimOnly_TGB_Range_glm <- glm(LTSA ~ ClimOnly_TGB_Range_SDM, family = binomial, data = predictions)
ClimOnly_TGB_pol_glm <- glm(LTSA ~ ClimOnly_TGB_pol_SDM, family = binomial,  data = predictions)
ClimOnly_TGB_eco_glm <- glm(LTSA ~ ClimOnly_TGB_eco_SDM, family = binomial, data = predictions)
ClimOnly_TGB_gen_glm <- glm(LTSA ~ ClimOnly_TGB_gen_SDM, family = binomial, data = predictions)
# ClimNonClim + TGB
ClimNonClim_TGB_Range_glm <- glm(LTSA ~ ClimNonClim_TGB_Range_SDM, family = binomial, data = predictions)
ClimNonClim_TGB_pol_glm <- glm(LTSA ~ ClimNonClim_TGB_pol_SDM, family = binomial, data = predictions)
ClimNonClim_TGB_eco_glm <- glm(LTSA ~ ClimNonClim_TGB_eco_SDM, family = binomial, data = predictions)
ClimNonClim_TGB_gen_glm <- glm(LTSA ~ ClimNonClim_TGB_gen_SDM, family = binomial, data = predictions)

#Sumarrize (get p-values)

# ClimOnly + RB
summary(ClimOnly_RB_Range_glm)
summary(ClimOnly_RB_pol_glm)
summary(ClimOnly_RB_eco_glm)
summary(ClimOnly_RB_gen_glm)

# ClimNonClim + RB
summary(ClimNonClim_RB_Range_glm)
summary(ClimNonClim_RB_pol_glm)
summary(ClimNonClim_RB_eco_glm)
summary(ClimNonClim_RB_gen_glm)

# ClimNonClim + TGB
summary(ClimNonClim_TGB_Range_glm)
summary(ClimNonClim_TGB_pol_glm)
summary(ClimNonClim_TGB_eco_glm)
summary(ClimNonClim_TGB_gen_glm)

# ClimOnly + TGB
summary(ClimOnly_TGB_Range_glm)
summary(ClimOnly_TGB_pol_glm)
summary(ClimOnly_TGB_eco_glm)
summary(ClimOnly_TGB_gen_glm)


# Percent Deviance explained 

100*with(summary(ClimNonClim_RB_pol_glm), 1 - deviance/null.deviance)
100*with(summary(ClimOnly_RB_pol_glm), 1 - deviance/null.deviance)
100*with(summary(ClimNonClim_RB_Range_glm), 1 - deviance/null.deviance)
100*with(summary(ClimOnly_RB_eco_glm), 1 - deviance/null.deviance)
100*with(summary(ClimOnly_RB_Range_glm), 1 - deviance/null.deviance)
100*with(summary(ClimNonClim_TGB_pol_glm), 1 - deviance/null.deviance)
100*with(summary(ClimNonClim_RB_eco_glm), 1 - deviance/null.deviance)
100*with(summary(ClimOnly_TGB_pol_glm), 1 - deviance/null.deviance)
100*with(summary(ClimOnly_RB_gen_glm), 1 - deviance/null.deviance)
100*with(summary(ClimNonClim_RB_gen_glm), 1 - deviance/null.deviance)
100*with(summary(ClimNonClim_TGB_Range_glm), 1 - deviance/null.deviance)
100*with(summary(ClimOnly_TGB_eco_glm), 1 - deviance/null.deviance)
100*with(summary(ClimNonClim_TGB_eco_glm), 1 - deviance/null.deviance)
100*with(summary(ClimOnly_TGB_gen_glm), 1 - deviance/null.deviance)
100*with(summary(ClimOnly_TGB_Range_glm), 1 - deviance/null.deviance)
100*with(summary(ClimNonClim_TGB_gen_glm), 1 - deviance/null.deviance)

#Plot them (get p-values)

# ClimOnly + RB
visreg(ClimOnly_RB_Range_glm, type="conditional", scale = "response", ylab = "Probability of Presence")
visreg(ClimOnly_RB_pol_glm, type="conditional", scale = "response",ylab = "Probability of Presence")
visreg(ClimOnly_RB_eco_glm, type="conditional", scale = "response",ylab = "Probability of Presence")
visreg(ClimOnly_RB_gen_glm, type="conditional", scale = "response",ylab = "Probability of Presence")

# ClimNonClim + RB
visreg(ClimNonClim_RB_Range_glm, type="conditional", scale = "response",ylab = "Probability of Presence")
visreg(ClimNonClim_RB_pol_glm, type="conditional", scale = "response",ylab = "Probability of Presence")
visreg(ClimNonClim_RB_eco_glm,type="conditional", scale = "response", ylab = "Probability of Presence")
visreg(ClimNonClim_RB_gen_glm,type="conditional", scale = "response", ylab = "Probability of Presence")

# ClimOnly + TGB
visreg(ClimOnly_TGB_Range_glm, type="conditional", scale = "response",ylab = "Probability of Presence")
visreg(ClimOnly_TGB_pol_glm, type="conditional", scale = "response",ylab = "Probability of Presence")
visreg(ClimOnly_TGB_eco_glm, type="conditional", scale = "response",ylab = "Probability of Presence")
visreg(ClimOnly_TGB_gen_glm,type="conditional", scale = "response", ylab = "Probability of Presence")

# ClimNonClim + TGB
visreg(ClimNonClim_TGB_Range_glm,type="conditional", scale = "response", ylab = "Probability of Presence")
visreg(ClimNonClim_TGB_pol_glm,type="conditional", scale = "response", ylab = "Probability of Presence")
visreg(ClimNonClim_TGB_eco_glm,type="conditional", scale = "response", ylab = "Probability of Presence")
visreg(ClimNonClim_TGB_gen_glm,type="conditional", scale = "response", ylab = "Probability of Presence")


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################

########################### END SCRIPT ###############################