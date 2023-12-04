########################## GOALS AND NOTES ###########################

### Project: Kaegan Finn MSc, Lit Revieew

### Author: Kaegan Finn
### Goal of this Script: 

# Explore results of Lit Review using various ggplots

### Notes:  

### Date: May 1, 2023

### Version of R:  R version 4.2.1

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(ggplot2)
library(ggpubr)
library(ggmosaic)
library(tidyverse)
library(rvg)

rm(list=ls())
########################### END SECTION ##############################

###################### FUNCTION DEFINITIONS ##########################

## NA

########################### END SECTION ##############################

########################## DATA AND ANALYSIS #########################

## Set working directory

getwd()
setwd()

################################# Get data ################################
dat <- read.csv("./TGB/model_subsets/TGB_range.csv")

# Keep only categ vars + LTSA pres abs

## change vars into correct forms
str(dat)
#As.Factor
dat <- transform(dat,Scientific = as.factor(Scientific))
dat <- transform(dat,CommonName = as.factor(CommonName))
str(dat)

# Count per scientific name
Scientific_count <-data.frame(table(dat$Scientific))%>% 
  rename(
    Scientific = Var1,
    Count = Freq)

write.csv(Scientific_count,"./TGB/model_subsets/TGB_SpeciesCount.csv")
