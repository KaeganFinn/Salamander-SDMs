########################## GOALS AND NOTES ###########################

### Project: Salamdander SDMs

### Author: Kaegan Finn

### Goal of this Script: 

# Check if independent survey sites span the range of environmental spce using pca

### Date: April 13, 2023

### Version of R:  R version 4.2.1

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(dismo)
library(sf)
library(terra)
library(rgdal)
library(dplyr)
library(rJava)
library(ggplot2)
library(ggmosaic)
  
  
rm(list=ls())
########################### END SECTION ##############################

## Set working directory

getwd()
setwd("/Users/kaegan/Dropbox/Mac/Desktop/_/MSc/Thesis/Models")

################################# 

#Get data 

dat <- read.csv("./Independent_data/stratification/pca_dat.csv")
dat <- na.omit(dat)
str(dat)


#PCA of all (8) continuous variables

## This code is from Josh Starmer of StatQuest: https://github.com/StatQuest/pca_demo/blob/master/pca_demo.R 

# Subsetting into just Type and env var values 
pca_dat  <- subset(dat, select = -c(1:22,31))
meta_dat  <- subset(dat, select = c(1:6))

# making the PCA
pca <- prcomp((pca_dat), scale = TRUE)

## plot pc1 and pc2
plot(pca$x[,1], pca$x[,2])

## make a scree plot
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)

barplot(pca.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")

## Extract PC xy coords
pca.data <- data.frame(Sample=rownames(pca$x),
                       X=pca$x[,1],
                       Y=pca$x[,2])


# bind metadata back onto 
pca_dat <- cbind(meta_dat,pca.data)
str(pca_dat)

pca_dat <- transform(pca_dat,Point.Type = as.factor(Point.Type))

ggplot(pca_dat, aes(x=X, y=Y, color = Point.Type)) +
  geom_point() +  
  scale_color_manual(values = c("grey","#00BFC3","#FF7670"))+
  theme_bw() +
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

## Check Landcover stratifcation 
lc_dat  <- subset(dat, select = -c(7:30))
str(lc_dat)
lc_dat <- transform(lc_dat,Landcover = as.factor(Landcover))
lc_dat <- transform(lc_dat,Point.Type = as.factor(Point.Type))


##MOSAIC PLOT
ggplot(data = lc_dat) +
  geom_mosaic(aes(x=product(Landcover), fill = Landcover, 
                  conds = product(Point.Type)))


########################### END SECTION ##############################

########################## FINAL COMMENTS ############################

########################### END SCRIPT ###############################









