########################## GOALS AND NOTES ###########################

### Project: Amphibian SDMs

### Author: Jayna Bergman 

### Goal of this Script: 

# This script reprojects shapefiles (study extents) into AEA projections

### Notes:  

# defined crs is in North American Albers Equal Area projection
# Study extents were originally downloaded from IUNC (in WGS 84 projection) and a 5km buffer was put on in ArcGIS. 

### Date: May 13, 2022

### Version of R:  R version 4.0.3

########################### END SECTION ##############################

############################ LIBRARIES ###############################

library(sf)

rm(list=ls())
########################### END SECTION ##############################

###################### FUNCTION DEFINITIONS ##########################

## NA

########################### END SECTION ##############################

########################## DATA AND ANALYSIS #########################

## Set working directory

getwd()
setwd()


## Defining projection - Using North America Albers Equal Area Conic projection (aea)
## Read in a shapefile with desired projection in order to have proper "PROJCS" name - if the string is written out "PROJCS" is unknown (this will still plot properly)

#"+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"


crs_aea <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

aea_shp <- st_read("./Environmental_Variables/Range/ClimateNonClimate/MAP.tif")

aea = st_crs(aea_shp) 

LTSA_range <- st_read("./Study_extents/Study_extents_aea/range_aea.shp")


################################# LTSA ################################

## Read in study extent shapefiles 

LTSA_range <- st_read("./Study_extents/Study_extents/range.shp")
LTSA_ecotone <- st_read("./Study_extents/Study_extents/ecotone.shp")
LTSA_genetic <- st_read("./Study_extents/Study_extents/genetic.shp")
LTSA_political <- st_read("./Study_extents/Study_extents/political.shp")


## reproject study extent shapefiles to aea projection (previously defined variable "aea")

LTSA_range_aea <- st_transform(LTSA_range, crs = crs)
plot(st_geometry(LTSA_range_aea))

LTSA_ecotone_aea <- st_transform(LTSA_ecotone, crs = crs)
plot(st_geometry(LTSA_ecotone_aea))

LTSA_genetic_aea <- st_transform(LTSA_genetic, crs = crs)
plot(st_geometry(LTSA_genetic_aea))

LTSA_political_aea <- st_transform(LTSA_political, crs = crs)
plot(st_geometry(LTSA_political_aea))

plot(st_geometry(LTSA_range_aea))
plot(st_geometry(LTSA_ecotone_aea), add = TRUE)
plot(st_geometry(LTSA_political_aea), add = TRUE)
plot(st_geometry(LTSA_genetic), add = TRUE)


## Save study extent shapefiles with aea projection

st_write(LTSA_range_aea, "./Study_extents/Study_extents_aea/range_aea.shp")
st_write(LTSA_ecotone_aea, "./Study_extents/Study_extents_aea/ecotone_aea.shp")
st_write(LTSA_genetic_aea, "./Study_extents/Study_extents_aea/genetic_aea.shp")
st_write(LTSA_political_aea, "./Study_extents/Study_extents_aea/political_aea.shp")


########################### END SECTION ###############################

########################## FINAL COMMENTS ############################

########################### END SCRIPT ###############################
