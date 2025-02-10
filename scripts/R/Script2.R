# Script for step 2
# Paulina Harder p.harder@tu-berlin.de
# In this script the previously downloaded NDVI data is preprocessed 
# and the mean NDVI per day is calculated. The climate and soil 
# moisture and evaporation data from DWD is downloaded and a dataframe 
# of all the data is created and downloaded 
setwd("~/scripts/R")
source("funcs_for_data.r")
# packages 
library(raster)
library(rasterVis)
library(sf) 
library(stars)
library(sp)
library(terra)
library(ggplot2)
library(dplyr)

# Select your years
years <- 2017:2024
# the following function:
# crops the NDVI & QFLAG data 
# filters the NDVI with the QFLAG
# calculates the mean NDVI per day
# takes some minutes so take a little break
# Thf_feld.shp is the shape of my study area 
NDVI <- process_years(years, "Thf_feld.shp")

# load climate data 
clim<-load_climate("Berlin-Tempelhof")

# load data responsible for plant growth soil moisture etc.
growth<-load_soil_moisture("Berlin-Tempelhof")

# join all the data 
CG<-left_join(growth, clim)
CG_NDVI<-left_join(CG, NDVI)
# we want to filter the data as we do not look at the winter period
CG_NDVI <- CG_NDVI %>%
  filter(format(Datum, "%m-%d") >= "04-01" & format(Datum, "%m-%d") <= "11-15")

write.csv(CG_NDVI,"../../output_files/CG_NDVI.csv", sep=";",row.names = F)




