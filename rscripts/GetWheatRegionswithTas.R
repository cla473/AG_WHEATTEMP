#!/usr/bin/env Rscript

#----------------------------------------------------------------------------------
# Generates a graph showing the values specified as a heat map on the map of Australia
#----------------------------------------------------------------------------------
# Sample Usage:
#     Rscript Process_AvgTemp.R -y 2014 
#----------------------------------------------------------------------------------
#rm(list =ls())
#library("optparse")
library("tidyverse")
library("sf")
#library("viridis")
#library("rvest")
#library("ggplot2")
#library("RColorBrewer")

source_dir <- "/OSM/CBR/AG_WHEATTEMP/work/ApsimNG-test/APSIM_run/apsimx"
output_dir <- "/OSM/CBR/AG_WHEATTEMP/work/ApsimNG/"

filenames <- list.files(source_dir)
filesDF <- as.data.frame(filenames) 
head(filesDF)
filesDF$long <- as.numeric(substring(filesDF$filenames, 1, 6))
filesDF$lat <- as.numeric(substring(filesDF$filenames, 7, 12))
head(filesDF)
min(filesDF$lat)
max(filesDF$lat)

TasDF <- filesDF[filesDF$lat <= -40,]
min(TasDF$long)

year <- 2009
source_data_dir <- "/OSM/CBR/AG_WHEATTEMP/work/output/grainfilling/"
source_shapefile_dir <- "/OSM/CBR/AG_WHEATTEMP/work/GIS_data/"

datafile <- paste0(source_data_dir, "07_GrainFilling_", year, ".csv")
all_df <- read_csv(datafile)

all_df <- all_df %>% 
    select (long, lat) %>% 
    distinct()

missing <- anti_join(filesDF, all_df)

#Retrieve the Region Shape File
regionfile <- paste0(source_shapefile_dir, "wheat_22regions_KC.shp")
print(paste0("region file: ", regionfile))
regions_mapped <- st_read(regionfile, quiet = TRUE)
as.tibble(regions_mapped)

# Create sf object with all_df data frame and CRS
points_sf <- st_as_sf(all_df, coords = c("long", "lat"), crs = 4326)
mapRegionCoords <- do.call(rbind, st_geometry(points_sf)) %>% 
                               as_tibble() %>% 
                               setNames(c("long", "lat")) %>% 
    mutate(inRegion = "Y")

