rm(list = ls())
library(tidyverse)
library(sf)
library(ggplot2)
#library(readr)
#library(lettercase)
library(viridis)
library(rvest)
library(ggrepel)
library(RColorBrewer)


filePath <- '/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/modMET_csv'
mapPath <- '/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/maps'
dataFile <- paste0(filePath, "/", "percentiles.txt")

all_df <- read_csv(dataFile)
str(all_df)


noFrost_df <- all_df %>% 
    select(Long, Lat, LFrost_90P) %>% 
    filter(LFrost_90P <= 182)

Frost_df <- all_df %>% 
    select(Long, Lat, LFrost_90P) %>% 
    filter(LFrost_90P > 182)

#now need to graph the LFrost_90P
# 1. Separate tout those sites with value of 182, and graph them as gray
# 2. graph the remaing values with colour scales from purple (as 183) to yellow (as 365)


# Retrieve the Region Shape File
source_shapefile_dir <- '/OSM/CBR/AG_WHEATTEMP/work/GIS_data'
regionfile <- paste0(source_shapefile_dir, "/wheat_22regions_KC/wheat_22regions_KC.shp")
print(paste0("region file: ", regionfile))

regions_mapped <- st_read(regionfile, quiet = TRUE)
as.tibble(regions_mapped)

# Retrieve the Australia shape file
australiafile <- paste0(source_shapefile_dir, "/states/aust_cd66states.shp")
aus_mapped <- st_read(australiafile, quiet = TRUE)
as.tibble(aus_mapped)

# need to do this as there is no CRS information with the AUS shape files
st_crs(aus_mapped) <- st_crs(regions_mapped)

# get the coordintaes and format as SpatialPoints
coords <- select(noFrost_df, Long, Lat)

# Create sf object with all_df data frame and CRS
noFrost_points_sf <- st_as_sf(noFrost_df, coords = c("Long", "Lat"), crs = 4326)
st_geometry(noFrost_points_sf)

# need to do this as there is no CRS information with the AUS shape files
st_crs(noFrost_points_sf) <- st_crs(regions_mapped)


#this just shows map of Australia
ggplot() + 
    geom_sf(data=aus_mapped) +
    coord_sf(xlim=c(112, 156), ylim=c(-10, -45)) +
    labs(x="long", y="lat") +
    theme_bw()

#this shows the points on the map, with colour scale for value required (LFrost_90P)
ggplot() + 
    geom_sf(data=noFrost_points_sf) +
    coord_sf(xlim=c(112, 156), ylim=c(-10, -45)) +
    labs(x="long", y="lat") +
    theme_bw()

ggplot() + 
    geom_sf(data=aus_mapped) +
    geom_sf(data=noFrost_points_sf, colour="grey", alpha=0.7, show.legend = "point") +
    coord_sf(xlim=c(112, 156), ylim=c(-10, -45)) +
    labs(x="long", y="lat") +
    theme_bw()


#now prepare the Frost_df
# get the coordintaes and format as SpatialPoints
coords <- select(Frost_df, Long, Lat)

# Create sf object with all_df data frame and CRS
Frost_points_sf <- st_as_sf(Frost_df, coords = c("Long", "Lat"), crs = 4326)
st_geometry(Frost_points_sf)

# need to do this as there is no CRS information with the AUS shape files
st_crs(Frost_points_sf) <- st_crs(regions_mapped)


colPalette <- colorRampPalette(rev(brewer.pal(9, "YlGnBu")))

ggplot() + 
    geom_sf(data=aus_mapped) +
    geom_sf(data=Frost_points_sf, aes(colour=LFrost_90P), alpha=0.7, show.legend = "point") +
    scale_colour_gradientn(colours=colPalette(10), limits=c(183, 365)) +
    coord_sf(xlim=c(112, 156), ylim=c(-10, -45)) +
    theme_bw()

outfile <- paste0(mapPath, "/LFrost_90P.png")
ggsave(outfile, width=7, height=6, units="in")

#need to add the noFrost data to this, change the shape of the chart, compare to Bangyou's.


