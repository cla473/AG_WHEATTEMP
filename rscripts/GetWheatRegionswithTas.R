#!/usr/bin/env Rscript

#----------------------------------------------------------------------------------
# Generates a graph showing the values specified as a heat map on the map of Australia
#----------------------------------------------------------------------------------
# Sample Usage:
#     Rscript Process_AvgTemp.R -y 2014 
#----------------------------------------------------------------------------------
rm(list =ls())
library("tidyverse")
library("sf")
#library("viridis")
#library("rvest")
library("ggplot2")
#library("RColorBrewer")

#----------------------------------------------------------------------
# Define the file locations and other variables
#----------------------------------------------------------------------
source_dir <- "/OSM/CBR/AG_WHEATTEMP/work/ApsimNG-test/APSIM_run/apsimx"
output_dir <- "/OSM/CBR/AG_WHEATTEMP/work/ApsimNG/"
source_data_dir <- "/OSM/CBR/AG_WHEATTEMP/work/output/grainfilling/"
source_shapefile_dir <- "/OSM/CBR/AG_WHEATTEMP/work/GIS_data/"
source_shapefile_dir_new <- "/OSM/CBR/AG_WHEATTEMP/work/GIS_data/GRDC_Regions NEW_region/"
source_shapefile_dir_SA2 <- "/OSM/CBR/AG_WHEATTEMP/work/GIS_data/Subregions_provisional_2015-11-03/"
year <- 2009

#----------------------------------------------------------------------
# Retrieve the shape files
#----------------------------------------------------------------------
#Retrieve the Region Shape File
regionfile <- paste0(source_shapefile_dir, "wheat_22regions_KC.shp")
print(paste0("region file: ", regionfile))
regions_mapped <- st_read(regionfile, quiet = TRUE)
as.tibble(regions_mapped)

#Retrieve the Region Shape File
newRegionfile <- paste0(source_shapefile_dir_new, "GRDC_Regions NEW_region.shp")
print(paste0("region file: ", newRegionfile))
file.exists(newRegionfile)
newRegions_mapped <- st_read(newRegionfile, quiet = TRUE)
as.tibble(newRegions_mapped)

#these are the provisional GRDC (SA2) regions (Prepared by David Gobbett 2015)
SA2Regionfile <- paste0(source_shapefile_dir_SA2, "Draft_GRDC_Subregions_2015.shp")
print(paste0("region file: ", SA2Regionfile))
file.exists(SA2Regionfile)
SA2Regions_mapped <- st_read(SA2Regionfile, quiet = TRUE)
as.tibble(SA2Regions_mapped)
str(SA2Regions_mapped)

#Retrieve the Australia shape file
australiafile <- paste0(source_shapefile_dir, "states/aust_cd66states.shp")
aus_mapped <- st_read(australiafile, quiet = TRUE)
as.tibble(aus_mapped)
st_crs(aus_mapped) <- st_crs(regions_mapped)

#----------------------------------------------------------------------
# Get the full list of Points
#----------------------------------------------------------------------
filenames <- list.files(source_dir)
allLocationsDF <- as.data.frame(filenames) 
head(allLocationsDF)
allLocationsDF$long <- as.numeric(substring(allLocationsDF$filenames, 1, 6))
allLocationsDF$lat <- as.numeric(substring(allLocationsDF$filenames, 7, 12))
head(allLocationsDF)
#----------------------------------------------------------------------

#now lets look at the file with validation simulation data
datafile <- paste0(source_data_dir, "07_GrainFilling_", year, ".csv")
allSims_df <- read_csv(datafile)
allSims_df$Status = "HasSimualtion"

allSims_df <- allSims_df %>% 
    select (long, lat, Status) %>% 
    distinct()


allLocsDF <- left_join(allLocationsDF, allSims_df, by=c("long", "lat"))
allLocsDF$Status[is.na(allLocsDF$Status)] <- "NoCrop"
unique(allLocsDF$Status)
table(allLocsDF$Status)

#add this for use later
allLocsDF$InRegion <- "False"

#now show where the points are
allLocsDF_points_sf <- st_as_sf(allLocsDF, coords = c("long", "lat"), crs = 4326)
st_geometry(allLocsDF_points_sf)
st_crs(allLocsDF_points_sf) <- st_crs(regions_mapped)

ggplot() + 
    geom_sf(data = aus_mapped) +
    geom_sf(data = allLocsDF_points_sf, aes(colour=Status), alpha=0.7, show.legend = "point") +
    coord_sf(xlim=c(112, 156), ylim=c(-10, -45)) +
    labs(x="long", y="lat", title="Crop Status") +
    theme_bw()

outfile <- paste0(output_dir, "map_no_crops", year, ".png")
print(outfile)
ggsave(outfile, width=7, height=6, units="in")


#------------------------------------------------------------------
#this will look at ONLY the missing crops
noCrops <- anti_join(allLocationsDF, allSims_df)
head(noCrops)

#plot the locations that did not get output files (crops didn't grow)
noCrops$noCrop <- "Y"
noCrops_points_sf <- st_as_sf(noCrops, coords = c("long", "lat"), crs = 4326)

ggplot() + 
    geom_sf(data = aus_mapped) +
    geom_sf(data = noCrops_points_sf, aes(colour=noCrop), alpha=0.7, show.legend = "point") +
    coord_sf(xlim=c(112, 156), ylim=c(-10, -45)) +
    labs(x="long", y="lat", title="Where crops didn't grow") +
    theme_bw()

outfile <- paste0(output_dir, "map_no_crops", year, ".png")
print(outfile)
ggsave(outfile, width=7, height=6, units="in")
#------------------------------------------------------------------

# get the coordintaes and format as SpatialPoints and create sf object with all_df data frame and CRS
#coords <- select(allSims_df, long, lat)
#allSims_points_sf <- st_as_sf(allSims_df, coords = c("long", "lat"), crs = 4326)
#st_geometry(allLocsDF_points_sf)
#st_crs(allSims_points_sf) <- st_crs(regions_mapped)

#Need to find out what points are eliminated when we apply the regions
pcheck <- allLocsDF_points_sf$geometry %>% 
    st_intersects(regions_mapped$geometry) %>% 
    map_lgl(function(x) length(x) > 0)

pcheck <- as.data.frame(pcheck)
allLocsDF_points_sf$Regions <- pcheck
allLocsDF_points_sf$InRegion[allLocsDF_points_sf$Regions == TRUE] <- "True"
table(allLocsDF_points_sf$InRegion)

ggplot() + 
    geom_sf(data = aus_mapped) +
    geom_sf(data = allLocsDF_points_sf, aes(colour=InRegion), alpha=0.7, show.legend = "point") +
    coord_sf(xlim=c(112, 156), ylim=c(-10, -45)) +
    labs(x="long", y="lat", title="Points being excluded due to Region mapping") +
    theme_bw()

outfile <- paste0(output_dir, "map_InRegion", year, ".png")
print(outfile)
ggsave(outfile, width=7, height=6, units="in")



#--------------------------------------------------------------------------------
# What do the NEW region files include
#--------------------------------------------------------------------------------
ggplot() + 
    geom_sf(data = aus_mapped) +
    geom_sf(data = newRegions_mapped) +
    geom_polygon(data = newRegions_mapped, aes(x=long, y=lat, group=group)) +
    coord_sf(xlim=c(112, 155), ylim=c(-10, -45)) +
    ggtitle("New GRDC Regions in Australia")

outfile <- paste0(output_dir, "map_NewRegions_InAus", year, ".png")
print(outfile)
ggsave(outfile, width=7, height=6, units="in")



#------------------------------------------------------------------
# Now show the points in the New Regions
#------------------------------------------------------------------
#Need to find out what points are eliminated when we apply the regions
pcheck <- allLocsDF_points_sf$geometry %>% 
    st_intersects(newRegions_mapped$geometry) %>% 
    map_lgl(function(x) length(x) > 0)

pcheck <- as.data.frame(pcheck)
allLocsDF_points_sf$newRegions <- pcheck
allLocsDF_points_sf$InNewRegions <- "False"
allLocsDF_points_sf$InNewRegions[allLocsDF_points_sf$newRegions == TRUE] <- "True"
table(allLocsDF_points_sf$InNewRegions)

allLocsDF_points_sf$InNewRegions <- as.factor(allLocsDF_points_sf$InNewRegions)
print(levels(allLocsDF_points_sf$InNewRegions))
allLocsDF_points_sf$InNewRegions <- relevel(allLocsDF_points_sf$InNewRegions, "True")

ggplot() + 
    geom_sf(data = aus_mapped) +
    geom_sf(data = allLocsDF_points_sf, aes(colour=InNewRegions), alpha=0.7, show.legend = "point") +
    coord_sf(xlim=c(112, 156), ylim=c(-10, -45)) +
    labs(x="long", y="lat", title="Points being excluded due to NEW Region mapping") +
    theme_bw()

outfile <- paste0(output_dir, "map_InNewRegions", year, ".png")
print(outfile)
ggsave(outfile, width=7, height=6, units="in")


#------------------------------------------------------------------
# Now show the points in the New Provisional GRDC (SA2) Regions
#------------------------------------------------------------------
#Need to find out what points are eliminated when we apply the regions
pcheck <- allLocsDF_points_sf$geometry %>% 
    st_intersects(SA2Regions_mapped$geometry) %>% 
    map_lgl(function(x) length(x) > 0)

pcheck <- as.data.frame(pcheck)
allLocsDF_points_sf$SA2Regions <- pcheck
allLocsDF_points_sf$InSA2Regions <- "False"
allLocsDF_points_sf$InSA2Regions[allLocsDF_points_sf$SA2Regions == TRUE] <- "True"
table(allLocsDF_points_sf$InSA2Regions)

allLocsDF_points_sf$InSA2Regions <- as.factor(allLocsDF_points_sf$InSA2Regions)
print(levels(allLocsDF_points_sf$InSA2Regions))
allLocsDF_points_sf$InSA2Regions <- relevel(allLocsDF_points_sf$InSA2Regions, "True")
allLocsDF_points_sf$InSA2Regions <- relevel(allLocsDF_points_sf$InSA2Regions, "False")

ggplot() + 
    geom_sf(data = aus_mapped) +
    geom_sf(data = allLocsDF_points_sf, aes(colour=InSA2Regions), alpha=0.7, show.legend = "point") +
    coord_sf(xlim=c(112, 156), ylim=c(-10, -45)) +
    labs(x="long", y="lat", title="Points being excluded due to SA2 Region mapping") +
    theme_bw()

outfile <- paste0(output_dir, "map_InSA2Regions", year, ".png")
print(outfile)
ggsave(outfile, width=7, height=6, units="in")



