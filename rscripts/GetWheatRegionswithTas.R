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
source_GIS_dir <- "/OSM/CBR/AG_WHEATTEMP/work/GIS_data/"
year <- 2009

#----------------------------------------------------------------------
# Retrieve the shape files
#----------------------------------------------------------------------
#Retrieve the Australia shape file
shapefile <- paste0(source_GIS_dir, "states/aust_cd66states.shp")
print(paste0("filename: ", shapefile))
aus_mapped <- st_read(shapefile, quiet = TRUE)
as.tibble(aus_mapped)

ggplot() + 
    geom_sf(data = aus_mapped)

#------------------
#Retrieve the Region Shape File
shapefile <- paste0(source_GIS_dir, "wheat_22regions_KC/wheat_22regions_KC.shp")
print(paste0("region file: ", shapefile))
regions_mapped <- st_read(shapefile, quiet = TRUE)
as.tibble(regions_mapped)

ggplot() + 
    geom_sf(data = regions_mapped)

st_crs(aus_mapped) <- st_crs(regions_mapped)
ggplot() + 
    geom_sf(data = aus_mapped) +
    geom_sf(data = regions_mapped, aes(fill=Australia1)) +
    labs(x="long", y="lat", title="Original 22 GRDC Wheat Regions")

#------------------
shapefile <- paste0(source_GIS_dir, "GRDC_Regions/GRDC_Regions.shp")
print(paste0("region file: ", shapefile))
GRDCregions_mapped <- st_read(shapefile, quiet = TRUE)
as.tibble(GRDCregions_mapped)

ggplot() + 
    geom_sf(data = GRDCregions_mapped)

ggplot() + 
    geom_sf(data = aus_mapped) +
    geom_sf(data = GRDCregions_mapped, aes(fill=AGECO_ZONE)) +
    labs(x="long", y="lat", title="Ageco Zones")

ggplot() + 
    geom_sf(data = aus_mapped) +
    geom_sf(data = GRDCregions_mapped, aes(fill=REGION)) +
    labs(x="long", y="lat", title="Ageco Zones as 3 Regions")



#------------------
#Retrieve the Region Shape File (this is from David Gobbett)
shapefile <- paste0(source_GIS_dir, "GRDC_Regions NEW_region/GRDC_Regions NEW_region.shp")
print(paste0("region file: ", shapefile))
file.exists(shapefile)
newRegions_mapped <- st_read(shapefile, quiet = TRUE)
as.tibble(newRegions_mapped)

ggplot() + 
    geom_sf(data = aus_mapped) +
    geom_sf(data = newRegions_mapped[newRegions_mapped$Region != "Excluded",], aes(fill=Region)) +
    labs(x="long", y="lat", title="New 3 Area Regions")


#these are the provisional GRDC (SA2) regions (Prepared by David Gobbett 2015)
shapefile <- paste0(source_GIS_dir, "Draft_GRDC_Subregions_2015/Draft_GRDC_Subregions_2015.shp")
print(paste0("region file: ", shapefile))
SA2Regions_mapped <- st_read(shapefile, quiet = TRUE)
as.tibble(SA2Regions_mapped)
str(SA2Regions_mapped)

ggplot() + 
    geom_sf(data = aus_mapped) +
    geom_sf(data = SA2Regions_mapped) +
    labs(x="long", y="lat", title="Draft GRDC SubRegions 2015")


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



