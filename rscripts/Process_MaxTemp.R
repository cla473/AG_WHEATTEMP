#!/usr/bin/env Rscript

#----------------------------------------------------------------------------------
# Generates a graph showing the values specified as a heat map on the map of Australia
#----------------------------------------------------------------------------------
# Sample Usage:
#     Rscript Process_MaxTemp.R -y 2014 
#----------------------------------------------------------------------------------
#rm(list =ls())
library("optparse")
library("sf")
library("tidyverse")
library("viridis")
library("rvest")
library("ggplot2")
library("RColorBrewer")

option_list = list(
    make_option(c("-y", "--year"), type="integer", default=2016, 
                help="what year will the data be filted by", metavar="integer")
); 

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

if (is.null(opt$year)){
    print_help(opt_parser)
    stop("At least one argument must be supplied (input file).n", call.=FALSE)
}
year <- opt$year

source_data_dir <- "/OSM/CBR/AG_WHEATTEMP/work/output/grainfilling/"
source_shapefile_dir <- "/OSM/CBR/AG_WHEATTEMP/work/GIS_data/"
output_dir <- "/OSM/CBR/AG_WHEATTEMP/work/maxTemp/"


#Retrieve the Region Shape File
regionfile <- paste0(source_shapefile_dir, "wheat_22regions_KC.shp")
print(paste0("region file: ", regionfile))

regions_mapped <- st_read(regionfile, quiet = TRUE)
as.tibble(regions_mapped)

#Retrieve the Australia shape file
australiafile <- paste0(source_shapefile_dir, "states/aust_cd66states.shp")
aus_mapped <- st_read(australiafile, quiet = TRUE)
as.tibble(aus_mapped)

#need to do this as there is no CRS information with the AUS shape files
st_crs(aus_mapped) <- st_crs(regions_mapped)

#year <- 2016
#Retrieve the data for the specified year
datafile <- paste0(source_data_dir, "07_GrainFilling_", year, ".csv")
all_df <- read_csv(datafile)

#get the coordintaes and format as SpatialPoints
coords <- select(all_df, long, lat)
#points_sp <- SpatialPoints(coords=coords, proj4string = CRS("+proj=longlat +ellps=GRS80 +no_defs"))

# Create sf object with all_df data frame and CRS
points_sf <- st_as_sf(all_df, coords = c("long", "lat"), crs = 4326)
st_geometry(points_sf)

#need to do this as there is no CRS information with the AUS shape files
st_crs(points_sf) <- st_crs(regions_mapped)

#Need to eliminate any points that are not within the regions
pcheck <- points_sf$geometry %>% 
    st_intersects(regions_mapped$geometry) %>% 
    map_lgl(function(x) length(x) > 0)

pcheck <- as.data.frame(pcheck)
points_sf$InRegion <- pcheck
points_sf <- points_sf[points_sf$InRegion==TRUE,]

#now map only this points that are within the regions, on the map of Australia ()
#ggplot() + 
#    geom_sf(data = aus_mapped) +
#    geom_sf(data = points_sf, aes(colour=maxTemp), alpha=0.7, show.legend = "point") +
#    coord_sf(xlim=c(112, 155), ylim=c(-10, -45)) +
#    scale_fill_continuous(low="#56B1F7", high="#132B43")
#    ggtitle("Australia")

#minVal <- round(min(all_df$maxTemp), 0) - 2
#maxVal <- round(max(all_df$maxTemp), 0) + 2

#maxdf <- all_df[all_df$maxTemp >= 30,]
#mindf <- all_df[all_df$maxTemp <= 19,]

#colPal <- colorRampPalette(rev(brewer.pal(9, "RdYlBu")))(maxVal-minVal)
colPal <- colorRampPalette(rev(brewer.pal(9, "RdYlBu"))) (35)
chtTitle <- paste0("Average Maximum Temperature \nWheat Grain Filling Period")

ggplot() + 
    geom_sf(data = aus_mapped) +
    geom_sf(data = points_sf, aes(colour=maxTemp), alpha=0.7, show.legend = "point") +
    scale_colour_gradientn(colours=colPal, limits=c(15, 35)) +
    coord_sf(xlim=c(112, 156), ylim=c(-10, -45)) +
    labs(x="long", y="lat") +
    theme_bw()
    
outfile <- paste0(output_dir, "map_", year, ".png")
print(outfile)
ggsave(outfile, width=7, height=6, units="in")

