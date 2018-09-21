#!/usr/bin/env Rscript

#----------------------------------------------------------------------------------
# Generates a graph showing the values specified as a heat map on the map of Australia
#----------------------------------------------------------------------------------
# Sample Usage:
#     Rscript Process_DayCount.R -y 2014 
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
#year <- 2009
source_data_dir <- "/OSM/CBR/AG_WHEATTEMP/work/output/grainfilling/"
source_shapefile_dir <- "/OSM/CBR/AG_WHEATTEMP/work/GIS_data/"
output_dir <- "/OSM/CBR/AG_WHEATTEMP/work/dayCount/"


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


#Retrieve the data for the specified year
datafile <- paste0(source_data_dir, "07_GrainFilling_", year, ".csv")
all_df <- read_csv(datafile)

maxDayCount <- max(all_df$dayCount)
minDayCount <- min(all_df$dayCount)

#eliminate any rows where day count <= 15
#all_df2 <- all_df[all_df$dayCount <= 16,]
all_df <- all_df[all_df$dayCount > 15,]

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

#colPal <- colorRampPalette(rev(brewer.pal(9, "RdYlBu")))(maxVal-minVal)
colPal <- colorRampPalette(brewer.pal(9, "PuBuGn")) (45)
#chtTitle <- paste0("Average Maximum Temperature \nWheat Grain Filling Period")

ggplot() + 
    geom_sf(data = aus_mapped) +
    geom_sf(data = points_sf, aes(colour=dayCount), alpha=0.7, show.legend = "point") +
    scale_colour_gradientn(colours=colPal, limits=c(15,60)) +
    coord_sf(xlim=c(112, 156), ylim=c(-10, -45)) +
    labs(x="long", y="lat") +
    theme_bw()
    

outfile <- paste0(output_dir, "map_", year, ".png")
print(outfile)
#ggsave(outfile, width = 16, height = 9, dpi = 100)
ggsave(outfile, width=7, height=6, units="in")

