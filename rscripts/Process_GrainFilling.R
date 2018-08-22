#!/usr/bin/env Rscript

#----------------------------------------------------------------------------------
# Generates a graph showing the values specified as a heat map on the map of Australia
#----------------------------------------------------------------------------------
# Sample Usage:
#     Rscript --vanilla Process_GrainFilling.R -y 2014 -m maxTemp
#
#----------------------------------------------------------------------------------

library("optparse")
library("sf")
library("tidyverse")
library("viridis")
library("rvest")
library("ggplot2")
library("sf")

option_list = list(
    make_option(c("-y", "--year"), type="integer", default=2016, 
                help="what year will the data be filted by", metavar="integer"),
    make_option(c("-m", "--measurment"), type="character", default="maxTemp", 
                help="what measurement is being reported on ", metavar="character")
); 

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

if (is.null(opt$year)){
    print_help(opt_parser)
    stop("At least one argument must be supplied (input file).n", call.=FALSE)
}
if (is.null(opt$measurment)){
    print_help(opt_parser)
    stop("At least one argument must be supplied (input file).n", call.=FALSE)
}

source_data_dir <- "\\\\ag-osm-02-cdc.it.csiro.au\\OSM_CBR_AG_WHEATTEMP_work\\output\\grainfilling\\"


#Retrieve the Region Shape File
regions_mapped <- st_read("C:\\Users\\cla473\\Documents\\Projects\\AG_WHEATTEMP\\GIS data\\wheat_22regions_KC.shp", quiet = TRUE)
as.tibble(regions_mapped)

#Retrieve the Australia shape file
aus_mapped <- st_read("C:\\Users\\cla473\\Documents\\Projects\\AG_WHEATTEMP\\GIS data\\states\\aust_cd66states.shp", quiet = TRUE)
as.tibble(aus_mapped)

#need to do this as there is no CRS information with the AUS shape files
st_crs(aus_mapped) <- st_crs(regions_mapped)


#Retrieve the data for the specified year
filename <- paste0("07_GrainFilling_", opt$year, ".csv")
all_df <- read_csv(paste0(source_data_dir, filename))

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
ggplot() + 
    geom_sf(data = aus_mapped) +
    geom_sf(data = points_sf, aes(colour=opt$measurment), alpha=0.7, show.legend = "point") +
    coord_sf(xlim=c(112, 155), ylim=c(-10, -45)) +
    ggtitle("Australia")


outfile <- paste0(source_data_dir,  "map_", opt$measurement, "_", opt$year, ".png")
#ggsave(outfile, width = 16, height = 9, dpi = 100)
ggsave(outfile, width=7, height=6, units="in")

