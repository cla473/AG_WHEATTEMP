rm(list=ls())


library(sf)
#library(sp)
library(tidyverse)
library(viridis)
library(rvest)
library(ggplot2)


#devtools::install_github("tidyverse/ggplot2")
#require(ggplot2)
#ls("package:ggplot2")
source_data_dir <- "\\\\ag-osm-02-cdc.it.csiro.au\\OSM_CBR_AG_WHEATTEMP_work\\output\\grainfilling\\"
regions_mapped <- st_read("C:\\Users\\cla473\\Documents\\Projects\\AG_WHEATTEMP\\GIS data\\wheat_22regions_KC.shp", quiet = TRUE)
aus_mapped <- st_read("C:\\Users\\cla473\\Documents\\Projects\\AG_WHEATTEMP\\GIS data\\states\\aust_cd66states.shp", quiet = TRUE)


cbPalette <- c("#a6cee3", "#ffffb3", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", "#ffff99")
cbPalette <- c(cbPalette, "#8dd3c7", "#1f78b4", "#bebada", "#fb8072", "#80b1d3", "#fdb462", "#b3de69", "#fccde5", "#d9d9d9", "#bc80bd", "#ccebc5")
pal <- c("blue", "green","yellow","red")


st_crs(regions_mapped)
class(regions_mapped)
glimpse(regions_mapped)
as.tibble(regions_mapped)

#this shows the regions (excluding TAS)
ggplot(regions_mapped) + 
    geom_sf() +
    coord_sf(xlim=c(112, 155), ylim=c(-10, -45)) +
    ggtitle("Australia")



st_crs(aus_mapped)
class(aus_mapped)
glimpse(aus_mapped)
as.tibble(aus_mapped)



#need to do this as there is no CRS information with the AUS shape files
st_crs(aus_mapped) <- st_crs(regions_mapped)


#this shows the Australia borders (including TAS)
ggplot(aus_mapped) + 
    geom_sf() +
    coord_sf(xlim=c(112, 155), ylim=c(-10, -45)) +
    ggtitle("Australia")

ggplot() + 
    geom_sf(data = aus_mapped) +
    geom_sf(data = regions_mapped) +
    coord_sf(xlim=c(112, 155), ylim=c(-10, -45)) +
    ggtitle("Australia")

ausGraph <- ggplot() + 
    coord_fixed(xlim=c(112, 155), ylim=c(-10, -45)) + 
    geom_polygon(data=ausDF, aes(x=long, y=lat,group=group), fill="dark gray", colour="black") + 
    scale_fill_manual(values=cbPalette)


year <- 2016
filename <- paste0("07_GrainFilling_", year, ".csv")
measurement <- "maxTemp"

# base plots
#ausGraph <- ggplot() + coord_fixed(xlim=c(112, 155), ylim=c(-10, -45)) + geom_polygon(data=ausDF, aes(x=long, y=lat,group=group), fill="dark gray", colour="black") + scale_fill_manual(values=cbPalette)
#regionGraph <-geom_polygon(data=RegionDF, aes(x=long, y=lat, group=group, fill=Australia_), colour="black")
all_df <- read_csv(paste0(source_data_dir, filename))

#if longitude >= 141 and latitude <= -40 then 'Tasmania' and needs to be included
#this is a fudge to include Tasmania as a 'Region' since it is not included in region maps
#all_df <- all_df %>% 
#    mutate(IsTAS = ifelse(abs(lat) >= 40, TRUE, FALSE))
#
#unique(all_df$IsTAS)
#tas <- all_df %>% 
#    filter(IsTAS == FALSE)


coords <- select(all_df, long, lat)
points_sp <- SpatialPoints(coords=coords, proj4string = CRS("+proj=longlat +ellps=GRS80 +no_defs"))
points_sp
str(points_sp)


# Create sf object with all_df data frame and CRS
points_sf <- st_as_sf(all_df, coords = c("long", "lat"), crs = 4326)
st_geometry(points_sf)

#this plots just the points
ggplot() + 
    geom_sf(data = points_sf, aes(colour=measurement), alpha=0.7, show.legend="point") +
    coord_sf(xlim=c(112, 155), ylim=c(-10, -45)) +
    ggtitle("Australia")

#this plots the points on the map of Australia
ggplot() + 
    geom_sf(data = aus_mapped) +
    geom_sf(data = points_sf, aes(colour=measurement), alpha=0.7, show.legend = "point") +
    coord_sf(xlim=c(112, 155), ylim=c(-10, -45)) +
    ggtitle("Australia")

str(points_sf)



#remove points outside regions
#p <- cbind(all_df$long, all_df$lat)
#pp <- SpatialPoints(p)
#pcheck <-!is.na(over(pp, regions_mapped)) 


st_crs(points_sf)
st_crs(regions_mapped)
st_crs(points_sf) <- st_crs(regions_mapped)


pcheck <- points_sf$geometry %>% 
    st_intersects(regions_mapped$geometry) %>% 
    map_lgl(function(x) length(x) > 0)

pcheck <- as.data.frame(pcheck)
points_sf$InRegion <- pcheck

#NOW ADD BACK IN TASMANIA
points_sf$InRegion[points_sf$IsTAS==TRUE] <- TRUE


#now filter everything for the regions plus Tasmiania
points_sf2 <- points_sf[points_sf$InRegion==TRUE,]


#now map only this points that are within the regions, on the map of Australia ()
ggplot() + 
    geom_sf(data = aus_mapped) +
    geom_sf(data = points_sf2, aes(colour=measurement), alpha=0.7, show.legend = "point") +
    coord_sf(xlim=c(112, 155), ylim=c(-10, -45)) +
    ggtitle("Australia")


# this didn't work with temp
#scale_fill_manual(values=cbPalette) +
    

outfile <- paste0(source_data_dir,  "map_", measurement, "_", year, ".png")
#ggsave(outfile, width = 16, height = 9, dpi = 100)
ggsave(outfile, width=7, height=6, units="in")
