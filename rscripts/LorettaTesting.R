rm(list=ls())

library(sf)
library(tidyverse)
library(viridis)
library(rvest)
library(ggplot2)


#devtools::install_github("tidyverse/ggplot2")
#require(ggplot2)
#ls("package:ggplot2")


regions_mapped <- st_read("C:\\Users\\cla473\\Documents\\Projects\\AG_WHEATTEMP\\GIS data\\wheat_22regions_KC.shp", quiet = TRUE)
# limit to first 2 counties
#nc <- nc[1:2,]

class(regions_mapped)
glimpse(regions_mapped)
as.tibble(regions_mapped)

#this shows the regions (excluding TAS)
ggplot(regions_mapped) + 
    geom_sf() +
    coord_sf(xlim=c(112, 155), ylim=c(-10, -45)) +
    ggtitle("Australia")

st_crs(aus_mapped)
st_crs(regions_mapped)

aus_mapped <- st_read("C:\\Users\\cla473\\Documents\\Projects\\AG_WHEATTEMP\\GIS data\\states\\aust_cd66states.shp", quiet = TRUE)
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
    geom_sf(data = regions_mapped) +
    coord_sf(xlim=c(112, 155), ylim=c(-10, -45)) +
    ggtitle("Australia")

ggplot() + 
    geom_sf(data = aus_mapped) +
    geom_sf(data = regions_mapped) +
    coord_sf(xlim=c(112, 155), ylim=c(-10, -45)) +
    ggtitle("Australia")





# base plots
#ausGraph <- ggplot() + coord_fixed(xlim=c(112, 155), ylim=c(-10, -45)) + geom_polygon(data=ausDF, aes(x=long, y=lat,group=group), fill="dark gray", colour="black") + scale_fill_manual(values=cbPalette)
#regionGraph <-geom_polygon(data=RegionDF, aes(x=long, y=lat, group=group, fill=Australia_), colour="black")
all_df <- read_csv("\\\\ag-osm-02-cdc.it.csiro.au\\OSM_CBR_AG_WHEATTEMP_work\\output\\grainfilling\\07_GrainFilling_2016.csv")

#if longitude >= 141 and latitude > -40 then 'Tasmania' and needs to be included
#this is a fudge to include Tasmania as a 'Region' since it is not included in region maps
all_df <- all_df %>% 
    mutate(IsTAS = ifelse(lat <= -40, TRUE, FALSE))

unique(all_df$IsTAS)

tas <- all_df %>% 
    filter(IsTAS == FALSE)


coords <- select(all_df, long, lat)
points_sp <- SpatialPoints(coords=coords, proj4string = CRS("+proj=longlat +ellps=GRS80 +no_defs"))
points_sp
str(points_sp)
#points_spdf <- SpatialPointsDataFrame(coords = coords, data=all_df, proj4string = CRS("+proj=longlat +ellps=GRS80 +no_defs"))
#points_spdf
#str(points_spdf)


# Create sf object with all_df data frame and CRS
points_sf <- st_as_sf(all_df, coords = c("long", "lat"), crs = 4326)
st_geometry(points_sf)

#this plots just the points
ggplot() + 
    geom_sf(data = points_sf, aes(colour=maxTemp), alpha=0.7, show.legend="point") +
    coord_sf(xlim=c(112, 155), ylim=c(-10, -45)) +
    ggtitle("Australia")

#this plots the points on the map of Australia
ggplot() + 
    geom_sf(data = aus_mapped) +
    geom_sf(data = points_sf, aes(colour=maxTemp), alpha=0.7, show.legend = "point") +
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
    geom_sf(data = points_sf2, aes(colour=maxTemp), alpha=0.7, show.legend = "point") +
    coord_sf(xlim=c(112, 155), ylim=c(-10, -45)) +
    ggtitle("Australia")

