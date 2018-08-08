rm(list=ls())

library(sf)
library(tidyverse)
library(viridis)
library(rvest)
library(ggplot2)


#devtools::install_github("tidyverse/ggplot2")
#require(ggplot2)
#ls("package:ggplot2")

GRDCregion_sf <- "C:\\Users\\cla473\\Documents\\Projects\\AG_WHEATTEMP\\GIS data\\wheat_22regions_KC.shp"
AUSmap_sf <- "C:\\Users\\cla473\\Documents\\Projects\\AG_WHEATTEMP\\GIS data\\states\\aust_cd66states.shp"


grdc_mapped <- st_read(GRDCregion_sf, quiet = TRUE)
# limit to first 2 counties
#nc <- nc[1:2,]

class(grdc_mapped)
glimpse(grdc_mapped)
as.tibble(grdc_mapped)

aus_mapped <- st_read(AUSmap_sf, quiet = TRUE)
class(aus_mapped)
glimpse(aus_mapped)
as.tibble(aus_mapped)



ggplot(grdc_mapped) + 
    geom_sf() +
    coord_sf(xlim=c(112, 155), ylim=c(-10, -45)) +
    ggtitle("Australia")

#    geom_polygon(data=aus_mapped, aes(x=long, y=lat,group=group), fill="dark gray", colour="black") +


#ausDF <- AusMap()
#RegionDF <- GRDCMap()

# base plots
#ausGraph <- ggplot() + coord_fixed(xlim=c(112, 155), ylim=c(-10, -45)) + geom_polygon(data=ausDF, aes(x=long, y=lat,group=group), fill="dark gray", colour="black") + scale_fill_manual(values=cbPalette)
#regionGraph <-geom_polygon(data=RegionDF, aes(x=long, y=lat, group=group, fill=Australia_), colour="black")



# convert to SpatialPolygonsDataFrame
GRDCregion_sp <- as(GRDCregion, "Spatial")
class(GRDCregion_sp)
str(GRDCregion_sp)


(grdc_geom <- st_geometry(GRDCregion))
st_geometry(GRDCregion) %>% class()
attributes(grdc_geom)
grdc_geom[[1]] %>% class



