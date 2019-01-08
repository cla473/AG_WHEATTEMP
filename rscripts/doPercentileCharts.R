rm(list = ls())
library(tidyverse)
library(sf)
library(ggplot2)
library(viridis)
library(rvest)
library(ggrepel)
library(RColorBrewer)


filePath <- '/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/modMET_csv'
mapPath <- '/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/maps'
dataFile <- paste0(filePath, "/", "percentiles.txt")

all_df <- read_csv(dataFile)
str(all_df)

# select only the columns we require, and calculate the dates that we want
# and identify Tasmanians sites, so that they can be added back in later
all_df <- all_df %>% 
    select(Long, Lat, LFrost_90P) %>% 
    mutate(Date = as.Date(LFrost_90P -1, "2017-01-01"),
           IsTas = ifelse((Lat < -40 & Lat > -45), "Y", ""))

#filterlimit <- 182
filterlimit <- 203

noFrost_df <- all_df %>% 
    filter(LFrost_90P <= filterlimit)

Frost_df <- all_df %>% 
    filter(LFrost_90P > filterlimit)

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

# Need to eliminate any points that are not within the regions
pcheck <- noFrost_points_sf$geometry %>% 
    st_intersects(regions_mapped$geometry) %>% 
    map_lgl(function(x) length(x) > 0)

pcheck <- as.data.frame(pcheck)
noFrost_points_sf$InRegion <- pcheck
#ADD BACK IN TASMANIA
noFrost_points_sf[noFrost_points_sf$IsTas == "Y", "InRegion"] <- TRUE

#FILTER BASED ON THE REGIONS
noFrost_points_sf <- noFrost_points_sf[noFrost_points_sf$InRegion==TRUE,]

# 
# #this just shows map of Australia
# ggplot() + 
#     geom_sf(data=aus_mapped) +
#     coord_sf(xlim=c(112, 156), ylim=c(-10, -45)) +
#     labs(x="long", y="lat") +
#     theme_bw()
# 
# #this shows the points on the map, with colour scale for value required (LFrost_90P)
# ggplot() + 
#     geom_sf(data=aus_mapped) +
#     geom_sf(data=noFrost_points_sf, colour="grey", alpha=0.7, show.legend = "point") +
#     coord_sf(xlim=c(112, 156), ylim=c(-10, -45)) +
#     labs(x="long", y="lat") +
#     theme_bw()


#now prepare the Frost_df
# get the coordintaes and format as SpatialPoints
coords <- select(Frost_df, Long, Lat)

# Create sf object with all_df data frame and CRS
Frost_points_sf <- st_as_sf(Frost_df, coords = c("Long", "Lat"), crs = 4326)
st_geometry(Frost_points_sf)

# need to do this as there is no CRS information with the AUS shape files
st_crs(Frost_points_sf) <- st_crs(regions_mapped)

# Need to eliminate any points that are not within the regions
pcheck <- Frost_points_sf$geometry %>% 
    st_intersects(regions_mapped$geometry) %>% 
    map_lgl(function(x) length(x) > 0)

pcheck <- as.data.frame(pcheck)
Frost_points_sf$InRegion <- pcheck
#ADD BACK IN TASMANIA
Frost_points_sf[Frost_points_sf$IsTas == "Y", "InRegion"] <- TRUE

#FILTER BASED ON THE REGIONS
Frost_points_sf <- Frost_points_sf[Frost_points_sf$InRegion==TRUE,]

colPalette <- colorRampPalette(rev(brewer.pal(9, "YlGnBu")))

#need to show 1-Jul, 1-Aug, 1-Sept, 1-Oct, 1-Nov, 1-Dec
#labels=c("1-Jul", "1-Aug", "1-Sept", "1-Oct", "1-Nov", "1-Dec")
#these are doy  182,   213,    244,   274,   305,   335

#labels=c(335, 305, 274, 244, 213, 182)'
#breaks=c(182, 213, 244, 274, 305, 335)'
#need to had the dates, instead of the doy
#=================================================================
# THIS IS THE CHART THAT TAKES AGES TO RENDER
#=================================================================

ggplot() + 
    geom_sf(data=aus_mapped) +
    geom_sf(data=noFrost_points_sf, colour="grey", size=0.6, show.legend="point") +
    geom_sf(data=Frost_points_sf, aes(colour=LFrost_90P), size=0.6, alpha=0.7, show.legend="point") +
    scale_colour_gradientn(colours=colPalette(10), limits=c(182, 365), 
                           breaks=c(182, 213, 244, 274, 305, 335),
                           labels=c("1-Jul", "1-Aug", "1-Sept", "1-Oct", "1-Nov", "1-Dec")) +
    coord_sf(xlim=c(112, 156), ylim=c(-10, -45)) +
    labs(x="Longitude", y="Latitude", title="Australia - 1957 to 2017", 
         subtitle="Date of Last Frost - 90th Percentile") + 
    theme_bw() +
    theme(legend.position="bottom", legend.direction="horizontal", legend.title=element_blank(),
          legend.key.width = unit(3.0, "cm"),
          legend.key.size = unit(0.4, "cm"), legend.spacing.y = unit(0.3, "cm"))  

#=================================================================

#legend.spacing.x = unit(0.5, "cm"),
#legend.text=element_text(margin=margin(r=10, unit="pt")
#NOTE:  keywidth doesn't seem to work, and although the legend is in the correct position,
#       still need to stretch it out so that all of the dates are visible, try with legend.text.
#       perhaps try legend.spacing.x = units(0.5, "cm"),
#        ..... haven't ran this one yet.

#=================================================================
# SAVING TAKES ALMOST AS LONG AS RENDERING
#=================================================================
outfile <- paste0(mapPath, "/LFrost_90P_8.png")
ggsave(outfile, width=7, height=6, units="in")



# #=================================================================
# # the following is my test environment
# #=================================================================
# #sample for testing
# df <- data.frame(x = runif(100), y = runif(100), z1=rnorm(100), z2=abs(rnorm(100)))
# p1 <- ggplot(df, aes(x, y)) +
#     geom_point(aes(colour=z1)) +
#     scale_color_gradientn(colours = terrain.colors(10), 
#                           breaks=c(1.5, 1, 0.5, 0, -0.5, -1, -1.5, -2),
#                           labels=c("1-Jul", "1-Aug", "1-Sept", "1-Oct", "1-Nov", "1-Dec", "1-Jan", "1-Feb"))
# p1
# 
# p1 + theme(legend.position="bottom", legend.direction="horizontal", legend.title=element_blank(),
#            legend.key.width = unit(3.0, "cm"),legend.key.size = unit(0.4, "cm"), legend.spacing.y = unit(0.3, "cm"))
# #for some reason, this shows the legend as Feb to July, not July to Feb
# 
# p1 + theme(legend.position="bottom", legend.direction="horizontal", legend.title=element_blank(),
#            legend.key.width = unit(3.0, "cm"),legend.key.size = unit(0.4, "cm"), legend.spacing.y = unit(0.3, "cm")) + 
#     guides(fill=guide_legend(reverse=TRUE))
# #this does nothing
# 
# p1 + theme(legend.position="bottom", legend.direction="horizontal", legend.title=element_blank(),
#            legend.key.width = unit(3.0, "cm"),legend.key.size = unit(0.4, "cm"), legend.spacing.y = unit(0.3, "cm")) + 
#     guides(colours=guide_legend(reverse=TRUE))
# #this shows the dates as "factors" not continuious scale, and is still in the wrong order
# 
# p1 + guides(fill=guide_legend(reverse=TRUE)) +
#     theme(legend.position="bottom", legend.direction="horizontal", legend.title=element_blank(),
#            legend.key.width = unit(3.0, "cm"),legend.key.size = unit(0.4, "cm"), legend.spacing.y = unit(0.3, "cm"))  
# #this also does nothing
# 
# p1 + guides(colours=guide_legend(reverse=TRUE)) +
#     theme(legend.position="bottom", legend.direction="horizontal", legend.title=element_blank(),
#           legend.key.width = unit(3.0, "cm"),legend.key.size = unit(0.4, "cm"), legend.spacing.y = unit(0.3, "cm"))  
# #this also does nothing
# 
# 
# 
# 
# 
# p2 <- ggplot(df, aes(x, y)) +
#     geom_point(aes(colour=z1)) +
#     scale_color_gradientn(colours = terrain.colors(10), 
#                           breaks=c(1.5, 1, 0.5, 0, -0.5, -1, -1.5, -2),
#                           labels=c("1-Jul", "1-Aug", "1-Sept", "1-Oct", "1-Nov", "1-Dec", "1-Jan", "1-Feb"),
#                           guide=guide_legend(reverse = TRUE))
# p2
# #this shows the dates as "factors", but is in the correct order
# 
# p2 + theme(legend.position="bottom", legend.direction="horizontal", legend.title=element_blank(),
#            legend.key.width = unit(3.0, "cm"),legend.key.size = unit(0.4, "cm"), legend.spacing.y = unit(0.3, "cm"))
# #this shows the dates as "factors", but is in the correct order
# 
# 
# #----------------------------------------------
# #this one works
# #----------------------------------------------
# p3 <- ggplot(df, aes(x, y)) +
#     geom_point(aes(colour=z1)) +
#     scale_color_gradientn(colours = terrain.colors(10), 
#                           breaks=c(1.5, 1, 0.5, 0, -0.5, -1, -1.5, -2),
#                           labels=c("1-Jul", "1-Aug", "1-Sept", "1-Oct", "1-Nov", "1-Dec", "1-Jan", "1-Feb"),
#                           guide=guide_colourbar(reverse = TRUE))
# p3
# p3 + theme(legend.position="bottom", legend.direction="horizontal", legend.title=element_blank(),
#            legend.key.width = unit(3.0, "cm"),legend.key.size = unit(0.4, "cm"), legend.spacing.y = unit(0.3, "cm"))
# #----------------------------------------------

#=======================================================================================
#=======================================================================================
# NOW LETS LOOK AT HEAT VALUES
#=======================================================================================
#=======================================================================================
all_df <- read_csv(dataFile)
str(all_df)

# select only the columns we require, and calculate the dates that we want
# and identify Tasmanians sites, so that they can be added back in later
all_df <- all_df %>% 
    select(Long, Lat, FHeat_50P) %>% 
    mutate(IsTas = ifelse((Lat < -40 & Lat > -45), "Y", ""))

filterlimit <- 342 #(365 - 23 (approx 10%))

#now need to graph the FHeat_20P
# 1. Separate tout those sites with value of 182, and graph them as gray
# 2. graph the remaing values with colour scales from purple (as 183) to yellow (as 365)
noHeat_df <- all_df %>% 
    filter(FHeat_50P >= filterlimit)

Heat_df <- all_df %>% 
    filter(FHeat_50P < filterlimit)

# get the coordintaes and format as SpatialPoints
coords <- select(noHeat_df, Long, Lat)

# Create sf object with all_df data frame and CRS
noHeat_points_sf <- st_as_sf(noHeat_df, coords = c("Long", "Lat"), crs = 4326)
st_geometry(noHeat_points_sf)

# need to do this as there is no CRS information with the AUS shape files
st_crs(noHeat_points_sf) <- st_crs(regions_mapped)

# Need to eliminate any points that are not within the regions
pcheck <- noHeat_points_sf$geometry %>% 
    st_intersects(regions_mapped$geometry) %>% 
    map_lgl(function(x) length(x) > 0)

pcheck <- as.data.frame(pcheck)
noHeat_points_sf$InRegion <- pcheck
#ADD BACK IN TASMANIA
noHeat_points_sf[noHeat_points_sf$IsTas == "Y", "InRegion"] <- TRUE

#FILTER BASED ON THE REGIONS
noHeat_points_sf <- noHeat_points_sf[noHeat_points_sf$InRegion==TRUE,]

# get the coordinates and format as SpatialPoints
coords <- select(Heat_df, Long, Lat)

# Create sf object with all_df data frame and CRS
Heat_points_sf <- st_as_sf(Heat_df, coords = c("Long", "Lat"), crs = 4326)
st_geometry(Heat_points_sf)

# need to do this as there is no CRS information with the AUS shape files
st_crs(Heat_points_sf) <- st_crs(regions_mapped)

# Need to eliminate any points that are not within the regions
pcheck <- Heat_points_sf$geometry %>% 
    st_intersects(regions_mapped$geometry) %>% 
    map_lgl(function(x) length(x) > 0)

pcheck <- as.data.frame(pcheck)
Heat_points_sf$InRegion <- pcheck
#ADD BACK IN TASMANIA
Heat_points_sf[Heat_points_sf$IsTas == "Y", "InRegion"] <- TRUE

#FILTER BASED ON THE REGIONS
Heat_points_sf <- Heat_points_sf[Heat_points_sf$InRegion==TRUE,]

colPalette_Heat <- colorRampPalette(rev(brewer.pal(9, "YlOrRd")))

#=================================================================
# THIS IS THE CHART THAT TAKES AGES TO RENDER
#=================================================================

ggplot() + 
    geom_sf(data=aus_mapped) +
    geom_sf(data=noHeat_points_sf, colour="grey", size=0.6, show.legend="point") +
    geom_sf(data=Heat_points_sf, aes(colour=FHeat_50P), size=0.6, alpha=0.7, show.legend="point") +
    scale_colour_gradientn(colours=colPalette_Heat(10), limits=c(182, 365), 
                           breaks=c(182, 213, 244, 274, 305, 335),
                           labels=c("1-Jul", "1-Aug", "1-Sept", "1-Oct", "1-Nov", "1-Dec")) +
    coord_sf(xlim=c(112, 156), ylim=c(-10, -45)) +
    labs(x="Longitude", y="Latitude", title="Australia - 1957 to 2017", 
         subtitle="Date of First Heat - 50th Percentile") + 
    theme_bw() +
    theme(legend.position="bottom", legend.direction="horizontal", legend.title=element_blank(),
          legend.key.width = unit(3.0, "cm"),
          legend.key.size = unit(0.4, "cm"), legend.spacing.y = unit(0.3, "cm"))  

#=================================================================
# SAVING TAKES ALMOST AS LONG AS RENDERING
#=================================================================
outfile <- paste0(mapPath, "/FHeat_50P", ".png")
ggsave(outfile, width=7, height=6, units="in")


