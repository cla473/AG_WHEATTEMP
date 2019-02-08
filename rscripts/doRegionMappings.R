#============================================================
# doRegionmappings.R
#============================================================
# This script gets a list of locations (cells) and generates
# a datatable of the regions with their geographical regions
# from the original 22 wheat regions; adds our new regions,
# the varieties for each region is added, along with the
# Maturity decription.
# This file will then be used for collating the data
#============================================================

rm(list = ls())
library(tidyverse)
library(sf)

regionfile <- "/OSM/CBR/AG_WHEATTEMP/work/GIS_data/wheat_22regions_KC/wheat_22regions_KC.shp"
australiafile <- "/OSM/CBR/AG_WHEATTEMP/work/GIS_data/states/aust_cd66states.shp"
mapPath <- '/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/maps'
outPath <- '/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/metCalcs'

outfile <- paste0(mapPath, "/", "mapRegionstest", ".png")

#get a list of the directories (cells/locations) that we have
dirPath <- '/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/Summaries/'
dirs <- list.dirs(path = dirPath, full.names = TRUE, recursive = FALSE)
cells_df <- data.frame(fullpath=dirs, stringsAsFactors=FALSE)
#str(dir_df)

# create the Longitude and Latidue and Identify Tasmania (region not included in original map)
cells_df <- as.data.frame(cells_df) %>% 
    mutate(Location = str_replace(fullpath, dirPath, ""),
           Location = str_replace(Location, "/", "")) %>% 
    mutate(Long = substr(Location, 1, 5),
           Lat = substr(Location, 6, 10)) %>% 
    mutate(Long = (as.numeric(Long) / 100),
           Lat = (as.numeric(Lat) / 100)) %>% 
    mutate(WheatRegion = ifelse((Lat < -40 & Lat > -45), "TAS", "")) %>% 
    select(Location, Long, Lat, WheatRegion)

# Create sf object with all_df data frame and CRS
cells_df <- st_as_sf(cells_df, coords = c("Long", "Lat"), crs = 4326)
st_geometry(cells_df)


# now retrieve the region polygons from the 22 Wheat Regions
regions_gdf <- st_read(regionfile, quiet = TRUE)
as.tibble(regions_gdf)

# lets rename the region columns to be more meaningful
regions_gdf <- regions_gdf %>% 
    rename(WheatRegion = Australia_,
           WheatRegionDesc = Australia1)

# Retrieve the Australia shape file
aus_mapped <- st_read(australiafile, quiet = TRUE)
as.tibble(aus_mapped)
# need to do this as there is no CRS information with the AUS shape files, nor our data 
st_crs(aus_mapped) <- st_crs(regions_gdf)
st_crs(cells_df) <- st_crs(regions_gdf)

# now build a function that can be used to map each location with its region
# the filter all sites by the 22 regions (and add back in Tasmania)
pcheck <- cells_df$geometry %>% 
    st_intersects(regions_gdf$geometry) %>% 
    map_lgl(function(x) length(x) > 0)

pcheck <- as.data.frame(pcheck)
cells_df$InRegion <- pcheck
# ADD BACK IN TASMANIA
cells_df[cells_df$WheatRegion == "TAS", "InRegion"] <- TRUE

# FILTER BASED ON THE REGIONS
cells_df <- cells_df[cells_df$InRegion==TRUE,]

# now we need to determine which cells are in which region
addRegiontoData <- function(cells_df, region_df, wheatRegion) {
    
    region_df <- regions_gdf
    region_df <- region_df[region_df$WheatRegion == wheatRegion,]
    
    pcheck <- cells_df$geometry %>% 
        st_intersects(region_df$geometry) %>% 
        map_lgl(function(x) length(x) > 0)
    
    pcheck <- as.data.frame(pcheck)
    cells_df$InRegion <- pcheck
    # update Region base on pcheck
    cells_df[cells_df$InRegion==TRUE, "WheatRegion"] <- wheatRegion
    
    return(cells_df)
}

regions <- as.data.frame(regions_gdf) %>% 
    select(WheatRegion, WheatRegionDesc) %>% 
    distinct()

unique(regions_gdf$WheatRegion)

#apply the above function to all of the data
for (row in 1:nrow(regions)) {
    cells_df <- addRegiontoData(cells_df, regions_gdf, regions[row, "WheatRegion"])
}

regions_mapped$newRegions <- as.factor(regions_mapped$newRegions)

# map this to test if it has worked
p <- ggplot() + 
    geom_sf(data=aus_mapped) +
    geom_sf(data=cells_df, aes(colour=WheatRegion), size=0.6, alpha=0.7, show.legend="point") 

ggsave(outfile, width=7, height=6, units="in")


# now merge the regions dataframe with our data frame, to add the WheatRegionDesc
outCells_df <- as.data.frame(cells_df) %>% 
    select(Location, WheatRegion) %>% 
    left_join(regions, by="WheatRegion")

#NOTE:  Region Id's are numbers so that the colours on the charts are not similiar to their neighbour
# need to add a region description to this still
outCells_df <- outCells_df %>% 
    mutate(NewRegion = case_when(WheatRegion == "QLD1" ~ 6,   # Central Queensland
                                 WheatRegion == "QLD2" ~ 3,   # Eastern Darling Downs
                                 WheatRegion == "QLD3" ~ 2,   # Western Darling Downs
                                 WheatRegion == "QLD4" ~ 2,   # Southern West Queensland
                                 WheatRegion == "NSW1" ~ 2,   # Northern NSW"
                                 WheatRegion == "NSW2" ~ 4,   # Western NSW
                                 WheatRegion == "NSW3" ~ 3,   # Eastern NSW
                                 WheatRegion == "NSW5" ~ 10,  # South-western NSW
                                 WheatRegion == "NSW6" ~ 10,  # South-eastern NSW
                                 WheatRegion == "VIC1" ~ 1,   # Wimmera
                                 WheatRegion == "VIC2" ~ 7,   # South Mallee
                                 WheatRegion == "SA1" ~ 9,    # Upper eyer peninsula
                                 WheatRegion == "SA1bis" ~ 5, # Lower eyer peninsula
                                 WheatRegion == "SA2" ~ 5,    # York peninsula
                                 WheatRegion == "SA2bis" ~ 9, # Mid North (SA)
                                 WheatRegion == "SA3" ~ 9,    # Murray Mallee
                                 WheatRegion == "SA3bis" ~ 7, # Mallee
                                 WheatRegion == "WA1" ~ 8,    # WA zone 1
                                 WheatRegion == "WA2" ~ 8,    # WA zone 2
                                 WheatRegion == "WA3" ~ 8,    # WA zone 3
                                 WheatRegion == "WA4" ~ 11,   # WA zone 4,
                                 WheatRegion == "WA5" ~ 11 )) # WA zone 5


# Need to create a file with the NewRegion mapped with the cultivars/maturity.
outCells_df <- outCells_df %>% 
    mutate(NewRegionDesc = case_when(NewRegion == 6 ~ "Central Queensland",
                                     NewRegion == 3 ~ "Eastern Darling Downs and Eastern NSW",    
                                     NewRegion == 2 ~ "Western Darling Downs, Southern West Queensland and Northern NSW", 
                                     NewRegion == 4 ~ "Western NSW",
                                     NewRegion == 10 ~ "South-eastern and South-western NSW",
                                     NewRegion == 1 ~ "Wimmera and central Mallee",
                                     NewRegion == 7 ~ "South Mallee",
                                     NewRegion == 9 ~ "Upper eyer peninsula, Mid North (SA) and Murray Mallee",
                                     NewRegion == 5 ~ "Lower eyer peninsula and York peninsula",
                                     NewRegion == 8 ~ "WA zone 1, 2, and 3",
                                     NewRegion == 11 ~ "WA zones 4 and 5" )) 


outfile <- paste0(outPath, "/", "RegionMappings.csv")
write.csv(outCells_df, file=outfile, row.names=FALSE)
