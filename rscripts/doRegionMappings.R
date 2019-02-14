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
aegicRegionfile <- "/OSM/CBR/AG_WHEATTEMP/work/GIS_data/GRDC_Regions/GRDC_Regions.shp"
mapPath <- '/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/maps'
outPath <- '/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/metCalcs'

outfile <- paste0(mapPath, "/", "mapRegionsNEW", ".png")


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


#get the Aegic Regions file
print(paste0("region file: ", aegicRegionfile))
aegic_gdf <- st_read(aegicRegionfile, quiet = TRUE)
as.tibble(aegic_gdf)
aegic_gdf <- aegic_gdf %>% 
    rename(WheatRegion = AGECO_ZONE)


# Retrieve the Australia shape file
aus_mapped <- st_read(australiafile, quiet = TRUE)
as.tibble(aus_mapped)

# need to do this as there is no CRS information with the AUS shape files, nor our data 
st_crs(aus_mapped) <- st_crs(regions_gdf)
st_crs(aegic_gdf) <- st_crs(regions_gdf)
st_crs(cells_df) <- st_crs(regions_gdf)

# filter all sites by the 22 regions (and add back in Tasmania)
pcheck <- cells_df$geometry %>% 
    st_intersects(regions_gdf$geometry) %>% 
    map_lgl(function(x) length(x) > 0)

pcheck <- as.data.frame(pcheck)
cells_df$InRegion <- pcheck
# ADD BACK IN TASMANIA
cells_df[cells_df$WheatRegion == "TAS", "InRegion"] <- TRUE

# FILTER BASED ON THE REGIONS
cells_df <- cells_df[cells_df$InRegion==TRUE,]

#check that we still have TAS
unique(cells_df$WheatRegion)

#------------------------------------------------------------
# now we need to determine which cells are in which region
addRegiontoData <- function(cells_df, reg_df, wheatRegion) {
    
    #reg_df <- regions_gdf
    #reg_df <- aegic_gdf
    #wheatRegion <- "WA Northern"
    reg_df <- reg_df[reg_df$WheatRegion == wheatRegion,]

    pcheck <- cells_df$geometry %>% 
        st_intersects(reg_df$geometry) %>% 
        map_lgl(function(x) length(x) > 0)
    
    pcheck <- as.data.frame(pcheck)
    cells_df$InRegion <- pcheck
    # update Region base on pcheck
    cells_df[cells_df$InRegion==TRUE, "WheatRegion"] <- wheatRegion
    
    return(cells_df)
}
#------------------------------------------------------------


# Need to identify the WA Regions that are in the Aegic Region for Northern WA, so
# We will do these first
#NSW1   NSW2   NSW3   NSW5   NSW6   QLD1   QLD2   QLD3   QLD4   SA1    SA1bis SA2    SA2bis SA3    SA3bis VIC1  
#VIC2   WA1    WA2    WA3    WA4    WA5   
#TAS


unique(regions_gdf$WheatRegion)

regions <- as.data.frame(regions_gdf) %>% 
    select(WheatRegion, WheatRegionDesc) %>% 
    distinct()

#apply the above function to all of the data
#r <- 1
for (row in 1:nrow(regions)) {
    regionToApply <- as.character(regions[row, "WheatRegion"])
    cells_df <- addRegiontoData(cells_df, regions_gdf, regionToApply)
}

#check that we still have TAS
unique(cells_df$WheatRegion)


#lets just look at WA Regions
unique(aegic_gdf$WheatRegion)

# identify the new WA Region
# Steps for re-arranging WA
# Any sites in the Aegic region will be come region 1 - WA Northern
# Remaining WA2 and WA3 as WA2 and becomes WA Central
# remains of WA4 will stay as WA4 and will become WA Eastern
# WA5 remains the same but with a new name "WA Mallee"

#wheatRegion <- "WA Northern"
cells_df <- addRegiontoData(cells_df, aegic_gdf, "WA Northern")

cells_df <- cells_df %>% 
    select(-InRegion) %>% 
    mutate(WheatRegion = ifelse(WheatRegion == "WA3", "WA2", WheatRegion))

#wa_cells <- cells_df %>% 
#    filter(str_detect(WheatRegion, "WA"))

#lets just look at WA Regions
unique(wa_cells$WheatRegion)

p <- ggplot() + 
    geom_sf(data=aus_mapped) +
    geom_sf(data=wa_cells, aes(colour=WheatRegion), size=0.6, alpha=0.7, show.legend="point") +
    coord_sf(xlim=c(112, 128), ylim=c(-22, -38)) +
    labs(x="Longitude", y="Latitude", title="WA revised Regions") 
p
outfileWA <- paste0(mapPath, "/", "mapRegionsWA", ".png")
ggsave(outfileWA, width=7, height=6, units="in")



#Make sure the new WA Region has a standard name, it is identifable in the original region file
cells_df <- cells_df %>% 
    mutate(WheatRegion = ifelse(WheatRegion == "WA Northern", "WA1", WheatRegion))


# map this to test if it has worked
p <- ggplot() + 
    geom_sf(data=aus_mapped) +
    geom_sf(data=cells_df, aes(colour=WheatRegion), size=0.6, alpha=0.7, show.legend="point") +
    labs(x="Longitude", y="Latitude", title="Revised Wheat Regions") 

ggsave(outfile, width=7, height=6, units="in")


# now merge the regions dataframe with our data frame, to add the WheatRegionDesc
regions <- regions %>% 
    mutate(WheatRegion = as.character(WheatRegion))

cells_df <- cells_df %>% 
    left_join(regions, by="WheatRegion")

cells_df <- cells_df %>% 
    mutate(NewRegion = case_when(WheatRegion == "QLD1" ~ 1, 
                                 WheatRegion == "QLD2" ~ 2, 
                                 WheatRegion == "NSW3" ~ 3, 
                                 WheatRegion == "NSW1" ~ 4, 
                                 WheatRegion == "QLD3" ~ 4, 
                                 WheatRegion == "QLD4" ~ 4, 
                                 WheatRegion == "NSW2" ~ 5, 
                                 WheatRegion == "NSW5" ~ 6, 
                                 WheatRegion == "NSW6" ~ 6, 
                                 WheatRegion == "VIC1" ~ 7, 
                                 WheatRegion == "SA3bis" ~ 8, 
                                 WheatRegion == "VIC2" ~ 8, 
                                 WheatRegion == "SA1" ~ 9, 
                                 WheatRegion == "SA2bis" ~ 9, 
                                 WheatRegion == "SA3" ~ 9, 
                                 WheatRegion == "SA1bis" ~ 10, 
                                 WheatRegion == "SA2" ~ 10, 
                                 WheatRegion == "WA1" ~ 11, 
                                 WheatRegion == "WA4" ~ 12, 
                                 WheatRegion == "WA2" ~ 13, 
                                 WheatRegion == "WA3" ~ 13, 
                                 WheatRegion == "WA5" ~ 14, 
                                 WheatRegion == "TAS" ~ 15  )) 


# Need to create a file with the NewRegion mapped with the cultivars/maturity.
cells_df <- cells_df %>% 
    mutate(NewRegionDesc = case_when(NewRegion == 1 ~ "Qld Central", 
                                     NewRegion == 2 ~ "QLD South East",    
                                     NewRegion == 3 ~ "NSW East",    
                                     NewRegion == 4 ~ "QLD South West - NSW North West",
                                     NewRegion == 5 ~ "NSW West",
                                     NewRegion == 6 ~ "NSW South",
                                     NewRegion == 7 ~ "Wimmera",
                                     NewRegion == 8 ~ "SA VIC Mallee",
                                     NewRegion == 9 ~ "SA Mallee",
                                     NewRegion == 10 ~ "SA Lower Eyre and Yorke Peninsula",
                                     NewRegion == 11 ~ "WA North",
                                     NewRegion == 12 ~ "WA East",
                                     NewRegion == 13 ~ "WA Central",
                                     NewRegion == 14 ~ "WA Mallee",
                                     NewRegion == 15 ~ "Tasmania" )) 

#check that we still have TAS
unique(cells_df$NewRegionDesc)

cells_df$NewRegionDesc <- as.factor(cells_df$NewRegionDesc)

getPalette = colorRampPalette(brewer.pal(12, "Paired"))

# map this to test if it has worked
p <- ggplot() + 
    geom_sf(data=aus_mapped) +
    geom_sf(data=cells_df, aes(colour=NewRegionDesc), size=0.5, alpha=0.7, show.legend="point") +
    scale_fill_brewer(palette = "Paired") +
    labs(x="Longitude", y="Latitude", title="Revised Wheat Regions using WA Aegic & TAS") 

ggsave(outfile, width=7, height=6, units="in")

#change the size of the points
#re-order the Legend and change its title

palette()
newCols <- c("black", "red", "green3", "blue", "cyan", "magenta", "yellow", "purple", "brown",
        "springgreen1", "wheat2", "orange1", "lightblue1", "palegreen1", "pink2")
palette(newCols)

brewer.pal(n=12, name="Paired")
brewer.pal(n=11, name="PiYG")
newCols <- c("#A6CEE3", "#1F78B4", "#053061", "#B2DF8A", "#33A02C", "#276419", "#FB9A99", "#E31A1C", 
  "#FDBF6F", "#FF7F00", "#CAB2D6", "#C51B7D", "#6A3D9A", "#FFFF99", "#B15928")

levels(cells_df$NewRegionDesc)

newlevels <- c("Qld Central", "QLD South East", "NSW East", "QLD South West - NSW North West",
               "NSW West", "NSW South", "Wimmera", "SA VIC Mallee", "SA Mallee", 
               "SA Lower Eyre and Yorke Peninsula", "WA North", "WA East", "WA Central",
               "WA Mallee", "Tasmania")

cells_df <- cells_df %>% 
    rename(Region = NewRegionDesc)

cells_df$Region <- factor(cells_df$Region, levels = newlevels)


p <- ggplot() + 
    geom_sf(data=aus_mapped) +
    geom_sf(data=cells_df, aes(colour=Region), size=0.5, alpha=0.7, show.legend="point") +
    scale_color_manual(values = newCols) +
    labs(x="Longitude", y="Latitude", title="Revised Wheat Regions using WA Aegic & TAS") 

outfile <- paste0(mapPath, "/", "mapRegionsNEWColours", ".png")
ggsave(outfile, width=7, height=6, units="in")



outCells_df <- as.data.frame(cells_df) %>% 
    select(Location, WheatRegion, WheatRegionDesc) 

outfileCsv <- paste0(outPath, "/", "RegionMappings.csv")
write.csv(outCells_df, file=outfileCsv, row.names=FALSE)
