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


generateChart <- function(all_df, valueOfInterest, filePath, mapPath) {

    #these are the default values
    chartSubTitle <- "chart SubTitle"
    filterlimit <- 0
    outfile <- paste0(mapPath, "/", valueOfInterest, ".png")

    isHeatFrost <- FALSE
    if (grepl(valueOfInterest, "Frost") == TRUE) {
        isHeatFrost <- TRUE
        #"LFrost_70P", "LFrost_80P", "LFrost_90P"
        filterlimit <- 203 #(182 + 21 (approx 10%))
        colPalette <- colorRampPalette(rev(brewer.pal(9, "YlGnBu")))
    }
    else if (grepl(valueOfInterest, "Heat") == TRUE) {
        isHeatFrost <- TRUE
        #"FHeat_10P", "FHeat_20P", "FHeat_30P", "FHeat_40P", "FHeat_50P"
        filterlimit <- 342 #(365 - 23 (approx 10%))
        colPalette <- colorRampPalette(rev(brewer.pal(9, "YlOrRd")))
    }
    else if (grepl(valueOfInterest, "FWL") == TRUE) {
        #"FWL1", "FWL2", "FWL3", "FWL4", "FWL5"
    }
    else if (grepl(valueOfInterest, "DIFF") == TRUE) {
        #"DIFFH1020", "DIFFH1030", "DIFFH1040", "DIFFH1050"
    }
    

    switch(valueOfInterest,
           # "LFrost_70P, LFrost_80P, LFrost_90P"
           "LFrost_70P" = { 
               chartSubTitle <- "Date of Last Frost Day (< 0°C) - 70th Percentile" 
               all_df <- all_df %>% rename(dayOfYear = LFrost_70P)
               },
           "LFrost_80P" = { 
               chartSubTitle <- "Date of Last Frost Day (< 0°C) - 80th Percentile" 
               all_df <- all_df %>% rename(dayOfYear = LFrost_80P)
               },
           "LFrost_90P" = { 
               chartSubTitle <- "Date of Last Frost Day (< 0°C) - 90th Percentile" 
               all_df <- all_df %>% rename(dayOfYear = LFrost_90P)
               },

           # "FHeat_10P, FHeat_20P, FHeat_30P, FHeat_40P, FHeat_50P"
           "FHeat_10P" = { 
               chartSubTitle <- "Date of First Heat Day (>= 32°C) - 10th Percentile" 
               all_df <- all_df %>% rename(dayOfYear = FHeat_10P)
               },
           "FHeat_20P" = { 
               chartSubTitle <- "Date of First Heat Day (>= 32°C) - 20th Percentile" 
               all_df <- all_df %>% rename(dayOfYear = FHeat_20P)
               },
           "FHeat_30P" = { 
               chartSubTitle <- "Date of First Heat Day (>= 32°C) - 30th Percentile" 
               all_df <- all_df %>% rename(dayOfYear = FHeat_30P)
               },
           "FHeat_40P" = { 
               chartSubTitle <- "Date of First Heat Day (>= 32°C) - 40th Percentile" 
               all_df <- all_df %>% rename(dayOfYear = FHeat_40P)
               },
           "FHeat_50P" = { 
               chartSubTitle <- "Date of First Heat Day (>= 32°C) - 50th Percentile" 
               all_df <- all_df %>% rename(dayOfYear = FHeat_50P)
               },

           # "FWL1, FWL2, FWL3, FWL4, FWL5"
           "FWL1" = { 
               chartSubTitle <- "Length of the flowering window between 90th Frost percentile and 10th Heat percentile" 
               all_df <- all_df %>% rename(dayOfYear = FWL1)
               },
           "FWL2" = { 
               chartSubTitle <- "Length of the flowering window between 90th Frost percentile and 20th Heat percentile" 
               all_df <- all_df %>% rename(dayOfYear = FWL2)
               },
           "FWL3" = { 
               chartSubTitle <- "Length of the flowering window between 90th Frost percentile and 30th Heat percentile" 
               all_df <- all_df %>% rename(dayOfYear = FWL3)
               },
           "FWL4" = { 
               chartSubTitle <- "Length of the flowering window between 90th Frost percentile and 40th Heat percentile" 
               all_df <- all_df %>% rename(dayOfYear = FWL4)
               },
           "FWL5" = { 
               chartSubTitle <- "Length of the flowering window between 90th Frost percentile and 50th Heat percentile" 
               all_df <- all_df %>% rename(dayOfYear = FWL5)
               },
           
           # "DIFFH1020, DIFFH1030, DIFFH1040, DIFFH1050"
           "DIFFH1020" = { 
               chartSubTitle <- "Number of days between 10th and 20th percentile for Heat" 
               all_df <- all_df %>% rename(dayOfYear = DIFFH1020)
               },
           "DIFFH1030" = { 
               chartSubTitle <- "Number of days between 10th and 30th percentile for Heat" 
               all_df <- all_df %>% rename(dayOfYear = DIFFH1030)
               },
           "DIFFH1040" = { 
               chartSubTitle <- "Number of days between 10th and 40th percentile for Heat" 
               all_df <- all_df %>% rename(dayOfYear = DIFFH1040)
               },
           "DIFFH1050" = { 
               chartSubTitle <- "Number of days between 10th and 50th percentile for Heat" 
               all_df <- all_df %>% rename(dayOfYear = DIFFH1050)
               }
    )

    # select only the columns we require, and calculate the dates that we want
    # and identify Tasmanians sites, so that they can be added back in later
    all_df <- all_df %>% 
        mutate(IsTas = ifelse((Lat < -40 & Lat > -45), "Y", ""))
    
    if (grepl(valueOfInterest, "Frost") == TRUE) {
        minData_df <- all_df %>% 
            select(Long, Lat, dayOfYear) %>% 
            filter(dayOfYear <= filterlimit) 
        
        mainData_df <- all_df %>% 
            select(Long, Lat, dayOfYear) %>% 
            filter(dayOfYear > filterlimit) 
    }
    else if (grepl(valueOfInterest, "Heat") == TRUE) {
        minData_df <- all_df %>%  
            select(Long, Lat, dayOfYear) %>% 
            filter(dayOfYear >= filterlimit) 
        
        mainData_df <- all_df %>% 
            select(Long, Lat, dayOfYear) %>% 
            filter(dayOfYear < filterlimit) 
    }
    else {
        mainData_df <- all_df %>% 
            select(Long, Lat, dayOfYear) 
    }

    #now need to graph the requuired Value
    # 1. Separate out those sites with that are in the 10% at the start of the Frost range,  
    #    or 10% at the end Heat range, and graph them as gray
    # 2. graph the remaing values with colour scales

    # Retrieve the Region Shape File
    source_shapefile_dir <- '/OSM/CBR/AG_WHEATTEMP/work/GIS_data'
    regionfile <- paste0(source_shapefile_dir, "/wheat_22regions_KC/wheat_22regions_KC.shp")
    #print(paste0("region file: ", regionfile))
    
    regions_mapped <- st_read(regionfile, quiet = TRUE)
    as.tibble(regions_mapped)
    
    # Retrieve the Australia shape file
    australiafile <- paste0(source_shapefile_dir, "/states/aust_cd66states.shp")
    aus_mapped <- st_read(australiafile, quiet = TRUE)
    as.tibble(aus_mapped)
    
    # need to do this as there is no CRS information with the AUS shape files
    st_crs(aus_mapped) <- st_crs(regions_mapped)
    
    if (isHeatFrost == TRUE) {
    
        # get the coordintaes and format as SpatialPoints
        coords <- select(minData_df, Long, Lat)
        
        # Create sf object with all_df data frame and CRS
        minData_points_sf <- st_as_sf(minData_df, coords = c("Long", "Lat"), crs = 4326)
        st_geometry(minData_points_sf)
        
        # need to do this as there is no CRS information with the AUS shape files
        st_crs(minData_points_sf) <- st_crs(regions_mapped)
        
        # Need to eliminate any points that are not within the regions
        pcheck <- minData_points_sf$geometry %>% 
            st_intersects(regions_mapped$geometry) %>% 
            map_lgl(function(x) length(x) > 0)
        
        pcheck <- as.data.frame(pcheck)
        minData_points_sf$InRegion <- pcheck
        #ADD BACK IN TASMANIA
        minData_points_sf[minData_points_sf$IsTas == "Y", "InRegion"] <- TRUE
        
        #FILTER BASED ON THE REGIONS
        minData_points_sf <- minData_points_sf[minData_points_sf$InRegion==TRUE,]
    }
    #now prepare the Frost_df
    # get the coordintaes and format as SpatialPoints
    coords <- select(mainData_df, Long, Lat)
    
    # Create sf object with all_df data frame and CRS
    mainData_points_sf <- st_as_sf(mainData_df, coords = c("Long", "Lat"), crs = 4326)
    st_geometry(mainData_points_sf)
    
    # need to do this as there is no CRS information with the AUS shape files
    st_crs(mainData_points_sf) <- st_crs(regions_mapped)
    
    # Need to eliminate any points that are not within the regions
    pcheck <- mainData_points_sf$geometry %>% 
        st_intersects(regions_mapped$geometry) %>% 
        map_lgl(function(x) length(x) > 0)
    
    pcheck <- as.data.frame(pcheck)
    mainData_points_sf$InRegion <- pcheck
    #ADD BACK IN TASMANIA
    mainData_points_sf[mainData_points_sf$IsTas == "Y", "InRegion"] <- TRUE
    
    #FILTER BASED ON THE REGIONS
    mainData_points_sf <- mainData_points_sf[mainData_points_sf$InRegion==TRUE,]

    #=================================================================
    # THIS IS THE CHART THAT TAKES AGES TO RENDER
    p <- ggplot() + 
        geom_sf(data=aus_mapped) 
    
    if (isHeatFrost == TRUE) {
        p <- p + geom_sf(data=minData_points_sf, colour="grey", size=0.6, show.legend="point") 
    }
    
    p <- p + geom_sf(data=mainData_points_sf, aes(colour=dayOfYear), size=0.6, alpha=0.7, show.legend="point") +
        scale_colour_gradientn(colours=colPalette(10), limits=c(182, 365), 
                               breaks=c(182, 213, 244, 274, 305, 335),
                               labels=c("1-Jul", "1-Aug", "1-Sept", "1-Oct", "1-Nov", "1-Dec")) +
        coord_sf(xlim=c(112, 156), ylim=c(-10, -45)) +
        labs(x="Longitude", y="Latitude", title="Australia - 1957 to 2017", 
             subtitle=chartSubTitle) + 
        theme_bw() +
        theme(legend.position="bottom", legend.direction="horizontal", legend.title=element_blank(),
              legend.key.width = unit(3.0, "cm"),
              legend.key.size = unit(0.4, "cm"), legend.spacing.y = unit(0.3, "cm"))  
    p
    
    #=================================================================
    # SAVING TAKES ALMOST AS LONG AS RENDERING
    ggsave(outfile, width=7, height=6, units="in")
}    

# "LFrost_70P, LFrost_80P, LFrost_90P"
# "FHeat_10P, FHeat_20P, FHeat_30P, FHeat_40P, FHeat_50P"
# "FWL1, FWL2, FWL3, FWL4, FWL5"
# "DIFFH1020, DIFFH1030, DIFFH1040, DIFFH1050"
generateChart(all_df, "LFrost_90P", filePath, mapPath) 



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


