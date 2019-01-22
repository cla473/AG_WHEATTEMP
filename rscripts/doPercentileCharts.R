rm(list = ls())
library(tidyverse)
library(sf)
library(ggplot2)
library(viridis)
library(rvest)
library(ggrepel)
library(RColorBrewer)


filePath <- '/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/metCalcs'
mapPath <- '/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/maps'
dataFile <- paste0(filePath, "/", "percentiles.csv")

all_df <- read_csv(dataFile)
str(all_df)

# The values in the file are listed as Location, Long, Lat, followed by:
#   Frost Values: LFrost_70P, LFrost_80P, LFrost_90P, 
#                 LFrost_Trend
#   Heat Values:  FHeat_10P, FHeat_20P, FHeat_30P, FHeat_40P, FHeat_50P
#                 FHeat_Trend
#   Flowering Windows: FWL1, FWL2, FWL3, FWL4, FWL5
#   Differences:   DIFFH1020, DIFFH1030, DIFFH1040, DIFFH1050
#                  Filtered where Diff <= 40
#                  DIFFH1020_Max40, DIFFH1030_Max40, DIFFH1040_Max40, DIFFH1050_Max40
valueOfInterest <- "FHeat_Trend"


#these are the default values
filterlimit <- 0
outfile <- paste0(mapPath, "/", valueOfInterest, ".png")

ChartType <- ""
HasMaxLimit <- FALSE
if (grepl("Trend", valueOfInterest) == TRUE) {
    #not sure what we do here
    ChartType <- "TREND"
} else if (grepl("Frost", valueOfInterest) == TRUE) {
    ChartType <- "FROST"
    #LFrost_70P, LFrost_80P, LFrost_90P
    filterlimit <- 203 #(182 + 21 (approx 10%))
} else if (grepl("Heat", valueOfInterest) == TRUE) {
    ChartType <- "HEAT"
    #FHeat_10P, FHeat_20P, FHeat_30P, FHeat_40P, FHeat_50P
    filterlimit <- 342 #(365 - 23 (approx 10%))
} else if (grepl("FWL", valueOfInterest) == TRUE) {
    #FWL1, FWL2, FWL3, FWL4, FWL5
    ChartType <- "FWL"
} else if (grepl("DIFF", valueOfInterest) == TRUE) {
    #DIFFH1020, DIFFH1030, DIFFH1040, DIFFH1050
    #These have been filtered for only those with DIFF value <= 40
    #DIFFH1020_40, DIFFH1030_40, DIFFH1040_40, DIFFH1050_40  
    ChartType <- "DIFF"
    if (grepl("_Max40", valueOfInterest) == TRUE) {
        HasMaxLimit <- TRUE
        filterlimit <- 40
    }
}
print(paste0("ChartType is ", ChartType))

#---------------------------------------------------------------------------------
# define the title for the chart
chartSubTitle <- ""
if (valueOfInterest == "LFrost_70P") { 
    chartSubTitle <- "Date of Last Frost Day (< 0°C) - 70th Percentile" 
} else if (valueOfInterest == "LFrost_80P" ) {
    chartSubTitle <- "Date of Last Frost Day (< 0°C) - 80th Percentile" 
} else if (valueOfInterest == "LFrost_90P") { 
    chartSubTitle <- "Date of Last Frost Day (< 0°C) - 90th Percentile" 

} else if (valueOfInterest == "FHeat_10P") { 
    chartSubTitle <- "Date of First Heat Day (>= 32°C) - 10th Percentile" 
} else if (valueOfInterest == "FHeat_20P") { 
    chartSubTitle <- "Date of First Heat Day (>= 32°C) - 20th Percentile" 
} else if (valueOfInterest == "FHeat_30P") { 
    chartSubTitle <- "Date of First Heat Day (>= 32°C) - 30th Percentile" 
} else if (valueOfInterest == "FHeat_40P") { 
    chartSubTitle <- "Date of First Heat Day (>= 32°C) - 40th Percentile" 
} else if (valueOfInterest == "FHeat_50P") { 
    chartSubTitle <- "Date of First Heat Day (>= 32°C) - 50th Percentile" 

} else if (valueOfInterest == "FWL1") { 
    chartSubTitle <- "Flowering window (Days) between 90th Frost percentile and 10th Heat percentile" 
} else if (valueOfInterest == "FWL2") { 
    chartSubTitle <- "Flowering window (Days) between 90th Frost percentile and 20th Heat percentile" 
} else if (valueOfInterest == "FWL3") { 
    chartSubTitle <- "Flowering window (Days) between 90th Frost percentile and 30th Heat percentile" 
} else if (valueOfInterest == "FWL4") { 
    chartSubTitle <- "Flowering window (Days) between 90th Frost percentile and 40th Heat percentile" 
} else if (valueOfInterest == "FWL5") { 
    chartSubTitle <- "Flowering window (Days) between 90th Frost percentile and 50th Heat percentile" 

} else if (valueOfInterest == "DIFFH1020" || valueOfInterest == "DIFFH1020_Max40") { 
    chartSubTitle <- "Number of days between 10th and 20th percentile for Heat" 
} else if (valueOfInterest == "DIFFH1030" || valueOfInterest == "DIFFH1030_Max40") { 
    chartSubTitle <- "Number of days between 10th and 30th percentile for Heat" 
} else if (valueOfInterest == "DIFFH1040" || valueOfInterest == "DIFFH1040_Max40") { 
    chartSubTitle <- "Number of days between 10th and 40th percentile for Heat" 
} else if (valueOfInterest == "DIFFH1050" || valueOfInterest == "DIFFH1050_Max40") {
    chartSubTitle <- "Number of days between 10th and 50th percentile for Heat"
    
} else if (valueOfInterest == "LFrost_Trend") {
    chartSubTitle <- expression("Trend of Last Frost day (day "*y^"-1"*")")
} else if (valueOfInterest == "FHeat_Trend") {
    chartSubTitle <- expression("Trend of First Heat day (day "*y^"-1"*")")
}
print(paste0("Chart SubTitle is ", chartSubTitle))

#---------------------------------------------------------------------------------
# rename the column we are interested in
if (valueOfInterest == "LFrost_70P") { 
    all_df2 <- all_df %>% rename(dayOfYear = LFrost_70P)
} else if (valueOfInterest == "LFrost_80P") { 
    all_df2 <- all_df %>% rename(dayOfYear = LFrost_80P)
} else if (valueOfInterest == "LFrost_90P") { 
    all_df2 <- all_df %>% rename(dayOfYear = LFrost_90P)

} else if (valueOfInterest == "FHeat_10P") { 
    all_df2 <- all_df %>% rename(dayOfYear = FHeat_10P)
} else if (valueOfInterest == "FHeat_20P") { 
    all_df2 <- all_df %>% rename(dayOfYear = FHeat_20P)
} else if (valueOfInterest == "FHeat_30P") { 
    all_df2 <- all_df %>% rename(dayOfYear = FHeat_30P)
} else if (valueOfInterest == "FHeat_40P") { 
    all_df2 <- all_df %>% rename(dayOfYear = FHeat_40P)
} else if (valueOfInterest == "FHeat_50P") { 
    all_df2 <- all_df %>% rename(dayOfYear = FHeat_50P)

} else if (valueOfInterest == "FWL1") { 
    all_df2 <- all_df %>% rename(dayOfYear = FWL1)
} else if (valueOfInterest == "FWL2") { 
    all_df2 <- all_df %>% rename(dayOfYear = FWL2)
} else if (valueOfInterest == "FWL3") { 
    all_df2 <- all_df %>% rename(dayOfYear = FWL3)
} else if (valueOfInterest == "FWL4") { 
    all_df2 <- all_df %>% rename(dayOfYear = FWL4)
} else if (valueOfInterest == "FWL5") { 
    all_df2 <- all_df %>% rename(dayOfYear = FWL5)
           
} else if (valueOfInterest == "DIFFH1020" || valueOfInterest == "DIFFH1020_Max40") { 
    all_df2 <- all_df %>% rename(dayOfYear = DIFFH1020)
} else if (valueOfInterest == "DIFFH1030" || valueOfInterest == "DIFFH1030_Max40") { 
    all_df2 <- all_df %>% rename(dayOfYear = DIFFH1030)
} else if (valueOfInterest == "DIFFH1040" || valueOfInterest == "DIFFH1040_Max40") { 
    all_df2 <- all_df %>% rename(dayOfYear = DIFFH1040)
} else if (valueOfInterest == "DIFFH1050" || valueOfInterest == "DIFFH1050_Max40") { 
    all_df2 <- all_df %>% rename(dayOfYear = DIFFH1050)

} else if (valueOfInterest == "LFrost_Trend") { 
    all_df2 <- all_df %>% rename(dayOfYear = LFrost_slope)
} else if (valueOfInterest == "FHeat_Trend") { 
    all_df2 <- all_df %>% rename(dayOfYear = FHeat_slope)
}


# select only the columns we require, and calculate the dates that we want
# and identify Tasmanians sites, so that they can be added back in later

all_df2 <- all_df2 %>% 
    mutate(IsTas = ifelse((Lat < -40 & Lat > -45), "Y", "")) 
    
if (ChartType == "TREND") {
    if (valueOfInterest == "LFrost_Trend") {
        all_df2 <- all_df2 %>% select(Long, Lat, dayOfYear, IsTas, LFrost_ttests)
    } else {
        all_df2 <- all_df2 %>% select(Long, Lat, dayOfYear, IsTas, FHeat_ttests)
    }
} else {
    all_df2 <- all_df2 %>% select(Long, Lat, dayOfYear, IsTas)
}

if (ChartType == "FROST") {
    minData_df <- all_df2 %>% filter(dayOfYear <= filterlimit) 
    mainData_df <- all_df2 %>% filter(dayOfYear > filterlimit) 
} else if (ChartType == "HEAT") {
    minData_df <- all_df2 %>% filter(dayOfYear >= filterlimit) 
    mainData_df <- all_df2 %>% filter(dayOfYear < filterlimit) 
} else if (HasMaxLimit == TRUE) {
    #this currently only applies to DIFF...
    mainData_df <- all_df2 %>% filter(dayOfYear <= filterlimit) 
} else if (valueOfInterest == "LFrost_Trend") {
    minData_df <- all_df2 %>% 
        filter(is.na(LFrost_ttests) == TRUE || LFrost_ttests > 0.10) %>% 
        select(Long, Lat, dayOfYear, IsTas)
    mainData_df <- all_df2 %>% 
        filter(LFrost_ttests <= 0.10) %>% 
        select(Long, Lat, dayOfYear, IsTas)
} else if (valueOfInterest == "FHeat_Trend") {
    minData_df <- all_df2 %>% 
        filter(is.na(FHeat_ttests) == TRUE || FHeat_ttests > 0.10) %>% 
        select(Long, Lat, dayOfYear, IsTas)
    mainData_df <- all_df2 %>% 
        filter(FHeat_ttests <= 0.10) %>% 
        select(Long, Lat, dayOfYear, IsTas)
} else {
    mainData_df <- all_df2
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

if (ChartType == "HEAT" || ChartType == "FROST" || ChartType == "TREND") {
    print("chart type is HEAT or FROST or TREND")
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

#now prepare the main part of the chart
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

#----------------------------------
# Show the RColorBrewer palette
#display.brewer.all()
#----------------------------------

if (ChartType == "FROST") {
    colPalette <- colorRampPalette(rev(brewer.pal(9, "YlGnBu")))
} else if (ChartType == "HEAT") {
    colPalette <- colorRampPalette(rev(brewer.pal(9, "YlOrRd")))
} else if (ChartType == "FWL") {
    colPalette <- colorRampPalette(brewer.pal(9, "YlGnBu"))
} else if (ChartType == "DIFF") {
    colPalette <- colorRampPalette(brewer.pal(9, "BuPu"))
} else if (ChartType == "TREND") {
    #This has 11 colours in it ... do we need them all ... and how do we show blocks instead of gradient
    colPalette <- colorRampPalette(rev(brewer.pal(10, "RdYlBu")))
}

#=================================================================
# THIS IS THE CHART THAT TAKES AGES TO RENDER
p <- ggplot() + 
    geom_sf(data=aus_mapped) 

if (ChartType == "HEAT" || ChartType == "FROST" || ChartType="TREND") {
    print("Add in addition -grey- points for HEAT and FROST charts")
    p <- p + geom_sf(data=minData_points_sf, colour="grey", size=0.6, show.legend="point") 
}

p <- p + geom_sf(data=mainData_points_sf, aes(colour=dayOfYear), size=0.6, alpha=0.7, show.legend="point") 

minLimit <- min(mainData_df$dayOfYear)
maxLimit <- max(mainData_df$dayOfYear)
uniqueValues <- length(unique(mainData_df$dayOfYear))
print(paste0("Min value: ", minLimit, ", Max value: ", maxLimit, ", Unique values: ", uniqueValues))

if (ChartType == "FWL") {
    p <- p + scale_colour_gradientn(colours=colPalette(10), limits=c(0, 183))
} else if (ChartType == "DIFF") {
    if (HasMaxLimit == TRUE) {
        p <- p + scale_colour_gradientn(colours=colPalette(10), limits=c(0, 40))
    } else {
        p <- p + scale_colour_gradientn(colours=colPalette(10), limits=c(0, 60))
    }
} else if (ChartType == "TREND") {
    #p <- p + scale_colour_gradientn(colours=colPalette(10), limits=c(-0.5, 0.5))
    if (valueOfInterest == "LFrost_Trend") {
        p <- p + scale_colour_gradientn(colours=colPalette(10), limits=c(-1.4, 1.4))
    } else {
        p <- p + scale_colour_gradientn(colours=colPalette(10), limits=c(-0.7, 0.7))
    }    
} else {
    #It is a Heat OR Frost Chart
    p <- p + scale_colour_gradientn(colours=colPalette(10), limits=c(182, 365), 
                                    breaks=c(182, 213, 244, 274, 305, 335),
                                    labels=c("1-Jul", "1-Aug", "1-Sept", "1-Oct", "1-Nov", "1-Dec")) 
}

p <- p + coord_sf(xlim=c(112, 156), ylim=c(-10, -45)) +
    labs(x="Longitude", y="Latitude", title="Australia - 1957 to 2017", subtitle=chartSubTitle) + 
    theme_bw() 

legendKeyWidth <- 2.5 
if (ChartType == "FWL" || ChartType == "DIFF" || ChartType == "TREND") {
    legendKeyWidth <- 2.0
}
print(paste0("legendKeyWidth is ", legendKeyWidth))

p <- p + theme(legend.position="bottom", 
               legend.direction="horizontal", 
               legend.title=element_blank(), 
               legend.key.width = unit(legendKeyWidth, "cm"),
               legend.key.size = unit(0.4, "cm"), 
               legend.spacing.y = unit(0.3, "cm"))  

print("Displaying Output")
p

#=================================================================
# SAVING TAKES ALMOST AS LONG AS RENDERING
ggsave(outfile, width=7, height=6, units="in")
print(paste0("File Saved as ", outfile))

#}


# The values in the file are listed as Location, Long, Lat, followed by:
#   Frost Values: LFrost_70P, LFrost_80P, LFrost_90P
#   Heat Values:  FHeat_10P, FHeat_20P, FHeat_30P, FHeat_40P, FHeat_50P
#   Flowering Windows: FWL1, FWL2, FWL3, FWL4, FWL5
#   Differences:   DIFFH1020, DIFFH1030, DIFFH1040, DIFFH1050
#valueOfInterest <- "DIFFH1050"
#generateChart(all_df, filePath, mapPath, dataFile, valueOfInterest) 





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
#p3 <- ggplot(df, aes(x, y)) +
#    geom_point(aes(colour=z1)) +
#    scale_color_gradientn(colours = terrain.colors(10),
#                          breaks=c(1.5, 1, 0.5, 0, -0.5, -1, -1.5, -2),
#                          labels=c("1-Jul", "1-Aug", "1-Sept", "1-Oct", "1-Nov", "1-Dec", "1-Jan", "1-Feb"),
#                          guide=guide_colourbar(reverse = TRUE))
#
#p3 <- p3 + theme(legend.position="bottom", legend.direction="horizontal", legend.title=element_blank(),
#           legend.key.width = unit(2.0, "cm"),legend.key.size = unit(0.4, "cm"), legend.spacing.y = unit(0.3, "cm"))
#p3
# #----------------------------------------------


