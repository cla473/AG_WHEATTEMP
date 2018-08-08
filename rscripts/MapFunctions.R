#library(grid)
library(maptools)
#Checking rgeos availability: FALSE
#        Note: when rgeos is not available, polygon geometry 	computations in maptools depend on gpclib,
#        which has a restricted licence. It is disabled by default;
#        to enable gpclib, type gpclibPermit()


#Error in getinfo.shape(fn) : Error opening SHP file
#In addition: Warning message:
#    readShapeSpatial is deprecated; use rgdal::readOGR or sf::st_read 


#set up palettes
cbPalette <- c("#a6cee3", "#ffffb3", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", "#ffff99")
cbPalette <- c(cbPalette, "#8dd3c7", "#1f78b4", "#bebada", "#fb8072", "#80b1d3", "#fdb462", "#b3de69", "#fccde5", "#d9d9d9", "#bc80bd", "#ccebc5")
pal <- c("blue", "green","yellow","red")

#theme to remove axis and grid
theme_bare <- theme(
    axis.line = element_blank(), 
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(),
    axis.ticks = element_blank(), 
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    axis.ticks.margin = unit(c(0,0,0,0), "lines"), 
    panel.background = element_rect(fill = "white"), 
    panel.border = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.margin = unit(c(0,0,0,0), "lines"), 
    plot.background = element_rect(fill = "white"),
    plot.margin = unit(c(0,0,0,0), "lines")
)

baseImageDir <- "\\\\fsqld4-bne.it.csiro.au\\PI-Share1\\Plants_and_temperature\\Fernanda\\Temperature 2017\\Climate\\GIS data"


#load the map
AusMap <- function() {
    aus <- readShapeSpatial(paste0(baseImageDir, "\\states\\aust_cd66states.shp"))
    aus@data$id <- rownames(aus@data)
    ausPoints <- fortify(aus, region = "id")
    ausDF <- merge(ausPoints, aus@data, by ="id")
    return(ausDF)
}

#read the GRDC regions
GRDCMap <- function() {
    GRDCregion <- readShapeSpatial(paste0(baseImageDir, "\\wheat_22regions_KC.shp"))
    GRDCregion@data$id <- rownames(GRDCregion@data)
    RegionPoints <- fortify(GRDCregion, region = "id")
    RegionDF <- merge(RegionPoints, GRDCregion@data, by="id")
    return (RegionDF)
}

#keep GRDCRegion object for point culling
GRDCregion <- readShapeSpatial(paste0(baseImageDir, "\\wheat_22regions_KC.shp"))



#Lindsays map
#MLAMap <- function() {
#  aus <- readShapeSpatial("c:\\Users\\fai04d\\OneDrive\\Legume Database 2013\\Map\\agro_ecol_reg(46)_geo.shp")
#  aus@data$id <- rownames(aus@data)
#  ausPoints <- fortify(aus, region = "id")
#  ausDF <- merge(ausPoints, aus@data, by ="id")
#  return(ausDF)
#}


ausDF <- AusMap()
RegionDF <- GRDCMap()

# base plots
ausGraph <- ggplot() + coord_fixed(xlim=c(112, 155), ylim=c(-10, -45)) + geom_polygon(data=ausDF, aes(x=long, y=lat,group=group), fill="dark gray", colour="black") + scale_fill_manual(values=cbPalette)
regionGraph <-geom_polygon(data=RegionDF, aes(x=long, y=lat, group=group, fill=Australia_), colour="black")
