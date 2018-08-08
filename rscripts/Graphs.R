rm(list=ls())
library("plyr")
library("ggplot2")
library("maptools")
library("grid")
library("akima")
source ("C:\\Users\\fai04d\\OneDrive\\GRDC Temperature Study 2014\\MapFunctions.R")

crop = "Fieldpea"
grps <- read.csv(paste("C:\\Users\\fai04d\\Documents\\GRDC Data 2014\\Crop simulations\\", crop, "\\All sites unlimited\\All Sites Last Bin Day.csv", sep=""))
grps[grps$PTQ == Inf,] #check for AvgTemp == 0

#set dates
date <- c("01-may", "15-may", "01-jun", "15-jun", "01-jul")
for (i in 1:5)
{
    grpsDate <- grps[grps$SowingDate == date[i],]
    grpsDate$site <- substr(grpsDate$ID, 10, 12)
    grpsPTQ <- ddply(grpsDate, c("latitude", "longitude"), function(df) mean(df$PTQ))
    grpsPTQvpd <- ddply(grpsDate, c("latitude", "longitude"), function(df) mean(df$PTQvpd))
    
    points <- cbind(grpsPTQ$longitude, grpsPTQ$latitude)
    points <- SpatialPoints(points)
    pointCheck <-!is.na(over(points, GRDCregion)) 
    grpsPTQ$InRegion <- as.data.frame(pointCheck)[,1]
    grpsPTQ <- grpsPTQ[grpsPTQ$InRegion == TRUE,]
    
    ausDF <- AusMap()
    RegionDF <- GRDCMap()
    
    # base plots
    ausGraph <- ggplot() + coord_fixed(xlim=c(112, 155), ylim=c(-10, -45)) + geom_polygon(data=ausDF, aes(x=long, y=lat,group=group), fill="dark gray", colour="black") + scale_fill_manual(values=cbPalette)
    regionGraph <-geom_polygon(data=RegionDF, aes(x=long, y=lat, group=group, fill=Australia_), colour="black")
    
    # Greater region plot
    RegionDF$Zone <- ifelse(grepl("QLD", RegionDF$Australia_), "North",
                     ifelse(grepl("WA", RegionDF$Australia_), "West",
                     ifelse(grepl("SA", RegionDF$Australia_), "South",
                     ifelse(RegionDF$Australia1 %in% c("Northern NSW", "Western NSW", "Eastern NSW"), "North",
                     ifelse(RegionDF$Australia1 %in% c("South-eastern NSW", "South-western NSW"), "East",
                     ifelse(RegionDF$Australia1 %in% c("South Mallee", "Wimmera"), "South", "NULL"))))))
    RegionDF$Zone <- factor(RegionDF$Zone, levels = c("North", "East", "South", "West"))
    regionGraph <- geom_polygon(data=RegionDF, aes(x=long, y=lat, group=group, fill=Zone), colour="black")
    ausGraph + regionGraph + theme_gray(base_size = 18)
    
    #heat map PTQ
    x <- grpsPTQ$longitude
    y <- grpsPTQ$latitude
    z <- grpsPTQ$V1
    
    heat.li <- interp(x, y, z, xo=seq(min(x), max(x), length = 600), yo=seq(min(y), max(y), length = 600),
                      linear = TRUE, extrap=FALSE, duplicate = "strip")
    heat.li <- cbind(expand.grid(heat.li$x, heat.li$y), c(heat.li$z))
    heat.li <- na.omit(heat.li)
    names(heat.li) <- c("x", "y", "z")
    #remove points outside regions
    p <- cbind(heat.li$x, heat.li$y)
    pp <- SpatialPoints(p)
    pcheck <-!is.na(over(pp, GRDCregion)) 
    heat.li$InRegion <- as.data.frame(pcheck)[,1]
    heat.li <- heat.li[heat.li$InRegion==TRUE,]
    
    ausGraph + geom_tile(data=heat.li, aes(x=x, y=y, colour=z, fill=NA)) + labs(title=paste("PTQ", date[i], sep=" ")) + scale_colour_gradientn(colours = rainbow(7), limits=c(0.1, 0.8)) + theme_bare #+
    #    geom_text(data=grpsDate, aes(x=longitude,y=latitude, label=site, size = 0.2))
    ggsave(paste("c:\\Users\\fai04d\\Dropbox\\GRDC\\Plots\\",crop, " PTQ Map ",date[i]," 1960-2013.png", sep=""), width=7, height=6, units="in")
    
    #heat map PTQvpd
    x <- grpsPTQvpd$longitude
    y <- grpsPTQvpd$latitude
    z <- grpsPTQvpd$V1
    
    heat.li <- interp(x, y, z, xo=seq(min(x), max(x), length = 1000), yo=seq(min(y), max(y), length = 1000),
                      linear = TRUE, extrap=FALSE, duplicate = "strip")
    heat.li <- cbind(expand.grid(heat.li$x, heat.li$y), c(heat.li$z))
    heat.li <- na.omit(heat.li)
    names(heat.li) <- c("x", "y", "z")
    #remove points outside regions
    p <- cbind(heat.li$x, heat.li$y)
    pp <- SpatialPoints(p)
    pcheck <-!is.na(over(pp, GRDCregion)) 
    heat.li$InRegion <- as.data.frame(pcheck)[,1]
    heat.li <- heat.li[heat.li$InRegion==TRUE,]
    
    ausGraph + geom_tile(data=heat.li, aes(x=x, y=y, colour=z, fill=NA)) + labs(title=paste("PTQvpd", date[i], sep=" ")) + scale_colour_gradientn(colours = rainbow(7), limits=c(0.1, 1.2)) + theme_bare
    ggsave(paste("c:\\Users\\fai04d\\Dropbox\\GRDC\\Plots\\",crop, " PTQvpd Map ",date[i]," 1960-2013.png", sep=""), width=7, height=6, units="in")
}