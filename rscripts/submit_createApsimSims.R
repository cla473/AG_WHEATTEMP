#Builds simulations using data from GridSoil.7z (needs to be extracted and converted first) and Ross' met files
rm(list = ls())
library(tidyverse)
library(xml2)


apsimLoc <- "//OSM/CBR/AG_WHEATTEMP/work/ApsimNG-LC/apsimx/"
baseApsimxFile <- "//OSM/CBR/AG_WHEATTEMP/work/ApsimNG-LC/prep/BaseWithCultivars.apsimx"

metLoc <- "//OSM/CBR/AG_WHEATTEMP/work/ApsimNG-LC/met"
soilLoc <- "//OSM/CBR/AG_WHEATTEMP/work/ApsimNG-test/APSIM_prep/SoilXML"

gridData <- read.csv("//OSM/CBR/AG_WHEATTEMP/work/ApsimNG-LC/metGrid_Soil.csv", sep=',', header=TRUE)
# gridData <- head(gridData, 5)

simFiles <- paste(apsimLoc, gridData$simFile, sep = "")
soilFiles <- paste(soilLoc, "/", gridData$soilLong * 100, gridData$soilLat * 100, ".soil", sep = "")

#these don't need the path, as we are just updating the reference in the apsimx file
metFiles <- as.character(gridData$metFile)

df <- data.frame(sims=simFiles, mets=metFiles, soils=soilFiles, stringsAsFactors = FALSE)


i <- 1
par_generate <- function(i, df, baseApsimxFile) {
    library(tidyverse)    
    library(xml2)
    
    simName <- df$sims[i]    #What is the name of apsimx  file that will be created
    met <- df$mets[i]        #get the name of the met file

    #Now read base the Apsim File
    sim <- read_xml(baseApsimxFile)

    #update the met file name
    xml_set_text(xml_find_first(sim, ".//Weather/FileName"), met)
    
    #replace the soil data
    soilData <- read_xml(df$soils[i])
    xml_replace(xml_find_first(sim, ".//Soil"), soilData)

    write_xml(sim, simName)
}


library(parallel)
library(snow)
# Get cluster from pearcey
cl <- getMPIcluster()
if (is.null(cl)) {
    # Create a cluster for local
    cl <- makeCluster(parallel::detectCores() - 1, type = 'SOCK')
}

parLapply(cl, seq_len(nrow(df)), par_generate, df, baseApsimxFile)



