# This will fix the Soil files, removing the duplicated 'name' attribute, where it exists.
# It is no longer required as there is a 'Name' child node, and having the attribute causes errors in new Apsim.
rm(list = ls())
library(tidyverse)
library(xml2)


inSoilLoc <- "//OSM/CBR/AG_WHEATTEMP/work/ApsimNG-test/APSIM_prep/SoilXML"
outSoilLoc <- "//OSM/CBR/AG_WHEATTEMP/work/ApsimNG-LC/SoilXML"

soils_df <- read.table("Soils_list.txt", header=FALSE, col.names = "Filename")
soils_df$inFile <- paste0(inSoilLoc, "/", soils_df$Filename)
soils_df$outFile <- paste0(outSoilLoc, "/", soils_df$Filename)

i <- 1
par_generate <- function(i, soils_df) {
    library(tidyverse)    
    library(xml2)
    
    soilData <- read_xml(soils_df$inFile[i])
    node <- xml_find_all(soilData, ".//Soil")
    xml_attrs(soilData) <- NULL
    node <- xml_find_all(soilData, ".//Sample")
    xml_attrs(node) <- NULL
    node <- xml_find_all(soilData, ".//InitialWater")
    xml_attrs(node) <- NULL
    node <- xml_find_all(soilData, ".//SoilCrop")
    xml_attrs(node) <- NULL
    
    write_xml(x=soilData, file=soils_df$outFile[i])
}


library(parallel)
library(snow)
# Get cluster from pearcey
cl <- getMPIcluster()
if (is.null(cl)) {
    # Create a cluster for local
    cl <- makeCluster(parallel::detectCores() - 1, type = 'SOCK')
}

parLapply(cl, seq_len(nrow(soils_df)), par_generate, soils_df)



