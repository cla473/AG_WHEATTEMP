#------------------------------------------------------------------------------------
# submit_weatherFilesAddVPD.R
#------------------------------------------------------------------------------------
# This will open modified apsim weather files and add calculations for 
# PQday, PQvpdday and PQvpdfdrday
#------------------------------------------------------------------------------------

rm(list = ls())
library(tidyverse)

metFileList <- list.files(path = '/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/modifiedMet/', pattern = "*.met", recursive = FALSE, full.names = TRUE) 

i <- 5
par_generate <- function(i, metFileList) {
    library(tidyverse)    

    filename <- metFileList[i]
    metData <- read_csv(metFileList[i])
    metData2 <- metData %>% 
        mutate(VPDMax = 610.7 * exp(17.4 * maxTemp/(239 + maxTemp))/1000 - vp/10,
               VPDMin = 610.7 * exp(17.4 * minTemp/(239 + minTemp))/1000 - vp/10,
               VPDFrac = 0.66) %>%
        mutate(VPD = VPDMax * VPDFrac - VPDMin * (1-VPDFrac)) %>% 
        mutate(PQday = PARIO/avgTemp,
               PQvpdday = PARIO/(avgTemp * VPD),
               PQvpdfdrday = (PARIO * FDR)/(avgTemp * VPD)) %>% 
        select(year, dayofYear, runDate, maxTemp, minTemp, avgTemp, daylength, ApsimTT, rain, radiation, evap, 
               vp, VPDMax, VPDMin, VPDFrac, VPD, PARIO, radnJ, PQ, PQday, PQvpdday, PQvpdfdrday,
               sinDEC, cosDEC, a, b, hour, sinB, SC, sinINT, Ta, FDR, vpsl, ETpt)
    
    outFilename <- str_replace(filename, "modifiedMet", "modMET_csv")
    outFilename <- str_replace(outFilename, ".met", ".csv")
    write.csv(metData2, file=outFilename)
}


library(parallel)
library(snow)
# Get cluster from pearcey
cl <- getMPIcluster()
if (is.null(cl)) {
    # Create a cluster for local
    cl <- makeCluster(parallel::detectCores() - 1, type = 'SOCK')
}

parLapply(cl, seq_len(length(metFileList)), par_generate, metFileList)



