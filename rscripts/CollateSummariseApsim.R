#---------------------------------------------------
# reads the summary files return from Apsim Simulations, and processes the files:
#  * applying growth phase information based on Stage and ZadokStage,
#  * and summares the data based on the growth phases
#---------------------------------------------------
# written for running on (HPC)
#---------------------------------------------------


rm(list = ls())
library(tidyverse)
library(lubridate)
#print(version)

# # define the working directories
apsim_sourcedir <- "/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/Condor/Output/"
metfile_sourcedir = "/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/modifiedMet"
outputDir <- "/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/Summaries/"


#-------------------------------------------------------------------------------------
# Input Details:
# Apsim files are in source directory /Output/cultivarName  - there are 65 of these
# within each of these directories, there are 51032 files, which is each location
# and sow dates are factorials in these simulations
# The Apsim files are stored as .RData files that have a 'guid' as the filename
# Details about the simulations are stored in
#"/OSM/CBR/AG_WHEATTEMP/work/ApsimNG-LC/Condor/TraitFactor/cultivarname (ie, Yitpi.RData"
cultDirs <- list.dirs(path = '/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/Condor/Output/', full.names = TRUE, recursive = FALSE)
#cultDirs <- list.dirs(path = '/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/Condor/origOutput/', full.names = TRUE, recursive = FALSE)
cultDirs[1]
#-------------------------------------------------------------------------------------
cultFileList <- list.files(path = cultDirs[1], recursive = FALSE, full.names = TRUE) 



i <- 1
par_generate <- function(i, cultFileList) {

    cultFileList[i]
    load(cultFileList[i])
    ls()
    res$head
    #-------------------------------------------------------------------------------------
    details <-data.frame(res$head, stringsAsFactors = FALSE)
    details$name <- as.character(details$name)
    details$value <- as.character(details$value)
    str(details)
    location <- unlist(details$value[1])
    cultivar <- unlist(details$value[2])
    #using the location, get the long and lat so that we can add them to the data
    long <- as.numeric(unlist(strsplit(location, '-'))[1]) / 100
    lat <- as.numeric(unlist(strsplit(location, '-'))[2]) / 100 * -1
    
    
    myData <- data.frame(res$out)
    head(myData)
    #unique(myData$SowDate)
    
    simData <- myData %>% 
        mutate(Longitude = long,
               Latitude = lat) %>% 
        select(Cultivar, Longitude, Latitude, SowDate, Year, sowingDate,
               runDate = Clock.Today, 
               LAI = Wheat.Leaf.LAI,
               Biomass = Wheat.AboveGround.Wt,
               Yield = Wheat.Grain.Wt,
               ZadokStage = Wheat.Phenology.Zadok.Stage,
               Stage = Wheat.Phenology.Stage,
               ApsimThermalTime = Wheat.Phenology.ThermalTime,
               NitrogenStress = Wheat.Leaf.Fn,
               WaterStress = Wheat.Leaf.Fw,
               WheatRootNUptake = Wheat.Root.NUptake
               )
    
    
    #now that we have the location (long & Lat), we can get the met file
    metfilename <- paste0(metfile_sourcedir, "/", location, ".met")
    metData <- read.csv(metfilename, sep=',', header=TRUE, stringsAsFactors=FALSE)
    metData$runDate = as.Date(metData$runDate, format="%Y-%m-%d")
    
    # ?????????
    # what do I need to filter from the met Data 
    metData <- metData %>% 
        select(year, dayofYear, runDate, maxTemp, minTemp, avgTemp, rain, evap, vp,
               calcThermalTime = ApsimTT, PARIO, PQ, daylength, FDR, vpsl, ETpt)
    
    
    #Now need to combine the datasets so that we can work with the full dataset
    fulldf <- inner_join(simData, metData, by="runDate")
    
    #THIS DOES NOT WORK, JUST LOCKS UP EVERYTHING
    fulldf <- fulldf %>%
        mutate(phases = case_when(fulldf$ZadokStage <= 7 ~ "01_Germinating",
                                  fulldf$ZadokStage <= 10 ~ "02_Emerging",
                                  fulldf$ZadokStage <= 31 ~ "03_Vegetative",
                                  fulldf$ZadokStage <= 39 ~ "04_StemElongation",
                                  fulldf$ZadokStage <= 65 ~ "05_EarlyReproductive",
                                  fulldf$ZadokStage <= 71 ~ "06_GrainSet",
                                  fulldf$ZadokStage <= 87 ~ "07_GrainFilling",
                                  fulldf$ZadokStage <= 89 ~ "08_Maturing",
                                  fulldf$ZadokStage <= 90 ~ "09_Ripening")) %>% 
        mutate(phases = ifelse(fulldf$Stage == 10, "10_Harvest", phases))
    
    #do the cumulative aggregates groupings for the location/sowingdate
    fulldf <- fulldf %>% 
        group_by(Cultivar, Longitude, Latitude, sowingDate) %>% 
        mutate(cumAvgTemp = cumsum(avgTemp), 
               cumApsimTT = cumsum(ApsimThermalTime),
               cumCalcTT = cumsum(calcThermalTime),
               cumRain = cumsum(rain)) %>% 
        ungroup()
    
    #do the cumulative aggregates groupings for the location/sowingdate/phases
    fulldf <- fulldf %>% 
        group_by(Cultivar, Longitude, Latitude, sowingDate, phases) %>% 
        mutate(cumPhaseAvgTemp = cumsum(avgTemp),
               cumPhaseApsimTT = cumsum(ApsimThermalTime),
               cumPhaseCalcTT = cumsum(calcThermalTime),
               cumPhaseRain = cumsum(rain), 
               cumPhaseETpt = cumsum(ETpt),
               cumPhasePARIO = cumsum(PARIO)) %>% 
        ungroup()
    
    
    fulldf <- fulldf %>% 
        mutate(Year2 = ifelse(month(runDate) < month(sowingDate), as.numeric(Year) - 1, as.numeric(Year))) %>%
        mutate(sowingDate2 = paste0(SowDate, "-", as.character(Year2))) %>% 
        mutate(sowingDate = as.Date(sowingDate2, format("%d-%b-%Y")))
    
    
    fulldf <- fulldf %>% 
        select(Cultivar, Longitude, Latitude, SowDate, sowingDate, Year, runDate, dayofYear, 
               phases, daylength, ZadokStage, Stage, 
               maxTemp, minTemp, avgTemp, ApsimThermalTime, calcThermalTime, rain, evap, vp,
               LAI, Biomass, Yield, NitrogenStress, WaterStress, WheatRootNUptake,PARIO, PQ,  FDR, vpsl, ETpt, 
               cumAvgTemp, cumApsimTT, cumCalcTT, cumRain, 
               cumPhaseAvgTemp, cumPhaseApsimTT, cumPhaseCalcTT, cumPhaseRain,
               cumPhaseETpt, cumPhasePARIO)

     #need to do calculations for percentiles:
    

               
    #-------------------------------------------------------------------------------------
    #-------------------------------------------------------------------------------------
    # this gets a name for this file, based on Cultivar and Location (Longitude & Latitude)
    namedf <- fulldf %>% 
        select (Cultivar, Longitude, Latitude) %>% 
        distinct() 
    #-------------------------------------------------------------------------------------
    
    basefilename = paste0(outputDir, (namedf$Longitude*100), "_", (abs(namedf$Latitude)*100), "_", namedf$Cultivar, ".csv") 
    write.csv(fulldf, file=basefilename)
    #-------------------------------------------------------------------------------------

    
    
    #-------------------------------------------------------------------------------------
    # NOW GENERATE SUMMARY FILES
    #-------------------------------------------------------------------------------------
    dfSummary <- fulldf %>% 
        group_by(Cultivar, Longitude, Latitude, SowDate, sowingDate, phases) %>% 
        summarise(season_cumAvgTemp = max(cumAvgTemp), 
                  season_cumApsimTT = max(cumApsimTT), 
                  season_cumCalcTT = max(cumCalcTT), 
                  season_cumRain = max(cumRain),
                  startDate = min(runDate),
                  endDate = max(runDate),
                  minTemp = mean(minTemp),
                  maxTemp = mean(maxTemp),
                  avgTemp = mean(avgTemp),
                  avgApsimTT = mean(ApsimThermalTime),
                  avgCalcTT = mean(calcThermalTime),
                  avgPARIO = mean(PARIO),
                  avgFDR = mean(FDR),
                  avgPQ = mean(PQ),
                  avgDayLength = mean(daylength),
                  dayCount = n()) %>% 
        ungroup()
    
    #this will return the last record for each group
    dfSum2 <- fulldf %>% 
        group_by(Cultivar, Longitude, Latitude, SowDate, sowingDate, phases) %>% 
        filter(row_number() == n()) %>% 
        ungroup()%>% 
        select (Cultivar, Longitude, Latitude, SowDate, sowingDate, phases,
                cumAvgTemp = cumPhaseAvgTemp, 
                cumApsimTT = cumPhaseApsimTT, 
                cumCalcTT = cumPhaseCalcTT, 
                cumRain = cumPhaseRain, 
                cumETpt = cumPhaseETpt,
                cumPARIO = cumPhasePARIO,
                Biomass, LAI, Yield) 
    
    
    # get the counts of various miniumum/maximum temperatures
    dfSum3 <- fulldf %>% 
        group_by(Cultivar, Longitude, Latitude, SowDate, sowingDate, phases) %>% 
        summarise(daysLTE0 = sum(minTemp <= 0), 
                  daysLTEm1 = sum(minTemp <= -1),
                  daysLTEm2 = sum(minTemp <= -2),
                  daysLTEm3 = sum(minTemp <= -3),
                  daysGTE30 = sum(maxTemp >= 30),
                  daysGTE32 = sum(maxTemp >= 32),
                  daysGTE34 = sum(maxTemp >= 34),
                  daysGTE36 = sum(maxTemp >= 36),
                  daysGTE38 = sum(maxTemp >= 38),
                  daysGTE40 = sum(maxTemp >= 40)) %>% 
        ungroup()
    
    # need to combine all of the summary dataframes
    dfSumAll <- inner_join(dfSummary, dfSum2, by=c("Cultivar", "Longitude", "Latitude", "SowDate", "sowingDate", "phases"))
    dfSumAll <- inner_join(dfSumAll, dfSum3, by=c("Cultivar", "Longitude", "Latitude", "SowDate", "sowingDate", "phases"))
    
    dfSumAll <- dfSumAll %>% 
        select(Cultivar, Longitude, Latitude, SowDate, sowingDate, 
               season_cumAvgTemp, season_cumApsimTT, season_cumCalcTT, season_cumRain, 
               phases, startDate, endDate, dayCount,
               minTemp, maxTemp, avgTemp, avgApsimTT, avgCalcTT, 
               avgPARIO, avgFDR, avgPQ, avgDayLength, cumAvgTemp, cumApsimTT, 
               cumCalcTT, cumRain, cumETpt, cumPARIO, Biomass, LAI, Yield, 
               daysLTE0, daysLTEm1, daysLTEm2, daysLTEm3, daysGTE30, daysGTE32, daysGTE34, daysGTE36, daysGTE38, daysGTE40)
    
    # how do I want to save these files .... should I group all of the files for one location in one file?
    filename = paste0(outputDir, "Summary_", (namedf$Longitude*100), "_", (abs(namedf$Latitude)*100), "_", namedf$Cultivar, ".csv") 
    write.csv(dfSumAll, file=filename)
}

#-----------------------------------------------------------------------
#the following will process the files 
#-----------------------------------------------------------------------

library(parallel)
library(snow)
# Get cluster from pearcey
cl <- getMPIcluster()
if (is.null(cl)) {
    # Create a cluster for local
    cl <- makeCluster(parallel::detectCores() - 1, type = 'SOCK')
}

parLapply(cl, seq_len(nrow(cultFileList)), par_generate, cultFileList)




