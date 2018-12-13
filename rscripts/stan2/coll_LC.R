#!/usr/bin/env/Rscript

#---------------------------------------------------
# reads the summary files return from Apsim Simulations, and processes the files:
#  * applying growth phase information based on Stage and ZadokStage,
#  * and summares the data based on the growth phases
#---------------------------------------------------
# written for running on (HPC)
#---------------------------------------------------

rm(list = ls())
library(optparse)
library(lubridate)
library(tidyverse)
 

# define the working directories
#apsim_sourcedir <- "/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/Condor/Output/"
#metfile_sourcedir <- "/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/modMET_csv"
metfile_sourcedir <- "/flush3/cla473/metfile"
outputDir <- "/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/Summaries/"

#-------------------------------------------------------------------------------------
# Input Details:
# Apsim files are in source directory /Output/cultivarName  - there are 65 of these
# within each of these directories, there are 51032 files, which is each location
# and sow dates are factorials in these simulations
# The Apsim files are stored as .RData files that have a 'guid' as the filename
# Details about the simulations are stored in
#"/OSM/CBR/AG_WHEATTEMP/work/ApsimNG-LC/Condor/TraitFactor/cultivarname (ie, Yitpi.RData"
#cultDirs <- list.dirs(path = '/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/Condor/Output/', full.names = TRUE, recursive = FALSE)
#cultDirs <- list.dirs(path = '/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/Condor/origOutput/', full.names = TRUE, recursive = FALSE)
#cultDirs[1]
#-------------------------------------------------------------------------------------


option_list = list(
    make_option(c("-f", "--filepath"), type="character", default=NULL, 
                help="The Directory for the Simulation (.RData) files to be processed.", metavar="character"),
    make_option(c("-p", "--pattern"), type="character", default="", metavar="character")
);

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

if (is.null(opt$filepath)){
    print_help(opt_parser)
    stop("At least one argument must be supplied (filepath)", call.=FALSE)
}
inpath <- opt$filepath
pattern <- opt$pattern
print(paste(inpath,pattern))

#this is for testing
#cultFileList <- list.files(path = '/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/Condor/Output/Bolac', recursive = FALSE, full.names = TRUE) 
#inpath <- "/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/Condor/Output/Bolac/" 
#inpath <- "/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/Condor/Output/Bonnie_Rock/"
#infile <- cultFileList[1]
#infile <- "/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/Condor/Output/Hartog/0a0b34b7fd1831a28b8f7f2aad302265.RData"

empty <- function(f) {
    info = file.info(f)
    return (is.na(info$size) || info$size %% 1024 == 0)
}

process1 <- function(infile, metfile_sourcedir, outputDir) {

    if (class(try(load(infile))) == "try-error") {
        cat(infile, "is bad\n")
        return()
    }
    #ls()
#    res$head
    #-------------------------------------------------------------------------------------
    details <-data.frame(res$head, stringsAsFactors = FALSE)
    details$name <- as.character(details$name)
    details$value <- as.character(details$value)
#    str(details)
    location <- unlist(details$value[1])
    cultivar <- unlist(details$value[2])

    outPath <- file.path(outputDir, location)
    dir.create(outPath, showWarnings=FALSE)
    
    #mod LMC - 12/12/2018 - this will now be saved as RData
    csvbasefilename = paste0(outPath, "/", cultivar, ".csv") 
    basefilename = paste0(outPath, "/", cultivar, ".RData") 
    summaryFilename = paste0(outPath, "/Summary_", cultivar, ".csv") 
    if ((!empty(csvbasefilename) || !empty(basefilename)) && !empty(summaryFilename)) return()

    cat(basename(infile),location,cultivar,"\n")

    #using the location, get the long and lat so that we can add them to the data
    long <- as.numeric(unlist(strsplit(location, '-'))[1]) / 100
    lat <- as.numeric(unlist(strsplit(location, '-'))[2]) / 100 * -1

    #Now lets look at the actual data    
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
               WheatRootNUptake = Wheat.Root.NUptake)
    
    
    #now that we have the location (long & Lat), we can get the met file
    metfilename <- paste0(metfile_sourcedir, "/", location, ".csv")
    metData <- read.csv(metfilename, sep=',', header=TRUE, stringsAsFactors=FALSE)
    metData$runDate = as.Date(metData$runDate, format="%Y-%m-%d")
    
    # what do I need to filter from the met Data 
    metData <- metData %>% 
        select(year, dayofYear, runDate, maxTemp, minTemp, avgTemp, daylength, rain, evap, vp, VPD,
               calcThermalTime = ApsimTT, PARIO, PQ, FDR, PQday, PQvpdday, PQvpdfdrday, vpsl, ETpt)
    
    
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
               maxTemp, minTemp, avgTemp, ApsimThermalTime, calcThermalTime, rain, evap, vp, VPD,
               LAI, Biomass, Yield, NitrogenStress, WaterStress, WheatRootNUptake,PARIO, PQ,  
               FDR, PQday, PQvpdday, PQvpdfdrday, vpsl, ETpt, 
               cumAvgTemp, cumApsimTT, cumCalcTT, cumRain, 
               cumPhaseAvgTemp, cumPhaseApsimTT, cumPhaseCalcTT, cumPhaseRain,
               cumPhaseETpt, cumPhasePARIO)
    

    #-------------------------------------------------------------------------------------
    # Save this data, so that we can review it if necessary
    #mod LMC - 12/12/2018 - modified this to save output as RData
    #write.csv(fulldf, file=basefilename)
    save(fulldf, file=basefilename)
    #-------------------------------------------------------------------------------------
    
    #-------------------------------------------------------------------------------------
    # NOW GENERATE SUMMARY FILES
    #-------------------------------------------------------------------------------------
    dfSum1 <- fulldf %>% 
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
                  avgVPD = mean(VPD),
                  avgPQday = mean(PQday),
                  avgPQvpdday = mean(PQvpdday),
                  avgPQvpdfdrday = mean(PQvpdfdrday),
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
    
    
    dfSummary <- inner_join(dfSum1, dfSum2, by=c("Cultivar", "Longitude", "Latitude", "SowDate", "sowingDate", "phases"))
    
    # get the counts of various miniumum/maximum temperatures
    dfSum3 <- fulldf %>% 
        group_by(Cultivar, Longitude, Latitude, SowDate, sowingDate, phases) %>% 
        summarise(daysMinT_LT0 = sum(minTemp < 0), 
                  daysMinT_LTm1 = sum(minTemp < -1),
                  daysMinT_LTm2 = sum(minTemp < -2),
                  daysMinT_LTm3 = sum(minTemp < -3),
                  daysMinT_GT4 = sum(minTemp > 4),
                  daysMinT_GT6 = sum(minTemp > 6),
                  daysMinT_GT8 = sum(minTemp > 8),
                  daysMinT_GT10 = sum(minTemp > 10),
                  daysMinT_GT12 = sum(minTemp > 12),
                  daysMinT_GT14 = sum(minTemp > 14),
                  daysMaxT_GT30 = sum(maxTemp > 30),
                  daysMaxT_GT32 = sum(maxTemp > 32),
                  daysMaxT_GT34 = sum(maxTemp > 34),
                  daysMaxT_GT36 = sum(maxTemp > 36),
                  daysMaxT_GT38 = sum(maxTemp > 38),
                  daysMaxT_GT40 = sum(maxTemp > 40)) %>% 
        ungroup()
    
    dfSummary <- inner_join(dfSummary, dfSum3, by=c("Cultivar", "Longitude", "Latitude", "SowDate", "sowingDate", "phases"))
    
    
    #need to add the season_startdate, season_enddate (Maturity), and the season dayCount
    dfSeasonDates <- fulldf %>%
        group_by(Cultivar, Longitude, Latitude, SowDate, sowingDate, phases) %>%
        filter(row_number() == n()) %>%
        filter(phases == "08_Maturing") %>%
        mutate(seasonStartDOY = yday(sowingDate),
               seasonEndDOY = yday(runDate)) %>%
        mutate(seasonEndDOY = ifelse(seasonEndDOY < 181, seasonEndDOY+365, seasonEndDOY)) %>% 
        mutate(seasonDayCount = seasonEndDOY - seasonStartDOY)%>%
        ungroup() %>%
        select(Cultivar, Longitude, Latitude, SowDate, sowingDate, seasonStartDOY, seasonEndDOY, seasonDayCount) 
    
    dfSummary <- left_join(dfSummary, dfSeasonDates, by=c("Cultivar", "Longitude", "Latitude", "SowDate", "sowingDate"))
    
    dfSummary <- dfSummary %>% 
        mutate(calcPQ = cumPARIO / cumAvgTemp,
               calcPQvpd = cumPARIO / (cumAvgTemp * avgVPD),
               calcPQvpdfdr = (cumPARIO * avgFDR)/(cumAvgTemp * avgVPD)) %>% 
        select(Cultivar, Longitude, Latitude, SowDate, sowingDate, 
               seasonStartDOY, seasonEndDOY, seasonDayCount,
               season_cumAvgTemp, season_cumApsimTT, season_cumCalcTT, season_cumRain, 
               phases, startDate, endDate, dayCount,
               minTemp, maxTemp, avgTemp, avgApsimTT, avgCalcTT, 
               avgPARIO, avgFDR, avgPQ, avgVPD, avgDayLength, cumAvgTemp, cumApsimTT, 
               avgPQday, avgPQvpdday, avgPQvpdfdrday, 
               cumCalcTT, cumRain, cumETpt, cumPARIO, Biomass, LAI, Yield, 
               calcPQ, calcPQvpd, calcPQvpdfdr,
               daysMinT_LT0, daysMinT_LTm1, daysMinT_LTm2, daysMinT_LTm3, 
               daysMinT_GT4, daysMinT_GT6, daysMinT_GT8, daysMinT_GT10, daysMinT_GT12, daysMinT_GT14, 
               daysMaxT_GT30, daysMaxT_GT32, daysMaxT_GT34, daysMaxT_GT36, daysMaxT_GT38, daysMaxT_GT40) %>% 
        mutate(FqDaysMinT_LT0 = (daysMinT_LT0/dayCount)*100,
               FqDaysMinT_LTm1 = (daysMinT_LTm1/dayCount)*100,
               FqDaysMinT_LTm2 = (daysMinT_LTm2/dayCount)*100,
               FqDaysMinT_LTm3 = (daysMinT_LTm3/dayCount)*100,
               FqDaysMinT_GT4 = (daysMinT_GT4/dayCount)*100,
               FqDaysMinT_GT6 = (daysMinT_GT6/dayCount)*100,
               FqDaysMinT_GT8 = (daysMinT_GT8/dayCount)*100,
               FqDaysMinT_GT10 = (daysMinT_GT10/dayCount)*100,
               FqDaysMinT_GT12 = (daysMinT_GT12/dayCount)*100,
               FqDaysMinT_GT14 = (daysMinT_GT14/dayCount)*100,
               FqDaysMaxT_GT30 = (daysMaxT_GT30/dayCount)*100,
               FqDaysMaxT_GT32 = (daysMaxT_GT32/dayCount)*100,
               FqDaysMaxT_GT34 = (daysMaxT_GT34/dayCount)*100,
               FqDaysMaxT_GT36 = (daysMaxT_GT36/dayCount)*100,
               FqDaysMaxT_GT38 = (daysMaxT_GT38/dayCount)*100,
               FqDaysMaxT_GT40 = (daysMaxT_GT40/dayCount)*100 ) 
    
    # how do I want to save these files .... should I group all of the files for one location in one file?
    write.csv(dfSummary, file=summaryFilename)

}

fileList <- list.files(path=inpath, pattern=paste("^", pattern, ".*.RData", sep=""), recursive = FALSE, full.names = TRUE) 

for (i in 1:length(fileList)) {
    process1(fileList[i], metfile_sourcedir, outputDir)
}



#library(parallel)
#library(snow)
## Get cluster from pearcey
#cl <- getMPIcluster()
#if (is.null(cl)) {
#    # Create a cluster for local
#    cl <- makeCluster(parallel::detectCores() - 1, type = 'SOCK')
#}
#
#parLapply(cl, seq_len(length(fileList)), par_generate, fileList, metfile_sourcedir, outputDir)

