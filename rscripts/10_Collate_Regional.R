#----------------------------------------------
#  10_Collate_Regional.R
#----------------------------------------------
#  This script uses a mapping file that contains all locations (cells) and their
#  regions.  It then gets the files for a region, and extracts out specified 
#  information, collating it in a central location
#----------------------------------------------
library(tidyverse)

# Directories
apsimFilePath <- "/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/Summaries/"
outFilePath <- "/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/CollatedData"

# 1. Get the list of files
# This file is generated in doRegionMappings.R
regionFile <- "/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/metCalcs/RegionMappings.csv"
cultivarFile <- "/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/metCalcs/CultivarListing.csv"

region_df <- read_csv(regionFile)
cults_df <- read_csv(cultivarFile)


#requiredRegion <- "TAS1"
#requiredRegion <- "QLD3_NSW1"
#requiredRegion <- "WA1"
#requiredRegion <- "QLD1"
#requiredRegion <- "QLD2"
#requiredRegion <- "NSW2"
#requiredRegion <- "NSW3"
#requiredRegion <- "NSW4"
requiredRegion <- "VIC1"
#requiredRegion <- "VIC2_SA1"
#requiredRegion <- "SA2"
#requiredRegion <- "WA2"
#requiredRegion <- "WA3"
#requiredRegion <- "WA4"

#---------------------------------------------------------------------
# FUNCTION: Collate_Regional_Info
# Takes a regional mapping file, and a the id for the required 
# region, iterates through the files for those regions, and
# .....
#---------------------------------------------------------------------
# The following is a function that call be called many times
#Collate_Regional_Info <- function(region_df, cults_df, requiredRegion) {


# Need to make sure our dates in all lower case, and that they are valid
sowDates <- unlist(cults_df %>% distinct(SowingDate))
# "15-mar", "1-Apr", "15-Apr", "29-Apr", "13-May", "27-May", "3-Jun", "17-Jun", "1-jul", "15-jul", "29-jul"
earlySowDates <- c("15-mar", "1-apr", "15-apr", "29-apr")
midSowDates <- c("13-may", "27-may", "3-jun")
lateSowDates <- c("17-jun", "1-jul", "15-jul", "29-jul")

#unique(region_df$RegionID)
# 1. get the cultivars and so dates that we are interested in

subCults_df <- cults_df %>% 
    mutate(SowingDate = tolower(SowingDate)) %>% 
    filter(RegionID == requiredRegion) %>% 
    select(RegionID, Zone, Cultivar, MaturityGroup, SowingDate)

# 2. filter the location file
cells_df <- region_df %>% 
    filter(RegionID == requiredRegion) %>% 
    select(RegionID, Location)

#now combine the cells with the specific cults
all_cellcults_df <- cells_df %>% 
    left_join(subCults_df, by="RegionID") %>% 
    mutate(filename = paste0(apsimFilePath, Location, "/Summary_", Cultivar, ".csv")) %>% 
    mutate(haveData = file.exists(filename)) %>% 
    filter(haveData == TRUE)


# Check if the output directory exists
# if not create it
dir.create(file.path(outFilePath, requiredRegion), showWarnings = FALSE)

#unique(alldata_df$phases)
#phases <- c("01_Germinating", "02_Emerging", "03_Vegetative", "04_StemElongation", 
#            "05_EarlyReproductive", "06_GrainSet", "07_GrainFilling", "08_Maturing",
#            "09_Ripening", "10_Harvest")
phases <- c("06_GrainSet", "07_GrainFilling")

#now create the directories for the phases
#for (phase in phases) {
#    dir.create(file.path(outFilePath, requiredRegion, phase), showWarnings = FALSE)
#}


#Need to filter the cellCults by each cultivar
cultivars <- unlist(all_cellcults_df %>% 
    distinct(Cultivar))

#WA1 
#cult <- "Magenta"
#cult <- "Mace"
#cult <- "Emurock"

#QLD1_NSW1 
#cult <- "Sentinel"
#cult <- "Impala"
#cult <- "Hartog"

#TAS1
#cult <- "Mace"
# cult <- "Thornbill"
# cult <- "EGAGregory"

#QLD1
#cult <- "EGAGregory"
#cult <- "Gauntlet"
#cult <- "Hartog"

#QLD2
#cult <- "EGAGregory"
#cult <- "Impala"
#cult <- "Hartog"

#VIC1
#cult <- "Wedgetail"
cult <- "Scout"
#cult <- "Emurock"

#for (cult in cultivars) {
    newCultivar <- TRUE

    cellcults_df <- all_cellcults_df %>% 
        filter(Cultivar == cult)     
    
    #row <- 1
    for (row in 1:nrow(cellcults_df)) {
        filename <- as.character(cellcults_df[row, "filename"])
        fileData <- read_csv(filename) %>% 
            select(-contains("season"))
        
        #now need to filter the data for this
        for (thisphase in phases) {
            #thisphase <- "07_GrainFilling"
            phasedfname <- paste0("df_", thisphase)
            
            filtered_df <- fileData %>% 
                filter(phases == thisphase) %>% 
                mutate(SowDOY = strftime(sowingDate, format="%j"),
                       startDOY = strftime(startDate, format="%j"),
                       endDOY= strftime(endDate, format="%j")) %>% 
                select(Cultivar, Longitude, Latitude, SowDate, sowingDate, SowDOY, phases, startDate, startDOY,
                       endDate, endDOY, dayCount:FqDaysMaxT_GT40)    
            
            if (newCultivar == TRUE) {
                switch(thisphase, 
                       "01_Germinating" = {  df_01_Germinating <- filtered_df },
                       "02_Emerging" = {  df_02_Emerging <- filtered_df },
                       "03_Vegetative" = {  df_03_Vegetative <- filtered_df },
                       "04_StemElongation" = {  df_04_StemElongation <- filtered_df },
                       "05_EarlyReproductive" = {  df_05_EarlyReproductive <- filtered_df },
                       "06_GrainSet" = {  df_06_GrainSet <- filtered_df },
                       "07_GrainFilling" = { df_07_GrainFilling <- filtered_df },
                       "08_Maturing" = { df_08_Maturing <- filtered_df },
                       "09_Ripening" = { df_09_Ripening <- filtered_df },
                       "10_Harvest" = { df_10_Harvest <- filtered_df }
                )
            } else if (!exists(phasedfname)) {
                switch(thisphase, 
                       "01_Germinating" = {  df_01_Germinating <- filtered_df },
                       "02_Emerging" = {  df_02_Emerging <- filtered_df },
                       "03_Vegetative" = {  df_03_Vegetative <- filtered_df },
                       "04_StemElongation" = {  df_04_StemElongation <- filtered_df },
                       "05_EarlyReproductive" = {  df_05_EarlyReproductive <- filtered_df },
                       "06_GrainSet" = {  df_06_GrainSet <- filtered_df },
                       "07_GrainFilling" = { df_07_GrainFilling <- filtered_df },
                       "08_Maturing" = { df_08_Maturing <- filtered_df },
                       "09_Ripening" = { df_09_Ripening <- filtered_df },
                       "10_Harvest" = { df_10_Harvest <- filtered_df }
                )

            } else {
                #append to the dataframe
                switch(thisphase, 
                       "01_Germinating" = {  df_01_Germinating <- bind_rows(df_01_Germinating, filtered_df) },
                       "02_Emerging" = {  df_02_Emerging <- bind_rows(df_02_Emerging, filtered_df) },
                       "03_Vegetative" = {  df_03_Vegetative <- bind_rows(df_03_Vegetative, filtered_df) },
                       "04_StemElongation" = {  df_04_StemElongation <- bind_rows(df_04_StemElongation, filtered_df) },
                       "05_EarlyReproductive" = {  df_05_EarlyReproductive <- bind_rows(df_05_EarlyReproductive, filtered_df) },
                       "06_GrainSet" = {  df_06_GrainSet <- bind_rows(df_06_GrainSet, filtered_df) },
                       "07_GrainFilling" = { df_07_GrainFilling <- bind_rows(df_07_GrainFilling, filtered_df) },
                       "08_Maturing" = { df_08_Maturing <- bind_rows(df_08_Maturing, filtered_df) },
                       "09_Ripening" = { df_09_Ripening <- bind_rows(df_09_Ripening, filtered_df) },
                       "10_Harvest" = { df_10_Harvest <- bind_rows(df_10_Harvest, filtered_df) }
                )
            }
            newCultivar <- FALSE
        }
    } 
    
    #now we can save the files
    if (exists("df_01_Germinating")) {    
        outfile = paste0(file.path(outFilePath, requiredRegion), "/", cult, "_01_Germinating.csv")
        write_csv(df_01_Germinating, path=outfile, append=FALSE)        
    }
    if (exists("df_02_Emerging")) {    
        outfile = paste0(file.path(outFilePath, requiredRegion), "/", cult, "_02_Emerging.csv")
        write_csv(df_02_Emerging, path=outfile, append=FALSE)        
    }
    if (exists("df_03_Vegetative")) {    
        outfile = paste0(file.path(outFilePath, requiredRegion), "/", cult, "_03_Vegetative.csv")
        write_csv(df_03_Vegetative, path=outfile, append=FALSE)        
    }
    if (exists("df_04_StemElongation")) {    
        outfile = paste0(file.path(outFilePath, requiredRegion), "/", cult, "_04_StemElongation.csv")
        write_csv(df_04_StemElongation, path=outfile, append=FALSE)        
    }
    if (exists("df_05_EarlyReproductive")) {    
        outfile = paste0(file.path(outFilePath, requiredRegion), "/", cult, "_05_EarlyReproductive.csv")
        write_csv(df_05_EarlyReproductive, path=outfile, append=FALSE)        
    }
    if (exists("df_06_GrainSet")) {    
        outfile = paste0(file.path(outFilePath, requiredRegion), "/", cult, "_06_GrainSet.csv")
        write_csv(df_06_GrainSet, path=outfile, append=FALSE)        
    }
    if (exists("df_07_GrainFilling")) {    
        outfile = paste0(file.path(outFilePath, requiredRegion), "/", cult, "_07_GrainFilling.csv")
        write_csv(df_07_GrainFilling, path = outfile, append=FALSE)        
    }
    if (exists("df_08_Maturing")) {    
        outfile = paste0(file.path(outFilePath, requiredRegion), "/", cult, "_08_Maturing.csv")
        write_csv(df_08_Maturing, path = outfile, append=FALSE)        
    }
    if (exists("df_09_Ripening")) {    
        outfile = paste0(file.path(outFilePath, requiredRegion), "/", cult, "_09_Ripening.csv")
        write_csv(df_09_Ripening, path = outfile, append=FALSE)        
    }
    if (exists("df_10_Harvest")) {    
        outfile = paste0(file.path(outFilePath, requiredRegion), "/", cult, "_10_Harvest.csv")
        write_csv(df_10_Harvest, path = outfile, append=FALSE)        
    }
#}
#}

#---------------------------------------------------------------------
#for some reason, the emurock file also contains mace data (dataframes not reset)
#dataFile <- "/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/CollatedData/WA1/Emurock_07_GrainFilling.csv"
#data_df <- read_csv(dataFile)
#data_df <- data_df %>% filter(Cultivar == "Emurock")
#write_csv(data_df, path = dataFile, append=FALSE)
#---------------------------------------------------------------------
#QLD3_NSW1 files are too big - need to investigate
#dataFile <- paste0(outFilePath, "/QLD3_NSW1/Impala_06_GrainSet.csv")
#dataFile <- paste0(outFilePath, "/QLD3_NSW1/Impala_07_GrainFilling.csv")
#dataFile <- paste0(outFilePath, "/QLD3_NSW1/Sentinel_06_GrainSet.csv")
dataFile <- paste0(outFilePath, "/SA2/Mace_06_GrainSet_fast_fast.csv")
data_df <- read_csv(dataFile)
#data_df <- df_07_GrainFilling

earlySowDates <- c("15-mar", "1-apr", "15-apr", "29-apr")
midSowDates <- c("13-may", "27-may", "3-jun")
lateSowDates <- c("17-jun", "1-jul", "15-jul", "29-jul")


#what is in the current file
cultivars <- unlist(data_df %>% distinct(Cultivar))
data_df <- data_df %>% filter(Cultivar == "Mace")

earlyFilename <- str_replace(dataFile, "_fast_fast.csv", "_fast.csv")
write_csv(data_df, path = earlyFilename, append=FALSE)

phases <- unlist(data_df %>% distinct(phases))
sowDates <- unlist(data_df %>% distinct(SowDate))

#Make the output file smaller by splitting the file into Maturity groups 
data_df_early <- data_df %>% filter(SowDate %in% earlySowDates)
earlyFilename <- str_replace(dataFile, ".csv", "_early.csv")
write_csv(data_df_early, path = earlyFilename, append=FALSE)

data_df_mid <- data_df %>% filter(SowDate %in% midSowDates)
midFilename <- str_replace(dataFile, "_fast.csv", "_mid.csv")
write_csv(data_df_mid, path = midFilename, append=FALSE)

data_df_late <- data_df %>% filter(SowDate %in% lateSowDates)
lateFilename <- str_replace(dataFile, ".csv", "_late.csv")
write_csv(data_df_late, path = lateFilename, append=FALSE)

#remove any unwanted data from the file (ie, unwanted phases)
#data_df <- data_df %>% filter(Cultivar == "Wedgetail")
#write_csv(data_df, path = dataFile, append=FALSE)

#---------------------------------------------------------------------
requiredRegion <- "QLD3_NSW1"
#cult <- "Impala"
cult <- "Sentinel"
#cult <- "Thornbill"
#phase <- "06_GrainSet"
phase <- "07_GrainFilling"
#season <- "early"
#season <- "mid"
season <- "late"
dataFile <- paste0(outFilePath, "/", requiredRegion, "/", cult, "_", phase, "_", season, ".csv")
data_df <- read_csv(dataFile)

data_df2 <- data_df %>% 
    mutate(SowDOY = strftime(sowingDate, format="%j"),
           startDOY = strftime(startDate, format="%j"),
           endDOY= strftime(endDate, format="%j")) %>% 
    select(Cultivar, Longitude, Latitude, SowDate, sowingDate, SowDOY, phases, startDate, startDOY,
           endDate, endDOY, dayCount:FqDaysMaxT_GT40)
write_csv(data_df2, path = dataFile, append=FALSE)
