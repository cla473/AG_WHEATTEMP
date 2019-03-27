#!/usr/bin/env Rscript

#This version filters by maturity

# usage: ./12_Collate_Regional.R -l 3
# usage: ./12_Collate_Regional.R -l 23

library("tidyverse")
library("optparse")

option_list = list(
    make_option(c("-l", "--line"), type="character", help="what lineno in the file is to be processed", metavar="character")
); 

#print("got to here")
opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

print(opt)
if (is.null(opt$line)){
    print_help(opt_parser)
    stop("At least one argument must be supplied (input file).n", call.=FALSE)
}

#lineNo <- 1
lineNo <- opt$line
print(paste0("line no is ", lineNo))


workingFile <- "/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/metCalcs/CultivarListing_working.csv"
working_df <- read_csv(workingFile)
working_df <- working_df %>% 
    filter(LineNo == lineNo)

requiredRegion <- as.character(working_df %>% select(RegionID))
requiredCultivar <- as.character(working_df %>% select(Cultivar))
requiredMaturity <- as.character(working_df %>% select(MaturityGroup))
requiredPhase <- as.character(working_df %>% select(Phase))

# if (requiredMaturity == "Fast") {
#     print("Maturity is fast!")
# } else if (requiredMaturity == "Mid") {  
#     print("Maturity is medium!")
# } else if (requiredMaturity == "Long") { 
#     print("Maturity is long!")
# } else {
#     print("Maturity is undefined ......!")
# }

if (is.na(requiredMaturity) || length(requiredMaturity) <= 0) {
    requiredMaturity <- "No"
}
print(paste0("Region is ", requiredRegion, " with ", requiredCultivar, " cultivar and ", requiredMaturity, " maturity."))

#This is the start of the working process
apsimFilePath <- "/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/Summaries/"
outFilePath <- "/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/CollatedData"

# 1. Get the list of files
# This file is generated in doRegionMappings.R
regionFile <- "/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/metCalcs/RegionMappings.csv"
cultivarFile <- "/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/metCalcs/CultivarListing.csv"

#phases <- c("06_GrainSet", "07_GrainFilling")

FastSowDates <- c("15-mar", "1-apr", "15-apr", "29-apr")
MidSowDates <- c("13-may", "27-may", "3-jun")
LongSowDates <- c("17-jun", "1-jul", "15-jul", "29-jul")

region_df <- read_csv(regionFile)
cults_df <- read_csv(cultivarFile)

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
    mutate(filename = paste0(apsimFilePath, Location, "/Summary_", requiredCultivar, ".csv")) %>%
    mutate(haveData = file.exists(filename)) %>%
    filter(haveData == TRUE)


# Check if the output directory exists
# if not create it
dir.create(file.path(outFilePath, requiredRegion), showWarnings = FALSE)

newCultivar <- TRUE

cellcults_df <- all_cellcults_df %>%
    filter(Cultivar == requiredCultivar)

#row <- 1
for (row in 1:nrow(cellcults_df)) {
    filename <- as.character(cellcults_df[row, "filename"])
    
    if (file.exists(filename)) {
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
    
            #add the filtering by the maturity, if required 
            if (requiredMaturity == "Fast") {
                filtered_df <- filtered_df %>% filter(SowDate %in% FastSowDates)
            } else if (requiredMaturity == "Mid") {  
                filtered_df <- filtered_df %>% filter(SowDate %in% MidSowDates) 
            } else if (requiredMaturity == "Long") { 
                filtered_df <- filtered_df %>% filter(SowDate %in% LongSowDates) 
            }
    
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
}

if (exists("df_01_Germinating")) {
    outfile <- paste0(file.path(outFilePath, requiredRegion), "/", requiredCultivar, "_01_Germinating.csv")
} else if (exists("df_02_Emerging")) {
    outfile <- paste0(file.path(outFilePath, requiredRegion), "/", requiredCultivar, "_02_Emerging.csv")
} else if (exists("df_03_Vegetative")) {
    outfile <- paste0(file.path(outFilePath, requiredRegion), "/", requiredCultivar, "_03_Vegetative.csv")
} else if (exists("df_04_StemElongation")) {
    outfile <- paste0(file.path(outFilePath, requiredRegion), "/", requiredCultivar, "_04_StemElongation.csv")
} else if (exists("df_05_EarlyReproductive")) {
    outfile <- paste0(file.path(outFilePath, requiredRegion), "/", requiredCultivar, "_05_EarlyReproductive.csv")
} else if (exists("df_06_GrainSet")) {
    outfile <- paste0(file.path(outFilePath, requiredRegion), "/", requiredCultivar, "_06_GrainSet.csv")
} else if (exists("df_07_GrainFilling")) {
    outfile = paste0(file.path(outFilePath, requiredRegion), "/", requiredCultivar, "_07_GrainFilling.csv")
} else if (exists("df_08_Maturing")) {
    outfile = paste0(file.path(outFilePath, requiredRegion), "/", requiredCultivar, "_08_Maturing.csv")
} else if (exists("df_09_Ripening")) {
    outfile = paste0(file.path(outFilePath, requiredRegion), "/", requiredCultivar, "_09_Ripening.csv")
} else if (exists("df_10_Harvest")) {
    outfile = paste0(file.path(outFilePath, requiredRegion), "/", requiredCultivar, "_10_Harvest.csv")
}

if (requiredMaturity == "Fast") {
    outfile <- str_replace(outfile, ".csv", "_fast.csv")
} else if (requiredMaturity == "Mid") {
    outfile <- str_replace(outfile, ".csv", "_mid.csv") 
} else if (requiredMaturity == "Long") { 
    outfile <- str_replace(outfile, ".csv", "_long.csv") 
}


if (exists("df_01_Germinating")) {
    write_csv(df_01_Germinating, path=outfile, append=FALSE)
} else if (exists("df_02_Emerging")) {
    write_csv(df_02_Emerging, path=outfile, append=FALSE)
} else if (exists("df_03_Vegetative")) {
    write_csv(df_03_Vegetative, path=outfile, append=FALSE)
} else if (exists("df_04_StemElongation")) {
    write_csv(df_04_StemElongation, path=outfile, append=FALSE)
} else if (exists("df_05_EarlyReproductive")) {
    write_csv(df_05_EarlyReproductive, path=outfile, append=FALSE)
} else if (exists("df_06_GrainSet")) {
    write_csv(df_06_GrainSet, path=outfile, append=FALSE)
} else if (exists("df_07_GrainFilling")) {
    write_csv(df_07_GrainFilling, path = outfile, append=FALSE)
} else if (exists("df_08_Maturing")) {
    write_csv(df_08_Maturing, path = outfile, append=FALSE)
} else if (exists("df_09_Ripening")) {
    write_csv(df_09_Ripening, path = outfile, append=FALSE)
} else if (exists("df_10_Harvest")) {
    write_csv(df_10_Harvest, path = outfile, append=FALSE)
}



