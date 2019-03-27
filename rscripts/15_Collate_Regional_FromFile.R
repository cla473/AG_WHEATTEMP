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

#lineNo <- 76
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

if (is.na(requiredMaturity) || length(requiredMaturity) <= 0) {
    requiredMaturity <- "No"
}
print(paste0("Region is ", requiredRegion, " for the ", requiredCultivar, " cultivar with ", requiredMaturity, " maturity, for ", requiredPhase, " phase."))

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


cellcults_df <- all_cellcults_df %>%
    filter(Cultivar == requiredCultivar)

newCultivar <- TRUE
#row <- 1
for (row in 1:nrow(cellcults_df)) {
    filename <- as.character(cellcults_df[row, "filename"])
    
    if (file.exists(filename)) {
        fileData <- read_csv(filename) %>%
            select(-contains("season"))
    
        filtered_df <- fileData %>%
            filter(phases == requiredPhase) %>%
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
            output_df <- filtered_df
        } else {
            output_df <- bind_rows(output_df, filtered_df)
        }
        newCultivar <- FALSE
    }
}

outfile <- paste0(file.path(outFilePath, requiredRegion), "/", requiredCultivar, "_" , requiredPhase, ".csv")

if (requiredMaturity == "Fast") {
    outfile <- str_replace(outfile, ".csv", "_fast.csv")
} else if (requiredMaturity == "Mid") {
    outfile <- str_replace(outfile, ".csv", "_mid.csv") 
} else if (requiredMaturity == "Long") { 
    outfile <- str_replace(outfile, ".csv", "_long.csv") 
}

write_csv(output_df, path=outfile, append=FALSE)




