#!/usr/bin/env Rscript

# usage: ./11_Collate_Regional.R -r NSW1 -p 06_GrainSet -c Wedgetail


library("tidyverse")
library("optparse")

#make_option(c("-p", "--phase"), type="character", help="what phase is to be processed", metavar="character"),

option_list = list(
    make_option(c("-r", "--region"), type="character", help="what region is to be processed", metavar="character"),
    make_option(c("-c", "--cultivar"), type="character", help="what cultivar is to be processed", metavar="character")
); 

#print("got to here")
opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

print(opt)
if (is.null(opt$region)){
    print_help(opt_parser)
    stop("At least one argument must be supplied (input file).n", call.=FALSE)
}

requiredRegion <- opt$region
#phase <- opt$phase
Cultivar <- opt$cultivar

phases <- c("06_GrainSet", "07_GrainFilling")
#phases2 <- "06_GrainSet, 07_GrainFilling"
#phases2 <- unlist(strsplit(phases2, split=","))
print(paste0("region is ", requiredRegion, ", phase are ", phases, ", and cultivar is ", Cultivar))

apsimFilePath <- "/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/Summaries/"
outFilePath <- "/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/CollatedData"

# 1. Get the list of files
# This file is generated in doRegionMappings.R
regionFile <- "/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/metCalcs/RegionMappings.csv"
cultivarFile <- "/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/metCalcs/CultivarListing.csv"

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
    mutate(filename = paste0(apsimFilePath, Location, "/Summary_", Cultivar, ".csv")) %>%
    mutate(haveData = file.exists(filename)) %>%
    filter(haveData == TRUE)


# Check if the output directory exists
# if not create it
dir.create(file.path(outFilePath, requiredRegion), showWarnings = FALSE)

newCultivar <- TRUE

cellcults_df <- all_cellcults_df %>%
    filter(Cultivar == Cultivar)

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
    outfile = paste0(file.path(outFilePath, requiredRegion), "/", Cultivar, "_01_Germinating.csv")
    write_csv(df_01_Germinating, path=outfile, append=FALSE)
}
if (exists("df_02_Emerging")) {
    outfile = paste0(file.path(outFilePath, requiredRegion), "/", Cultivar, "_02_Emerging.csv")
    write_csv(df_02_Emerging, path=outfile, append=FALSE)
}
if (exists("df_03_Vegetative")) {
    outfile = paste0(file.path(outFilePath, requiredRegion), "/", Cultivar, "_03_Vegetative.csv")
    write_csv(df_03_Vegetative, path=outfile, append=FALSE)
}
if (exists("df_04_StemElongation")) {
    outfile = paste0(file.path(outFilePath, requiredRegion), "/", Cultivar, "_04_StemElongation.csv")
    write_csv(df_04_StemElongation, path=outfile, append=FALSE)
}
if (exists("df_05_EarlyReproductive")) {
    outfile = paste0(file.path(outFilePath, requiredRegion), "/", Cultivar, "_05_EarlyReproductive.csv")
    write_csv(df_05_EarlyReproductive, path=outfile, append=FALSE)
}
if (exists("df_06_GrainSet")) {
    outfile = paste0(file.path(outFilePath, requiredRegion), "/", Cultivar, "_06_GrainSet.csv")
    write_csv(df_06_GrainSet, path=outfile, append=FALSE)
}
if (exists("df_07_GrainFilling")) {
    outfile = paste0(file.path(outFilePath, requiredRegion), "/", Cultivar, "_07_GrainFilling.csv")
    write_csv(df_07_GrainFilling, path = outfile, append=FALSE)
}
if (exists("df_08_Maturing")) {
    outfile = paste0(file.path(outFilePath, requiredRegion), "/", Cultivar, "_08_Maturing.csv")
    write_csv(df_08_Maturing, path = outfile, append=FALSE)
}
if (exists("df_09_Ripening")) {
    outfile = paste0(file.path(outFilePath, requiredRegion), "/", Cultivar, "_09_Ripening.csv")
    write_csv(df_09_Ripening, path = outfile, append=FALSE)
}
if (exists("df_10_Harvest")) {
    outfile = paste0(file.path(outFilePath, requiredRegion), "/", Cultivar, "_10_Harvest.csv")
    write_csv(df_10_Harvest, path = outfile, append=FALSE)
}
