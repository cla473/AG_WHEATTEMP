#------------------------------------------------------------
# getFrostInformation.R
#------------------------------------------------------------
# This will check each of the Apsim Weather files, and collate a list of Percentiles for all weather files,
# for both Frost and Heat Days

rm(list = ls())
library(tidyverse)

metFilePath <- '/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/modMET_csv'
metListFilename <- paste0(metFilePath, "/", "metFilesList.txt")

# #Only need to do this if the is no file
# metFilesList <- list.files(path=metFilePath, pattern=".csv", recursive = FALSE, full.names = TRUE)
# metList_df <- data.frame(metFilesList) %>%
#     select(Fullname = metFilesList) %>%
#     mutate(Fullname = as.character(Fullname)) %>%
#     mutate(Filename = basename(Fullname)) %>%
#     select(Filename, Fullname)
# head(metList_df)
# write.csv(metList_df, file=metListFilename, row.names=FALSE)
met_df <- read_csv(metListFilename)
 
#start with empty outputfile
# headers <- "Location, Long, Lat, FFrost_90P, FHeat_10P, FHeat_20P, FHeat_30P, FHeat_40P, FHeat_50P"
outfile <- paste0(metFilePath, "/", "percentiles.txt")
# write(headers, file=outfile)
i <- 10158

for (i in 45001:51033)
{
    print(paste0("rowId: ", i))
    #this is a temporary break for validation purposes only
    #if (rowId >= 3) {
    #    break
    #}
    met_df$Filename[i]
    met_df$Fullname[i]
    
    #from the Filename, we can get the long and lat
    Location <- str_replace(met_df$Filename[i], ".csv", "")
    Long <- as.numeric(unlist(strsplit(Location, '-'))[1]) / 100
    Lat <- as.numeric(unlist(strsplit(Location, '-'))[2]) / 100 * -1
    
    metData <- read_csv(met_df$Fullname[i])
    
    #1.  Need to get the DOY of the Last Frost (<=0) for each year,
    #    If non exists then assign DOY 182
    #    If the DOY is les then 182 then set it to 182
    
    
    #this dataframe will contain only is the last frost day records for year that has a last frost day
    mDataFrost <- metData %>% 
        mutate(isFrostday = ifelse(minTemp < 0, "Y", "N")) %>% 
        mutate(isFrostday = ifelse(dayofYear < 182, "N", isFrostday)) %>% 
        filter(isFrostday == "Y") %>% 
        select(year, dayofYear, minTemp) %>% 
        arrange(year, desc(dayofYear)) %>% 
        group_by(year) %>% 
        slice(1) 
        
    # the following builds a dataframe of years and then combines it with the dataframe from above,
    # and then replaces any missing values (NA) with 182.
    firstFrostsDays <- metData %>% 
        select(year) %>% 
        distinct() %>% 
        arrange(year) %>% 
        merge(mDataFrost, by="year", all.x = TRUE) %>% 
        mutate(dayofYear = ifelse(is.na(dayofYear), 182, dayofYear)) %>% 
        select(dayofYear) %>% 
        arrange(dayofYear)
    
    firstFrostsDays <- unlist(firstFrostsDays)
    FFrost_90P <- unname(quantile(firstFrostsDays, probs=c(0.90)))
    
    #Still need threshold for heat days (ie, frost is <0)
    #Head day are also only concerned with days afte 182
    mDataHeat <- metData %>% 
        mutate(isHeatday = ifelse(maxTemp > 32, "Y", "N")) %>% 
        mutate(isHeatday = ifelse(dayofYear < 182, "N", isHeatday)) %>% 
        filter(isHeatday == "Y") %>% 
        select(year, dayofYear, maxTemp) %>% 
        arrange(year, desc(dayofYear)) %>% 
        group_by(year) %>% 
        slice(1) 
    
    firstHeatDays <- metData %>% 
        select(year) %>% 
        distinct() %>% 
        arrange(year) %>% 
        merge(mDataHeat, by="year", all.x = TRUE) %>% 
        mutate(dayofYear = ifelse(is.na(dayofYear), 182, dayofYear)) %>% 
        select(dayofYear) %>% 
        arrange(dayofYear)
    
    firstHeatDays <- unlist(firstHeatDays)
    FHeat_Probs <- unname(quantile(firstHeatDays, probs=c(0.10, 0.20, 0.30, 0.40, 0.50)))
    FHeat_10P <- FHeat_Probs[1]
    FHeat_20P <- FHeat_Probs[2]
    FHeat_30P <- FHeat_Probs[3]
    FHeat_40P <- FHeat_Probs[4]
    FHeat_50P <- FHeat_Probs[5]
    
    outrow = paste(Location, Long, Lat, FFrost_90P, FHeat_10P, FHeat_20P, FHeat_30P, FHeat_40P, FHeat_50P, sep=", ")
    write(outrow, file=outfile, append=TRUE)
}

#percentfile <- read_csv(outfile)
#Will have a table:
# Long    Lat     FFrost_90P  FHeat_10P, FHeat_20P, FHeat_30P, FHeat_40P, FH_50P
# 14975  -3035    250
# 15032  -2525    214


