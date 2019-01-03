#------------------------------------------------------------
# getFrostInformation.R
#------------------------------------------------------------
# This will check each of the Apsim Weather files, and collate a list of Percentiles for all weather files,
# for both Frost and Heat Days

rm(list = ls())
library(tidyverse)

metFilePath <- '/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/modMET_csv'
metListFilename <- paste0(metFilePath, "/", "metFilesList.txt")
#metListFilename <- paste0(metFilePath, "/", "metFilesList_tmp.txt")

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
outfile <- paste0(metFilePath, "/", "percentiles.txt")
# outfile <- paste0(metFilePath, "/", "percentiles_tmp.txt")
headers <- "Location, Long, Lat, LFrost_70P, LFrost_80P, LFrost_90P, FHeat_10P, FHeat_20P, FHeat_30P, FHeat_40P, FHeat_50P"
write(headers, file=outfile)


i <- 1
par_generate <- function(i, met_df, outfile) {
#get data for moree and have a look   15120-2945
#for (i in 45001:51033) {}
#for (i in 1:26) {
    library(tidyverse)
    
    print(paste0("rowId: ", i))
    #this is a temporary break for validation purposes only
    #if (rowId >= 3) {
    #    break
    #}
    #met_df$Filename[i]
    #met_df$Fullname[i]
    
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
    lastFrostsDays <- metData %>% 
        select(year) %>% 
        distinct() %>% 
        arrange(year) %>% 
        merge(mDataFrost, by="year", all.x = TRUE) %>% 
        mutate(dayofYear = ifelse(is.na(dayofYear), 182, dayofYear)) %>% 
        select(dayofYear) %>% 
        arrange(dayofYear)
    
    lastFrostsDays <- unlist(lastFrostsDays)
    LFrost_Probs <- unname(quantile(lastFrostsDays, probs=c(0.70, 0.80, 0.90)))
    LFrost_70P <- LFrost_Probs[1]
    LFrost_80P <- LFrost_Probs[2]
    LFrost_90P <- LFrost_Probs[3]
    
    #Still need threshold for heat days
    #Head day are also only concerned with days after 182
    mDataHeat <- metData %>% 
        mutate(isHeatday = ifelse(maxTemp > 32, "Y", "N")) %>% 
        mutate(isHeatday = ifelse(dayofYear < 182, "N", isHeatday)) %>% 
        filter(isHeatday == "Y") %>% 
        select(year, dayofYear, maxTemp) %>% 
        arrange(year, dayofYear) %>% 
        group_by(year) %>% 
        slice(1) 
    
    firstHeatDays <- metData %>% 
        select(year) %>% 
        distinct() %>% 
        arrange(year) %>% 
        merge(mDataHeat, by="year", all.x = TRUE) %>% 
        mutate(dayofYear = ifelse(is.na(dayofYear), 365, dayofYear)) %>% 
        select(dayofYear) %>% 
        arrange(dayofYear)
    
    firstHeatDays <- unlist(firstHeatDays)
    FHeat_Probs <- unname(quantile(firstHeatDays, probs=c(0.10, 0.20, 0.30, 0.40, 0.50)))
    FHeat_10P <- FHeat_Probs[1]
    FHeat_20P <- FHeat_Probs[2]
    FHeat_30P <- FHeat_Probs[3]
    FHeat_40P <- FHeat_Probs[4]
    FHeat_50P <- FHeat_Probs[5]
    
    outrow = paste(Location, Long, Lat, LFrost_70P, LFrost_80P, LFrost_90P, FHeat_10P, FHeat_20P, FHeat_30P, FHeat_40P, FHeat_50P, sep=", ")
    write(outrow, file=outfile, append=TRUE)
}

#percentfile <- read_csv(outfile)
#Will have a table:
# Location, Long, Lat, LFrost_70P, LFrost_80P, LFrost_90P, FHeat_10P, FHeat_20P, FHeat_30P, FHeat_40P, FH_50P
# 14975  -3035    250
# 15032  -2525    214


library(parallel)
library(snow)
# Get cluster from pearcey
cl <- getMPIcluster()
if (is.null(cl)) {
    # Create a cluster for local
    cl <- makeCluster(parallel::detectCores() - 1, type = 'SOCK')
}

parLapply(cl, seq_len(nrow(met_df)), par_generate, met_df, outfile)




cols <- unlist(strsplit(headers, ","))  #convert the column names (string) to vector
cols <- trimws(cols)    #remove any leading/trailing spaces from names
newData <- data.frame(matrix(ncol=length(cols), nrow=0),stringsAsFactors = FALSE)
colnames(newData) <- cols

#this is to tidy up the file
conn <- file(outfile, open="r")
lines <- readLines(conn) 
i <- 8
for (i in 2:length(lines)) {

    #now see if how many parts we have in this line
    data <- data.frame(str_split(lines[i], ",", simplify=TRUE), stringsAsFactors=FALSE)
    #print(str(i))
    
    #we have only those required for this line, leave as it its
    if (ncol(data) == 11) {
        #write these to the dataframe            
        colnames(data) <- cols
        newData <- rbind(newData, data)
    }
    if (ncol(data) > 11) {
        #get the first set of data
        data2 <- data %>% 
            select(1:11) %>% 
            mutate(X11 = str_trim(X11)) %>% 
            mutate(X11 = substring(X11, 1, 3))
        colnames(data2) <- cols
        newData <- rbind(newData, data2)
        
        #remove first 10 columns
        #then take last 10 from column 11
        data2 <- data %>% 
            select(11:21) %>% 
            mutate(X11 = str_trim(X11),
                   X21 = str_trim(X21)) %>% 
            mutate(X11 = substring(X11, 4, 14),
                   X21 = substring(X21, 1, 3))
        colnames(data2) <- cols
        newData <- rbind(newData, data2)
    }
    if (ncol(data) > 21) {
        data2 <- data %>% 
            select(21:31) %>% 
            mutate(X21 = str_trim(X21),
                   X31 = str_trim(X31)) %>% 
            mutate(X21 = substring(X21, 4, 14),
                   X31 = substring(X31, 1, 3))
        colnames(data2) <- cols
        newData <- rbind(newData, data2)            
    }
    if (ncol(data) > 31) {
        data2 <- data %>% 
            select(31:41) %>% 
            mutate(X31 = str_trim(X31),
                   X41 = str_trim(X41)) %>% 
            mutate(X31 = substring(X31, 4, 14),
                   X41 = substring(X41, 1, 3))
        colnames(data2) <- cols
        newData <- rbind(newData, data2)            
    }
    if (ncol(data) > 41) {
        data2 <- data %>% 
            select(41:51) %>% 
            mutate(X41 = str_trim(X41),
                   X51 = str_trim(X51)) %>% 
            mutate(X41 = substring(X41, 4, 14),
                   X51 = substring(X51, 1, 3))
        colnames(data2) <- cols
        newData <- rbind(newData, data2)            
    }
    if (ncol(data) > 51) {
        print(paste0("more than 41 cols on line ", str(i)))
    }
}


newData2 <- newData %>% 
    arrange(Location)
newData2[2:11] <- sapply(newData2[2:11], as.numeric)


#now we should have all of the data (51032 rows)
newFile <- paste0(metFilePath, "/", "percentiles_new.txt")
write.csv(newData2, file=newFile, row.names=FALSE)
