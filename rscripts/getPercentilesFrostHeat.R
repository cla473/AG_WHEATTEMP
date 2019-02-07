#------------------------------------------------------------
# getPercentilesFrostHeat.R
#------------------------------------------------------------
# This will check each of the Apsim Weather files, and collate a list of Percentiles for all weather files,
# for both Frost and Heat Days

rm(list = ls())
library(tidyverse)

metFilePath <- '/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/modMET_csv'
metListFilename <- paste0(metFilePath, "/", "metFilesList.txt")
outFilePath <- '/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/metCalcs'
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
outfile_Percentile <- paste0(outFilePath, "/", "percentiles2.csv")
headers <- "Location, Long, Lat, "
headers <- paste0(headers, "LFrost_70P, LFrost_80P, LFrost_90P, ")
headers <- paste0(headers, "FHeat_10P, FHeat_20P, FHeat_30P, FHeat_40P, FHeat_50P, ")
headers <- paste0(headers, "FWL1, FWL2, FWL3, FWL4, FWL5, ")
headers <- paste0(headers, "DIFFH1020, DIFFH1030, DIFFH1040, DIFFH1050")
write(headers, file=outfile_Percentile)

outfile_Frost <- paste0(outFilePath, "/", "LastFrosts.csv")
outfile_Heat <- paste0(outFilePath, "/", "FirstHeats.csv")
FrostHeat_headers <- paste(as.character(c("Location", "Long", "Lat", 1957:2017)), sep= "' '", collapse=", ")
write(FrostHeat_headers, file=outfile_Frost)
write(FrostHeat_headers, file=outfile_Heat)

i <- 10
#par_generate <- function(i, met_df, outfile_Percentile, outfile_Heat, outfile_Frost) {
#for (i in 1:51033) {}
for (i in 40001:51032) {
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
        select(year, dayofYear) %>% 
        arrange(year)
    
    #add this the the the last frost file:
    #+------------+--------+--------+------+------+---------------
    #| Location   | Long   | Lat    | 1957 | 1958 | 1960 | etc ...
    #+------------+--------+--------+------+------+------+--------
    #| 11400-2725 | 114.00 | -27.25 |  182 |  182 |  182 | 182 ...
    #| 11400-2730 | 114.00 | -27.30 |  187 |  185 |  182 | 185 ...
    LastFrosts <- lastFrostsDays %>% 
        spread(key = year, value=dayofYear) %>% 
        mutate(Location = Location,
               Long = Long,
               Lat = Lat) %>% 
        select(Location, Long, Lat, everything())

    #write this to the Last Frost File - don't forget to exclude the header
    write.table(LastFrosts, file=outfile_Frost, sep = "," , append = TRUE, col.names = FALSE, row.names = FALSE)

    #now we can do the percentile calculations
    lastFrostsDays <- lastFrostsDays %>% 
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
        select(year, dayofYear) %>% 
        arrange(year)

    
    FirstHeats <- firstHeatDays %>% 
        spread(key = year, value=dayofYear) %>% 
        mutate(Location = Location,
               Long = Long,
               Lat = Lat) %>% 
        select(Location, Long, Lat, everything())
    
    #write this to the Last Frost File - don't forget to exclude the header
    write.table(FirstHeats, file=outfile_Heat, sep = "," , append = TRUE, col.names = FALSE, row.names = FALSE)

    firstHeatDays <- firstHeatDays %>% 
        select(dayofYear) %>% 
        arrange(dayofYear) 
        
    firstHeatDays <- unlist(firstHeatDays)
    FHeat_Probs <- unname(quantile(firstHeatDays, probs=c(0.10, 0.20, 0.30, 0.40, 0.50)))
    FHeat_10P <- FHeat_Probs[1]
    FHeat_20P <- FHeat_Probs[2]
    FHeat_30P <- FHeat_Probs[3]
    FHeat_40P <- FHeat_Probs[4]
    FHeat_50P <- FHeat_Probs[5]
    
    #need to add extra columns here
    # FWL1	Length of the flowering window between the 90th frost percentile and 10th heat percentile
    # FWL2	Length of the flowering window between the 90th frost percentile and 20th heat percentile
    # FWL3	Length of the flowering window between the 90th frost percentile and 30th heat percentile
    # FWL4	Length of the flowering window between the 90th frost percentile and 40th heat percentile
    # FWL5	Length of the flowering window between the 90th frost percentile and 50th heat percentile
    FWL1 <- FHeat_10P - LFrost_90P
    FWL2 <- FHeat_20P - LFrost_90P
    FWL3 <- FHeat_30P - LFrost_90P
    FWL4 <- FHeat_40P - LFrost_90P
    FWL5 <- FHeat_50P - LFrost_90P
    # 
    # DIFFH1020	Number of days between 10th and 20th percentile for heat
    # DIFFH1030	Number of days between 10th and 30th percentile for heat
    # DIFFH1040	Number of days between 10th and 40th percentile for heat
    # DIFFH1050	Number of days between 10th and 50th percentile for heat
    DIFFH1020 <- FHeat_20P - FHeat_10P
    DIFFH1030 <- FHeat_30P - FHeat_10P
    DIFFH1040 <- FHeat_40P - FHeat_10P
    DIFFH1050 <- FHeat_50P - FHeat_10P
    
    #now generate the string to output to the file 
    outrow <- paste(Location, Long, Lat, sep=", ")
    outrow <- paste(outrow, LFrost_70P, LFrost_80P, LFrost_90P, sep=", ")
    outrow <- paste(outrow, FHeat_10P, FHeat_20P, FHeat_30P, FHeat_40P, FHeat_50P, sep=", ")
    outrow <- paste(outrow, FWL1, FWL2, FWL3, FWL4, FWL5, sep=", ")
    outrow <- paste(outrow, DIFFH1020, DIFFH1030, DIFFH1040, DIFFH1050, sep=", ")
    write(outrow, file=outfile_Percentile, append=TRUE)
}

#percentfile <- read_csv(outfile_Percentile)
#Will have a table:
# Location, Long, Lat, LFrost_70P, LFrost_80P, LFrost_90P, FHeat_10P, FHeat_20P, FHeat_30P, FHeat_40P, FH_50P
# 14975  -3035    250
# 15032  -2525    214


#=====================================================================================
# this is used to run the above as a function in parallel
# this works very quickly, but does manage to mess the file up a bit, and the code
# show below is required to sort it out agian
#=====================================================================================

library(parallel)
library(snow)
# Get cluster from pearcey
cl <- getMPIcluster()
if (is.null(cl)) {
    # Create a cluster for local
    cl <- makeCluster(parallel::detectCores() - 1, type = 'SOCK')
}

#parLapply(cl, seq_len(nrow(met_df)), par_generate, met_df, outfile_Percentile)
parLapply(cl, seq_len(nrow(met_df)), par_generate, met_df, outfile_Percentile, outfile_Heat, outfile_Frost)



#=====================================================================================
# This is to tidy up the file
# some of the data being written to the file is on the same line as other data, and is
# followed by a blank row.  This can happen multiple times.
#=====================================================================================
cols <- unlist(strsplit(headers, ","))  #convert the column names (string) to vector
cols <- trimws(cols)    #remove any leading/trailing spaces from names
newData <- data.frame(matrix(ncol=length(cols), nrow=0),stringsAsFactors = FALSE)
colnames(newData) <- cols

conn <- file(outfile_Percentile, open="r")
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

#=====================================================================================
# This function has been provided by Bangyou Zheng 13/12/2018
# Some utility functions for weather analysis
# Significantly t-test with autocorrelation for time serial data 
# 
# Method is presented by Santer et al. 2000
# @param x A vector of time serial data
# @export

ttest_ts <- function(y, slope = NULL)
{
    if(sum(is.na(y)) == 0) {
        y <- as.numeric(y)
        num <- length(y)
        x <- seq(along = y)
        if (is.null(slope))
        {
            slope <- cor(x, y) * sd(y)/sd(x)
        }
        
        sb_m <- sqrt(sum((x - mean(x)) ^ 2))
        inercept <- (sum(y) - slope * sum(x)) / num
        et_x <- y - (inercept + slope * x)
        ne_x <- cor(et_x[-1], et_x[-(num)])
        ne_x <- num * (1 - ne_x) / (1 + ne_x)
        se_x <- sqrt((1 / (ne_x - 2)) * sum(et_x * et_x, na.rm = TRUE))
        sb_x <- se_x / sb_m
        tb_x <- abs(slope / sb_x)
        p_x <- (1 - pt(tb_x, df = ne_x - 2)) * 2
        return (p_x)
    } else {
        return (NA)
    }
}

ttest_slope <- function(y) {
    if(sum(is.na(y)) == 0) {
        y <- as.numeric(y)
        x <- seq(along = y)
        slope <- cor(x, y) * sd(y)/sd(x)
    } else {
        return (NA)
    }
}

#=====================================================================================

#=====================================================================================
# This will open the ForstHeat and LastFrost files and add the mean/median for each
# Not sure how this wil be used as yet.
#=====================================================================================

lastFrostsDays <- read_csv(outfile_Frost)
LFMean <- lastFrostsDays %>% 
    mutate(LFrost_mean = unlist(select(., `1957`:`2017`) %>% 
                                    pmap(~ mean(c(...))))) %>% 
    mutate(LFrost_median = unlist(select(., `1957`:`2017`) %>% 
                                      pmap(~ median(c(...))))) %>% 
    mutate(LFrost_slope = unlist(select(., `1957`:`2017`) %>% 
                                      pmap(~ ttest_slope(c(...))))) %>% 
    mutate(LFrost_ttests = unlist(select(., `1957`:`2017`) %>% 
                                      pmap(~ ttest_ts(c(...))))) %>% 
    select(Location, Long, Lat, LFrost_mean, LFrost_median, LFrost_slope, LFrost_ttests)


firstHeatDays <- read_csv(outfile_Heat)
FHMean <- firstHeatDays %>% 
    mutate(FHeat_mean = unlist(select(., `1957`:`2017`) %>% 
                                   pmap(~ mean(c(...))))) %>% 
    mutate(FHeat_median = unlist(select(., `1957`:`2017`) %>% 
                                     pmap(~ median(c(...))))) %>% 
    mutate(FHeat_slope = unlist(select(., `1957`:`2017`) %>% 
                                     pmap(~ ttest_slope(c(...))))) %>% 
    mutate(FHeat_ttests = unlist(select(., `1957`:`2017`) %>% 
                                     pmap(~ ttest_ts(c(...))))) %>% 
    select(Location, Long, Lat, FHeat_mean, FHeat_median, FHeat_slope, FHeat_ttests)


percentiles <- read_csv(outfile_Percentile)
# #This is what is in the original percentile file
# percentiles <- percentiles %>%
#     select(Location, Long, Lat, LFrost_70P, LFrost_80P, LFrost_90P,
#        FHeat_10P, FHeat_20P, FHeat_30P, FHeat_40P, FHeat_50P,
#        FWL1, FWL2, FWL3, FWL4, FWL5,
#        DIFFH1020, DIFFH1030, DIFFH1040, DIFFH1050)

newPercentiles <- merge(percentiles, LFMean, by=c("Location", "Long", "Lat"))
newPercentiles <- merge(newPercentiles, FHMean, by=c("Location", "Long", "Lat"))

newPercentiles <- newPercentiles %>% 
    select(Location, Long, Lat, LFrost_70P, LFrost_80P, LFrost_90P, 
           LFrost_mean, LFrost_median, LFrost_slope, LFrost_ttests,
           FHeat_10P, FHeat_20P, FHeat_30P, FHeat_40P, FHeat_50P, 
           FHeat_mean, FHeat_median, FHeat_slope, FHeat_ttests,
           FWL1, FWL2, FWL3, FWL4, FWL5,
           DIFFH1020, DIFFH1030, DIFFH1040, DIFFH1050)

new_Percentile <- paste0(outFilePath, "/", "percentiles.csv")
write.csv(newPercentiles, file=new_Percentile, row.names=FALSE)

#==============================================================================================
# 02 February 2019
# Need to add to the DIFF set:
#   DIFF2030, DIFF3040, DIFF4050
#==============================================================================================

rm(list = ls())
library(tidyverse)

outFilePath <- '/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/metCalcs'
outfile_Percentile <- paste0(outFilePath, "/", "percentiles.csv")
percentiles_df <- read_csv(outfile_Percentile)

percentiles_df <- percentiles_df %>% 
    mutate(DIFFH2030 = FHeat_30P - FHeat_20P,
           DIFFH3040 = FHeat_40P - FHeat_30P,
           DIFFH4050 = FHeat_50P - FHeat_40P)

write.csv(percentiles_df, file=outfile_Percentile, row.names=FALSE)
