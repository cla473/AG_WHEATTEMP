#------------------------------------------------------------
# getFrostInformation.R
#------------------------------------------------------------
# This will check each of the Apsim Weather files, and collate a list of Percentiles for all weather files,
# for both Frost and Heat Days

rm(list = ls())
library(tidyverse)
library(xml2)
library(weaana)

metListFile <- "//OSM/CBR/AG_WHEATTEMP/work/ApsimNG-LC/met/metList.csv"
met_df <- read_csv(metListFile)

i <- 1
met_df$filename[i]
met_df$fullname[i]

#metColNames <- c('year', 'day', 'radn', 'maxt', 'mint', 'rain', 'evap', 'vp', 'code')
#metData <- read.csv(met_df$fullname[i], header=FALSE, skip=6, col_names = metColNames, stringsAsFactors=FALSE)
#metData <- read_csv(met_df$fullname[i], skip=10, col_names = metColNames)
#metData$runDate = as.Date(metData$runDate, format="%Y-%m-%d")

#filename <- "/OSM/CBR/AG_WHEATTEMP/work/ApsimNG-LC/met/silo_14375-353.met"
filename <- "/OSM/CBR/AG_WHEATTEMP/work/ApsimNG-LC/met/silo_14975-3035.met"
#filename <- met_df$fullname[i]

library(APSIM)
met <- loadMet(filename)
mData <- slot(met, "data")
str(mData)

#1.  Need to get the DOY of the Last Frost (<=0) for each year,
#    If non exists then assign DOY 182
#    If the DOY is les then 182 then set it to 182


#this dataframe will contain only is the last frost day records for year that has a last frost day
mData3 <- mData %>% 
    mutate(isFrostday = ifelse(mint < 0, "Y", "N")) %>% 
    mutate(isFrostday = ifelse(day < 182, "N", isFrostday)) %>% 
    filter(isFrostday == "Y") %>% 
    select(year, day, mint) %>% 
    arrange(year, desc(day)) %>% 
    group_by(year) %>% 
    slice(1) 
    
# the following builds a dataframe of years and then combines it with the dataframe from above,
# and then replaces any missing values (NA) with 182.
firstFrostsDays <- mData %>% 
    select(year) %>% 
    distinct() %>% 
    arrange(year) %>% 
    merge(mData3, by="year", all.x = TRUE) %>% 
    mutate(day = ifelse(is.na(day), 182, day)) %>% 
    select(day) %>% 
    arrange(day)

firstFrostsDays <- unlist(firstFrostsDays)
quantile(firstFrostsDays, probs=c(0.10, 0.50, 0.90))


#Still need threshold for heat days (ie, frost is <0)
#Head day are also only concerned with days afte 182


#Will have a table:
# Long    Lat     FFrost_90P  FHeat_10P, FHeat_20P, FHeat_30P, FHeat_40P, FH_50P
# 14975  -3035    250
# 15032  -2525    214


