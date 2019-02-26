#Tidy up files

library(tidyverse)

# Directories
apsimFilePath <- "/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/Summaries/"
outFilePath <- "/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/CollatedData"



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
