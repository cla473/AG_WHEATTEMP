#------------------------------------------------------------
# getFrostInformation.R
#------------------------------------------------------------
# This will check each of the Apsim Weather files, and collate a list of Percentiles for all weather files,
# for both Frost and Heat Days

rm(list = ls())
library(tidyverse)

dirs <- list.dirs(path = '/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/Summaries/', full.names = TRUE, recursive = FALSE)
dir_df <- data.frame(fullpath=dirs, stringsAsFactors=FALSE)
#str(dir_df)

dir_df['location'] <- lapply(dir_df['fullpath'], gsub, pattern='/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/Summaries/', replacement='', fixed=TRUE)
dir_df['location'] <- lapply(dir_df['location'], gsub, pattern='/', replacement='', fixed=TRUE)

i <- 2
for( i in 104:nrow(dir_df)) 
{
    currentDir <- dir_df['fullpath'][i,]
    currentFiles <- list.files(path = currentDir, pattern = "*.csv", recursive = FALSE, full.names = TRUE) 
    files_df <- data.frame(fullpath=currentFiles, stringsAsFactors=FALSE)
    rm(currentFiles)
    #str(files_df)
    files_df2 <- files_df %>% 
        mutate(filename = basename(fullpath)) %>% 
        filter(!str_detect(filename, "Summary_"))
    
    if (nrow(files_df2) > 0 )
    {
        for (j in 1:nrow(files_df2)) 
        {
            basefilename = files_df2["fullpath"][j,]
            data <- read_csv(basefilename)
            #str(data)
            data2 <- data %>% 
                select(-1)
            
            rm(data)
            #now get the new filename
            newbasefilename <- str_replace(basefilename, ".csv", ".RData")
            save(data2, file=newbasefilename)
            
            #now check that original exists, and then remove original
            if (file.exists(newbasefilename)) 
            {
                file.remove(basefilename)
            }
            rm(data2)
        }
    }
    rm(files_df2)
}




