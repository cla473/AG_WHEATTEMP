rm(list = ls())
library(tidyverse)


cultDirs <- list.dirs(path = '/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/Condor/Output/', full.names = TRUE, recursive = FALSE)
x <- data.frame(fullpath=cultDirs, no=0)

x['cultivar'] <- lapply(x['fullpath'], gsub, pattern='/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/Condor/Output/', replacement='', fixed=TRUE)
x['cultivar'] <- lapply(x['cultivar'], gsub, pattern='/', replacement='', fixed=TRUE)

total = 0
for( i in seq(length(cultDirs))) {
    x$no[i] <- length(list.files(path = cultDirs[i], recursive = FALSE, full.names = TRUE)  )
    total = total + x$no[i]
}

print(paste0("total count: ", total))


# --------------------------------------------------------
# Check to see which jobs have been completed or not
# --------------------------------------------------------
rm(list = ls())
library(RClusterRun)
library(bindrcpp)
con <- connectTaskDB()
dbListTables(con)
jobs <- dbGetQuery(con, 'SELECT * FROM task_apsimng')
disconnectTaskDB(con)
str(jobs)
temps <- jobs %>% 
    select(id, outputs) %>% 
    mutate(filename = basename(outputs))
head(temps,5)

#pos <- file.exist(paste0('/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/Condor/Output/', output_files$outputs)
#mysql <- mysql[!pos,]
