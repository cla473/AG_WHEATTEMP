# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   15:32 Thursday, 08 February 2018
# * Copyright: AS IS

# 
# 
# # Download weather records -------------------------------------------------
# rm(list = ls())
# library(tidyverse)
# library(rproject)
# library(My)
# project_fun()
# sites <- taGetSites()
# 
# myDownloadSILO(sites$Number, 'Results/Condor/Mets')
# 



# Merge met and sim files -----------------------------
rm(list = ls())
library(tidyverse)
library(xml2)
sites_files <- list.files('apsimx/', '*.apsimx', full.names = TRUE)
sites <- sub('(.*)\\.apsimx$', '\\1', basename(sites_files))
mets <- paste0('met/silo_', sites, '.met')
df <- data.frame(name = sites,site = sites_files, met = mets, stringsAsFactors = FALSE)

# pos <- file.exists(paste0('/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/Condor/Input/Mets/', df$name, '.RData'))
df <- df[!pos,]

#This creates a compressed .RData file for each site, containing the apsimx file and the met file
i <- 1
par_generate <- function(i, df) {
    library(tidyverse)
    met <- readLines(df$met[i])
    sim <- readLines(df$site[i])
    res <- list(met = met, sim = sim)
    save(res, file = paste0('/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/Condor/Input/Mets/', df$name[i], '.RData'))
}

library(parallel)
library(snow)
# Get cluster from pearcey
cl <- getMPIcluster()
if (is.null(cl)) {
    # Create a cluster for local
    cl <- makeCluster(parallel::detectCores() - 1, type = 'SOCK')
}

parLapply(cl, seq_len(nrow(df)), par_generate, df)



# Create condor project ------------------- 
rm(list = ls())
library(RClusterRun)
sharedrive <- c('\\\\ag-osm-02-cdc.it.csiro.au\\OSM_CBR_AG_WHEATTEMP_source', '\\\\condor-sub-bz6.it.csiro.au')
inputs <- c('\\\\condor-sub-bz6.it.csiro.au\\e\\cla473\\apsimng.exe')
init_commands <- c('apsimng.exe')
finish_commands <- ''
con <- connectTaskDB()
removeProject(con, name = 'apsimng')

newProject(con, 'apsimng', sharedrive = sharedrive, 
           user = 'nexus\\cla473',
           inputs = inputs, init_commands = init_commands,
           finish_commands = finish_commands)
disconnectTaskDB(con)


# Write simulations into database------------------------------------------------
rm(list = ls())

myApsimClusterPreparingTask <- function (project, factors, base, folder_factor, par_factor, 
          input_factor, skip_factor = NULL, folder_prefix = NULL, r_script = "R\\bin\\i386\\Rscript.exe", 
          ids = 0) {
    library(digest)
    if (ids == 0) {
        library(RClusterRun)
        con <- connectTaskDB()
        cleanTasks(con, project)
        disconnectTaskDB(con)
    }
    c_factor <- factors
    if (!is.null(skip_factor)) {
        c_factor <- c_factor[!(names(c_factor) %in% skip_factor)]
    }
    c_par_factor <- c_factor[(names(c_factor) %in% par_factor)]
    c_par_factor <- expand.grid(c_par_factor, stringsAsFactors = FALSE)
    c_com_factor <- expand.grid(c_factor[!(names(c_factor) %in% par_factor)], stringsAsFactors = FALSE)
    
    for (i in seq(length = nrow(c_par_factor))) {
        i_factors <- c_com_factor
        for (j in seq(along = par_factor)) {
            i_factors[[par_factor[j]]] <- c_par_factor[[i, par_factor[j]]]
        }
        v_names <- names(i_factors)
        sim_title <- as.character(apply(i_factors, 1, FUN = function(x) {
            x <- gsub(" ", "", x)
            x <- paste(paste(v_names, x, sep = "="), collapse = ",")
            return(x)
        }))
        
        file_prefix <- unlist(lapply(sim_title, digest, algo = "md5"))
        mysql <- as.data.frame(matrix(NA, nrow = nrow(i_factors), ncol = 0))
        mysql$id <- seq(along = file_prefix) + ids
        ids <- max(mysql$id)
        input_name <- i_factors[, input_factor]
        if (length(input_factor) > 1) {
            input_name <- apply(i_factors[, input_factor], 1, paste, collapse = "_")
        }
        mysql$inputs <- sprintf("%s\\Input\\Mets\\%s.RData", base, input_name)
        mysql$commands <- paste(r_script, " RCode.R ", sim_title, " ", file_prefix, " 1>nul 2>nul", sep = "")
        if (is.null(folder_prefix)) {
            i_folder_output <- paste(c_par_factor[i, ], collapse = "_")
        } else {
            i_folder_output <- paste0(folder_prefix, "/", paste(c_par_factor[i,], collapse = "_"))
        }
        output_dir <- sprintf("%s\\Output\\%s", base, i_folder_output)
        mysql$outputs <- paste(output_dir, "\\", file_prefix, ".RData", sep = "")
        con <- connectTaskDB()
        addTasks(con, project, mysql)
        disconnectTaskDB(con)
        output_files <- i_factors
        output_files$prefix <- file_prefix
        output_files$outputs <- paste0(i_folder_output, "/", file_prefix, ".RData")
        save(output_files, mysql, file = file.path(folder_factor, 
                                                   sprintf("%s.RData", i_folder_output)), compress = TRUE)
    }
    folder_outputs <- apply(c_par_factor, 1, paste, collapse = "_")
    if (!is.null(folder_prefix)) {
        folder_outputs <- paste0(folder_prefix, "/", folder_outputs)
    }
    lapply(paste0("Condor/Output/", folder_outputs), dir.create, showWarnings = FALSE)
    ids
}


#-----------------------------------------------------------------------
library(tidyverse)
sites_files <- list.files('apsimx/', '*.apsimx', full.names = TRUE)
sites <- sub('(.*)\\.apsimx$', '\\1', basename(sites_files))
factors <- read_csv('CultivarSowDateMatrix.csv')
cultivars <- unique(factors$Cultivar)
sowdate <- unique(factors$SowDate)

factors <- list(cultivars = cultivars, sites = sites)
r_script = 'R\\bin\\i386\\Rscript.exe'
ids <- myApsimClusterPreparingTask(
    project = 'apsimng', 
    factors = factors,
    base = '\\\\ag-osm-02-cdc.it.csiro.au\\OSM_CBR_AG_WHEATTEMP_source\\ApsimNG-LC\\Condor',
    folder_factor = 'Condor/TraitFactor',
    par_factor = c('cultivars'),
    input_factor = 'sites',
    skip_factor = NULL,
    folder_prefix = NULL,
    r_script = r_script)


# check for missing files -----------------------------------------------------------
rm(list = ls())
library(tidyverse)
# clear the 'temp' directory which is where this writes to
lapply(list.files('temp', full.names = TRUE), file.remove)
files <- list.files('Condor/TraitFactor/', "*.RData", full.names = TRUE)
i <- 1
par_check <- function(i, files) {
    load(files[i])
    out_files <- paste0('/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/Condor/Output/', output_files$outputs)
    pos <- file.exists(out_files)
    if (sum(!pos) == 0) {
        return()
    }
    mysql <- mysql[!pos,]
    save(mysql, file = paste0('temp/', basename(files[i]), '.RData'))
}


# Create a cluster for local
library(parallel)
library(snow)
# Get cluster from pearcey
cl <- getMPIcluster()
if (is.null(cl)) {
    cl <- makeCluster(parallel::detectCores() - 1, type = 'SOCK')
}

parLapply(cl, seq_along(files), par_check, files)


missing_files <- list.files('temp', full.names = TRUE)
res <- NULL
for (i in seq(along = missing_files)) {
    load(missing_files[i])
    res[[i]] <- mysql
}

#now connect with db, clear old jobs, save new jobs
library(tidyverse)
res <- bind_rows(res)
library(RClusterRun)
con <- connectTaskDB()
cleanTasks(con, 'apsimng')
addTasks(con, 'apsimng', res)
disconnectTaskDB(con)



library(RClusterRun)
con <- connectTaskDB()
cleanTasks(con, 'apsimng')
disconnectTaskDB(con)

missing_files <- list.files('temp', full.names = TRUE)

for (i in seq(along = missing_files)) {
    load(missing_files[i])
    
    library(RClusterRun)
    con <- connectTaskDB()
    addTasks(con, 'apsimng', mysql)
    disconnectTaskDB(con)
}
#now connect with db, clear old jobs, save new jobs
library(tidyverse)
#DONT FORGET:
# to go to Condor Project Control ("//caps-condor.it.csiro.au/cluster/control") and start the project
# go to Rabbit MQ Management and check that it is running



# Compress the input files -----------------------------
rm(list = ls())
library(RAPSIM)
# Create the RCode.R 
rscript <- readLines('taCondor.R')
pos <- grep('args <- commandArgs\\(TRUE\\)', rscript)
rscript <- rscript[seq(pos, length(rscript))]
rcode_file <- 'Condor/InputFilesTrait/RCode.R'
writeLines(rscript, rcode_file)

input <- './Condor/InputFilesTrait/*'
compressInputs(input, 'E:/cla473/apsimng.exe')
# file.remove(rcode_file)





# Test this project -----------------------------
rm(list = ls())
library(RClusterRun)
con <- connectTaskDB()
testProject(con, 'apsimng')
disconnectTaskDB(con)






# Run jobs under condor -----------------------------------
rm(list = ls())
setwd('Condor/test')

sim_title <- 'sites=13585-3380,cultivars=Agtscythe,sowdate=1-apr'
file_prefix <- '24c1d880447a2e671dd70e65c956136b'

args <- commandArgs(TRUE)

sim_title <- args[[1]]
file_prefix <- args[[2]]

sowdate <- "15-mar, 1-apr, 15-apr, 29-apr, 13-may, 27-may, 3-jun, 17-jun, 1-jul, 15-jul, 29-jul"

# Read head information from out file
splitTitle <- function(title, ...) {
    temp <- strsplit(title, ' *, *')[[1]]
    temp <- strsplit(temp, ' *= *')
    temp <- as.data.frame(t(as.data.frame(temp)))
    row.names(temp) <- seq(length = nrow(temp))
    names(temp) <- c('name', 'value')
    res <- as.character(temp$value)
    names(res) <- as.character(temp$name)
    res <- t(as.data.frame(res, stringsAsFactors = FALSE))
    return(as.data.frame(res))
}

replaceElementVlaue <- function(value, xml, key = NULL) {    
    if (!is.null(key)) {
        pos <- grep(sprintf('<%s.*>(.*)</%s>', key, key), xml)
        if (length(pos) == 0) {
            stop(paste0('Cannot find "', key, '".'))
        }
    } else {
        pos <- seq(along = xml)
    }
    value <- rep(value, length = length(pos))
    value <- as.character(unlist(value))
    for (i in seq(along = pos)) {
        c_value <- gsub("\\\\", "\\\\\\\\", value[i])
        xml[pos[i]] <- sub(paste("(^.*<.*>).*(</.*>.*$)", sep = ""),
                           paste("\\1", c_value, "\\2", sep = ""), xml[pos[i]])
    }
    return(xml)
}


sim_factors <- splitTitle(sim_title)
file_name <- paste0(sim_factors$sites, '.RData')

load(file_name)
sim <- res$sim
met <- res$met

# Find the genotype

sim_name <- paste(sim_factors$sites, '.apsimx', sep = '')
met_name <- paste(sim_factors$sites, '.met', sep = '')
writeLines(met, met_name)


#this will swap the sow date from "Experiment Sowing Rule"
pos_sow_date <- grep('<string>\\[SowingRule\\].Script.SowDate *= *(.*)</string>', sim)
sim[pos_sow_date] <- sub('(<string>\\[SowingRule\\].Script.SowDate *= *)(.*)(</string>)',
                         paste0('\\1', sowdate, '\\3'), sim[pos_sow_date])

#this will swap the cultivar from "Experiment Sowing Rule"
pos_cultivar <- grep('<string>\\[SowingRule\\].Script.CultivarName *= *(.*)</string>', sim)
sim[pos_cultivar] <- sub('(<string>\\[SowingRule\\].Script.CultivarName *= *)(.*)(</string>)',
                         paste0('\\1', sim_factors$cultivars, '\\3'), sim[pos_cultivar])

#this will change the met filename to be relative"
pos_met <- grep('<FileName>(.*)</FileName>', sim)
sim[pos_met] <- sub('(<FileName>)(.*)(</FileName>)', paste0('\\1', met_name, '\\3'), sim[pos_met])


writeLines(sim, sim_name)

# Run Apsimx
# sim_name <- "13585-3380"

apsim <- 'minimum_apsimx/Models.exe'
cmd <- paste0(apsim, ' ', sim_name, ' /Csv')
system(cmd)

# Read output
sim_name <- gsub('(.*)\\.apsimx$', '\\1', sim_name)
out <- read.csv(paste0(sim_name, '.Report.csv'), as.is = TRUE)
out$Year <- format(as.Date(as.character(out$Clock.Today)), "%Y")
out$Clock.Today = as.Date(as.character(out$Clock.Today),format="%Y-%m-%d")
out$sowingDate = as.Date(as.character(paste0(out$SowDate, "-", out$Year)), format("%d-%b-%Y"))

#out <- subset(out, Clock.Today >= sowingDate)
out <- subset(out, Wheat.Phenology.Stage > 1)
#out <- subset(out, Wheat.Phenology.Zadok.Stage > 5)
#out <- subset(out, (Clock.Today > sowingDate & Wheat.Phenology.Zadok.Stage > 5))
write.csv(x=out, file=paste0(sim_name, '.Report.csv'))

head <- data.frame(name = names(sim_factors), value = as.vector(as.matrix(sim_factors)))

res <- list(head = head, out = out)
save(res, file = paste0(file_prefix, '.RData'))

