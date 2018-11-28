#---------------------------------------------------
# Write job details (Apsim simulations) into database for Running on Condor
# checks the progress of the condor jobs by checking the number of output files
#---------------------------------------------------
# written for running on Windows
#---------------------------------------------------

rm(list = ls())

myApsimClusterPreparingTask <- function (project, factors, base, folder_factor, par_factor, 
                                         input_factor, skip_factor = NULL, folder_prefix = NULL, r_script = "R\\bin\\i386\\Rscript.exe", 
                                         ids = 0) 
{
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

    for (i in seq(length = nrow(c_par_factor))) 
    {
        i_factors <- c_com_factor
        for (j in seq(along = par_factor)) 
        {
            i_factors[[par_factor[j]]] <- c_par_factor[[i, par_factor[j]]]
        }
        v_names <- names(i_factors)
        sim_title <- as.character(apply(i_factors, 1, FUN = function(x) 
        {
            x <- gsub(" ", "", x)
            x <- paste(paste(v_names, x, sep = "="), collapse = ",")
            return(x)
        }))
        file_prefix <- unlist(lapply(sim_title, digest, algo = "md5"))
        mysql <- as.data.frame(matrix(NA, nrow = nrow(i_factors), ncol = 0))
        mysql$id <- seq(along = file_prefix) + ids
        ids <- max(mysql$id)
        input_name <- i_factors[, input_factor]
        if (length(input_factor) > 1) 
        {
            input_name <- apply(i_factors[, input_factor], 1, paste, collapse = "_")
        }
        mysql$inputs <- sprintf("%s\\Input\\Mets\\%s.RData", base, input_name)
        mysql$commands <- paste(r_script, " RCode.R ", sim_title, " ", file_prefix, " 1>nul 2>nul", sep = "")
        if (is.null(folder_prefix)) 
        {
            i_folder_output <- paste(c_par_factor[i, ], collapse = "_")
        }
        else 
        {
            i_folder_output <- paste0(folder_prefix, "/", paste(c_par_factor[i, ], collapse = "_"))
        }
        output_dir <- sprintf("%s\\Output\\%s", base, i_folder_output)
        mysql$outputs <- paste(output_dir, "\\", file_prefix, ".RData", sep = "")
        print(nrow(mysql))
        con <- connectTaskDB()
        addTasks(con, project, mysql)
        disconnectTaskDB(con)
        output_files <- i_factors
        output_files$prefix <- file_prefix
        output_files$outputs <- paste0(i_folder_output, "/", file_prefix, ".RData")
        save(output_files, mysql, file = file.path(folder_factor, sprintf("%s.RData", i_folder_output)), compress = TRUE)
    }
    folder_outputs <- apply(c_par_factor, 1, paste, collapse = "_")
    if (!is.null(folder_prefix)) 
    {
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
sowdate <- sowdate[1:6]
factors <- list(cultivars = cultivars, sowdate = sowdate, sites = sites)
r_script = 'R\\bin\\i386\\Rscript.exe'
ids <- myApsimClusterPreparingTask(
    project = 'apsimng', 
    factors = factors,
    base = '\\\\ag-osm-02-cdc.it.csiro.au\\OSM_CBR_AG_WHEATTEMP_source\\ApsimNG-LC\\Condor',
    folder_factor = 'Condor/TraitFactor',
    par_factor = c('cultivars', 'sowdate'),
    input_factor = 'sites',
    skip_factor = NULL,
    folder_prefix = NULL,
    r_script = r_script)



