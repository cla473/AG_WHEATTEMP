
library("tidyverse")

origFile <- "/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/metCalcs/CultivarListing.csv"
outFile <- "/OSM/CBR/AG_WHEATTEMP/source/ApsimNG-LC/metCalcs/CultivarListing_working.csv"
origFile_df <- read_csv(origFile)
head(origFile_df, 10)

maturityGroups <- unlist(origFile_df %>% 
    select(MaturityGroup) %>% 
    unique())
maturityGroups

base_df <- origFile_df %>% 
    select(RegionID, Cultivar)

output_df <- bind_rows(
    base_df %>% 
        mutate(MaturityGroup = "Fast"), 
    base_df %>% 
        mutate(MaturityGroup = "Mid"), 
    base_df %>% 
        mutate(MaturityGroup = "Long")) %>% 
    mutate(Phase = "05_EarlyReproductive") %>% 
    arrange(RegionID, Cultivar, MaturityGroup)

output_df <- tibble::rowid_to_column(output_df, "LineNo")
write_csv(output_df, path=outFile, append=FALSE)

