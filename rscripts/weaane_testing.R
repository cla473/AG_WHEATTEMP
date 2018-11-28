library(weaana)


records <- readWeatherRecords('met/silo_11800-3285.met')

lfd <- lastFrostDay(records, stress = 0)
firstHeatDay(records, stress = 34)

quantile(lfd$lastFrostDay, c(0.1, 0.5, 0.9))

record <- getWeatherRecords(records)
      
library(tidyverse)   
record %>% 
    tbl_df() %>% 
    mutate(mintlessthan1 = mint<1) %>% 
    group_by(year) %>% 
    summarise(n = sum(mintlessthan1) / n())
    count(year)
