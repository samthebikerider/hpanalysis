#####################################################
# Author: Samuel Rosenberg
# Company: Pacific Northwest National Laboratory
# Created on: 2023-08-03
# Description: 
#####################################################
# Todo:
# 
# 
# 
#####################################################
############ data import and processing #############
#####################################################
# Clear Workspace
rm(list = ls())
library(tidyverse)
library(lubridate)

# packages
if(Sys.info()[7] == "rose775"){
  source("/Users/rose775/Library/CloudStorage/OneDrive-PNNL/Desktop/Projects/ccHP/hpanalysis/functions_for_use.R")
  e350_min_sites_wd <- "/Volumes/cchpc/raw2/sites/e350_min/"
  e350_sec_sites_wd <- "/Volumes/cchpc/raw2/sites/e350_sec/"
  sites_wd <- "/Volumes/cchpc/raw2/sites/"
  
} else if(Sys.info()[7] == "keen930"){
  source("C:/Users/keen930/OneDrive - PNNL/Documents/CCHP/hpanalysis/functions_for_use.R")
  e350_min_sites_wd <- "C:/Users/keen930/Downloads/sites/e350_min/"
  e350_sec_sites_wd <- "C:/Users/keen930/Downloads/sites/e350_sec/"
  sites_wd <- "C:/Users/keen930/Downloads/sites/"
  
} else if(Sys.info()[7] == "zhan682"){
  source("C:/Users/zhan682/OneDrive - PNNL/Documents/CCHP/hpanalysis/functions_for_use.R")
}


# vars

setwd(e350_min_sites_wd) 
e350_sites <- unique(substr(list.files(pattern="*.csv"), 6, 11))



# combine sites
for (i in e350_sites){
  
## 1-second data ##
setwd(e350_sec_sites_wd)
df_sec <- read_csv(paste("site_", i, "_second.csv", sep = ""))

df_sec$`Timestamp (UTC)` <- parse_date_time(df_sec$`Timestamp (UTC)`, orders = c("ymd_HMS", "mdy_HMS"))
df_sec$`Timestamp (UTC)` <- as.POSIXct(df_sec$`Timestamp (UTC)`)

df_sec <- df_sec %>% drop_na(`Timestamp (UTC)`)

  # I already have a step to fill in the missing timestamps in cleaning.R, we don't need it here
# df_sec_full <- fill_missing_timestamps(df_sec, "Timestamp (UTC)", format = "%Y-%m-%d %H:%M:%S", interval = "sec")
# df_sec_full$timestamp <- as.POSIXct(df_sec_full$timestamp, format = "%Y-%m-%d %H:%M:%S")

# Converting to minute data now just to get something working
df_sec <- timeAverage(df_sec %>% rename(date = "Timestamp (UTC)"), avg.time = "min", data.thresh = 75, statistic = "mean") %>%
  rename("Timestamp (UTC)" = "date")


## 1-minute data ##
setwd(e350_min_sites_wd)
df_min <- read.csv(paste("site_", i, "_minute.csv", sep = ""))

df_min$Timestamp..UTC. <- parse_date_time(df_min$Timestamp..UTC., orders = c("ymd_HMS", "mdy_HMS"))
df_min$Timestamp..UTC. <- as.POSIXct(df_min$Timestamp..UTC.)

df_min <- df_min %>% drop_na(Timestamp..UTC.) %>% rename("Timestamp (UTC)" = 'Timestamp..UTC.')

  # I already have a step to fill in the missing timestamps in cleaning.R, we don't need it here
# df_min_full <- fill_missing_timestamps(df_min, "Timestamp..UTC.", format = "%Y-%m-%d %H:%M:%S", interval = "sec")
# df_min_full$timestamp <- as.POSIXct(df_min_full$timestamp, format = "%Y-%m-%d %H:%M:%S")

df <- left_join(df_sec, df_min, by = "Timestamp (UTC)")

# dups <- subset(df, duplicated(df$timestamp))

setwd("~") # reset wd
setwd(sites_wd) # set wd to sites folder for saving single site file
write_csv(df, paste("site_", i, ".csv", sep = "")) # write csv of single file for site

}


