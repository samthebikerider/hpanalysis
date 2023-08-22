#####################################################
# Author: Samuel Rosenberg
# Company: Pacific Northwest National Laboratory
# Created on: 2023-08-03
# Description: This script follows raw_data_bind_sites.R
# and binds the 1-minute E50 data with the 1-second data.
# It exports the joined file to the "sites" folder.
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


setwd(e350_min_sites_wd) 
e350_sites <- unique(substr(list.files(pattern="*.csv"), 6, 11))



# Combine sites
for (i in e350_sites){
  
######### 1-second data #########
setwd(e350_sec_sites_wd)
df_sec <- read_csv(paste("site_", i, "_second.csv", sep = ""))

df_sec$`Timestamp (UTC)` <- parse_date_time(df_sec$`Timestamp (UTC)`, orders = c("ymd_HMS", "mdy_HMS"))
df_sec$`Timestamp (UTC)` <- as.POSIXct(df_sec$`Timestamp (UTC)`)

df_sec <- df_sec %>% drop_na(`Timestamp (UTC)`)



######### 1-minute data #########
setwd(e350_min_sites_wd)
df_min <- read.csv(paste("site_", i, "_minute.csv", sep = ""))

df_min$Timestamp..UTC. <- parse_date_time(df_min$Timestamp..UTC., orders = c("ymd_HMS", "mdy_HMS"))
df_min$Timestamp..UTC. <- as.POSIXct(df_min$Timestamp..UTC.)

df_min <- df_min %>% drop_na(Timestamp..UTC.) %>% rename('Timestamp (UTC)' = "Timestamp..UTC.")



######### Join 1-second and 1-minute data #########
df <- left_join(df_sec, df_min, by = "Timestamp (UTC)")



setwd("~") # reset wd
setwd(sites_wd) # set wd to sites folder for saving single site file
write_csv(df, paste("site_", i, ".csv", sep = "")) # write csv of single file for site

}


