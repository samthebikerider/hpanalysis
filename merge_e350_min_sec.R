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

# packages
if(Sys.info()[7] == "rose775"){
  source("/Users/rose775/Library/CloudStorage/OneDrive-PNNL/Desktop/Projects/ccHP/hpanalysis/functions_for_use.R")
  e350_min_sites_wd <- "/Volumes/cchpc/raw2/sites/e350_min/"
  e350_sec_sites_wd <- "/Volumes/cchpc/raw2/sites/e350_sec/"
  sites_wd <- "/Volumes/cchpc/raw2/sites/"
  
} else if(Sys.info()[7] == "keen930"){
  source("C:/Users/keen930/OneDrive - PNNL/Documents/CCHP/hpanalysis/functions_for_use.R")
  e350_min_sites_wd <- "Q:/raw2/sites/e350_min/"
  e350_sec_sites_wd <- "Q:/raw2/sites/e350_sec/"
  sites_wd <- "Q:/raw2/sites/sites/"
  
} else if(Sys.info()[7] == "zhan682"){
  source("C:/Users/zhan682/OneDrive - PNNL/Documents/CCHP/hpanalysis/functions_for_use.R")
}
library(tidyverse)
library(lubridate)

# vars

setwd(e350_min_sites_wd) 
sites <- list.files(pattern="*.csv")
sites <- unique(substr(sites, 6, 11))

# combine sites
i = "4228VB"

setwd(e350_sec_sites_wd)
df_sec <- read_csv(paste("site_", i, "_second.csv", sep = ""))
df_sec$`Timestamp (UTC)` <- parse_date_time(df_sec$`Timestamp (UTC)`, orders = c("ymd_HMS", "mdy_HMS"))
df_sec$`Timestamp (UTC)` <- as.POSIXct(df_sec$`Timestamp (UTC)`)
df_sec <- df_sec %>% drop_na(`Timestamp (UTC)`)
df_sec_full <- fill_missing_timestamps(df_sec, "Timestamp (UTC)", format = "%y-%m-%d %H:%M:%S", interval = "sec")


setwd(e350_min_sites_wd)
df_min <- read.csv(paste("site_", i, "_minute.csv", sep = ""))
df_min$Timestamp..UTC. <- parse_date_time(df_min$Timestamp..UTC., orders = c("ymd_HMS", "mdy_HMS"))
df_min$Timestamp..UTC. <- as.POSIXct(df_min$Timestamp..UTC.)
df_min <- df_min %>% drop_na(Timestamp..UTC.)
df_min_full <- fill_missing_timestamps(df_min, "Timestamp..UTC.", format = "%y-%m-%d %H:%M:%S", interval = "sec")

df <- left_join(df_sec_full, df_min_full, by = "timestamp")



# for (i in sites){
#   setwd(e350_sec_sites_wd)
#   df_sec <- read_csv(paste("site_", i, "_second.csv", sep = ""))
#   
#   setwd(e350_min_sites_wd)
#   df_min <- read.csv(paste("site_", i, "_minute.csv", sep = ""))
# }
