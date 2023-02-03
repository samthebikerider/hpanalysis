#####################################################
# Author: Samuel Rosenberg
# Company: Pacific Northwest National Laboratory
# Created on: 2023-02-01
# Description: 
#####################################################
# Todo:
# 
# 4 SA and 3 room temps
# 
#####################################################
############ data import and processing #############
#####################################################
# Clear Workspace
rm(list = ls())

# packages
library(tidyverse)
library(zoo)
library(xts)

# set wd, read data
path = "/Users/rose775/OneDrive - PNNL/Desktop/Projects/ccHP/Project Management/Data Analysis/Raw Data/"
michaels_path = "/Users/rose775/OneDrive - PNNL/Desktop/Projects/ccHP/Project Management/Data Analysis/Raw Data/Michaels"

setwd(path)

#####################################################
# Author: Samuel Rosenberg
# Company: Pacific Northwest National Laboratory
# Created on: 2023-02-01
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
library(tidyverse)
library(zoo)
library(xts)
library(stringr)

# set wd, read data
path = "/Users/rose775/OneDrive - PNNL/Desktop/Projects/ccHP/Project Management/Data Analysis/Raw Data/Michaels"
setwd(path)

# read csv function that adds home ID column
read_csv_homeID <- function(filename){
  df <- read.csv(filename)
  df[-1] <- lapply(df[-1], as.numeric)
  df$HomeID <- substr(filename, 14, 19)
  df <- df %>% relocate(HomeID)
  # df$index <- as.POSIXlt(df$index, format = "%Y-%m-%d %H:%M:%S%")
  df$minutes <- 1/60
  df <- df %>%
    mutate(Aux_Power = rowSums(across(c(Aux1_Power, Aux2_Power, Aux3_Power, Aux4_Power)), na.rm = T))
  df
}

##read all files in folder, result is separate dataframe for each .csv
temp = list.files(pattern="\\.csv$")
for (i in 1:length(temp)) assign(temp[i], read_csv_homeID(temp[i]))

# bind rows for each site
site_6950NE <- bind_rows(`PNNL_ccASHP__6950NE (1).csv`, `PNNL_ccASHP__6950NE (2).csv`,
                         `PNNL_ccASHP__6950NE (3).csv`, `PNNL_ccASHP__6950NE (4).csv`,
                         `PNNL_ccASHP__6950NE (5).csv`)
site_8820XE <- bind_rows(`PNNL_ccASHP__8220XE (1).csv`, `PNNL_ccASHP__8220XE (2).csv`,
                         `PNNL_ccASHP__8220XE (3).csv`, `PNNL_ccASHP__8220XE (4).csv`,
                         `PNNL_ccASHP__8220XE (5).csv`)
site_9944LD <- bind_rows(`PNNL_ccASHP__9944LD (3).csv`, `PNNL_ccASHP__9944LD (4).csv`,
                         `PNNL_ccASHP__9944LD (5).csv`)

# function to aggregate dfs
agg_dfs <- function(df, site, tz){
  path = "/Users/rose775/OneDrive - PNNL/Desktop/Projects/ccHP/Project Management/Data Analysis/hourly_site_data"
  setwd(path)
  df_agg <- df %>%
    group_by("date_UTC" = as.Date(`index`), "hour_of_day_UTC" = as.POSIXlt(`index`)$hour) %>%
    summarise("HP_pwr_kW" = mean(`HP_Power`, na.rm = T),
               "fan_pwr_kW" = mean(`Fan_Power`, na.rm = T),
               "AHU_pwr_kW" = mean(`AHU_Power`, na.rm = T),
               "auxheat_pwr_kW" = mean(`Aux_Power`, na.rm = T),
               "OA_temp_F" = mean(`OA_TempF`, na.rm = T),
               "OA_RH" = mean(`OA_RH`, na.rm = T),
               "SA_temp_blower_cabinet_F" = NA,
               "SA_RH_blower_cabinet" = NA,
               "SA_temp_duct1_F" = mean(`SA1_TempF`, na.rm = T),
               "SA_RH_duct1" = mean(`SA1_RH`, na.rm = T),
               "SA_temp_duct2_F" = mean(`SA2_TempF`, na.rm = T),
               "SA_RH_duct2" = mean(`SA2_RH`, na.rm = T),
               "SA_temp_duct3_F" = mean(`SA3_TempF`, na.rm = T),
               "SA_RH_duct3" = mean(`SA3_RH`, na.rm = T),
               "SA_temp_duct4_F" = mean(`SA4_TempF`, na.rm = T),
               "SA_RH_duct4" = mean(`SA4_RH`, na.rm = T),
               "RA_temp_F" = mean(`RA_TempF`, na.rm = T),
               "RA_RH" = mean(`RA_RH`, na.rm = T),
               "AHU_ambient_temp_F" = mean(`AHU_TempF`, na.rm = T),
               "AHU_ambient_RH" = mean(`AHU_RH`, na.rm = T),
               "room1_temp_F" = mean(`Room1_TempF`, na.rm = T),
               "room1_RH" = mean(`Room1_RH`, na.rm = T),
               "room2_temp_F" = mean(`Room2_TempF`, na.rm = T),
               "room2_RH" = mean(`Room2_RH`, na.rm = T),
               "room3_temp_F" = mean(`Room3_TempF`, na.rm = T),
               "room3_RH" = mean(`Room3_RH`, na.rm = T),
               "room4_temp_F" = NA,
               "room4_RH" = NA,
               "reversing_valve_V" = NA,
               "minutes_present_in_hour" = sum(`minutes`))
  df_agg <- as.data.frame(df_agg)
  df_agg <- df_agg %>% mutate(across(where(is.numeric), ~ round(., 2)))
  df_agg$local_datetime <- paste(df_agg$date_UTC, df_agg$hour_of_day_UTC)
  df_agg$local_datetime <- as.POSIXct(df_agg$local_datetime, format = "%Y-%m-%d %H", tz = "UTC")
  df_agg$local_datetime <- format(df_agg$local_datetime, tz = tz, usetz = T)
  df_agg <- df_agg %>% relocate(local_datetime)
  # return(df_agg)
  write.csv(df_agg, str_glue('{site}_aggregated_hourly.csv'), row.names = FALSE)
}

agg_dfs(site_6950NE, "6950NE", "US/Central")
agg_dfs(site_8820XE, "8820XE", "US/Central")
agg_dfs(site_6950NE, "9944LD", "US/Mountain")

