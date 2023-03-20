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
  df$seconds <- 1
  df <- df %>%
    mutate(Aux_Power = rowSums(across(c(Aux1_Power, Aux2_Power, Aux3_Power, Aux4_Power)), na.rm = T))
  df
}

##read all files in folder, result is separate dataframe for each .csv
temp = list.files(pattern="\\.csv$")
for (i in 1:length(temp)) assign(temp[i], read_csv_homeID(temp[i]))

# bind rows for each site
site_6950NE <- bind_rows(`PNNL_ccASHP__6950NE_2022-12-19.csv`, `PNNL_ccASHP__6950NE_2023-01-03.csv`,
                         `PNNL_ccASHP__6950NE_2023-01-09.csv`, `PNNL_ccASHP__6950NE_2023-01-16.csv`,
                         `PNNL_ccASHP__6950NE_2023-01-23.csv`, `PNNL_ccASHP__6950NE_2023-02-01.csv`,
                         `PNNL_ccASHP__6950NE_2023-02-07.csv`, `PNNL_ccASHP__6950NE_2023-02-14.csv`)

site_8820XE <- bind_rows(`PNNL_ccASHP__8220XE_2022-12-19.csv`, `PNNL_ccASHP__8220XE_2023-01-03.csv`,
                         `PNNL_ccASHP__8220XE_2023-01-09.csv`, `PNNL_ccASHP__8220XE_2023-01-16.csv`,
                         `PNNL_ccASHP__8220XE_2023-01-23.csv`, `PNNL_ccASHP__8220XE_2023-02-01.csv`,
                         `PNNL_ccASHP__8220XE_2023-02-07.csv`, `PNNL_ccASHP__8220XE_2023-02-14.csv`)

site_9944LD <- bind_rows(`PNNL_ccASHP__9944LD_2023-01-09.csv`, `PNNL_ccASHP__9944LD_2023-01-16.csv`,
                         `PNNL_ccASHP__9944LD_2023-01-23.csv`, `PNNL_ccASHP__9944LD_2023-02-01.csv`,
                         `PNNL_ccASHP__9944LD_2023-02-07.csv`, `PNNL_ccASHP__9944LD_2023-02-14.csv`)

site_2563EH <- bind_rows(`PNNL_ccASHP__2563EH_2023-02-01.csv`, `PNNL_ccASHP__2563EH_2023-02-07.csv`,
                         `PNNL_ccASHP__2563EH_2023-02-14.csv`)

site_2896BR <- bind_rows(`PNNL_ccASHP__2896BR_2023-02-01.csv`, `PNNL_ccASHP__2896BR_2023-02-07.csv`,
                         `PNNL_ccASHP__2896BR_2023-02-14.csv`)

site_6112OH <- bind_rows(`PNNL_ccASHP__6112OH_2023-02-14.csv`)

site_8726VB <- bind_rows(`PNNL_ccASHP__8726VB_2023-02-14.csv`)

# function to aggregate dfs
agg_dfs <- function(df, site, tz){
  path = "/Users/rose775/OneDrive - PNNL/Desktop/Projects/ccHP/Project Management/Data Analysis/hourly_site_data"
  setwd(path)
  column_names <- colnames(df)
  df_agg <- df %>%
    group_by("date_UTC" = as.Date(`index`), "hour_of_day_UTC" = as.POSIXlt(`index`)$hour) %>%
    summarise("HP_pwr_kW" = ifelse("HP_Power" %in% column_names == T, mean(`HP_Power`, na.rm = T), NA),
               "fan_pwr_kW" = ifelse("Fan_Power" %in% column_names == T, mean(`Fan_Power`, na.rm = T), NA),
               "AHU_pwr_kW" = ifelse("AHU_Power" %in% column_names == T, mean(`AHU_Power`, na.rm = T), NA),
               "auxheat_pwr_kW" = ifelse("Aux_Power" %in% column_names == T, mean(`Aux_Power`, na.rm = T), NA),
               "OA_temp_F" = ifelse("OA_TempF" %in% column_names == T, mean(`OA_TempF`, na.rm = T), NA),
               "OA_RH" = ifelse("OA_RH" %in% column_names == T, mean(`OA_RH`, na.rm = T), NA),
               "SA_temp_blower_cabinet_F" = ifelse("SA_temp_blower_cabinet_F" %in% column_names == T, mean(`SA_temp_blower_cabinet_F`, na.rm = T), NA),
               "SA_RH_blower_cabinet" = ifelse("SA_RH_blower_cabinet" %in% column_names == T, mean(`SA_RH_blower_cabinet`, na.rm = T), NA),
               "SA_temp_duct1_F" = ifelse("SA1_TempF" %in% column_names == T, mean(`SA1_TempF`, na.rm = T), NA),
               "SA_RH_duct1" = ifelse("SA1_RH" %in% column_names == T, mean(`SA1_RH`, na.rm = T), NA),
               "SA_temp_duct2_F" = ifelse("SA2_TempF" %in% column_names == T, mean(`SA2_TempF`, na.rm = T), NA),
               "SA_RH_duct2" = ifelse("SA2_RH" %in% column_names == T, mean(`SA2_RH`, na.rm = T), NA),
               "SA_temp_duct3_F" = ifelse("SA3_TempF" %in% column_names == T, mean(`SA3_TempF`, na.rm = T), NA),
               "SA_RH_duct3" = ifelse("SA3_RH" %in% column_names == T, mean(`SA3_RH`, na.rm = T), NA),
               "SA_temp_duct4_F" = ifelse("SA4_TempF" %in% column_names == T, mean(`SA4_TempF`, na.rm = T), NA),
               "SA_RH_duct4" = ifelse("SA4_RH" %in% column_names == T, mean(`SA4_RH`, na.rm = T), NA),
               "RA_temp_F" = ifelse("RA_TempF" %in% column_names == T, mean(`RA_TempF`, na.rm = T), NA),
               "RA_RH" = ifelse("RA_RH" %in% column_names == T, mean(`RA_RH`, na.rm = T), NA),
               "AHU_ambient_temp_F" = ifelse("AHU_TempF" %in% column_names == T, mean(`AHU_TempF`, na.rm = T), NA),
               "AHU_ambient_RH" = ifelse("AHU_RH" %in% column_names == T, mean(`AHU_RH`, na.rm = T), NA),
               "room1_temp_F" = ifelse("Room1_TempF" %in% column_names == T, mean(`Room1_TempF`, na.rm = T), NA),
               "room1_RH" = ifelse("Room1_RH" %in% column_names == T, mean(`Room1_RH`, na.rm = T), NA),
               "room2_temp_F" = ifelse("Room2_TempF" %in% column_names == T, mean(`Room2_TempF`, na.rm = T), NA),
               "room2_RH" = ifelse("Room2_RH" %in% column_names == T, mean(`Room2_RH`, na.rm = T), NA),
               "room3_temp_F" = ifelse("Room3_TempF" %in% column_names == T, mean(`Room3_TempF`, na.rm = T), NA),
               "room3_RH" = ifelse("Room3_RH" %in% column_names == T, mean(`Room3_RH`, na.rm = T), NA),
               "room4_temp_F" = ifelse("Room4_TempF" %in% column_names == T, mean(`Room4_TempF`, na.rm = T), NA), 
               "room4_RH" = ifelse("Room4_RH" %in% column_names == T, mean(`Room4_RH`, na.rm = T), NA),
               "reversing_valve_signal_V" = ifelse("RV_Volts" %in% column_names == T, mean(`RV_Volts`, na.rm = T), NA),
               "seconds_non_zero_in_hour" = sum(`seconds`))
  df_agg <- as.data.frame(df_agg)
  df_agg <- df_agg %>% mutate(across(where(is.numeric), ~ round(., 2)))
  df_agg$datetime_UTC <- paste(df_agg$date_UTC, df_agg$hour_of_day_UTC)
  df_agg$datetime_UTC <- as.POSIXct(df_agg$datetime_UTC, format = "%Y-%m-%d %H", tz = "UTC")
  df_agg$local_datetime <- format(df_agg$datetime_UTC, tz = tz, usetz = T)
  df_agg <- df_agg %>% relocate(datetime_UTC)
  df_agg <- df_agg %>% relocate(local_datetime)
  df_agg <- subset(df_agg, select=-c(date_UTC, hour_of_day_UTC))
  # return(df_agg)
  write.csv(df_agg, str_glue('{site}_aggregated_hourly.csv'), row.names = FALSE)
}

agg_dfs(site_6950NE, "6950NE", "US/Central")
agg_dfs(site_8820XE, "8820XE", "US/Central")
agg_dfs(site_9944LD, "9944LD", "US/Mountain")
agg_dfs(site_2563EH, "2563EH", "US/Eastern")
agg_dfs(site_2896BR, "2896BR", "US/Eastern")

