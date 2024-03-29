#####################################################
# Author: Samuel Rosenberg
# Company: Pacific Northwest National Laboratory
# Created on: 2023-02-24
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
library(dplyr)
library(lubridate)

# Clear Workspace
rm(list = ls())

# packages
library(tidyverse)
# library(zoo)
library(xts)
library(stringr)

# set wd, read data
path = "/Users/rose775/OneDrive - PNNL/Desktop/Projects/ccHP/Project Management/Data Analysis/Raw Data/Energy350/1-Minute"
setwd(path)
# check
# read csv function that adds home ID column
read_csv_homeID <- function(filename){
  df <- read.csv(filename, check.names = F)
  names(df) <- iconv(names(df), to = "ASCII", sub = "")
  df[df == "#N/A"] <- NA
  df[-2] <- lapply(df[-2], as.numeric)
  df$HomeID <- substr(filename, 1, 6)
  df <- df %>% relocate(HomeID)
  df$seconds <- 60
  df
}

##read all files in folder, result is separate dataframe for each .csv
temp = list.files(pattern="\\.csv$")
for (i in 1:length(temp)) assign(temp[i], read_csv_homeID(temp[i]))

# bind rows for each site
#site 1 - 4228VB
site_4228VB <- bind_rows(`4228VB_2022.12.19_Week51_minute.csv`, `4228VB_2022.12.26_Week52_minute.csv`,
                         `4228VB_2023.01.02_Week01_minute.csv`, `4228VB_2023.01.09_Week02_minute.csv`,
                         `4228VB_2023.01.16_Week03_minute.csv`, `4228VB_2023.01.23_Week04_minute.csv`,
                         `4228VB_2023.01.30_Week05_minute.csv`, `4228VB_2023.02.06_Week06_minute.csv`,
                         `4228VB_2023.02.13_Week07_minute.csv`, `4228VB_2023.02.20_Week08_minute.csv`,
                         `4228VB_2023.02.27_Week09_minute.csv`, `4228VB_2023.03.06_Week10_minute.csv`,
                         `4228VB_2023.03.13_Week11_minute.csv`, `4228VB_2023.03.20_Week12_minute.csv`,
                         `4228VB_2023.03.27_Week13_minute.csv`, `4228VB_2023.04.03_Week14_minute.csv`,
                         `4228VB_2023.04.10_Week15_minute.csv`)


# function to aggregate dfs
agg_dfs <- function(df, site, tz){
  path = "/Users/rose775/OneDrive - PNNL/Desktop/Projects/ccHP/Project Management/Data Analysis/trane_5_min"
  setwd(path)
  column_names <- colnames(df)
  df$`Timestamp (UTC)` <- as.POSIXct(df$`Timestamp (UTC)`, format = "%m/%d/%Y %H:%M", tz = "UTC")
  df$datetime_UTC <- floor_date(df$`Timestamp (UTC)`, "5 mins")
  df_agg <- df %>%
    group_by(datetime_UTC) %>%
    summarise("ODU_pwr_kW" = ifelse("HP_Power [kW]" %in% column_names == TRUE, mean(`HP_Power [kW]`, na.rm = T), NA),
              "fan_pwr_kW" = ifelse("Fan_Power [kW]" %in% column_names == TRUE, mean(`Fan_Power [kW]`, na.rm = T), NA),
              "AHU_pwr_kW" = ifelse("AHU_Power [kW]" %in% column_names == TRUE, mean(`AHU_Power [kW]`, na.rm = T), NA),
              "auxheat_pwr_kW" = ifelse("AuxHeat_Power [kW]" %in% column_names == TRUE, mean(`AuxHeat_Power [kW]`, na.rm = T), NA),
              "OA_temp_F" = ifelse("OA_Temp [F]" %in% column_names == TRUE, mean(`OA_Temp [F]`, na.rm = T), NA),
              "OA_RH" = ifelse("OA_RH [%]" %in% column_names == TRUE, mean(`OA_RH [%]`, na.rm = T), NA),
              "SA_temp_blower_cabinet_F" = ifelse("SA_Blower_Temp [F]" %in% column_names == TRUE, mean(`SA_Blower_Temp [F]`, na.rm = T), NA),
              "SA_RH_blower_cabinet" = ifelse("SA_Blower_RH [%]" %in% column_names == TRUE, mean(`SA_Blower_RH [%]`, na.rm = T), NA),
              "SA_temp_duct1_F" = ifelse("SA_Duct1_Temp [F]" %in% column_names == TRUE, mean(`SA_Duct1_Temp [F]`, na.rm = T), NA),
              "SA_RH_duct1" = ifelse("SA_Duct1_RH [%]" %in% column_names == TRUE, mean(`SA_Duct1_RH [%]`, na.rm = T), NA),
              "SA_temp_duct2_F" = ifelse("SA_Duct2_Temp [F]" %in% column_names == TRUE, mean(`SA_Duct2_Temp [F]`, na.rm = T), NA),
              "SA_RH_duct2" = ifelse("SA_Duct2_RH [%]" %in% column_names == TRUE, mean(`SA_Duct2_RH [%]`, na.rm = T), NA),
              "SA_temp_duct3_F" = ifelse("SA_Duct3_Temp [F]" %in% column_names == TRUE, mean(`SA_Duct3_Temp [F]`, na.rm = T), NA),
              "SA_RH_duct3" = ifelse("SA_Duct3_RH [%]" %in% column_names == TRUE, mean(`SA_Duct3_RH [%]`, na.rm = T), NA),
              "SA_temp_duct4_F" = ifelse("SA_Duct4_Temp [F]" %in% column_names == TRUE, mean(`SA_Duct4_Temp [F]`, na.rm = T), NA),
              "SA_RH_duct4" = ifelse("SA_Duct4_RH [%]" %in% column_names == TRUE, mean(`SA_Duct4_RH [%]`, na.rm = T), NA),
              "RA_temp_F" = ifelse("RA_Temp [F]" %in% column_names == TRUE, mean(`RA_Temp [F]`, na.rm = T), NA),
              "RA_RH" = ifelse("RA_RH [%]" %in% column_names == TRUE, mean(`RA_RH [%]`, na.rm = T), NA),
              "AHU_ambient_temp_F" = ifelse("AHU_Ambient_Temp [F]" %in% column_names == TRUE, mean(`AHU_Ambient_Temp [F]`, na.rm = T), NA),
              "AHU_ambient_RH" = ifelse("AHU_RH [%]" %in% column_names == TRUE, mean(`AHU_RH [%]`, na.rm = T), NA),
              "room1_temp_F" = ifelse("Room1_Temp [F]" %in% column_names == TRUE, mean(`Room1_Temp [F]`, na.rm = T), NA),
              "room1_RH" = ifelse("Room1_RH [%]" %in% column_names == TRUE, mean(`Room1_RH [%]`, na.rm = T), NA),
              "room2_temp_F" = ifelse("Room2_Temp [F]" %in% column_names == TRUE, mean(`Room2_Temp [F]`, na.rm = T), NA),
              "room2_RH" = ifelse("Room2_RH [%]" %in% column_names == TRUE, mean(`Room2_RH [%]`, na.rm = T), NA),
              "room3_temp_F" = ifelse("Room3_Temp [F]" %in% column_names == TRUE, mean(`Room3_Temp [F]`, na.rm = T), NA),
              "room3_RH" = ifelse("Room3_RH [%]" %in% column_names == TRUE, mean(`Room3_RH [%]`, na.rm = T), NA),
              "room4_temp_F" = ifelse("Room4_Temp [F]" %in% column_names == TRUE, mean(`Room4_Temp [F]`, na.rm = T), NA),
              "room4_RH" = ifelse("Room4_RH [%]" %in% column_names == TRUE, mean(`Room4_RH [%]`, na.rm = T), NA),
              "reversing_valve_signal_V" = ifelse("reversing_valve_signal_V" %in% column_names == TRUE, mean(`reversing_valve_signal_V`, na.rm = T), NA),
              "seconds_non_zero_in_5min_period" = sum(`seconds`))
  df_agg <- as.data.frame(df_agg)
  df_agg <- df_agg %>%
    mutate(HP_system_pwr_kW = select(., ODU_pwr_kW:auxheat_pwr_kW) %>% rowSums(na.rm = T))
  df_agg$local_datetime <- format(df_agg$datetime_UTC, tz = tz, usetz = T)
  df_agg <- df_agg %>% relocate(datetime_UTC)
  df_agg <- df_agg %>% relocate(local_datetime)
  df_agg <- df_agg %>% mutate(across(where(is.numeric), ~ round(., 2)))
  write.csv(df_agg, str_glue('{site}_aggregated_5_min.csv'), row.names = FALSE)
  
}

agg_dfs(site_4228VB, "4228VB", "US/Mountain")



