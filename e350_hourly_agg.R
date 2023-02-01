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
path = "/Users/rose775/OneDrive - PNNL/Desktop/Projects/ccHP/Project Management/Data Analysis/Raw Data/Energy350/1-Minute"
setwd(path)

# read csv function that adds home ID column
read_csv_homeID <- function(filename){
  df <- read.csv(filename, check.names = F)
  names(df) <- iconv(names(df), to = "ASCII", sub = "")
  df[df == "#N/A"] <- NA
  df[-2] <- lapply(df[-2], as.numeric)
  df$HomeID <- substr(filename, 1, 6)
  df <- df %>% relocate(HomeID)
  df$`Timestamp (UTC)` <- as.POSIXlt(df$`Timestamp (UTC)`, format = "%m/%d/%Y %H:%M")
  df
}

##read all files in folder, result is separate dataframe for each .csv
temp = list.files(pattern="\\.csv$")
for (i in 1:length(temp)) assign(temp[i], read_csv_homeID(temp[i]))

# bind rows for each site
#site 1 - 4228VB
site_4228VB <- bind_rows(`4228VB_2022.12.19_Week51_1-minute.csv`, `4228VB_2022.12.26_Week52_minute.csv`,
                `4228VB_2023.01.02_Week01_minute.csv`, `4228VB_2023.01.09_Week02_minute.csv`,
                `4228VB_2023.01.16_Week03_minute.csv`, `4228VB_2023.01.23_Week04_minute.csv`)
#site 2 - 

# function to aggregate dfs
agg_dfs <- function(df, site){
  path = "/Users/rose775/OneDrive - PNNL/Desktop/Projects/ccHP/Project Management/Data Analysis/Raw Data/Energy350/hourly"
  setwd(path)
  df_agg <- df %>%
    group_by("date" = as.Date(`Timestamp (UTC)`), "hour_of_day" = as.POSIXlt(`Timestamp (UTC)`)$hour) %>%
    summarise("reversing_valve_signal_V" = mean(`ReversingValveSignal [V]`, na.rm = T),
              "HP_pwr_kw" = mean(`HP_Power [kW]`, na.rm = T),
              "fan_pwr_kw" = mean(`Fan_Power [kW]`, na.rm = T),
              "AHU_pwr_kw" = mean(`AHU_Power [kW]`, na.rm = T),
              "aux_heat_relay_mean" = mean(`Aux_Heat_Relay [On/Off]`, na.rm = T),
              "aux_heat_relay_sum" = sum(`Aux_Heat_Relay [On/Off]`, na.rm = T),
              "auxheat_pwr_kw" = mean(`AuxHeat_Power [kW]`, na.rm = T),
              "OA_temp_F" = mean(`OA_Temp [F]`, na.rm = T),
              "OA_RH" = mean(`OA_RH [%]`, na.rm = T),
              "SA_blower_temp_F" = mean(`SA_Blower_Temp [F]`, na.rm = T),
              "SA_blower_RH" = mean(`SA_Blower_RH [%]`, na.rm = T),
              "SA_duct1_temp_F" = mean(`SA_Duct1_Temp [F]`, na.rm = T),
              "SA_duct1_RH" = mean(`SA_Duct1_RH [%]`, na.rm = T),
              "SA_duct2_temp_F" = mean(`SA_Duct2_Temp [F]`, na.rm = T),
              "SA_duct2_RH" = mean(`SA_Duct2_RH [%]`, na.rm = T),
              "RA_temp_F" = mean(`RA_Temp [F]`, na.rm = T),
              "RA_RH" = mean(`RA_RH [%]`, na.rm = T),
              "AHU_ambient_temp_F" = mean(`AHU_Ambient_Temp [F]`, na.rm = T),
              "AHU_RH" = mean(`AHU_RH [%]`, na.rm = T),
              "room1_temp_F" = mean(`Room1_Temp [F]`, na.rm = T),
              "room1_RH" = mean(`Room1_RH [%]`, na.rm = T),
              "room2_temp_F" = mean(`Room2_Temp [F]`, na.rm = T),
              "room2_RH" = mean(`Room2_RH [%]`, na.rm = T),
              "room3_temp_F" = mean(`Room3_Temp [F]`, na.rm = T),
              "room3_RH" = mean(`Room3_RH [%]`, na.rm = T),
              "room4_temp_F" = mean(`Room4_Temp [F]`, na.rm = T),
              "room4_RH" = mean(`Room4_RH [%]`, na.rm = T),
              "calculated_airflow_cfm" = mean(`Calculated Airflow [cfm]`, na.rm = T))
  
  write.csv(df_agg, str_glue('{site}_aggregated_hourly.csv'))
}

agg_dfs(site_4228VB, "4228VB")
