#####################################################
# Author: Samuel Rosenberg
# Company: Pacific Northwest National Laboratory
# Created on: 2023-02-01
# Description: 
#####################################################
# Todo:
# at some point data will get too large to do it this way.in future, we will 
# need to run the hourly function and bind_rows() with existing hourly 
# aggregated data because there is no need to do it over and over
# 
# maybe we add a "new" folder and an "old" folder and files are moved to "old"
# after hourly agg is done
#
# integrate hourly agg into read so that it does the agg for each file, only
# stores one csv at a time in memory
#
# reversing valve data?
#
# for date and hour - it's UTC, should we change? add "local date and time" cols?
#####################################################
############ data import and processing #############
#####################################################
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
  df$minutes <- 1
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
agg_dfs <- function(df, site, tz){
  path = "/Users/rose775/OneDrive - PNNL/Desktop/Projects/ccHP/Project Management/Data Analysis/hourly_site_data"
  setwd(path)
  df_agg <- df %>%
    group_by("date_UTC" = as.Date(`Timestamp (UTC)`, format = "%m/%d/%Y %H:%M", tz = "UTC"),
             "hour_of_day_UTC" = as.POSIXlt(`Timestamp (UTC)`, format = "%m/%d/%Y %H:%M", tz = "UTC")$hour) %>%
    summarise("HP_pwr_kW" = mean(`HP_Power [kW]`, na.rm = T),
              "fan_pwr_kW" = mean(`Fan_Power [kW]`, na.rm = T),
              "AHU_pwr_kW" = mean(`AHU_Power [kW]`, na.rm = T),
              "auxheat_pwr_kW" = mean(`AuxHeat_Power [kW]`, na.rm = T),
              "OA_temp_F" = mean(`OA_Temp [F]`, na.rm = T),
              "OA_RH" = mean(`OA_RH [%]`, na.rm = T),
              "SA_temp_blower_cabinet_F" = mean(`SA_Blower_Temp [F]`, na.rm = T),
              "SA_RH_blower_cabinet" = mean(`SA_Blower_RH [%]`, na.rm = T),
              "SA_temp_duct1_F" = mean(`SA_Duct1_Temp [F]`, na.rm = T),
              "SA_RH_duct1" = mean(`SA_Duct1_RH [%]`, na.rm = T),
              "SA_temp_duct2_F" = mean(`SA_Duct2_Temp [F]`, na.rm = T),
              "SA_RH_duct2" = mean(`SA_Duct2_RH [%]`, na.rm = T),
              "SA_temp_duct3_F" = NA,
              "SA_RH_duct3" = NA,
              "SA_temp_duct4_F" = NA,
              "SA_RH_duct4" = NA,
              "RA_temp_F" = mean(`RA_Temp [F]`, na.rm = T),
              "RA_RH" = mean(`RA_RH [%]`, na.rm = T),
              "AHU_ambient_temp_F" = mean(`AHU_Ambient_Temp [F]`, na.rm = T),
              "AHU_ambient_RH" = mean(`AHU_RH [%]`, na.rm = T),
              "room1_temp_F" = mean(`Room1_Temp [F]`, na.rm = T),
              "room1_RH" = mean(`Room1_RH [%]`, na.rm = T),
              "room2_temp_F" = mean(`Room2_Temp [F]`, na.rm = T),
              "room2_RH" = mean(`Room2_RH [%]`, na.rm = T),
              "room3_temp_F" = mean(`Room3_Temp [F]`, na.rm = T),
              "room3_RH" = mean(`Room3_RH [%]`, na.rm = T),
              "room4_temp_F" = mean(`Room4_Temp [F]`, na.rm = T),
              "room4_RH" = mean(`Room4_RH [%]`, na.rm = T),
              "reversing_valve_V" = NA,
              "minutes_non_zero_in_hour" = sum(`minutes`))
  df_agg <- as.data.frame(df_agg)
  df_agg <- df_agg %>% mutate(across(where(is.numeric), ~ round(., 2)))
  df_agg$local_datetime <- paste(df_agg$date_UTC, df_agg$hour_of_day_UTC)
  df_agg$local_datetime <- as.POSIXct(df_agg$local_datetime, format = "%Y-%m-%d %H", tz = "UTC")
  df_agg$local_datetime <- format(df_agg$local_datetime, tz = tz, usetz = T)
  df_agg <- df_agg %>% relocate(local_datetime)
  df_agg <- 
  # return(df_agg)
  write.csv(df_agg, str_glue('{site}_aggregated_hourly.csv'), row.names = FALSE)
}

agg_dfs(site_4228VB, "4228VB", "US/Mountain")
