#####################################################
# Author: Samuel Rosenberg
# Company: Pacific Northwest National Laboratory
# Created on: 2023-02-03
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

# set wd, read data
path = "/Users/rose775/OneDrive - PNNL/Desktop/Projects/ccHP/Project Management/Data Analysis/Raw Data/NRCan/5-Second"
setwd(path)

# check
# read csv function that adds home ID column
read_csv_homeID <- function(filename){
  df <- read.csv(filename, check.names = F)
  df$HomeID <- substr(filename, 1, 6)
  df <- df %>% relocate(HomeID)
  df$seconds <- 5
  df
}

##read all files in folder, result is separate dataframe for each .csv
temp = list.files(pattern="\\.csv$")
for (i in 1:length(temp)) assign(temp[i], read_csv_homeID(temp[i]))

# units
site_5291QJ <- bind_rows(`5291QJ_M24BC 2023-01-08 to 2023-01-14_Weekly_24B.csv`, `5291QJ_M24BC 2023-01-15 to 2023-01-21_Weekly_24B.csv`,
                         `5291QJ_M24BC 2023-01-22 to 2023-01-28_Weekly_24B.csv`, `5291QJ_M24BC 2023-01-29 to 2023-02-04 Weekly_24B.csv`,
                         `5291QJ_M24BC 2023-02-05 to 2023-02-07 Weekly_24B.csv`)

# function to aggregate dfs
agg_dfs <- function(df, site, tz){
  path = "/Users/rose775/OneDrive - PNNL/Desktop/Projects/ccHP/Project Management/Data Analysis/hourly_site_data"
  setwd(path)
  column_names <- colnames(df)
  name_file <- str_glue('{site}_aggregated_hourly.csv')
  df_agg <- df %>%
    group_by("date_UTC" = as.Date(`Timestamp`, format = "%m/%d/%Y %H:%M:%S", tz = "UTC"),
             "hour_of_day_UTC" = as.POSIXlt(`Timestamp`, format = "%m/%d/%Y %H:%M:%S", tz = "UTC")$hour) %>%
    summarise("HP_pwr_kW" = ifelse("HP_Power [kW]" %in% column_names == TRUE, mean(`HP_Power [kW]`, na.rm = T), NA),
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
  write.csv(df_agg, name_file, row.names = FALSE)
}