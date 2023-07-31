#####################################################
# Author: Samuel Rosenberg
# Company: Pacific Northwest National Laboratory
# Created on: 2023-04-21
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
library(openair)

#####################################################
################### Functions #######################
#####################################################
# read csv function that adds home ID column
read_csv_homeID <- function(filename){
  df <- read.csv(filename, check.names = F)
  names(df) <- iconv(names(df), to = "ASCII", sub = "")
  df[df == "#N/A"] <- NA
  df[-2] <- lapply(df[-2], as.numeric)
  df$HomeID <- substr(filename, 1, 6)
  df <- df %>% relocate(HomeID)
  df
}
# function to aggregate dfs
agg_dfs <- function(df, tz){
  column_names <- colnames(df)
  df$date <- as.POSIXct(df$`Timestamp (UTC)`, format = "%m/%d/%Y %H:%M", tz = "UTC") # format and name date column for timeAverage
  to_keep <- c("date", "HP_Power [kW]", "Fan_Power [kW]", "AHU_Power [kW]", "Aux_Heat_Power [kW]",
               "OA_Temp [F]", "OA_RH [%]", "SA_Blower_Temp [F]", "SA_Blower_RH [%]",
               "SA_Duct1_Temp [F]", "SA_Duct1_RH [%]", "SA_Duct2_Temp [F]", "SA_Duct2_RH [%]",
               "SA_Duct3_Temp [F]", "SA_Duct3_RH [%]", "SA_Duct4_Temp [F]", "SA_Duct4_RH [%]",
               "RA_Temp [F]", "RA_RH [%]", "AHU_Ambient_Temp [F]", "AHU_RH [%]", "Room1_Temp [F]", "Room1_RH [%]",
               "Room2_Temp [F]", "Room2_RH [%]", "Room3_Temp [F]", "Room3_RH [%]", "Room4_Temp [F]", "Room4_RH [%]",
               "ReversingValveSignal [V]") # list of cols used in agg
  df2 <- subset(df, select = names(df) %in% to_keep) # keep only cols  used in agg
  df_agg <- timeAverage(df2, avg.time = "hour", data.thresh = 75, statistic = "mean") # agg hourly
  cols_renamed <- c(ODU_pwr_kW = "HP_Power [kW]", fan_pwr_kW = "Fan_Power [kW]",
                    AHU_pwr_kW = "AHU_Power [kW]", auxheat_pwr_kW = "Aux_Heat_Power [kW]",
                    OA_temp_F = "OA_Temp [F]", OA_RH = "OA_RH [%]",
                    SA_temp_blower_cabinet_F = "SA_Blower_Temp [F]",
                    SA_RH_blower_cabinet = "SA_Blower_RH [%]",
                    SA_temp_duct1_F = "SA_Duct1_Temp [F]", SA_RH_duct1 = "SA_Duct1_RH [%]",
                    SA_temp_duct2_F = "SA_Duct2_Temp [F]", SA_RH_duct2 = "SA_Duct2_RH [%]",
                    SA_temp_duct3_F = "SA_Duct3_Temp [F]", SA_RH_duct3 = "SA_Duct3_RH [%]",
                    SA_temp_duct4_F = "SA_Duct4_Temp [F]", SA_RH_duct4 = "SA_Duct4_RH [%]",
                    RA_temp_F = "RA_Temp [F]", RA_RH = "RA_RH [%]",
                    AHU_ambient_temp_F = "AHU_Ambient_Temp [F]", AHU_ambient_RH = "AHU_RH [%]",
                    room1_temp_F = "Room1_Temp [F]", room1_RH = "Room1_RH [%]",
                    room2_temp_F = "Room2_Temp [F]", room2_RH = "Room2_RH [%]",
                    room3_temp_F = "Room3_Temp [F]", room3_RH = "Room3_RH [%]",
                    room4_temp_F = "Room4_Temp [F]", room4_RH = "Room4_RH [%]",
                    reversing_valve_signal_V = "ReversingValveSignal [V]",
                    datetime_UTC = "date") # list current and new colnames
  df_agg <- df_agg %>%
    rename(any_of(cols_renamed)) # rename cols using list
  df_agg <- df_agg %>%
    rowwise() %>% 
    mutate(HP_system_pwr_kW = sum(c_across(colnames(df_agg)[colnames(df_agg) %in% c("ODU_pwr_kW", "fan_pwr_kW", "AHU_pwr_kW", "auxheat_pwr_kW")]), na.rm = T)) # sum for total system power
  df_agg <- df_agg %>% mutate(across(where(is.numeric), ~ round(., 2))) # as.numeric and round
  df_agg$local_datetime <- format(df_agg$datetime_UTC, tz = tz, usetz = T) # add, convert, format local date col
  return(df_agg)
}

# function to merge aggregated df with existing df
merge_aggd_dfs <- function(df, site, tz){
  path = "/Users/rose775/OneDrive - PNNL/Desktop/Projects/ccHP/Project Management/Data Analysis/hourly_site_data"
  setwd(path)
  name_file <- str_glue('{site}_aggregated_hourly.csv')
  df_agg <- agg_dfs(df, tz)
  df_out <- df_agg[!duplicated(df_agg), ]
  df_out$site_id <- site
  df_out$unit_id <- site
  df_out <- df_out %>%
    select(any_of(c("site_id", "unit_id", "local_datetime", "datetime_UTC", "ODU_pwr_kW", "fan_pwr_kW",
           "AHU_pwr_kW",	"auxheat_pwr_kW",	"OA_temp_F",	"OA_RH",	"SA_temp_blower_cabinet_F",
           "SA_RH_blower_cabinet",	"SA_temp_duct1_F",	"SA_RH_duct1",	"SA_temp_duct2_F",
           "SA_RH_duct2",	"RA_temp_F",	"RA_RH",	"AHU_ambient_temp_F",	"AHU_ambient_RH",
           "room1_temp_F",	"room1_RH",	"room2_temp_F",	"room2_RH",	"room3_temp_F",	"room3_RH",
           "room4_temp_F",	"room4_RH",	"HP_system_pwr_kW",	"reversing_valve_signal_V")))
  write.csv(df_out, name_file, row.names = FALSE)
  # return(df_out)
  
}

#####################################################
###################### Work #########################
#####################################################
# set wd, read data
path = "/Users/rose775/OneDrive - PNNL/Desktop/Projects/ccHP/Project Management/Data Analysis/Raw Data/Energy350/1-Minute/"
setwd(path)
temp <- list.files()
for (i in 1:length(temp)) assign(temp[i], read_csv_homeID(temp[i]))

site_5539NO <- bind_rows(`5539NO_2023.02.13_Week07_minute.csv`, `5539NO_2023.02.20_Week08_minute.csv`,
                         `5539NO_2023.02.27_Week09_minute.csv`, `5539NO_2023.03.06_Week10_minute.csv`,
                         `5539NO_2023.03.13_Week11_minute.csv`, `5539NO_2023.03.20_Week12_minute.csv`,
                         `5539NO_2023.03.27_Week13_minute.csv`, `5539NO_2023.04.03_Week14_minute.csv`,
                         `5539NO_2023.04.10_Week15_minute.csv`)

site_4228VB <- bind_rows(`4228VB_2022.12.19_Week51_minute.csv`, `4228VB_2022.12.26_Week52_minute.csv`,
                         `4228VB_2023.01.02_Week01_minute.csv`, `4228VB_2023.01.09_Week02_minute.csv`,
                         `4228VB_2023.01.16_Week03_minute.csv`, `4228VB_2023.01.23_Week04_minute.csv`,
                         `4228VB_2023.01.30_Week05_minute.csv`, `4228VB_2023.02.06_Week06_minute.csv`,
                         `4228VB_2023.02.13_Week07_minute.csv`, `4228VB_2023.02.20_Week08_minute.csv`,
                         `4228VB_2023.02.27_Week09_minute.csv`, `4228VB_2023.03.06_Week10_minute.csv`,
                         `4228VB_2023.03.13_Week11_minute.csv`, `4228VB_2023.03.20_Week12_minute.csv`,
                         `4228VB_2023.03.27_Week13_minute.csv`, `4228VB_2023.04.03_Week14_minute.csv`,
                         `4228VB_2023.04.10_Week15_minute.csv`, `4228VB_2023.04.17_Week16_minute.csv`,
                         `4228VB_2023.04.24_Week17_minute.csv`)

# run the function
merge_aggd_dfs(site_5539NO, "5539NO", "US/Eastern") # save site .csv
merge_aggd_dfs(site_4228VB, "4228VB", "US/Mountain") # save site .csv


