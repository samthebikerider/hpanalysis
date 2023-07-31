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
  df <- read.csv(filename)
  df[-1] <- lapply(df[-1], as.numeric)
  df$HomeID <- substr(filename, 14, 19)
  df <- df %>% relocate(HomeID)
  df$seconds <- 1
  df <- df %>%
    mutate(Aux_Power = rowSums(across(c(Aux1_Power, Aux2_Power, Aux3_Power, Aux4_Power)), na.rm = T))
  df
}

# function to aggregate dfs
agg_dfs <- function(df, tz){
  df$date <- as.POSIXct(df$index, format = "%Y-%m-%d %H:%M:%S", tz = "UTC") # format and name date column for timeAverage
  to_keep <- c("HomeID", "date", "HP_Power", "Fan_Power", "AHU_Power", "Aux_Power",
               "OA_TempF", "OA_RH", "SA_temp_blower_cabinet_F", "SA_RH_blower_cabinet",
               "SA1_TempF", "SA1_RH", "SA2_TempF", "SA2_RH", "SA3_TempF", "SA3_RH",
               "SA4_TempF", "SA4_RH", "RA_TempF", "RA_RH", "AHU_TempF", "AHU_RH",
               "Room1_TempF", "Room1_RH","Room2_TempF", "Room2_RH", "Room3_TempF",
               "Room3_RH", "Room4_TempF", "Room4_RH", "RV_Volts") # list of cols used in agg
  df2 <- subset(df, select = names(df) %in% to_keep) # keep only cols  used in agg
  df_agg <- timeAverage(df2, avg.time = "hour", data.thresh = 75, statistic = "mean") # agg hourly
  cols_renamed <- c(ODU_pwr_kW = "HP_Power", fan_pwr_kW = "Fan_Power",
                    AHU_pwr_kW = "AHU_Power", auxheat_pwr_kW = "Aux_Power",
                    OA_temp_F = "OA_TempF", 
                    SA_temp_duct1_F = "SA1_TempF", SA_RH_duct1 = "SA1_RH",
                    SA_temp_duct2_F = "SA2_TempF", SA_RH_duct2 = "SA2_RH",
                    SA_temp_duct3_F = "SA3_TempF", SA_RH_duct3 = "SA3_RH",
                    SA_temp_duct4_F = "SA4_TempF", SA_RH_duct4 = "SA4_RH",
                    RA_temp_F = "RA_TempF",
                    AHU_ambient_temp_F = "AHU_TempF", AHU_ambient_RH = "AHU_RH",
                    room1_temp_F = "Room1_TempF", room1_RH = "Room1_RH",
                    room2_temp_F = "Room2_TempF", room2_RH = "Room2_RH",
                    room3_temp_F = "Room3_TempF", room3_RH = "Room3_RH",
                    room4_temp_F = "Room4_TempF", room4_RH = "Room4_RH",
                    reversing_valve_signal_V = "RV_Volts",
                    datetime_UTC = "date") # list current and new colnames
  df_agg <- df_agg %>%
    rename(any_of(cols_renamed)) # rename cols using list
  df_agg <- df_agg %>%
    mutate(HP_system_pwr_kW = rowSums(across(c(ODU_pwr_kW, fan_pwr_kW, AHU_pwr_kW, auxheat_pwr_kW)), na.rm = T)) # sum for total system power
  df_agg <- df_agg %>% mutate(across(where(is.numeric), ~ round(., 2))) # as.numeric and round
  df_agg$local_datetime <- format(df_agg$datetime_UTC, tz = tz, usetz = T) # add, convert, format local date col
  df_agg <- df_agg %>% relocate(datetime_UTC) # move utc datetime to front
  df_agg <- df_agg %>% relocate(local_datetime) # move local datetime to front
  return(df_agg)
}

# function to merge aggregated df with existing df
merge_aggd_dfs <- function(df, site, tz){
  path = "/Users/rose775/OneDrive - PNNL/Desktop/Projects/ccHP/Project Management/Data Analysis/hourly_site_data/"
  setwd(path)
  name_file <- str_glue('{site}_aggregated_hourly.csv')
  df_out <- agg_dfs(df, tz)
  df_out <- df_out[!duplicated(df_out), ]
  df_out$site_id <- site
  df_out$unit_id <- site
  df_out <- df_out %>% relocate(unit_id)
  df_out <- df_out %>% relocate(site_id)
  write.csv(df_out, name_file, row.names = FALSE) # save site .csv
  # return(df_out) # make df obj
}

#####################################################
###################### Work #########################
#####################################################
# set wd, read data
path = "/Users/rose775/OneDrive - PNNL/Desktop/Projects/ccHP/Project Management/Data Analysis/Raw Data/Michaels/"
setwd(path)
temp <- list.files(pattern="PNNL_ccASHP__9944LD")
for (i in 1:length(temp)) assign(temp[i], read_csv_homeID(temp[i]))

df <- bind_rows(`PNNL_ccASHP__9944LD_2023-01-09.csv`, `PNNL_ccASHP__9944LD_2023-01-16.csv`,
                `PNNL_ccASHP__9944LD_2023-01-23.csv`, `PNNL_ccASHP__9944LD_2023-02-01.csv`,
                `PNNL_ccASHP__9944LD_2023-02-07.csv`, `PNNL_ccASHP__9944LD_2023-02-14.csv`,
                `PNNL_ccASHP__9944LD_2023-03-24.csv`, `PNNL_ccASHP__9944LD_2023-04-17.csv`)

# run the function
merge_aggd_dfs(df, "9944LD", "US/Mountain") # save site .csv
# test <- merge_aggd_dfs(df, "9944LD", "US/Mountain") make df obj