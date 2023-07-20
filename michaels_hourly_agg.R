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
library(data.table)
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
  df <- df %>%
    rowwise() %>% 
    mutate(Aux_Power = sum(c_across(colnames(df)[colnames(df) %in% c("Aux1_Power", "Aux2_Power", "Aux3_Power", "Aux4_Power")]), na.rm = T)) # sum for aux power
  df
}

# function to read csvs that are new and append list of csv's to reflect processing
grab_data <- function(pattern){
  # load aggd_files.csv
  path = "/Users/rose775/OneDrive - PNNL/Desktop/Projects/ccHP/Project Management/Data Analysis/hourly_site_data/"
  setwd(path)
  aggd_files <- read.csv("aggd_files.csv")
  
  # load files not already agg'd
  path = "/Users/rose775/OneDrive - PNNL/Desktop/Projects/ccHP/Project Management/Data Analysis/Raw Data/Michaels"
  setwd(path)
  temp <- list.files(pattern=pattern)
  temp_list <- as.list(temp)
  temp_list <- temp_list[!(temp_list %in% aggd_files$aggd_files)]
  temp <- unlist(temp_list, use.names = F)
  df <- temp %>% map_df(~read_csv_homeID(.))
  return(df)

  # append list to column and date to column
  # add newly aggregated data to list of already aggregated data
  aggd_files_out <- data.frame(matrix(nrow=length(temp_list), ncol = 2))
  colnames(aggd_files_out) <- c("aggd_files", "date")
  aggd_files_out$aggd_files <- temp
  aggd_files_out$date <- Sys.Date()
  aggd_files_out <- rbind(aggd_files, aggd_files_out)
  path = "/Users/rose775/OneDrive - PNNL/Desktop/Projects/ccHP/Project Management/Data Analysis/hourly_site_data/"
  setwd(path)
  write.csv(aggd_files_out, "aggd_files.csv", row.names = FALSE)
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
    rowwise() %>% 
    mutate(HP_system_pwr_kW = sum(c_across(colnames(df_agg)[colnames(df_agg) %in% c("ODU_pwr_kW", "fan_pwr_kW", "AHU_pwr_kW", "auxheat_pwr_kW")]), na.rm = T)) # sum for total system power
  df_agg <- df_agg %>% mutate(across(where(is.numeric), ~ round(., 2))) # as.numeric and round
  df_agg$local_datetime <- format(df_agg$datetime_UTC, tz = tz, usetz = T) # add, convert, format local date col
  return(df_agg)
}

# function to merge aggregated df with existing df
merge_aggd_dfs <- function(df, site, tz){
  df_cols <- c("site_id", "unit_id", "local_datetime", "datetime_UTC", "ODU_pwr_kW", "fan_pwr_kW",
               "AHU_pwr_kW",	"auxheat_pwr_kW",	"OA_temp_F",	"OA_RH",	"SA_temp_blower_cabinet_F",
               "SA_RH_blower_cabinet",	"SA_temp_duct1_F",	"SA_RH_duct1",	"SA_temp_duct2_F",
               "SA_RH_duct2","SA_temp_duct3_F", "SA_RH_duct3", "SA_temp_duct4_F", "SA_RH_duct4",
               "RA_temp_F",	"RA_RH",	"AHU_ambient_temp_F",	"AHU_ambient_RH",
               "room1_temp_F",	"room1_RH",	"room2_temp_F",	"room2_RH",	"room3_temp_F",	"room3_RH",
               "room4_temp_F",	"room4_RH",	"HP_system_pwr_kW",	"reversing_valve_signal_V")
  setwd("/Users/rose775/OneDrive - PNNL/Desktop/Projects/ccHP/Project Management/Data Analysis/hourly_site_data/new/")
  name_file <- str_glue('{site}_aggregated_hourly.csv')
  df_out <- agg_dfs(df, tz)
  missing_cols <- setdiff(df_cols, names(df_out))
  df_out[missing_cols] <- NA
  df_out <- df_out[df_cols]
  df_temp <- read.csv(name_file)
  df_temp$datetime_UTC <- as.POSIXct(df_temp$datetime_UTC, format = "%Y-%m-%d %H", tz = "UTC")
  df_out <- rbind(df_temp, df_agg)
  df_out <- df_out[!duplicated(df_out), ]
  df_out$site_id <- site
  df_out$unit_id <- site
  df_out <- df_out %>%
    select(any_of(df_cols))
  write.csv(df_out, name_file, row.names = FALSE)
  #return(df_out)
}

#####################################################
###################### Work #########################
#####################################################

# agg each site
site_6950NE <- grab_data("PNNL_ccASHP__6950NE")
merge_aggd_dfs(site_6950NE, "6950NE", "US/Central")
rm(site_6950NE)

site_8220XE <- grab_data("PNNL_ccASHP__8220XE")
merge_aggd_dfs(site_8220XE, "8220XE", "US/Central")
rm(site_8220XE)


site_9944LD <- grab_data("PNNL_ccASHP__9944LD")
merge_aggd_dfs(site_9944LD, "9944LD", "US/Mountain")
rm(site_9944LD)


site_2563EH <- grab_data("PNNL_ccASHP__2563EH")
merge_aggd_dfs(site_2563EH, "2563EH", "US/Eastern")
rm(site_2563EH)


site_2896BR <- grab_data("PNNL_ccASHP__2896BR")
merge_aggd_dfs(site_2896BR, "2896BR", "US/Eastern")
rm(site_2896BR)


site_6112OH <- grab_data("PNNL_ccASHP__6112OH")
merge_aggd_dfs(site_6112OH, "6112OH", "US/Eastern")
rm(site_6112OH)


site_8726VB <- grab_data("PNNL_ccASHP__8726VB")
merge_aggd_dfs(site_8726VB, "8726VB", "US/Eastern")
rm(site_8726VB)
