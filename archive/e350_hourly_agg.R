#####################################################
# Author: Samuel Rosenberg
# Company: Pacific Northwest National Laboratory
# Created on: 2023-05-22
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
  df <- read.csv(filename, check.names = F)
  names(df) <- iconv(names(df), to = "ASCII", sub = "")
  df[df == "#N/A"] <- NA
  df[-2] <- lapply(df[-2], as.numeric)
  df$HomeID <- substr(filename, 1, 6)
  df <- df %>% relocate(HomeID)
  df
}

# function to read csvs that are new and append list of csv's to reflect processing
grab_data <- function(pattern){
  # load aggd_files.csv
  path = "/Users/rose775/OneDrive - PNNL/Desktop/Projects/ccHP/Project Management/Data Analysis/hourly_site_data/"
  setwd(path)
  aggd_files <- read.csv("aggd_files.csv")
  
  # load files not already agg'd
  path = "/Users/rose775/OneDrive - PNNL/Desktop/Projects/ccHP/Project Management/Data Analysis/Raw Data/Energy350/1-Minute"
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
  column_names <- colnames(df)
  df$date <- as.POSIXct(df$`Timestamp (UTC)`, format = "%m/%d/%Y %H:%M", tz = "UTC") # format and name date column for timeAverage
  to_keep <- c("HomeID", "date", "HP_Power [kW]", "Fan_Power [kW]", "AHU_Power [kW]", "Aux_Heat_Power [kW]",
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
    mutate(HP_system_pwr_kW = sum(c_across(colnames(df_agg)[colnames(df_agg) %in% c("ODU_pwr_kW", "fan_pwr_kW", "AHU_pwr_kW", "auxheat_pwr_kW")]), na.rm=T))
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
  setwd("/Users/rose775/OneDrive - PNNL/Desktop/Projects/ccHP/Project Management/Data Analysis/hourly_site_data/")
  name_file <- str_glue('{site}_aggregated_hourly.csv')
  df_agg <- agg_dfs(df, tz)
  missing_cols <- setdiff(df_cols, names(df_agg))
  df_agg[missing_cols] <- NA
  df_agg <- df_agg[df_cols]
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
site_4228VB <- grab_data("4228VB")
merge_aggd_dfs(site_4228VB, "4228VB", "US/Mountain")
rm(site_4228VB)

site_5539NO <- grab_data("5539NO")
merge_aggd_dfs(site_5539NO, "5539NO", "US/Eastern")
rm(site_5539NO)



