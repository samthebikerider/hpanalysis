#####################################################
# Author: Samuel Rosenberg
# Company: Pacific Northwest National Laboratory
# Created on: 2023-04-03
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
# Clear Workspace
rm(list = ls())

# packages
library(tidyverse)
library(xts)
library(stringr)

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
  path = "/Users/rose775/OneDrive - PNNL/Desktop/Projects/ccHP/Project Management/Data Analysis/hourly_site_data"
  setwd(path)
  name_file <- str_glue('{site}_aggregated_hourly.csv')
  df_agg <- agg_dfs(df, tz)
  df_temp <- read.csv(name_file)
  df_temp$datetime_UTC <- as.POSIXct(df_temp$datetime_UTC, format = "%Y-%m-%d %H", tz = "UTC")
  df_out <- rbind(df_temp, df_agg)
  df_out <- df_out[!duplicated(df_out), ]
  df_out$site_id <- site
  df_out$unit_id <- site
  df_out <- df_out %>%
    select(site_id, unit_id, local_datetime, datetime_UTC, ODU_pwr_kW, fan_pwr_kW,
           AHU_pwr_kW,	auxheat_pwr_kW,	OA_temp_F,	OA_RH,	SA_temp_blower_cabinet_F,
           SA_RH_blower_cabinet,	SA_temp_duct1_F,	SA_RH_duct1,	SA_temp_duct2_F,
           SA_RH_duct2,	RA_temp_F,	RA_RH,	AHU_ambient_temp_F,	AHU_ambient_RH,
           room1_temp_F,	room1_RH,	room2_temp_F,	room2_RH,	room3_temp_F,	room3_RH,
           room4_temp_F,	room4_RH,	HP_system_pwr_kW,	reversing_valve_signal_V)
  write.csv(df_out, name_file, row.names = FALSE)
  
}

###################### Work
# import list of already aggregated files
path = "/Users/rose775/OneDrive - PNNL/Desktop/Projects/ccHP/Project Management/Data Analysis/Raw Data/hourly_agg_files/"
setwd(path)
aggd_files <- read.csv("aggd_files.csv")

# read all files in folder that aren't already in agg data
# result is separate dataframe for each .csv
path = "/Users/rose775/OneDrive - PNNL/Desktop/Projects/ccHP/Project Management/Data Analysis/Raw Data/Energy350/1-Minute"
setwd(path)
temp <- list.files(pattern="\\.csv$")
temp_list <- as.list(temp)
temp_list <- temp_list[!(temp_list %in% aggd_files$aggd_files)]
temp <- unlist(temp_list, use.names = F)
for (i in 1:length(temp)) assign(temp[i], read_csv_homeID(temp[i]))

# add newly aggregated data to list of already aggregated data
path = "/Users/rose775/OneDrive - PNNL/Desktop/Projects/ccHP/Project Management/Data Analysis/Raw Data/hourly_agg_files/"
setwd(path)
aggd_files_out <- data.frame(matrix(unlist(temp_list), nrow=length(temp_list), byrow=TRUE))
colnames(aggd_files_out)[1] = "aggd_files"
aggd_files_out <- rbind(aggd_files, aggd_files_out)
# write.csv(aggd_files_out, "aggd_files.csv", row.names = FALSE)

# build sites
site_4228VB <- do.call(rbind, mget(ls(pattern = "4228VB")))
rm(list=ls(pattern="^4228VB"))
site_5539NO <- do.call(rbind, mget(ls(pattern = "5539NO")))
rm(list=ls(pattern="^5539NO"))


# execute aggregation 
merge_aggd_dfs(site_4228VB, "4228VB", "US/Mountain")
merge_aggd_dfs(site_5539NO, "5539NO", "US/Eastern")
















# Below is the initial script that does the hourly aggregation with the called
# frames, as opposed to above which only reads in new files that have not been 
# included in the aggregation previously. The goal is to reduce the amount
# of data that needs to be in the environment

# #####################################################
# # Author: Samuel Rosenberg
# # Company: Pacific Northwest National Laboratory
# # Created on: 2023-02-01
# # Description:
# #####################################################
# # Todo:
# # at some point data will get too large to do it this way.in future, we will
# # need to run the hourly function and bind_rows() with existing hourly
# # aggregated data because there is no need to do it over and over
# #
# # maybe we add a "new" folder and an "old" folder and files are moved to "old"
# # after hourly agg is done
# #
# # integrate hourly agg into read so that it does the agg for each file, only
# # stores one csv at a time in memory
# #
# # reversing valve data?
# #
# # for date and hour - it's UTC, should we change? add "local date and time" cols?
# #####################################################
# ############ data import and processing #############
# #####################################################
# # Clear Workspace
# rm(list = ls())
# 
# # packages
# library(tidyverse)
# # library(zoo)
# library(xts)
# library(stringr)
# 
# # set wd, read data
# path = "/Users/rose775/OneDrive - PNNL/Desktop/Projects/ccHP/Project Management/Data Analysis/Raw Data/Energy350/1-Minute"
# setwd(path)
# # check
# # read csv function that adds home ID column
# read_csv_homeID <- function(filename){
#   df <- read.csv(filename, check.names = F)
#   names(df) <- iconv(names(df), to = "ASCII", sub = "")
#   df[df == "#N/A"] <- NA
#   df[-2] <- lapply(df[-2], as.numeric)
#   df$HomeID <- substr(filename, 1, 6)
#   df <- df %>% relocate(HomeID)
#   df$seconds <- 60
#   df
# }
# 
# ##read all files in folder, result is separate dataframe for each .csv
# temp = list.files(pattern="\\.csv$")
# for (i in 1:length(temp)) assign(temp[i], read_csv_homeID(temp[i]))
# 
# # bind rows for each site
# #site 1 - 4228VB
# site_4228VB <- bind_rows(`4228VB_2022.12.19_Week51_minute.csv`, `4228VB_2022.12.26_Week52_minute.csv`,
#                          `4228VB_2023.01.02_Week01_minute.csv`, `4228VB_2023.01.09_Week02_minute.csv`,
#                          `4228VB_2023.01.16_Week03_minute.csv`, `4228VB_2023.01.23_Week04_minute.csv`,
#                          `4228VB_2023.01.30_Week05_minute.csv`, `4228VB_2023.02.06_Week06_minute.csv`,
#                          `4228VB_2023.02.13_Week07_minute.csv`, `4228VB_2023.02.20_Week08_minute.csv`,
#                          `4228VB_2023.02.27_Week09_minute.csv`)
# #site 2 - 5539NO
# site_5539NO <- bind_rows(`5539NO_2023.02.13_Week07_minute.csv`, `5539NO_2023.02.20_Week08_minute.csv`,
#                          `5539NO_2023.02.27_Week09_minute.csv`)
# 
# 
# # function to aggregate dfs
# agg_dfs <- function(df, site, tz){
#   path = "/Users/rose775/OneDrive - PNNL/Desktop/Projects/ccHP/Project Management/Data Analysis/hourly_site_data"
#   setwd(path)
#   column_names <- colnames(df)
#   name_file <- str_glue('{site}_aggregated_hourly.csv')
#   df_agg <- df %>%
#     group_by("date_UTC" = as.Date(`Timestamp (UTC)`, format = "%m/%d/%Y %H:%M", tz = "UTC"),
#              "hour_of_day_UTC" = as.POSIXlt(`Timestamp (UTC)`, format = "%m/%d/%Y %H:%M", tz = "UTC")$hour) %>%
#     summarise("HP_system_pwr_kW" = NA,
#               "ODU_pwr_kW" = ifelse("HP_Power [kW]" %in% column_names == TRUE, mean(`HP_Power [kW]`, na.rm = T), NA),
#               "fan_pwr_kW" = ifelse("Fan_Power [kW]" %in% column_names == TRUE, mean(`Fan_Power [kW]`, na.rm = T), NA),
#               "AHU_pwr_kW" = ifelse("AHU_Power [kW]" %in% column_names == TRUE, mean(`AHU_Power [kW]`, na.rm = T), NA),
#               "auxheat_pwr_kW" = ifelse("AuxHeat_Power [kW]" %in% column_names == TRUE, mean(`AuxHeat_Power [kW]`, na.rm = T), NA),
#               "OA_temp_F" = ifelse("OA_Temp [F]" %in% column_names == TRUE, mean(`OA_Temp [F]`, na.rm = T), NA),
#               "OA_RH" = ifelse("OA_RH [%]" %in% column_names == TRUE, mean(`OA_RH [%]`, na.rm = T), NA),
#               "SA_temp_blower_cabinet_F" = ifelse("SA_Blower_Temp [F]" %in% column_names == TRUE, mean(`SA_Blower_Temp [F]`, na.rm = T), NA),
#               "SA_RH_blower_cabinet" = ifelse("SA_Blower_RH [%]" %in% column_names == TRUE, mean(`SA_Blower_RH [%]`, na.rm = T), NA),
#               "SA_temp_duct1_F" = ifelse("SA_Duct1_Temp [F]" %in% column_names == TRUE, mean(`SA_Duct1_Temp [F]`, na.rm = T), NA),
#               "SA_RH_duct1" = ifelse("SA_Duct1_RH [%]" %in% column_names == TRUE, mean(`SA_Duct1_RH [%]`, na.rm = T), NA),
#               "SA_temp_duct2_F" = ifelse("SA_Duct2_Temp [F]" %in% column_names == TRUE, mean(`SA_Duct2_Temp [F]`, na.rm = T), NA),
#               "SA_RH_duct2" = ifelse("SA_Duct2_RH [%]" %in% column_names == TRUE, mean(`SA_Duct2_RH [%]`, na.rm = T), NA),
#               "SA_temp_duct3_F" = ifelse("SA_Duct3_Temp [F]" %in% column_names == TRUE, mean(`SA_Duct3_Temp [F]`, na.rm = T), NA),
#               "SA_RH_duct3" = ifelse("SA_Duct3_RH [%]" %in% column_names == TRUE, mean(`SA_Duct3_RH [%]`, na.rm = T), NA),
#               "SA_temp_duct4_F" = ifelse("SA_Duct4_Temp [F]" %in% column_names == TRUE, mean(`SA_Duct4_Temp [F]`, na.rm = T), NA),
#               "SA_RH_duct4" = ifelse("SA_Duct4_RH [%]" %in% column_names == TRUE, mean(`SA_Duct4_RH [%]`, na.rm = T), NA),
#               "RA_temp_F" = ifelse("RA_Temp [F]" %in% column_names == TRUE, mean(`RA_Temp [F]`, na.rm = T), NA),
#               "RA_RH" = ifelse("RA_RH [%]" %in% column_names == TRUE, mean(`RA_RH [%]`, na.rm = T), NA),
#               "AHU_ambient_temp_F" = ifelse("AHU_Ambient_Temp [F]" %in% column_names == TRUE, mean(`AHU_Ambient_Temp [F]`, na.rm = T), NA),
#               "AHU_ambient_RH" = ifelse("AHU_RH [%]" %in% column_names == TRUE, mean(`AHU_RH [%]`, na.rm = T), NA),
#               "room1_temp_F" = ifelse("Room1_Temp [F]" %in% column_names == TRUE, mean(`Room1_Temp [F]`, na.rm = T), NA),
#               "room1_RH" = ifelse("Room1_RH [%]" %in% column_names == TRUE, mean(`Room1_RH [%]`, na.rm = T), NA),
#               "room2_temp_F" = ifelse("Room2_Temp [F]" %in% column_names == TRUE, mean(`Room2_Temp [F]`, na.rm = T), NA),
#               "room2_RH" = ifelse("Room2_RH [%]" %in% column_names == TRUE, mean(`Room2_RH [%]`, na.rm = T), NA),
#               "room3_temp_F" = ifelse("Room3_Temp [F]" %in% column_names == TRUE, mean(`Room3_Temp [F]`, na.rm = T), NA),
#               "room3_RH" = ifelse("Room3_RH [%]" %in% column_names == TRUE, mean(`Room3_RH [%]`, na.rm = T), NA),
#               "room4_temp_F" = ifelse("Room4_Temp [F]" %in% column_names == TRUE, mean(`Room4_Temp [F]`, na.rm = T), NA),
#               "room4_RH" = ifelse("Room4_RH [%]" %in% column_names == TRUE, mean(`Room4_RH [%]`, na.rm = T), NA),
#               "reversing_valve_signal_V" = ifelse("reversing_valve_signal_V" %in% column_names == TRUE, mean(`reversing_valve_signal_V`, na.rm = T), NA),
#               "seconds_non_zero_in_hour" = sum(`seconds`))
#   df_agg <- as.data.frame(df_agg)
#   df_agg <- df_agg %>%
#     mutate(HP_system_pwr_kW = select(., ODU_pwr_kW:auxheat_pwr_kW) %>% rowSums(na.rm = T))
#   df_agg$datetime_UTC <- paste(df_agg$date_UTC, df_agg$hour_of_day_UTC)
#   df_agg$datetime_UTC <- as.POSIXct(df_agg$datetime_UTC, format = "%Y-%m-%d %H", tz = "UTC")
#   df_agg$local_datetime <- format(df_agg$datetime_UTC, tz = tz, usetz = T)
#   df_agg <- df_agg %>% relocate(datetime_UTC)
#   df_agg <- df_agg %>% relocate(local_datetime)
#   df_agg <- df_agg %>% mutate(across(where(is.numeric), ~ round(., 2)))
#   df_agg <- subset(df_agg, select=-c(date_UTC, hour_of_day_UTC))
#   # return(df_agg)
#   write.csv(df_agg, name_file, row.names = FALSE)
# }
# 
# agg_dfs(site_4228VB, "4228VB", "US/Mountain")
# agg_dfs(site_5539NO, "5539NO", "US/Eastern")
