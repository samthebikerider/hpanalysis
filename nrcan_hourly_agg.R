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
library(openair)

# set wd, read data
path = "/Users/rose775/OneDrive - PNNL/Desktop/Projects/ccHP/Project Management/Data Analysis/Raw Data/NRCan/1-Minute"
setwd(path)

# check
# read csv function that adds home ID column
read_csv_homeID <- function(filename){
  df <- read.csv(filename, check.names = F)
  df[-2] <- lapply(df[-2], as.numeric)
  df$HomeID <- substr(filename, 1, 6)
  df <- df %>% relocate(HomeID)
  df
  df
}

##read all files in folder, result is separate dataframe for each .csv
temp = list.files(pattern="\\.csv$")
for (i in 1:length(temp)) assign(temp[i], read_csv_homeID(temp[i]))

# units
site_5291QJ <- bind_rows(`5291QJ_M24BC 2023-01-08 to 2023-01-14 Weekly_24B.csv`, `5291QJ_M24BC 2023-01-15 to 2023-01-21 Weekly_24B.csv`,
                         `5291QJ_M24BC 2023-01-22 to 2023-01-28 Weekly_24B.csv`, `5291QJ_M24BC 2023-01-29 to 2023-02-04 Weekly_24B.csv`,
                         `5291QJ_M24BC 2023-02-05 to 2023-02-07 Weekly_24B.csv`, `5291QJ_M24BC 2023-03-07 to 2023-03-11 Weekly_24B.csv`,
                         `5291QJ_M24BC 2023-03-12 to 2023-03-18 Weekly_24B.csv`, `5291QJ_M24BC 2023-03-19 to 2023-03-25 Weekly_24B.csv`,
                         `5291QJ_M24BC 2023-03-27 to 2023-04-01 Weekly_24B.csv`, `5291QJ_M24BC 2023-04-02 to 2023-04-08 Weekly_24B.csv`,
                         `5291QJ_M24BC 2023-04-09 to 2023-04-15 Weekly_24B.csv`)

site_2458CE <- bind_rows(`2458CE_M24BC 2023-03-07 to 2023-03-11 Weekly_24C.csv`, `2458CE_M24BC 2023-03-12 to 2023-03-18 Weekly_24C.csv`,
                         `2458CE_M24BC 2023-03-19 to 2023-03-25 Weekly_24C.csv`, `2458CE_M24BC 2023-03-27 to 2023-04-01 Weekly_24C.csv`,
                         `2458CE_M24BC 2023-04-02 to 2023-04-08 Weekly_24C.csv`, `2458CE_M24BC 2023-04-09 to 2023-04-15 Weekly_24C.csv`)

# function to aggregate dfs
agg_dfs <- function(df){
  column_names <- colnames(df)
  df$date <- as.POSIXct(df$Timestamp, format = "%m/%d/%Y %H:%M", tz = "US/Eastern") # format and name date column for timeAverage
  df$OA_temp_F <- (df$`Ambient T (oC)` * (9/5)) + 32
  df$RA_temp_F <- (df$`Return T (oC)` * (9/5)) + 32
  df$SA_temp_duct1_F <- (df$`Supply T (oC)` * (9/5)) + 32
  df$room1_temp_F <- (df$`Main Floor T-stat T (oC)` * (9/5)) + 32
  aux_cols <- c("Heat Bank Power (W) - 1min AVG",
                "Heat Bank Stage 1 Power (W) - 1min AVG",
                "Heat Bank Stage 2 Power (W) - 1min AVG",
                "Heat Bank Stage 3 Power (W) - 1min AVG")
  df <- df %>% 
    rowwise() %>% 
    mutate(auxheat_pwr_kW=sum(c_across(colnames(df)[colnames(df) %in% aux_cols]),na.rm=T))
  df <- df %>%
    mutate(AHU_pwr_kW = rowSums(across(c(auxheat_pwr_kW, `Blower Power (W) - 1min AVG`)), na.rm = T))
  to_keep <- c("date", "Return RH (%RH)", "RA_temp_F",
               "Supply RH (%RH)", "SA_temp_duct1_F", "Main Floor T-stat RH (%RH)", "room1_temp_F",
               "Ambient RH (%RH)", "OA_temp_F", "Outdoor Unit Power (W) - 1min AVG",
               "Blower Power (W) - 1min AVG", "auxheat_pwr_kW", "AHU_pwr_kW") # list of cols used in agg
  df2 <- subset(df, select = names(df) %in% to_keep) # keep only cols  used in agg
  df_agg <- timeAverage(df2, avg.time = "hour", data.thresh = 75, statistic = "mean") # agg hourly
  cols_renamed <- c(ODU_pwr_kW = "Outdoor Unit Power (W) - 1min AVG",
                    fan_pwr_kW = "Blower Power (W) - 1min AVG",
                    OA_RH = "Ambient RH (%RH)",
                    SA_RH_duct1 = "Supply RH (%RH)",
                    RA_RH = "Return RH (%RH)",
                    room1_RH = "Main Floor T-stat RH (%RH)",
                    local_datetime = "date") # list current and new colnames
  df_agg <- df_agg %>%
    rename(any_of(cols_renamed)) # rename cols using list
  df_agg$ODU_pwr_kW <- df_agg$ODU_pwr_kW / 1000
  df_agg$fan_pwr_kW <- df_agg$fan_pwr_kW / 1000
  df_agg$AHU_pwr_kW <- df_agg$AHU_pwr_kW / 1000
  df_agg$auxheat_pwr_kW <- df_agg$auxheat_pwr_kW / 1000
  df_agg <- df_agg %>%
    mutate(HP_system_pwr_kW = rowSums(across(c(ODU_pwr_kW, fan_pwr_kW, AHU_pwr_kW, auxheat_pwr_kW)), na.rm = T)) # sum for total system power
  df_agg <- df_agg %>% mutate(across(where(is.numeric), ~ round(., 2))) # as.numeric and round
  df_agg$datetime_UTC <- format(df_agg$local_datetime, tz = "UTC", usetz = T) # add, convert, format local date col
  return(df_agg)
}

# function to merge aggregated df with existing df
merge_aggd_dfs <- function(df, site){
  path = "/Users/rose775/OneDrive - PNNL/Desktop/Projects/ccHP/Project Management/Data Analysis/hourly_site_data/"
  setwd(path)
  name_file <- str_glue('{site}_aggregated_hourly.csv')
  df_out <- agg_dfs(df)
  df_out <- df_out[!duplicated(df_out), ]
  df_out$site_id <- site
  df_out$unit_id <- site
  df_out$reversing_valve_signal_V <- NA
  df_out$SA_temp_duct2_F <- NA
  df_out$SA_RH_duct2 <- NA
  df_out$SA_RH_duct3 <- NA
  df_out$SA_temp_duct3_F <- NA
  df_out$SA_temp_duct4_F <- NA
  df_out$SA_RH_duct4 <- NA
  df_out$room2_temp_F <- NA
  df_out$room2_RH <- NA
  df_out$room3_temp_F <- NA
  df_out$room3_RH <- NA
  df_out$room4_temp_F <- NA
  df_out$room4_RH <- NA
  df_out$AHU_ambient_temp_F <- NA
  df_out$AHU_ambient_RH <- NA
  df_out$SA_temp_blower_cabinet_F <- NA
  df_out$SA_RH_blower_cabinet <- NA
  df_out <- df_out %>%
    select(site_id, unit_id, local_datetime, datetime_UTC, ODU_pwr_kW, fan_pwr_kW,
           AHU_pwr_kW,	auxheat_pwr_kW,	OA_temp_F,	OA_RH,	SA_temp_blower_cabinet_F,
           SA_RH_blower_cabinet,	SA_temp_duct1_F,	SA_RH_duct1,	SA_temp_duct2_F,
           SA_RH_duct2,	RA_temp_F,	RA_RH,	AHU_ambient_temp_F,	AHU_ambient_RH,
           room1_temp_F,	room1_RH,	room2_temp_F,	room2_RH,	room3_temp_F,	room3_RH,
           room4_temp_F,	room4_RH,	HP_system_pwr_kW,	reversing_valve_signal_V)
  write.csv(df_out, name_file, row.names = FALSE) # save site .csv
  # return(df_out) # make df obj
}

merge_aggd_dfs(site_5291QJ, "5291QJ")
merge_aggd_dfs(site_2458CE, "2458CE")