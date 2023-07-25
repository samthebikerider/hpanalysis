#####################################################
# Author: Samuel Rosenberg
# Company: Pacific Northwest National Laboratory
# Created on: 2023-07-24
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

original_data <- read.csv("/Users/rose775/OneDrive - PNNL/Desktop/Projects/ccHP/hpanalysis/sample_data_testing.csv")

#####################################################
# function to fill missing timestamps. flexible for use with different intervals, formats, etc. 
#####################################################
fill_missing_timestamps <- function(df, timestamp_col, format, interval){
  
  df[[timestamp_col]] <- as.POSIXct(df[[timestamp_col]], format = format)
  
  ts <- seq.POSIXt(min(df[[timestamp_col]]), max(df[[timestamp_col]]), by= interval)
  ts <- seq.POSIXt(as.POSIXct(min(df[[timestamp_col]]), format), as.POSIXct(max(df[[timestamp_col]]), format), by = interval)
  
  ts <- seq.POSIXt(min(df[[timestamp_col]]), max(df[[timestamp_col]]), by = interval)
  ts <- format.POSIXct(ts, format)
  
  df_ts <- data.frame(timestamp=ts)
  df_ts[[timestamp_col]] <- df_ts$timestamp
  df_ts[[timestamp_col]] <- as.POSIXct(df_ts[[timestamp_col]], format = format)
  
  df_out <- full_join(df_ts, df)
  
  return(df_out)
}

x <- fill_missing_timestamps(original_data, "timestamp", "%m/%d/%y %H:%M:%S", "sec")