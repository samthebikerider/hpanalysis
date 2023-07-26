#####################################################
# Author: Samuel Rosenberg
# Company: Pacific Northwest National Laboratory
# Created on: 2023-07-26
# Description: Functions to be widely used in the CCHP data cleaning and analysis
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

# fill missing rows with an appropriate timestamp and NA 
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