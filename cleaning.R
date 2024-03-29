#####################################################
# Author: Kevin Keene
# Company: Pacific Northwest National Laboratory
# Created on: 2023-07-20
# Description: This script pulls the data files created
  # by the "raw_data_bind_sites.R" file and cleans them,
  # saving the outputs for each site in the "clean" folder.
#####################################################
# Todo:
#
#
#
#####################################################

# Clear Workspace
rm(list = ls())

# Open libraries
library(tidyverse)
library(lubridate)
library(stringr)
library(data.table)
library(openair)



### Data Load and Cleaning ----

# Set working library to read data
if(Sys.info()[7] == "rose775"){   
    source("/Users/rose775/Library/CloudStorage/OneDrive-PNNL/Desktop/Projects/ccHP/hpanalysis/functions_for_use.R") 
    wd <- "/Volumes/cchpc/"
    # Need output wd_out location with RCS filepath
    # wd_out <- 
} else if(Sys.info()[7] == "keen930"){
    source("C:/Users/keen930/OneDrive - PNNL/Documents/CCHP/hpanalysis/functions_for_use.R") 
    wd <- "Q:/"
    wd_out <- "R:/"
} else if(Sys.info()[7] == "zhan682"){
    ## Yiting to add for her file paths 
}





# Read data
  ## First two indexes are folder names, just need three to the end
site_IDs <- unique(substr(list.files(path = paste0(wd, "raw2/sites/")), 6, 11))[-2][-1]
metadata <- read_csv(file = paste0(wd, "site-metadata.csv"))


for (i in site_IDs){
  print(paste("beginning site", i, sep = " "))
  
  ## Load data from 'sites' folder ----
  df <- list.files(path = paste0(wd, "raw2/sites"), pattern = i, full.names = T) %>%
    map_df(~read_csv(.)) %>%
    mutate(site_ID = i)
  
  # Establish timezone as UTC (R defaults to local); Michaels and E350 format different
  if(exists("index", where=df)){
    df <- df %>% mutate(datetime_UTC =  force_tz(index, tzone = "UTC")) %>%
      select(-index)
  } else if(exists("Timestamp (UTC)", where=df)){
    df <- df %>% mutate(datetime_UTC =  force_tz(`Timestamp (UTC)`, tzone = "UTC")) %>%
      select(-`Timestamp (UTC)`)
  }
  
    
  df <- df %>% rename(any_of(c(
            # Michaels column names
            ODU_pwr_kW = "HP_Power", fan_pwr_kW = "Fan_Power",
            AHU_pwr_kW = "AHU_Power", 
            auxheat1_pwr_kW = "Aux1_Power", auxheat2_pwr_kW = "Aux2_Power", 
            auxheat3_pwr_kW = "Aux3_Power", auxheat4_pwr_kW = "Aux4_Power",
            auxheat_pwr_kW = "Aux_Power", 
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
            
            # Energy350 column names
            ODU_pwr_kW = "HP_Power [kW]", fan_pwr_kW = "FanPower [kW]", 
            AHU_pwr_kW = "AHU_Power [kW]", auxheat_pwr_kW = "Aux_Heat_Power [kW]",
            reversing_valve_signal_V = "Reversing_Valve_Signal [VDC]",
            OA_temp_F = "OA_Temp..�.�.F.", OA_RH = "OA_RH....", 
            SA_temp_duct1_F = "SA_Duct1_Temp..�.�.F.", SA_RH_duct1 = "SA_Duct1_RH....", 
            SA_temp_duct2_F = "SA_Duct2_Temp..�.�.F.", SA_RH_duct2 = "SA_Duct2_RH....",
            SA_temp_duct3_F = "SA_Duct3_Temp..�.�.F.", SA_RH_duct3 = "SA_Duct3_RH....", 
            SA_temp_duct4_F = "SA_Duct4_Temp..�.�.F.", SA_RH_duct4 = "SA_Duct4_RH....",
            RA_temp_F = "RA_Temp..�.�.F.", RA_OH = "RA_RH....", 
            AHU_ambient_temp_F = "AHU_Ambient_Temp..�.�.F.", AHU_ambient_RH = "AHU_RH....", 
            room1_temp_F = "Room1_Temp..�.�.F.", room1_RH = "Room1_RH....",
            room2_temp_F = "Room2_Temp..�.�.F.", room2_RH = "Room2_RH....", 
            room3_temp_F = "Room3_Temp..�.�.F.", room3_RH = "Room3_RH....", 
            room4_temp_F = "Room4_Temp..�.�.F.", room4_RH = "Room4_RH...."))) 
  

  print(paste("site", i, "loaded, cleaning commencing", sep = " "))
  
  
  
  ## Cleaning steps ----
  
  # For Michaels sites, some auxheat have reversed power and one site does not have auxheat4_pwr_kW
  if(exists("auxheat1_pwr_kW", where = df) & !exists("auxheat4_pwr_kw", where=df)){
    df$auxheat4_pwr_kW <- as.numeric(NA)}
  if(exists("auxheat1_pwr_kW", where = df)){
    df <- df %>% mutate(auxheat1_pwr_kW = ifelse(auxheat1_pwr_kW < 0, - auxheat1_pwr_kW, auxheat1_pwr_kW),
    auxheat2_pwr_kW = ifelse(auxheat2_pwr_kW < 0, - auxheat2_pwr_kW, auxheat2_pwr_kW),
    auxheat3_pwr_kW = ifelse(auxheat3_pwr_kW < 0, - auxheat3_pwr_kW, auxheat3_pwr_kW),
    auxheat4_pwr_kW = ifelse(auxheat4_pwr_kW < 0, - auxheat4_pwr_kW, auxheat4_pwr_kW),
    
    # Create auxheat_pwr_kW as sum of individual legs (rowSums defaults to zero if all NA, so need to force to NA)
    auxheat_pwr_kW = ifelse(is.na(auxheat1_pwr_kW) & is.na(auxheat2_pwr_kW) & is.na(auxheat3_pwr_kW) & is.na(auxheat4_pwr_kW), NA,
                            rowSums(cbind(auxheat1_pwr_kW, auxheat2_pwr_kW, auxheat3_pwr_kW, auxheat4_pwr_kW), na.rm=T)))}
  
  
  # room4_temp_F, SA_temp_duct3_F, SA_temp_duct4_F, SA_RH_duct3, SA_RH_duct4, reversing_valve_signal_V 
    # exists for some dataframes but not others, enter as NA if it does not exist.
  vars_to_check <- c("room4_temp_F", "room4_RH", "SA_temp_duct3_F", "SA_temp_duct4_F", "SA_RH_duct3", "SA_RH_duct4", "reversing_valve_signal_V")
  for (var_name in vars_to_check){
    if(!exists(var_name, where = df)){
      df[[var_name]] <- as.numeric(NA)
    }
  }
  
  # For all E350 sites, all 1-minute temperature data needs to be interpolated to 1-second
  if(i == "4228VB" | i == "5539NO"){
    df <- df %>%
      group_by(Break=cut(Timestamp, breaks="1 min")) %>%
      mutate(SA_temp_duct1_F=mean(SA_temp_duct1_F, na.rm=T), SA_RH_duct1=mean(SA_RH_duct1, na.rm=T),
             SA_temp_duct2_F=mean(SA_temp_duct2_F, na.rm=T), SA_RH_duct2=mean(SA_RH_duct2, na.rm=T),
             SA_temp_duct3_F=mean(SA_temp_duct3_F, na.rm=T), SA_RH_duct3=mean(SA_RH_duct3, na.rm=T),
             SA_temp_duct4_F=mean(SA_temp_duct4_F, na.rm=T), SA_RH_duct4=mean(SA_RH_duct4, na.rm=T),
             OA_temp_F=mean(OA_temp_F, na.rm=T), OA_RH=mean(OA_RH, na.rm=T),
             RA_temp_F=mean(RA_temp_F, na.rm=T), RA_OH=mean(RA_OH, na.rm=T),
             room1_temp_F=mean(room1_temp_F, na.rm=T), room1_RH=mean(room1_RH, na.rm=T),
             room2_temp_F=mean(room2_temp_F, na.rm=T), room2_RH=mean(room2_RH, na.rm=T),
             room3_temp_F=mean(room3_temp_F, na.rm=T), room3_RH=mean(room3_RH, na.rm=T),
             room4_temp_F=mean(room4_temp_F, na.rm=T), room4_RH=mean(room4_RH, na.rm=T),
             AHU_ambient_temp_F=mean(AHU_ambient_temp_F, na.rm=T), AHU_ambient_RH=mean(AHU_ambient_RH, na.rm=T)) %>%
      ungroup() %>%
      select(-Break)
  }
  
  # Load Trane RV data for site 4228VB
  if(i == "4228VB"){
    # Load data
    trane_rv <- list.files(path = paste0(wd, "trane_rv/"),pattern="*.csv", full.names=T) %>% 
      map_df(~fread(.)) %>%
      as.data.frame() %>%
      mutate(datetime_UTC = as.POSIXct(strptime(DateTime, tz="US/Mountain", format="%m/%d/%Y %I:%M:%S %p") %>%
                                      with_tz(tzone="UTC"))) %>%
      select(datetime_UTC, DEFROST_ON_1)
    
    # Print summary of this data for record #
    write.csv(
      trane_rv %>% group_by(date(datetime_UTC)) %>%
        summarize(DEFROST_ON = round(sum(DEFROST_ON_1==1, na.rm=T)*100/n(), 1),
                  DEFROST_OFF = round(sum(DEFROST_ON_1==0, na.rm=T)*100/n(), 1),
                  DEFROST_NA = round(sum(is.na(DEFROST_ON_1))*100/n(), 1),
                  Data_Points = n()),
      file=paste0(wd, "spring_performance_data/daily_ops/", i, "/Trane_RV_Data_Summary.csv"),
      row.names=F)
      

    # Merge to dataframe
    df <- df %>% merge(trane_rv, by="datetime_UTC", all.x=T, all.y=F) %>%
      rename(reversing_valve_signal_V="DEFROST_ON_1") %>%
    
    # Trane RV data is every four seconds for site 4228VB (and sometimes up to 16 seconds)
    group_by(Break=cut(datetime_UTC, breaks="20 secs")) %>%
      mutate(reversing_valve_signal_V=ceiling(mean(DEFROST_ON_1, na.rm=T))) %>% ungroup() %>%
      select(-Break, -DEFROST_ON_1)
    
    rm(trane_rv)
  }
  
  
    # Fill in missing timestamps, if any, with NA data
  df <- fill_missing_timestamps(df, "datetime_UTC", "%F %T", "min") %>% select(-timestamp) %>%
    # Not sure why, but it is creating 1 NA timestamp
    filter(!is.na(datetime_UTC))

  
  df <- df %>% mutate(
    # Correct RV Volts for before Dec 23 at 6950NE and 8220XE--off by a factor of 10
    reversing_valve_signal_V = ifelse((site_ID == "6950NE" | site_ID == "8220XE") & 
                                      datetime_UTC < strptime("2022-12-23 17:30:00", format="%F %T", tz="UTC"), 
                                      reversing_valve_signal_V * 10, reversing_valve_signal_V),
    
    # Power readings are flipped negative at some sites, need to correct
    fan_pwr_kW = ifelse(fan_pwr_kW < 0, - fan_pwr_kW, fan_pwr_kW),
    AHU_pwr_kW = ifelse(AHU_pwr_kW < 0, - AHU_pwr_kW, AHU_pwr_kW),
    ODU_pwr_kW = ifelse(ODU_pwr_kW < 0, - ODU_pwr_kW, ODU_pwr_kW),
    
    
    # Create HP_system_pwr_kW as sum of all powers (rowSums defaults to zero if all NA)
    HP_system_pwr_kW = ifelse(is.na(ODU_pwr_kW) & is.na(fan_pwr_kW) & is.na(auxheat_pwr_kW), NA,
                              rowSums(cbind(ODU_pwr_kW, fan_pwr_kW, auxheat_pwr_kW), na.rm=T)),
    
    # Create SA_RH and SA_temp_F as sum of four sensors, rowMeans does not default to 0 if all NA
    SA_RH = rowMeans(cbind(SA_RH_duct1, SA_RH_duct2, SA_RH_duct3, SA_RH_duct4), na.rm=T),
    SA_temp_F = rowMeans(cbind(SA_temp_duct1_F, SA_temp_duct2_F, SA_temp_duct3_F, SA_temp_duct4_F), na.rm=T))
  
  df <- df %>%     
    # There are some overlaps in data from their data dumps--remove duplicated rows
    distinct(datetime_UTC, ODU_pwr_kW, auxheat_pwr_kW, fan_pwr_kW, .keep_all = T) %>%
    
    # Some rows have duplicated timestamps with one having NAs for ODU_pwr_kW or fan_pwr_kW--remove the NAs
      # Arrange will put the NAs last if there are duplicates
    arrange(datetime_UTC, ODU_pwr_kW, fan_pwr_kW) %>% 
    filter(!duplicated(datetime_UTC) | (!is.na(fan_pwr_kW) & !is.na(ODU_pwr_kW)))
  
  # Add fields for the date, hour and weekday for each timestamp, converting to the local timezone
  timezone = metadata$Timezone[metadata$Site_ID==i]
  df <- df %>% mutate(date_local = as.character(date(with_tz(datetime_UTC, tzone=timezone))),
                        hour_local = hour(with_tz(datetime_UTC, tzone=timezone)),
                        weekday_local = lubridate::wday(with_tz(datetime_UTC, tzone=timezone), label=T))
  
  
  print(paste("site", i, "cleaned, commencing diagnostics", sep = " "))
  
  
  ## Diagnostics tables to show missing/NA data ----
  
  # NA Data Summary
  write.csv(
      df %>%
        group_by(site_ID, date_local, weekday_local) %>%
        summarize(ODU_pwr_kW_NA = round(sum(is.na(ODU_pwr_kW))*100/ n(), 1),
                  auxheat_pwr_kW_NA = round(sum(is.na(auxheat_pwr_kW))*100/ n(),1),
                  fan_pwr_kW_NA = round(sum(is.na(fan_pwr_kW))*100/ n(),1),
                  reversing_valve_signal_V_NA = round(sum(is.na(reversing_valve_signal_V))*100/ n(),1),
                  OA_Temp_NA = round(sum(is.na(OA_temp_F))*100/ n(),1),
                  SA_Temp_NA = round(sum(is.na(SA_temp_F))*100/ n(),1),
                  Duplicated_timestamps = round(sum(duplicated(datetime_UTC))*2*100/ n(),1),
                  Data_missing = round(100 - (n() - sum(duplicated(datetime_UTC)))*100/ 86400, 1)),
      file=paste0(wd_out, "daily_ops/", i, "/Missing_Power_Data_Summary.csv"),
      row.names=F)

  
  
  
  
  
  print(paste("site", i, "diagnosted completed, printing aggregated files", sep = " "))
  
  ## Output 1-minute data in case we ever need to use a smaller dataset to run something ##
  df_1m <- timeAverage(df %>% rename(date = "datetime_UTC"), avg.time = "min", data.thresh = 75, statistic = "mean") %>%
    rename(datetime_UTC = "date") %>%
    # timeAverage will drop categorical variables, need to recalculate
    mutate(site_ID = i,
           date_local = as.character(date(with_tz(datetime_UTC, tzone=timezone))),
           hour_local = hour(with_tz(datetime_UTC, tzone=timezone)),
           weekday_local = lubridate::wday(with_tz(datetime_UTC, tzone=timezone), label=T))
  

  write_csv(df_1m, paste0(wd, "clean/1_min/", i, ".csv"))
  write_csv(df, paste0(wd, "clean/1_sec/", i, ".csv"))
  
  rm(df, df_1m, timezone)
  
  
  
  ### 
  
  
  
  print(paste("site", i, "complete", sep = " "))
}
  

