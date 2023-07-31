#####################################################
# Author: Kevin Keene
# Company: Pacific Northwest National Laboratory
# Created on: 2023-07-20
# Description: This script pulls the data files created
  # by the "raw_data_bind_sites.R" file and cleans them,
  # saving the outputs for each site aggregated by 1 
  # second, 1 minute, 5 minutes, and 1 hour.
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
    # Need output wd location ("R:/" for Kevin)
    # wd_out <- 
  } else {
    source("C:/Users/keen930/OneDrive - PNNL/Documents/CCHP/hpanalysis/functions_for_use.R") 
    wd <- "Q:/"
    wd_out <- "R:/"
}





# Read data
site_IDs <- unique(substr(list.files(path = paste0(wd, "raw2/sites/")), 6, 11))
metadata <- read_csv(file = paste0(wd, "site-metadata.csv"))

for (i in site_IDs){
  print(paste("beginning site", i, sep = " "))
  
  ## Load data from 'sites' folder ----
  df <- list.files(path = paste0(wd, "raw2/sites"), pattern = i, full.names = T) %>%
    map_df(~read_csv(.)) %>%
    mutate(site_ID = i,
           datetime_UTC =  force_tz(index, tzone = "UTC")) %>%
    
    ## Remove the select once Sam puts this in his script ##
    select(any_of(c("datetime_UTC", "site_ID", "index", "HP_Power", "Fan_Power", "AHU_Power", "Aux_Power",
           "OA_TempF", "OA_RH",
           "SA1_TempF", "SA1_RH", "SA2_TempF", "SA2_RH", "SA3_TempF", "SA3_RH",
           "SA4_TempF", "SA4_RH", "RA_TempF", "RA_RH", "AHU_TempF", "AHU_RH",
           "Room1_TempF", "Room1_RH","Room2_TempF", "Room2_RH", "Room3_TempF",
           "Room3_RH", "Room4_TempF", "Room4_RH", "RV_Volts"))) %>% 
    select(-index) %>%
    rename(any_of(c(ODU_pwr_kW = "HP_Power", fan_pwr_kW = "Fan_Power",
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
            reversing_valve_signal_V = "RV_Volts"))) 
  
  
  print(paste("site", i, "loaded, cleaning commencing", sep = " "))
  
  ## Cleaning steps ----
    # Fill in missing timestamps, if any, with NA data
  summary(df$datetime_UTC)
  df <- fill_missing_timestamps(df, "datetime_UTC", "%F %T", "sec") %>% select(-timestamp)
  summary(df$datetime_UTC)
  
  
  df <- df %>% mutate(
    # Correct RV Volts for before Dec 23 at 6950NE and 8220XE--off by a factor of 10
    reversing_valve_signal_V = ifelse((site_ID == "6950NE" | site_ID == "8220XE") & 
                                      datetime_UTC < strptime("2022-12-23 17:30:00", format="%F %T", tz="UTC"), 
                                      reversing_valve_signal_V * 10, reversing_valve_signal_V),
    
    # Power readings are flipped negative at some sites, need to correct
    fan_pwr_kW = ifelse(fan_pwr_kW < 0, - fan_pwr_kW, fan_pwr_kW),
    AHU_pwr_kW = ifelse(AHU_pwr_kW < 0, - AHU_pwr_kW, AHU_pwr_kW),
    ODU_pwr_kW = ifelse(ODU_pwr_kW < 0, - ODU_pwr_kW, ODU_pwr_kW),
    
    ## Add these back once Sam adds the aux unit powers in his script ##
    # auxheat1_pwr_kW = ifelse(auxheat1_pwr_kW < 0, - auxheat1_pwr_kW, auxheat1_pwr_kW),
    # auxheat2_pwr_kW = ifelse(auxheat2_pwr_kW < 0, - auxheat2_pwr_kW, auxheat2_pwr_kW),
    # auxheat3_pwr_kW = ifelse(auxheat3_pwr_kW < 0, - auxheat3_pwr_kW, auxheat3_pwr_kW),
    # auxheat4_pwr_kW = ifelse(auxheat4_pwr_kW < 0, - auxheat4_pwr_kW, auxheat4_pwr_kW),
    
    # Create auxheat_pwr_kW as sum of individual legs (rowSums defaults to zero if all NA, so need to force to NA)
    ## Add this back, or remove if it is already in Sam's script ##
    # auxheat_pwr_kW = ifelse(is.na(auxheat1_pwr_kW) & is.na(auxheat2_pwr_kW) & is.na(auxheat3_pwr_kW) & is.na(auxheat4_pwr_kW), NA,
    #                    rowSums(cbind(auxheat1_pwr_kW, auxheat2_pwr_kW, auxheat3_pwr_kW, auxheat4_pwr_kW), na.rm=T)),
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
  
  
  ## Diagnostics charts and tables ----
  
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
      file=paste0(wd_out, "daily_ops/", id, "/Missing_Power_Data_Summary.csv"),
      row.names=F)

  
  
  
  
  
  print(paste("site", i, "diagnosted completed, printing aggregated files", sep = " "))
  
  df_1h <- timeAverage(df %>% rename(date = datetime_UTC), avg.time = "hour", data.thresh = 75, statistic = "mean")
  df_1m <- timeAverage(df %>% rename(date = datetime_UTC), avg.time = "minute", data.thresh = 75, statistic = "mean")
  df_5m <- timeAverage(df %>% rename(date = datetime_UTC), avg.time = "5 minute", data.thresh = 75, statistic = "mean")
  
  write_csv(df_1h, paste0(wd, "clean/1_hour/", i, ".csv"))
  write_csv(df_1m, paste0(wd, "clean/1_min/", i, ".csv"))
  write_csv(df, paste0(wd, "clean/1_sec/", i, ".csv"))
  write_csv(df_5m, paste0(wd, "clean/5_min/", i, ".csv"))
  
  rm(df, df_1h, df_1m, df_5m, timezone)
  
  
  
  ### 
  
  
  
  print(paste("site", i, "complete", sep = " "))
}
  

  
  ##
  
  
  ### Time Series Daily Investigation Plots ----
  

# Temperature time series comparison chart
RoomTempTimeSeries <- function(interval, timestart, timeend){
  # Look at a time series graph for all temperature monitors, time interval (e.g, 5-minute), time period, and site
  # Interval is in units of minutes, and so the maximum interval would be 60 minutes/1 hour.
  # The time start and end should be a date string in format for example "4/01/2022 0:00".
  df %>% mutate(datetime_UTC = datetime_UTC %>% with_tz(metadata$Timezone[metadata$Site_ID==sitename]),
                Interval = minute(datetime_UTC) %/% interval) %>% 
    filter(Site_ID == sitename &
             datetime_UTC >= strptime(timestart,"%F %H:%M", tz=metadata$Timezone[metadata$Site_ID==sitename]) &
             datetime_UTC <= strptime(timeend,"%F %H:%M", tz=metadata$Timezone[metadata$Site_ID==sitename])) %>%
    group_by(Site_ID, Date, Hour, Interval) %>% 
    summarize(datetime_UTC = datetime_UTC[1],
              Room1_TempF = mean(Room1_TempF,na.rm=T),
              Room2_TempF = mean(Room2_TempF,na.rm=T),
              Room3_TempF = mean(Room3_TempF,na.rm=T),
              Room4_TempF = mean(Room4_TempF,na.rm=T),
              AHU_TempF = mean(AHU_TempF,na.rm=T)) %>%
    ggplot(aes(x=as.POSIXct(datetime_UTC))) +
    geom_line(aes(y=Room1_TempF, color = "Room 1"),size=0.5) + 
    geom_line(aes(y=Room2_TempF, color = "Room 2"),size=0.5) +
    geom_line(aes(y=Room3_TempF, color = "Room 3"),size=0.5) +
    geom_line(aes(y=Room4_TempF, color = "Room 4"),size=0.5) +
    geom_line(aes(y=AHU_TempF, color = "AHU Ambient"),size=0.5) +
    scale_y_continuous(breaks = seq(0,200, by=10), minor_breaks = seq(0, 200, by=1)) +
    scale_color_manual(name = "", values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
    labs(title=paste0("Room temperature time series plot for site ", sitename),x="",y="Temperature (F)") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          panel.grid.major = element_line(size = 0.9),
          panel.grid.minor = element_line(size = 0.1),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5),) +
    guides(color=guide_legend(override.aes=list(size=3)))
}
# RoomTempTimeSeries(5, "2023-02-23 00:00", "2023-02-26 00:00")
  # Save a sample of data to the folder for each site
    # Adjust dates manually
ggsave(paste0(sitename, '_Room_Temperature_Comparison.png'),
       plot = RoomTempTimeSeries(5, "2023-02-16 00:00", "2023-02-17 00:00"),
       path = paste0(wd,'/Graphs/',sitename, '/'),
       width=12, height=4, units='in')


# Supply temperature time series comparison chart
SupplyTempTimeSeries <- function(interval, timestart, timeend){
  # Look at a time series graph for the four supply temperature monitors, time interval (e.g, 5-minute), time period, and site
  # Interval is in units of minutes, and so the maximum interval would be 60 minutes/1 hour.
  # The time start and end should be a date string in format for example "4/01/2022 0:00".
  df %>% mutate(datetime_UTC = datetime_UTC %>% with_tz(metadata$Timezone[metadata$Site_ID==sitename]),
                Interval = minute(datetime_UTC) %/% interval) %>% 
    filter(Site_ID == sitename &
             datetime_UTC >= strptime(timestart,"%F %H:%M", tz=metadata$Timezone[metadata$Site_ID==sitename]) &
             datetime_UTC <= strptime(timeend,"%F %H:%M", tz=metadata$Timezone[metadata$Site_ID==sitename])) %>%
    group_by(Site_ID, Date, Hour, Interval) %>% 
    summarize(datetime_UTC = datetime_UTC[1],
              SA1_TempF = mean(SA1_TempF,na.rm=T),
              SA2_TempF = mean(SA2_TempF,na.rm=T),
              SA3_TempF = mean(SA3_TempF,na.rm=T),
              SA4_TempF = mean(SA4_TempF,na.rm=T)) %>%
    ggplot(aes(x=as.POSIXct(datetime_UTC))) +
    geom_line(aes(y=SA1_TempF, color = "SA1"),size=0.5) + 
    geom_line(aes(y=SA2_TempF, color = "SA2"),size=0.5) + 
    geom_line(aes(y=SA3_TempF, color = "SA3"),size=0.5) + 
    geom_line(aes(y=SA4_TempF, color = "SA4"),size=0.5) + 
    scale_y_continuous(breaks = seq(0,200, by=10), minor_breaks = seq(0,200, by=1)) +
    scale_color_manual(name = "", values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
    labs(title=paste0("Supply temperature time series plot for site ", sitename),x="",y="Temperature (F)") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          panel.grid.major = element_line(size = 0.9),
          panel.grid.minor = element_line(size = 0.1),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5),) +
    guides(color=guide_legend(override.aes=list(size=3)))
}
# SupplyTempTimeSeries(5, "2023-03-16 00:00", "2023-03-19 00:00")
# Save a sample week of data to the folder for each site
  # Adjust date manually
ggsave(paste0(sitename, '_Supply_Temperature_Comparison.png'),
       plot = SupplyTempTimeSeries(5, "2023-03-16 00:00", "2023-03-19 00:00"),
       path = paste0(wd,'/Graphs/',sitename, '/'),
       width=12, height=4, units='in')



# Power time series comparison chart with OAT and SAT
OperationTimeSeries <- function(site, timestart, timeend){
  # The time start and end should be character with format "%Y-%m-%d".
  df %>% mutate(datetime_UTC = datetime_UTC %>% with_tz(metadata$Timezone[metadata$Site_ID==site])) %>% 
    filter(Site_ID == site &
             datetime_UTC >= strptime(timestart,"%F", tz=metadata$Timezone[metadata$Site_ID==site]) &
             datetime_UTC <= strptime(timeend,"%F", tz=metadata$Timezone[metadata$Site_ID==site])) %>%
    ggplot(aes(x=as.POSIXct(datetime_UTC))) +
    geom_line(aes(y=OA_temp_F/5, color = "Outdoor Air Temperature"),size=0.3) + 
    geom_line(aes(y=SA_temp_F/5, color = "Supply Air Temperature"),size=0.3) +
    geom_line(aes(y=ODU_pwr_kW, color = "Outdoor Unit Power"),size=0.3) + 
    geom_line(aes(y=fan_pwr_kW, color = "Supply Fan Power"),size=0.3) +
    geom_line(aes(y=auxheat_pwr_kW, color = "Auxiliary Power"),size=0.3) + 
    scale_y_continuous(name = "Power (kW)",
                       limits = c(-4, 25),
                       sec.axis = sec_axis(~.*5, name ="Temperature (F)")) +
    scale_color_manual(name = "", breaks = c("Auxiliary Power","Outdoor Unit Power","Supply Fan Power","Outdoor Air Temperature","Supply Air Temperature"),
                       values = c("#E69F00","gray","#009E73","black","#56B4E9","#CC79A7", "#F0E442", "#0072B2", "#D55E00")) +
    labs(title=paste0("System operation time series plot for site ", site),x="") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          panel.grid.major = element_line(size = 0.5),
          panel.grid.minor = element_line(size = 0.1),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5)) +
    guides(color=guide_legend(override.aes=list(size=3)))
}
# OperationTimeSeries(sitename, "2023-02-12", "2023-02-13")
# Loop to print daily operation time series graphs, one for each day for each site
for(id in unique(df$Site_ID)){
  dates <- unique(df$Date[df$Site_ID==id])  
  for(d in dates[-1]){
    d1 = substr(as.character(strptime(d, "%F", tz=metadata$Timezone[metadata$Site_ID==id]) + 60*60*24), 1, 10) # Date plus one day
    ggsave(paste0(id, '_Daily-Operation_',d,'.png'),
           plot = OperationTimeSeries(id, d, d1),
           path = paste0(wd,'/Graphs/',id, '/Daily Operation/'),
           width=12, height=4, units='in')
  }
}
rm(dates,d1,d,id)

