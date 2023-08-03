#####################################################
# Author: Samuel Rosenberg
# Company: Pacific Northwest National Laboratory
# Created on: 2023-07-19
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
if(Sys.info()[7] == "rose775"){
  source("/Users/rose775/Library/CloudStorage/OneDrive-PNNL/Desktop/Projects/ccHP/hpanalysis/functions_for_use.R")
} else{
  print("Kevin please replace this print with your path to hpanalysis so you can load the functions using source")
}

library(tidyverse)
library(readxl)
library(data.table)



################ variables #########################
e350_min_wd <- "/Volumes/cchpc/raw2/e350/min/"
e350_sec_wd <- "/Volumes/cchpc/raw2/e350/sec/"
michaels_wd <- "/Volumes/cchpc/raw2/michaels/"
sites_wd <- "/Volumes/cchpc/raw2/sites/"
#####################################################


################ functions ##########################
#####################################################


################ e350 ###############################
# read data
setwd("~") # reset wd
setwd(e350_min_wd) # set wd to e350 minute raw data
e350_min <- list.files(pattern="*.csv")

setwd("~") # reset wd
setwd(e350_sec_wd) # set wd to e350 second raw data
e350_sec <- list.files(pattern="*.csv")

e350_sites <- unique(substr(e350_min, 1, 6))

# second data
for (i in e350_sites){
  setwd("~") # reset wd
  setwd(e350_sec_wd) # set wd to e350 raw data
  df <- list.files(path = e350_sec_wd, pattern = i) %>%
    map_df(~fread(., colClasses = c("Timestamp (UTC)" = "character"))) # read all files for the current site iteration and bind_rows to single df
  to_keep <- c("Timestamp (UTC)", "HP_Power [kW]", "Fan_Power [kW]", "AHU_Power [kW]", "Aux_Heat_Power [kW]",
               "ReversingValveSignal [V]") # list of cols used in agg
  df <- subset(df, select = names(df) %in% to_keep) # keep only cols  used in agg
  setwd("~") # reset wd
  setwd(sites_wd) # set wd to sites folder for saving single site file
  write_csv(df, paste("site_", i, "_second.csv", sep = "")) # write csv of single file for site
  
  rm(df) # remove current df from wd to clear up space
  print(paste("site", i, "complete", sep = " ")) # message in console to say that a site has been completed
}

# minute data
for (i in e350_sites){
  setwd("~") # reset wd
  setwd(e350_min_wd) # set wd to e350 raw data
  df <- list.files(path = e350_min_wd, pattern = i) %>%
    map_df(~fread(.)) # read all files for the current site iteration and bind_rows to single df
  to_keep <- c("Timestamp (UTC)", "OA_Temp [F]", "OA_RH [%]", "SA_Blower_Temp [F]", "SA_Blower_RH [%]",
               "SA_Duct1_Temp [F]", "SA_Duct1_RH [%]", "SA_Duct2_Temp [F]", "SA_Duct2_RH [%]",
               "SA_Duct3_Temp [F]", "SA_Duct3_RH [%]", "SA_Duct4_Temp [F]", "SA_Duct4_RH [%]",
               "RA_Temp [F]", "RA_RH [%]", "AHU_Ambient_Temp [F]", "AHU_RH [%]", "Room1_Temp [F]", "Room1_RH [%]",
               "Room2_Temp [F]", "Room2_RH [%]", "Room3_Temp [F]", "Room3_RH [%]", "Room4_Temp [F]", "Room4_RH [%]") # list of cols used in agg
  df <- subset(df, select = names(df) %in% to_keep) # keep only cols  used in agg
  setwd("~") # reset wd
  setwd(sites_wd) # set wd to sites folder for saving single site file
  write_csv(df, paste("site_", i, "_minute.csv", sep = "")) # write csv of single file for site
  
  rm(df) # remove current df from wd to clear up space
  print(paste("site", i, "complete", sep = " ")) # message in console to say that a site has been completed
}

#####################################################


################ michaels ###########################
# read data
setwd("~") # reset wd
setwd(michaels_wd) # set wd to michaels raw data

michaels_files <- list.files()
michaels_site_IDs <- unique(substr(michaels_files, 14, 19))

for (i in michaels_site_IDs){
  setwd("~") # reset wd
  setwd(michaels_wd) # set wd to michaels raw data
  df <- list.files(path = michaels_wd, pattern = i) %>%
           map_df(~fread(.)) # read all files for the current site iteration and bind_rows to single df
  df <- df %>%
    rowwise() %>% 
    mutate(Aux_Power = sum(c_across(colnames(df)[colnames(df) %in% c("Aux1_Power", "Aux2_Power", "Aux3_Power", "Aux4_Power")]), na.rm = T)) # sum for aux power
  to_keep <- c("index", "HP_Power", "Fan_Power", "AHU_Power", "Aux_Power",
               "OA_TempF", "OA_RH", "SA_temp_blower_cabinet_F", "SA_RH_blower_cabinet",
               "SA1_TempF", "SA1_RH", "SA2_TempF", "SA2_RH", "SA3_TempF", "SA3_RH",
               "SA4_TempF", "SA4_RH", "RA_TempF", "RA_RH", "AHU_TempF", "AHU_RH",
               "Room1_TempF", "Room1_RH","Room2_TempF", "Room2_RH", "Room3_TempF",
               "Room3_RH", "Room4_TempF", "Room4_RH", "RV_Volts", "Aux1_Power",
               "Aux2_Power", "Aux3_Power", "Aux4_Power") # list of cols used in agg
  df <- subset(df, select = names(df) %in% to_keep) # keep only cols  used in agg
  
  setwd("~") # reset wd
  setwd(sites_wd) # set wd to sites folder for saving single site file
  write_csv(df, paste("site_", i, ".csv", sep = "")) # write csv of single file for site
  
  rm(df) # remove current df from wd to clear up space
  print(paste("site", i, "complete", sep = " ")) # message in console to say that a site has been completed
}
#####################################################



