#####################################################
# Author: Samuel Rosenberg
# Company: Pacific Northwest National Laboratory
# Created on: 2023-07-19
# Description: Reads raw CSV files from M&V contractors
  # and binds into one file for each site.
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
} else if(Sys.info()[7] = "keen930"){
  source("C:/Users/keen930/OneDrive - PNNL/Documents/CCHP/hpanalysis/functions_for_use.R")
} else if(Sys.info()[7] = "zhan682"){
  ## Yiting to update filepath ##
  source("C:/Users/zhan682/OneDrive - PNNL/Documents/CCHP/hpanalysis/functions_for_use.R")
}

library(tidyverse)
library(readxl)
library(data.table)



################ variables #########################
e350_wd <- "/Volumes/cchpc/raw2/e350/"
michaels_wd <- "/Volumes/cchpc/raw2/michaels/"
sites_wd <- "/Volumes/cchpc/raw2/sites/"
#####################################################


################ functions ##########################
#####################################################


################ e350 ###############################
# 
# read data
# setwd("~")
# setwd(e350_wd)
# 
# # df <- read_excel("4228VB_2022.12.19_Week51_1-second.xlsx")
# 
# e350_files <- list.files(pattern="second")
# e350_site_IDs <- unique(substr(e350_files, 1, 6))
# 
# # for (i in e350_files){
# #   assign(paste(i), read_excel(i))
# # }
# 
# for (i in e350_site_IDs){
#   assign(paste("site", i, sep = "_"), list.files(pattern = i, full.names = T) %>%
#            map_df(~read_excel(., col_types = "skip")))
# }

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
  
  # KK: I can sum aux power in the cleaning phase ##
  # df <- df %>%
  #   rowwise() %>% 
  #   mutate(Aux_Power = sum(c_across(colnames(df)[colnames(df) %in% c("Aux1_Power", "Aux2_Power", "Aux3_Power", "Aux4_Power")]), na.rm = T)) # sum for aux power
  
  to_keep <- c("index", "HP_Power", "Fan_Power", "AHU_Power",
               "Aux1_Power", "Aux2_Power", "Aux3_Power", "Aux4_Power",
               "OA_TempF", "OA_RH", "SA_temp_blower_cabinet_F", "SA_RH_blower_cabinet",
               "SA1_TempF", "SA1_RH", "SA2_TempF", "SA2_RH", "SA3_TempF", "SA3_RH",
               "SA4_TempF", "SA4_RH", "RA_TempF", "RA_RH", "AHU_TempF", "AHU_RH",
               "Room1_TempF", "Room1_RH","Room2_TempF", "Room2_RH", "Room3_TempF",
               "Room3_RH", "Room4_TempF", "Room4_RH", "RV_Volts") # list of cols used in agg
  df <- subset(df, select = names(df) %in% to_keep) # keep only cols  used in agg
  
  setwd("~") # reset wd
  setwd(sites_wd) # set wd to sites folder for saving single site file
  write_csv(df, paste("site_", i, ".csv", sep = "")) # write csv of single file for site
  
  rm(df) # remove current df from wd to clear up space
  print(paste("site", i, "complete", sep = " ")) # message in console to say that a site has been completed
}
#####################################################



