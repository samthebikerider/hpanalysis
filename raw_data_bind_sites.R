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
# michaels_site_IDs <- unique(substr(michaels_files, 14, 19))
michaels_site_IDs <- c("8220XE", "8726VB", "9944LD")

for (i in michaels_site_IDs){
  setwd("~") # reset wd
  setwd(michaels_wd) # set wd to michaels raw data
  df <- list.files(path = michaels_wd, pattern = i) %>%
           map_df(~fread(.)) # read all files for the current site iteration and bind_rows to single df
  
  setwd("~") # reset wd
  setwd(sites_wd) # set wd to sites folder for saving single site file
  write_csv(df, paste("site_", i, ".csv", sep = "")) # write csv of single file for site
  
  rm(df) # remove current df from wd to clear up space
  print(paste("site", i, "complete", sep = " ")) # message in console to say that a site has been completed
}
#####################################################



