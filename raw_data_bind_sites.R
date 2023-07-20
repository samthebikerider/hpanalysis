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

################variables#######################
e350_wd <- "/Volumes/cchpc/raw2/e350/"
michaels_wd <- "/Volumes/cchpc/raw2/michaels/"
#####################################################


################functions############################
#####################################################


################e350#################################
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


################michaels#############################
# read data
setwd("~")
setwd(michaels_wd)

michaels_files <- list.files()
michaels_site_IDs <- unique(substr(michaels_files, 14, 19))

for (i in michaels_site_IDs){
  df <- list.files(pattern = i, full.names = T) %>%
           map_df(~read_csv(.))
  write_csv(df, paste("~/Volumes/cchpc/raw2/sites/site_", i, ".csv", sep = ""))
  rm(df)
  print(paste("site", i, "complete", sep = " "))
}
#####################################################





