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

# set wd, read data
path = "/Users/rose775/OneDrive - PNNL/Desktop/Projects/ccHP/Project Management/Data Analysis/Raw Data/NRCan/5-Second"
setwd(path)

# check
# read csv function that adds home ID column
read_csv_homeID <- function(filename){
  df <- read.csv(filename, check.names = F)
  # names(df) <- iconv(names(df), to = "ASCII", sub = "")
  df[-2] <- lapply(df[-2], as.numeric)
  df$HomeID <- substr(filename, 1, 6)
  df <- df %>% relocate(HomeID)
  df$seconds <- 5
  df
}

##read all files in folder, result is separate dataframe for each .csv
temp = list.files(pattern="\\.csv$")
for (i in 1:length(temp)) assign(temp[i], read_csv_homeID(temp[i]))