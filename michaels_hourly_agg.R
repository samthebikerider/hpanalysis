#####################################################
# Author: Samuel Rosenberg
# Company: Pacific Northwest National Laboratory
# Created on: 2023-02-01
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
library(zoo)
library(xts)

# set wd, read data
path = "/Users/rose775/OneDrive - PNNL/Desktop/Projects/ccHP/Project Management/Data Analysis/Raw Data/"
michaels_path = "/Users/rose775/OneDrive - PNNL/Desktop/Projects/ccHP/Project Management/Data Analysis/Raw Data/Michaels"

setwd(path)

#####################################################
# Author: Samuel Rosenberg
# Company: Pacific Northwest National Laboratory
# Created on: 2023-02-01
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
library(zoo)
library(xts)
library(stringr)

# set wd, read data
path = "/Users/rose775/OneDrive - PNNL/Desktop/Projects/ccHP/Project Management/Data Analysis/Raw Data/Michaels"
setwd(path)

# df <- read_csv("PNNL_ccASHP__6950NE (1).csv")

# read csv function that adds home ID column
read_csv_homeID <- function(filename){
  df <- read.csv(filename)
  df$HomeID <- substr(filename, 14, 19)
  df <- df %>% relocate(HomeID)
  df$index <- as.POSIXlt(df$index, format = "%Y-%m-%d %H:%M:%S")
  df
}

##read all files in folder, result is separate dataframe for each .csv
temp = list.files(pattern="\\.csv$")
for (i in 1:length(temp)) assign(temp[i], read_csv_homeID(temp[i]))

# bind rows for each site
site_6950NE <- bind_rows(`PNNL_ccASHP__6950NE (1).csv`, `PNNL_ccASHP__6950NE (2).csv`,
                         `PNNL_ccASHP__6950NE (3).csv`, `PNNL_ccASHP__6950NE (4).csv`,
                         `PNNL_ccASHP__6950NE (5).csv`)
site_8820XE <- bind_rows(`PNNL_ccASHP__8220XE (1).csv`, `PNNL_ccASHP__8220XE (2).csv`,
                         `PNNL_ccASHP__8220XE (3).csv`, `PNNL_ccASHP__8220XE (4).csv`,
                         `PNNL_ccASHP__8220XE (5).csv`)
site_9944LD <- bind_rows(`PNNL_ccASHP__9944LD (3).csv`, `PNNL_ccASHP__9944LD (4).csv`,
                         `PNNL_ccASHP__9944LD (5).csv`)

