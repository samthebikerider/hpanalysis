#####################################################
# Author: Samuel Rosenberg
# Company: Pacific Northwest National Laboratory
# Created on: 2023-04-04
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

# read in hourly data
path = "/Users/rose775/OneDrive - PNNL/Desktop/Projects/ccHP/Project Management/Data Analysis/hourly_site_data/"
setwd(path)
temp = list.files(pattern="\\.csv$")
for (i in 1:length(temp)) assign(temp[i], read_csv(temp[i]))

# make df names a bit easier
site_2563 <- `2563EH_aggregated_hourly.csv`
rm(`2563EH_aggregated_hourly.csv`)
site_2896 <- `2896BR_aggregated_hourly.csv`
rm(`2896BR_aggregated_hourly.csv`)
site_4228 <- `4228VB_aggregated_hourly.csv`
rm(`4228VB_aggregated_hourly.csv`)
site_5539 <- `5539NO_aggregated_hourly.csv`
rm(`5539NO_aggregated_hourly.csv`)
site_6112 <- `6112OH_aggregated_hourly.csv`
rm(`6112OH_aggregated_hourly.csv`)
site_6950 <- `6950NE_aggregated_hourly.csv`
rm(`6950NE_aggregated_hourly.csv`)
site_8726 <- `8726VB_aggregated_hourly.csv`
rm(`8726VB_aggregated_hourly.csv`)
site_8820 <- `8820XE_aggregated_hourly.csv`
rm(`8820XE_aggregated_hourly.csv`)
site_9944 <- `9944LD_aggregated_hourly.csv`
rm(`9944LD_aggregated_hourly.csv`)

df_list <- list(site_2563, site_2896, site_4228, site_5539, site_6112,
             site_6950, site_8726, site_8820, site_9944)

line_plot_ts <- function(data, y_var, y_lab, title, x_lab){
  p1 <- ggplot(data) +
    geom_line(aes_string(x = "local_datetime", y = y_var)) +
    theme_bw() +
    theme(plot.title = element_text(family = "Times New Roman", size = 14, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 12, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 12, hjust = 0.5),
          axis.text.x = element_text(angle = 90, family = "Times New Roman", size = 12, hjust = 0.5)) +
    scale_x_datetime(date_labels = "%H:%M", breaks = "1 hour") + 
    ylab(y_lab) + 
    xlab(x_lab) +
    ggtitle(title)
  p1
}

##################### general EDA #########################

# missing values
missing_vals_site_2563 <- data.frame(sapply(site_2563, function(x) sum(is.na(x))))
missing_vals_site_2896 <- data.frame(sapply(site_2896, function(x) sum(is.na(x))))
missing_vals_site_4228 <- data.frame(sapply(site_4228, function(x) sum(is.na(x))))
missing_vals_site_5539 <- data.frame(sapply(site_5539, function(x) sum(is.na(x))))
missing_vals_site_6112 <- data.frame(sapply(site_6112, function(x) sum(is.na(x))))
missing_vals_site_6950 <- data.frame(sapply(site_6950, function(x) sum(is.na(x))))
missing_vals_site_8726 <- data.frame(sapply(site_8726, function(x) sum(is.na(x))))
missing_vals_site_8820 <- data.frame(sapply(site_8820, function(x) sum(is.na(x))))
missing_vals_site_9944 <- data.frame(sapply(site_9944, function(x) sum(is.na(x))))

