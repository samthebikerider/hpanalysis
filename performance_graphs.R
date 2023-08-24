#####################################################
# Author: Yiting Zhang
# Company: Pacific Northwest National Laboratory
# Created on: 2023-08-17
# Description: This script pulls the data files created
# by the "calculations" file and and prints out graphs.
#####################################################
# Todo:
# 1. Move functions starting from 1b. Table of 1a with temperature and RH in 'Data Processing.R'
# to 'functions_for_use.R', and call them here
# 2. Update the file path and variable names in orginal functions
#####################################################


# Clear Global Environment
rm(list=ls())
  # the one in 'functions_for_use.R' has been commented for test


# Packages
library(tidyverse)
library(lubridate)
library(stringr)
library(data.table)


### Data Load and Cleaning ----

# Set working library to read data
if(Sys.info()[7] == "rose775"){   
  source("/Users/rose775/Library/CloudStorage/OneDrive-PNNL/Desktop/Projects/ccHP/hpanalysis/functions_for_use.R") 
  wd <- "/Volumes/cchpc/"
  
} else if(Sys.info()[7] == "keen930"){
  source("C:/Users/keen930/OneDrive - PNNL/Documents/CCHP/hpanalysis/functions_for_use.R") 
  wd <- "Q:/spring_performance_data/"
  wd_out <- "Q:/spring_performance_data/"
  # may need change
  
} else if(Sys.info()[7] == "zhan682"){
  source("C:/Users/zhan682/OneDrive - PNNL/Documents/GitHub/hpanalysis/functions_for_use.R")
  wd <- "\\\\rc-smb1\\qprojects\\cchpc\\spring_performance_data"
  wd_out <- "\\\\rc-smb1\\qprojects\\cchpc\\spring_performance_data"
  # The backslash \ character is being interpreted as an escape character in the string,
  # and cause error. Use double backslashes \\ to represent a single backslash.
  }

## Load data
  # Don't see "7083LM" in "calculated_data"
  ## KK: I think for this script, it would be better to have site_IDs be an input
    # so that we don't necessarily need to run for all sites if we just want to 
    # do a subset of them.
    # We currently don't have spring data for 7083LM because they still need 
    # to sign some agreement for this site. 2896BR is also dropping out of the study
    # and has very little spring data, so we will not want to include that site 
    # fot the spring graphs.

  ## YZ: Here we can have site_IDs be a mannual input. Uncomment each site to use.
    # But I changed ' ' to 'stop', don't know why if not do " break at 'stop' ", it seems never end
    # You can still uncomment the line below for running all sites.
# site_IDs <- unique(substr(list.files(path = paste0(wd, "/calculated_data")), 1, 6))

site_IDs <- c(
  "2563EH",
  # "2896BR", # very little data
  "6112OH",
  # "6950NE",
  # "7083LM", # no data
  # "8220XE",
  # "8726VB",
  # "9944LD",
  # "4228VB",
  # "5539NO",
  # "5291QJ",
  # "2458CE",
  "stop") 

metadata <- read_csv(file = '\\\\rc-smb1\\qprojects\\cchpc\\site-metadata.csv')
# metadata <- read_csv(file = "Q:/site-metadata.csv")

## Set minimum temperature for temperature bins for when sample size is too small
  # Don't see "7083LM" in "calculated_data" and here, update later
  # information from winter scripts
# lookup_table <- data.frame(
#   site_ID = c("4228VB", "9944LD", "8220XE", "2563EH", "5291QJ", 
#               "2896BR", "6112OH", "6950NE", "5539NO", "2458CE", "8726VB"),
#   temp_min = c(5, -20, -15, -10, -20, -10, 10, -15, 10, 15, 5),
#   temp_max = c(55, 45, 55, 55, 45, 55, 55, 55, 55, 45, 55)
# )

  # update for spring scripts
lookup_table <- data.frame(
  site_ID = c("4228VB", "9944LD", "8220XE", "2563EH", "5291QJ", 
              "2896BR", "6112OH", "6950NE", "5539NO", "2458CE", "8726VB"),
  start_date = c("2023-04-01", "2023-04-01", "2023-04-01", "2023-04-01", "2023-04-01", 
                 "2023-04-01", "2023-04-01", "2023-04-01", "2023-04-01", "2023-04-01", "2023-04-01"),
  temp_min = c(25, 35, 15, 20, 10, 20, 15, 15, 25, 10, 15), # choose lowest temp in Feb
  temp_max = c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100) # set to 100
)

# Create an empty data frame to store data for all site IDs
df <- data.frame()
for (i in site_IDs){
  if (i == "stop"){
    break
  }
  print(paste("beginning site", i, sep = " "))
  
  ## Load data from 'calculated_data' folder ----
  temp_df <- list.files(path = paste0(wd, "/calculated_data"), pattern = i, full.names = T) %>%
    map_df(~read_csv(.))
  site_info <- lookup_table[lookup_table$site_ID == i, ]
  
  ## For spring performance data, take data only after April 01
  ## KK: can we make this an input before the loop so it will be easy to
    # choose which sites and what timeframe at the beginning?
  ## YZ: Revised. I added a start_date list in lookup_table, should be changed manually later.
  temp_df <- temp_df %>% filter(datetime_UTC > strptime(site_info$start_date, format = "%F", tz=metadata$Timezone[metadata$Site_ID==i]))
  
  if (nrow(site_info) == 1) {
    temp_min <- site_info$temp_min
    temp_max <- site_info$temp_max
    
    temp_df <- temp_df %>%
      mutate(temp_min = ifelse(is.na(temp_min), temp_min, temp_min),
             temp_max = ifelse(is.na(temp_max), temp_max, temp_max))
  } else {
    warning(paste("Site ID", i, "not found in lookup table."))
  }
  # Append the processed data to the combined data frame
  df <- bind_rows(df, temp_df)
}

## Set minimum temperature for temperature bins for when sample size is too small

### Diagnostic Graphs (continued) ----

# Vector storing every date in the dataframe
dates <- as.character(unique(df$date_local))

# Convert NAs to numeric (instead of logic) so they do not give errors
df <- df %>% mutate_at(c("auxheat1_pwr_kW", "room4_temp_F", "room4_RH", 
                         "SA_temp_duct3_F", "SA_temp_duct4_F", 
                         "number_aux_legs", "defrost_cycle_runtimes"), as.numeric)


# 1a. Operating mode daily summary
# Winter 2023 Performance
# ggsave('operating_mode_winter_2023_performance.png',
#        plot = operating_mode_season(i, "12/15/2022 00:00", "3/30/2023 23:59"),
#        path = paste0(wd_out,'daily ops/',i),
#        width=12, height=4, units='in')

  ## YZ: I reformatted this
for (i in site_IDs) {
  ggsave(paste0(i, '_operating_mode_spring.png'),
        plot = operating_mode_season(i, "4/01/2023 00:00", "6/14/2023 23:59"),
        path = paste0(wd_out,'daily_ops/', i),
        width=12, height=4, units='in')
}
print("completed operating mode daily summary graphs")

# ggsave('operating_mode_summer_2023_performance.png',
#        plot = operating_mode_season(i, "6/15/2023 00:00", "9/30/2023 23:59"),
#        path = paste0(wd_out,'daily ops/',i),
#        width=12, height=4, units='in')


# 1b.
  # Write table for operating mode daily summary with temperature and RH
  # Print summary site comparison
for (i in site_IDs) {
  generate_operating_mode_summary_table(i, wd_out)
  # print_operating_summary_comparison(i, wd_out) (To Do later)
}



# 1c. Operating mode summary by OAT bin
  # Look at a time series graph of each day to see fraction of time in each operating mode
for (i in site_IDs) {
  ggsave(paste0(i, '_operating_mode_percent_time_by_OAT.png'),
        plot = operating_mode_OAT(i),
        path = paste0(wd_out,'/daily_ops/',i),
        width=12, height=4, units='in')
}
print("completed supply temperature graphs")


# Note: For spring performance data, Operating mode summary is made for 0-32F, below 100F

# 1d. Operating mode summary for all sites 0-32F (To Confirm)
    # KK: We don't need this one for now, I will comment out.
# ggsave('Operating_Mode_0-32F_Site_Comparison.png',
#        plot = operation_32F_all_sites(),
#        path = paste0(wd_out,'/Graphs/Site Comparison/'),
#        width=12, height=4, units='in')
# print("completed operating mode summary graphs for all sites 0-32F")


# 1e. Operating mode summary for all sites below 100F (To Confirm)
  ## KK: Below 100F is too big of a range to be meaningful...
    # We can revisit later if there is a range we want to look into, but
    # let's not worry about this one for now.
# ggsave('Operating_Mode_100F_below_Site_Comparison.png',
#        plot = operation_100F_all_sites(),
#        path = paste0(wd_out,'/Graphs/Site Comparison/'),
#        width=12, height=4, units='in')
# print("completed operating mode summary graphs for all sites below 100F")


### Outdoor Air Graphs ----

# 5a. Aux staging by OAT bin without defrost

for (i in site_IDs) {
  ggsave(paste0(i, '_aux_use_vs_OAT_Bin.png'),
         plot = aux_staging_no_defrost(i),
         path = paste0(wd_out,'/daily_ops/',i),
         width=12, height=4, units='in')
}
print("completed supply temperature graphs")


# 6a. Heating and Cooling capacity (i.e., heating load, cooling load) (Btu/h) by OAT bin
  ## KK: it looks like there are some positive cooling output and negative heat output,
    # which shouldn't be possible, but I made some updates to the calculations.R 
    # script which hopefully will fix that.
  ## KK: Also, can we change the colors of the categories in this graph so that
    # heating is a warmer/red color and cooling is a cooler/blue color, and then
    # defrost is maybe green or something else?
  
  ## YZ: I have changed the colors. Since this file read data in calculated_data, 
    # should we use calculations.R to update them?

for (i in site_IDs) {
  ggsave(paste0(i, '_heat_cool_capacity_vs_OAT_Bin.png'),
       plot = heat_cool_capacity_OAT_Bin(i),
       path = paste0(wd_out,'/daily_ops/',i),
       width=12, height=4, units='in')
}
print("completed supply temperature graphs")


# 6b. Table to show the "maximum" heating and cooling capacity in each OAT bin
    ## KK: Note that cooling output will be negative values, so we will need the 5th percentile
      # instead of the 95th percentile.
    ## YZ: Revised.
results_list <- list()
for (i in site_IDs) {
  result <- print_heat_cool_capacity(i)
  results_list[[i]] <- result
}
combined_results <- bind_rows(results_list, .id = "site_ID")
write.csv(combined_results, paste0(wd_out, '/graphs/site_comparison/Max Heat and Cool Capacity.csv'), row.names = FALSE)

print("completed table of the maximum heating and cooling capacity")


# 7a. COP vs outdoor air temperature for each site
  ## KK: note we already have a column for total power, now called HP_system_pwr_kW
    # I know it's confusing because HP doesn't include aux normally, but that's what
    # they decided to call the column--it includes HP, aux, and fan power.
  ## YZ: Thank you! Revised.

  ## KK: also note that cooling output is negative, which is what we want, but
    # COP should always be positive, so for this graph I think we should subtract
    # the cooling output from the heating output.
  ## YZ: I Revised as below in functions_for_use.R:
    # COP_Total = (sum(heat_output_btu_h, na.rm = TRUE) - sum(cooling_output_btu_h, na.rm = TRUE)) / sum(HP_system_pwr_kW, na.rm = TRUE) / 3412)

ggsave('COP_vs_OAT_Bin.png',
       plot = heat_cool_COP_all_sites(unique(metadata$Manufacturer)),
       path = paste0(wd,'/graphs/site_comparison/'),
       width=12, height=4, units='in')
# Print graph to folder with one for each manufacturer
for(manu in unique(metadata$Manufacturer)){
  ggsave(paste0('COP_vs_OAT_Bin_', manu, '.png'),
         plot = heat_cool_COP_all_sites(manu),
         path = paste0(wd,'/graphs/site_comparison/'),
         width=12, height=4, units='in')
}

print("completed COP vs outdoor air temperature for each site")


# 8. COP vs outdoor air temperature
  # KK: this graph needs cooling added. I think the way we will want to do it
  # is for cooling to be included under "Heat Pump", so that will be heat pump heating
  # and heat pump cooling, then there will be "Defrost", "Heat Pump + Aux" and "Aux Only"
  # as the other options. So we should have:
  # percent_HP = sum(operating_mode %in% c("Heating-HP Only", "Cooling"), na.rm=T)/n()
for (i in site_IDs) {
  ggsave(paste0(i, '_COP_vs_OAT.png'),
         plot = COP_vs_OAT(i),
         path = paste0(wd_out,'/daily_ops/',i),
         width=12, height=4, units='in')
}

print("completed COP vs outdoor air temperature graphs")

### Demand Response Graphs ----
# # U.S. Sites
# DemandResponseEvents <- data.frame(
#   Site_ID = c(rep("4228VB",4), rep("6950NE",4), rep("8220XE",4), rep("9944LD",4)),
#   Event_Type = rep(c("GCCW","GCMW","CCMW","CCCW"), 4),
#   Start = c(strptime("2023-02-02 09:00:00", format="%F %T", tz="US/Mountain"),strptime("2023-02-06 13:00:00", format="%F %T", tz="US/Mountain"),strptime("2023-02-07 13:00:00", format="%F %T", tz="US/Mountain"),strptime("2023-02-09 09:00:00", format="%F %T", tz="US/Mountain"),
#             strptime("2023-02-03 09:00:00", format="%F %T", tz="US/Central"),strptime("2023-02-06 13:00:00", format="%F %T", tz="US/Central"),strptime("2023-02-13 09:00:00", format="%F %T", tz="US/Central"),strptime("2023-02-09 09:00:00", format="%F %T", tz="US/Central"),
#             strptime("2023-02-03 09:00:00", format="%F %T", tz="US/Central"),strptime("2023-02-10 09:00:00", format="%F %T", tz="US/Central"),strptime("2023-02-13 09:00:00", format="%F %T", tz="US/Central"),strptime("2023-02-09 09:00:00", format="%F %T", tz="US/Central"),
#             strptime("2023-02-13 09:00:00", format="%F %T", tz="US/Mountain"),strptime("2023-02-02 09:00:00", format="%F %T", tz="US/Mountain"),strptime("2023-02-03 09:00:00", format="%F %T", tz="US/Mountain"),strptime("2023-02-10 13:00:00", format="%F %T", tz="US/Mountain")),
#   End = c(strptime("2023-02-02 13:00:00", format="%F %T", tz="US/Mountain"),strptime("2023-02-06 17:00:00", format="%F %T", tz="US/Mountain"),strptime("2023-02-07 17:00:00", format="%F %T", tz="US/Mountain"),strptime("2023-02-09 13:00:00", format="%F %T", tz="US/Mountain"),
#           strptime("2023-02-03 13:00:00", format="%F %T", tz="US/Central"),strptime("2023-02-06 17:00:00", format="%F %T", tz="US/Central"),strptime("2023-02-13 13:00:00", format="%F %T", tz="US/Central"),strptime("2023-02-09 13:00:00", format="%F %T", tz="US/Central"),
#           strptime("2023-02-03 13:00:00", format="%F %T", tz="US/Central"),strptime("2023-02-10 13:00:00", format="%F %T", tz="US/Central"),strptime("2023-02-13 13:00:00", format="%F %T", tz="US/Central"),strptime("2023-02-09 13:00:00", format="%F %T", tz="US/Central"),
#           strptime("2023-02-13 13:00:00", format="%F %T", tz="US/Mountain"),strptime("2023-02-02 13:00:00", format="%F %T", tz="US/Mountain"),strptime("2023-02-03 13:00:00", format="%F %T", tz="US/Mountain"),strptime("2023-02-10 17:00:00", format="%F %T", tz="US/Mountain"))
# )
# 
# # NRCan sites
# DemandResponseEvents <- data.frame(
#   Site_ID = c(rep("2458CE",3), rep("5291QJ",3)),
#   Event_Type = rep(c("GC","CC","CC"), 2),
#   Start = c(strptime("2023-04-04 10:00:00", format="%F %T", tz="Canada/Eastern"),strptime("2023-04-05 12:00:00", format="%F %T", tz="Canada/Eastern"),strptime("2023-04-07 9:00:00", format="%F %T", tz="Canada/Eastern"),
#             strptime("2023-04-04 10:00:00", format="%F %T", tz="Canada/Eastern"),strptime("2023-04-05 12:00:00", format="%F %T", tz="Canada/Eastern"),strptime("2023-04-07 9:00:00", format="%F %T", tz="Canada/Eastern")),
#   End = c(strptime("2023-04-04 14:00:00", format="%F %T", tz="Canada/Eastern"),strptime("2023-04-05 16:00:00", format="%F %T", tz="Canada/Eastern"),strptime("2023-04-07 15:00:00", format="%F %T", tz="Canada/Eastern"),
#           strptime("2023-04-04 14:00:00", format="%F %T", tz="Canada/Eastern"),strptime("2023-04-05 16:00:00", format="%F %T", tz="Canada/Eastern"),strptime("2023-04-07 15:00:00", format="%F %T", tz="Canada/Eastern"))
# )
# 
# # Demand Reponse Time Series Investigation
# DemandResponseTimeSeries("2458CE", "2023-04-04", "2023-04-05")
# # Loop to print. Requires manually deleting of days that do not have events in folder.
# for (i in site_IDs){
#   # To Do
#   for(d in unique(df$date_local[df$site_ID==i])){
#     # d1 = d + one day
#     d1 = substr(as.character(strptime(d, "%Y-%m-%d", tz=metadata$Timezone[metadata$Site_ID==i]) + 60*60*24), 1, 10) # Date plus one day
#     ggsave(paste0(i, '_Daily_Demand_Response_',d,'.png'),
#            plot = DemandResponseTimeSeries(i, d, d1),
#            path = paste0(wd_out,'/Graphs/Demand Response/',i,'/'),
#            width=12, height=4, units='in')
#   }
# }

