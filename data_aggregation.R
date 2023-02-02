#####################################################
# Author: Samuel Rosenberg
# Company: Pacific Northwest National Laboratory
# Created on: 2023-01-25
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
path = "/Users/rose775/OneDrive - PNNL/Desktop/Projects/ccHP"
setwd(path)

df_sec <- read.csv("4228VB_2023.01.09_Week02_second.csv", check.names = F)
df_min <- read.csv("4228VB_2023.01.09_Week02_minute.csv", check.names = F)

# fix troublesome chars in col names
names(df_sec) <- iconv(names(df_sec), to = "ASCII", sub = "")
names(df_min) <- iconv(names(df_min), to = "ASCII", sub = "")

# datetime stuff
df_min$`Timestamp (UTC)` <- as.POSIXlt(df_min$`Timestamp (UTC)`, format = "%m/%d/%Y %H:%M")
df_sec$`Timestamp (UTC)` <- as.POSIXlt(df_sec$`Timestamp (UTC)`, format = "%m/%d/%Y %H:%M:%S")

# this cfm col in minute data is sticky
# TODO: fix the rest of the NAs
df_min$`Calculated Airflow [cfm]` <- as.numeric(df_min$`Calculated Airflow [cfm]`)

# agg minute to hourly
# Notes: 
#   - reversing valve signal = mean, should change to median?
#   - power channels summed
#   - aux heat relay summed to show count of minutes on/off per hour
#   - aux heat relay mean to show average state over an hour
#   - all temps and rh taken mean

df_min_hourly <- df_min %>%
  group_by(as.Date(`Timestamp (UTC)`), as.POSIXlt(`Timestamp (UTC)`)$hour) %>%
  summarise("reversing_valve_signal_V" = mean(`ReversingValveSignal [V]`),
            "HP_pwr_kw" = mean(`HP_Power [kW]`),
            "fan_pwr_kw" = mean(`Fan_Power [kW]`),
            "AHU_pwr_kw" = mean(`AHU_Power [kW]`),
            "aux_heat_relay_mean" = mean(`Aux_Heat_Relay [On/Off]`),
            "aux_heat_relay_sum" = sum(`Aux_Heat_Relay [On/Off]`),
            "auxheat_pwr_kw" = mean(`AuxHeat_Power [kW]`),
            "OA_temp_F" = mean(`OA_Temp [F]`),
            "OA_RH" = mean(`OA_RH [%]`),
            "SA_blower_temp_F" = mean(`SA_Blower_Temp [F]`),
            "SA_blower_RH" = mean(`SA_Blower_RH [%]`),
            "SA_duct1_temp_F" = mean(`SA_Duct1_Temp [F]`),
            "SA_duct1_RH" = mean(`SA_Duct1_RH [%]`),
            "SA_duct2_temp_F" = mean(`SA_Duct2_Temp [F]`),
            "SA_duct2_RH" = mean(`SA_Duct2_RH [%]`),
            "RA_temp_F" = mean(`RA_Temp [F]`),
            "RA_RH" = mean(`RA_RH [%]`),
            "AHU_ambient_temp_F" = mean(`AHU_Ambient_Temp [F]`),
            "AHU_RH" = mean(`AHU_RH [%]`),
            "room1_temp_F" = mean(`Room1_Temp [F]`),
            "room1_RH" = mean(`Room1_RH [%]`),
            "room2_temp_F" = mean(`Room2_Temp [F]`),
            "room2_RH" = mean(`Room2_RH [%]`),
            "room3_temp_F" = mean(`Room3_Temp [F]`),
            "room3_RH" = mean(`Room3_RH [%]`),
            "room4_temp_F" = mean(`Room4_Temp [F]`),
            "room4_RH" = mean(`Room4_RH [%]`),
            "calculated_airflow_cfm" = mean(`Calculated Airflow [cfm]`))


# agg second to hourly
# Notes: 
#   - reversing valve signal = mean, should change to median?
#   - power channels summed

df_sec_hourly <- df_sec %>%
  group_by(as.Date(`Timestamp (UTC)`), as.POSIXlt(`Timestamp (UTC)`)$hour) %>%
  summarise("reversing_valve_signal_VDC" = mean(`Reversing_Valve_Signal [VDC]`),
            "HP_pwr_sec_kw" = mean(`HP_Power [kW]`),
            "fan_pwr_sec_kw" = mean(`FanPower [kW]`),
            "AHU_pwr_sec_kw" = mean(`AHU_Power [kW]`),
            "aux_heat_pwr_sec_kw" = mean(`Aux_Heat_Power [kW]`))

# merge aggregated second and minute df's into an output
df_hourly <- df_min_hourly %>% inner_join(df_sec_hourly,
                                          by = c("as.Date(`Timestamp (UTC)`)",
                                                 "as.POSIXlt(`Timestamp (UTC)`)$hour")) %>%
              mutate_if(is.numeric, round, digits = 2)
            
# save csv of hourly data
write_csv(df_hourly, "4228VB_2023.01.09_Week02_hour.csv")