#####################################################
# Author: Kevin Keene
# Company: Pacific Northwest National Laboratory
# Created on: 2023-08-02
# Description: This script pulls the data files created
# by the "cleaning" file and adds calculated columns 
# and prints out daily diagnostic graphs.
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




### Data Load and Cleaning ----

# Set working library to read data
if(Sys.info()[7] == "rose775"){   
  source("/Users/rose775/Library/CloudStorage/OneDrive-PNNL/Desktop/Projects/ccHP/hpanalysis/functions_for_use.R") 
  wd <- "/Volumes/cchpc/"
  # Need output wd location ("R:/" for Kevin)
  # wd_out <- 
} else if(Sys.info()[7] == "keen930"){
  source("C:/Users/keen930/OneDrive - PNNL/Documents/CCHP/hpanalysis/functions_for_use.R") 
  wd <- "Q:/spring_performance_data/"
  wd_out <- "Q:/spring_performance_data/"
} else if(Sys.info()[7] == "zhan682"){
  source("C:/Users/zhan682/OneDrive - PNNL/Documents/GitHub/hpanalysis/functions_for_use.R")
  wd <- "\\\\rc-smb1\\qprojects\\cchpc\\spring_performance_data"
  wd_out <- "\\\\rc-smb1\\qprojects\\cchpc\\spring_performance_data"
  }


## Load data
site_IDs <- unique(substr(list.files(path = paste0(wd, "clean/1_min/")), 1, 6))
# metadata <- read_csv(file = paste0(wd, "site-metadata.csv"))
metadata <- read_csv(file = "Q:/site-metadata.csv")


for (i in site_IDs){
  print(paste("beginning site", i, sep = " "))
  
  ## Load data from 'clean' folder ----
    # Pulling in 1-min for now, but should change to 1-sec with RC
  df <- list.files(path = paste0(wd, "clean/1_min/"), pattern = i, full.names = T) %>%
    map_df(~read_csv(.))
  
## For spring performance data, take data only after April 01
  df <- df %>% filter(datetime_UTC > strptime("2023-04-01", format = "%F", tz=metadata$Timezone[metadata$Site_ID==i]))
  
## Calculate columns 

# Add column that determines the number of aux legs that are active at every timestamp
  # Use the auxheat legs for Michaels; E350 will need to infer based on power level  
if(exists("auxheat1_pwr_kW", where=df)){
  df <- df %>% mutate(
  number_aux_legs = ifelse(sum(auxheat1_pwr_kW > 0.1, auxheat2_pwr_kW > 0.1, auxheat3_pwr_kW > 0.1, auxheat4_pwr_kW > 0.1)==4, 4,
                           ifelse(sum(auxheat1_pwr_kW > 0.1, auxheat2_pwr_kW > 0.1, auxheat3_pwr_kW > 0.1, auxheat4_pwr_kW > 0.1)==3, 3,
                                  ifelse(sum(auxheat1_pwr_kW > 0.1, auxheat2_pwr_kW > 0.1, auxheat3_pwr_kW > 0.1, auxheat4_pwr_kW > 0.1)==2, 2,
                                         sum(auxheat1_pwr_kW > 0.1, auxheat2_pwr_kW > 0.1, auxheat3_pwr_kW > 0.1, auxheat4_pwr_kW > 0.1)==1, 1,
                                         0))))
  } else {
    df <- df %>% mutate(
      number_aux_legs = ifelse(auxheat_pwr_kW > 16, 4,
                               ifelse(auxheat_pwr_kW > 11, 3,
                                      ifelse(auxheat_pwr_kW > 6, 2,
                                             ifelse(auxheat_pwr_kW > 0.1, 1, 0)))))
  }



  
## Operating mode ##
  # Objective: Create a column with operating mode: 
    # Heating-HP Only
    # Heating-Aux/HP
    # Heating-Aux Only
    # System Off
    # Defrost
    # Cooling

df <- df %>% mutate(operating_mode =
  # 1. If any of the key COP parameters are NA, Operating Mode is NA
    ifelse(is.na(fan_pwr_kW) | is.na(ODU_pwr_kW) | is.na(auxheat_pwr_kW) | is.na(SA_temp_F) | is.na(SA_RH) | is.na(RA_temp_F), NA,
           
  # 2. Identify defrost mode                      
      # For Michaels sites, 0V on RV indicates heating mode and 27V indicates cooling/defrost.
      # NOTE: It looks like site 7083LM, greater than 20V means heating mode, and between 0.4 and 0.6V is system off, and 0.8 to 1.1V might be defrost
    ifelse(site_ID %in% c("2563EH", "2896BR", "6950NE", "8220XE", "9944LD", "6112OH", "8726VB") & !is.na(reversing_valve_signal_V) & reversing_valve_signal_V > 25 & ODU_pwr_kW > 0.2, "Defrost",
    ifelse(site_ID == "7083LM" & reversing_valve_signal_V > 0.8 & !is.na(reversing_valve_signal_V) & reversing_valve_signal_V < 1.1 & ODU_pwr_kW > 0.2, "Defrost", 
      
      # For site 4228VB, Trane provided RV data but there are some gaps which require secondary indicators
    ifelse(site_ID == "4228VB" & is.na(reversing_valve_signal_V) &
             auxheat_pwr_kW > 4.0 & ODU_pwr_kW > 0.2 & ODU_pwr_kW < 1.25 & fan_pwr_kW > 0.35, "Defrost",
        ifelse(site_ID == "4228VB" & !is.na(reversing_valve_signal_V) & reversing_valve_signal_V==1 & ODU_pwr_kW > 0.2, "Defrost",
      
      # For site 5539NO, RV between 0.6 V and 3.0 V indicates defrost mode
    ifelse(site_ID == "5539NO" & !is.na(reversing_valve_signal_V) & reversing_valve_signal_V > 0.6 & reversing_valve_signal_V < 3 & ODU_pwr_kW > 0.2, "Defrost",
  
      # For site 2458CE, secondary indicators are used
    ifelse(site_ID=="2458CE" & ODU_pwr_kW > 0.1 & ODU_pwr_kW < 1.75 & fan_pwr_kW > 0.4 & auxheat_pwr_kW > 4, "Defrost",
      # For site 5291QJ, secondary indicators are used
    ifelse(site_ID=="5291QJ" & ODU_pwr_kW > 0.5 & fan_pwr_kW > 0.02 & fan_pwr_kW < 0.06 , "Defrost",
  
  # 3. If not defrost mode, use power data to determine the mode         
    ifelse(ODU_pwr_kW > 0.2 & auxheat_pwr_kW < 0.1, "Heating-HP Only",
         ifelse(ODU_pwr_kW < 0.2 & auxheat_pwr_kW > 0.1, "Heating-Aux Only",
                ifelse(ODU_pwr_kW > 0.2 & auxheat_pwr_kW > 0.1, "Heating-Aux/HP",
                       "System Off")))))))))))) %>%
 
  # 4. Correct for cooling mode to differentiate from defrost mode
    # Indicators: Operating Mode is Defrost, Aux Power is less than 0.1 kW and OA Temp is greater than 30F.
    mutate(operating_mode=
      
      # Site 4228VB Trane RV data does not identify cooling as defrost on, so SA temp is the only real indicator
      ifelse(site_ID == "4228VB" & ODU_pwr_kW > 0.2 & auxheat_pwr_kW < 0.1 & SA_temp_F < 60 & OA_temp_F > 35, "Cooling",
      
      # For all other sites, cooling mode should be previously identified as defrost mode
      ifelse(operating_mode=="Defrost" & auxheat_pwr_kW < 0.1 & OA_temp_F > 30, "Cooling", 
             operating_mode)))


# Heat pump status
  # Used to calculate the length of heat pump cycles and to determine if the 
  # HP ODU is on or off at every timestamp.
df <- df %>% mutate(HP_status = ifelse(is.na(fan_pwr_kW), NA, ifelse(ODU_pwr_kW > 0.2, "On", "Off")))



## Flag time periods with operational issues
    # A "0" denotes no operational issues and a "1" indicates issues
    # See CCHP Analysis Notebook for more information on these events.
df <- df %>% mutate(
  operational_issue = ifelse(
    (site_ID == "5539NO" & datetime_UTC >= strptime("2023-04-03", "%F", tz="US/Central")) |
      (site_ID == "9944LD" & datetime_UTC > strptime("2023-01-26", "%F", tz="US/Mountain") & datetime_UTC < strptime("2023-01-27 12:00:00", "%F %T", tz="US/Mountain")) |
      (site_ID == "9944LD" & datetime_UTC > strptime("2023-02-01 12:00:00", "%F %T", tz="US/Mountain")) |
      (site_ID == "4228VB" & datetime_UTC > strptime("2022-12-30", "%F", tz="US/Mountain") & datetime_UTC < strptime("2023-01-02 18:00:00", "%F %T", tz="US/Mountain")) |
      (site_ID == "4228VB" & datetime_UTC > strptime("2023-01-05", "%F", tz="US/Mountain") & datetime_UTC < strptime("2023-01-06 21:00:00", "%F %T", tz="US/Mountain")) |
      (site_ID == "4228VB" & datetime_UTC > strptime("2023-01-07 15:00:00", "%F %T", tz="US/Mountain") & datetime_UTC < strptime("2023-01-08 12:00:00", "%F %T", tz="US/Mountain")) |
      (site_ID == "4228VB" & datetime_UTC > strptime("2023-03-02 6:00:00", "%F %T", tz="US/Mountain") & datetime_UTC < strptime("2023-03-06 12:00:00", "%F %T", tz="US/Mountain")),
    1, 0))







## Make a column for HP and defrost cycle times. The last timestamp of each cycle
  # will store the duration of that cycle in minutes.
df$HP_cycle_runtimes <- run_cycle_calc(df$datetime_UTC, df$HP_status, "On")
df$defrost_cycle_runtimes <- run_cycle_calc(df$datetime_UTC, df$operating_mode, "Defrost")


## Corrections needed for defrost modes with secondary indicators
  # See notes for each site in the CCHP Analysis Notebook for more details.
df <- df %>% mutate(
  defrost_cycle_runtimes = ifelse((site_ID=="4228VB" | site_ID=="5291QJ" | site_ID=="2458CE") & 
                                    defrost_cycle_runtimes < 1, NA, 
                                  ifelse(site_ID=="6221OH" & defrost_cycle_runtimes < 0.5, NA,
                                  ifelse(site_ID=="4228VB" & defrost_cycle_runtimes > 60, NA,
                                         defrost_cycle_runtimes))))


df <- df %>%
  
  # Air supply (based on fan power curve)
  mutate(supply_flow_rate_CFM = 
  # Correlate fan power (kW) to volumetric flow rate (CFM) based on initial testing
  # Function will output the air supply in CFM for an input power in Watts
  # We will need to add a line for each site in this function.
  # Note: Michaels gave curves in W and E350 in kW, so Michaels' sites need * 1,000
    ifelse(site_ID=="6950NE", 116.48 * (fan_pwr_kW*1000) ^ 0.3866,
    ifelse(site_ID=="8220XE", 152.27 * (fan_pwr_kW*1000) ^ 0.3812,
    ifelse(site_ID=="4228VB", 1647.7 * fan_pwr_kW^0.394,
    ifelse(site_ID=="9944LD", 115.09 * (fan_pwr_kW*1000)^0.3926,
    ifelse(site_ID=="2563EH", 169.1 * (fan_pwr_kW*1000)^0.2978,
    ifelse(site_ID=="2896BR", 108.64 * (fan_pwr_kW*1000)^0.4405,
    ifelse(site_ID=="6112OH", 98.457 * (fan_pwr_kW*1000)^0.4346,
    ifelse(site_ID=="7083LM", 120.66 * (fan_pwr_kW*1000)^0.3657,
    ifelse(site_ID=="5539NO", 1415.9 * fan_pwr_kW^0.4138,
    ifelse(site_ID=="8726VB", 109.14 * (fan_pwr_kW*1000)^0.424,
                  supply_flow_rate_CFM)))))))))),
 

## Heating and cooling related calculations:

  # Partial Water Pressure (Pw) = RH * Pws; 
  # Pws = f(T); T in Fahrenheit, based on curve fit
  # 6th order poly fit: Pws = -0.000000001546*T^6 + 0.000000516256*T^5 - 0.000020306966*T^4 + 0.002266035021*T^3 + 0.190010315225*T^2 + 6.715900713408*T + 125.349159019000
  partial_water_pressure_supply = SA_RH / 100 * 
    (-0.000000001546*SA_temp_F^6 + 
       0.000000516256*SA_temp_F^5 - 
       0.000020306966*SA_temp_F^4 + 
       0.002266035021*SA_temp_F^3 + 
       0.190010315225*SA_temp_F^2 + 
       6.715900713408*SA_temp_F + 
       125.349159019000),

  # Air pressure (merge from metadata)
  # Pressure_pa = df %>% 
  #   merge(metadata %>% select(site_ID, Pressure), by="site_ID", all.x=T, all.y=F) %>%
  #   pull(Pressure),
 
  # Humidity ratio of supply air (0.62198 * Pw / (P - Pw))
  # P = 97,717 Pascals at 1,000 ft elevation (we could use a more accurate look up for each location)
  supply_humidity_ratio = 
    0.62198 * partial_water_pressure_supply / (97717 - partial_water_pressure_supply),
  
  # Heat Output
    # Q-heating = (dry air density) * (blower airflow rate) * (specific heat) * (delta Temp)
  heat_output_btu_h = ifelse(is.na(operating_mode) | operating_mode == "Cooling", NA,
      0.0715 *                                                # Density of air at 35C (lb/ft3)
      supply_flow_rate_CFM * 60 *                             # CFM * min/hour
      (0.24 + 0.444 *  supply_humidity_ratio) *               # Specific heat capacity (Btu/F-lb)
      (SA_temp_F - RA_temp_F)),                                 # Temperature delta
  
  # Adjusted Heat Output
    # Many of the sites are significantly affected by SAT sensor placement, which can be
    # observed from the heat output differing from aux power in aux only mode. The aux heat
    # is causing the air to not fully mix at the sensors. This factor will adjust the heat
    # output from the aux contribution only.
    # heat_output_btu_h_adjusted = 
    # # Heat output from HP
    # heat_output_btu_h - auxheat_pwr_kW * 3412 +
    # # Adjusted aux power
    # auxheat_pwr_kW * 3412 * ifelse(site_ID=="5539NO", 1.5,
    #                           ifelse(site_ID=="8220XE", 0.8,
    #                                  ifelse(site_ID=="6950NE", 0.8,
    #                                         ifelse(site_ID=="9944LD", 0.5,
    #                                                ifelse(site_ID=="4228VB", 0.95,
    #                                                       ifelse(site_ID=="2896BR", 1,
    #                                                              ifelse(site_ID=="2563EH", 1.5,
    #                                                                     ifelse(site_ID=="6112OH", 1.2,
    #                                                                            1)))))))),

  # Cooling output
    # Q-cooling = (dry air density) * (blower airflow rate) * (specific heat) * (delta Temp) / (1 + Humidity Ratio)
  cooling_output_btu_h = ifelse(is.na(operating_mode) | operating_mode %in% c("Heating-HP Only",
                                                                              "Heating-Aux/HP",
                                                                              "Heating-Aux Only",
                                                                              "Defrost"), NA,
      0.0765 *                                                          # Density of air at 15C (lb/ft3)
      supply_flow_rate_CFM * 60 *                                       # CFM * min/hour
      (0.24 + 0.444 *  supply_humidity_ratio) *                         # Specific heat capacity (Btu/F-lb)
      (SA_temp_F - RA_temp_F) /
      (1 + supply_humidity_ratio))
)

print(paste("completed calculations for site", i, "now creating diagnostic graphs",sep = " "))


### Diagnostic Graphs ----


# Vector storing every date in the dataframe
dates <- as.character(unique(df$date_local))


# Convert NAs to numeric (instead of logic) so they do not give errors
df <- df %>% mutate_at(c("room4_temp_F", "SA_temp_duct3_F", "SA_temp_duct4_F", "defrost_cycle_runtimes"), as.numeric)

# Room temperature time series
  # Loop through dates (skip the first one to not have a partial day)
for(d in dates[-1]){
  # d1 = d + one day
  d1 = substr(as.character(strptime(d, "%F", tz=metadata$Timezone[metadata$Site_ID==i]) + 60*60*24), 1, 10)
  ggsave(paste0(i, '_Daily-Room-Temp_',d,'.png'),
         plot = daily_room_temperature_comparison(i, d, d1),
         path = paste0(wd_out,'daily_ops/',i, '/daily_room_temps/'),
         width=12, height=4, units='in')
}

print("completed room temperature graphs")

# Supply temperature time series
  # Loop through dates (skip the first one to not have a partial day)
for(d in dates[-1]){
  # d1 = d + one day
  d1 = substr(as.character(strptime(d, "%F", tz=metadata$Timezone[metadata$Site_ID==i]) + 60*60*24), 1, 10)
  ggsave(paste0(i, '_Daily-Supply-Temp_',d,'.png'),
         plot = daily_supply_temperature_comparison(i, d, d1),
         path = paste0(wd_out,'daily_ops/',i, '/daily_SATs/'),
         width=12, height=4, units='in')
}

print("completed supply temperature graphs")


# Defrost cycle time series
  # Loop through dates (skip the first one to not have a partial day)
# for(d in dates[-1]){
#   # d1 = d + one day
#   d1 = substr(as.character(strptime(d, "%F", tz=metadata$Timezone[metadata$Site_ID==i]) + 60*60*24), 1, 10)
#   ggsave(paste0(i, '_Daily-Defrost_',d,'.png'),
#          plot = daily_defrost_plot(i, d, d1),
#          path = paste0(wd_out,'daily_ops/',i, '/daily_defrost/'),
#          width=12, height=4, units='in')
# }
# 
# print("completed defrost cycle graphs")


# Daily operation power and temperature time series
  # Loop through dates (skip the first one to not have a partial day)
for(d in dates[-1]){
  # d1 = d + one day
  d1 = substr(as.character(strptime(d, "%F", tz=metadata$Timezone[metadata$Site_ID==i]) + 60*60*24), 1, 10)
  ggsave(paste0(i, '_Daily-Operation_',d,'.png'),
         plot = daily_operation_plot(i, d, d1),
         path = paste0(wd_out,'daily_ops/',i, '/daily_operation/'),
         width=12, height=4, units='in')
}

print("completed daily operation graphs")


# COP, heat and cooling output, and power time series
  # Loop through dates (skip the first one to not have a partial day)
for(d in dates[-1]){
  # d1 = d + one day
  d1 = substr(as.character(strptime(d, "%F", tz=metadata$Timezone[metadata$Site_ID==i]) + 60*60*24), 1, 10)
  ggsave(paste0(i, '_Daily-COP_',d,'.png'),
         plot = daily_COP_plot(i, d, d1),
         path = paste0(wd_out,'daily_ops/',i, '/daily_COPs/'),
         width=12, height=4, units='in')
}

print("completed COP graphs")


# Operating mode daily summary
  # Winter 2023 Performance
# ggsave('operating_mode_winter_2023_performance.png',
#        plot = operating_mode_season(i, "12/15/2022 00:00", "3/30/2023 23:59"),
#        path = paste0(wd_out,'daily ops/',i),
#        width=12, height=4, units='in')
ggsave(paste0(i, '_operating_mode_spring.png'),
       plot = operating_mode_season(i, "4/01/2023 00:00", "6/14/2023 23:59"),
       path = paste0(wd_out,'daily_ops/', i),
       width=12, height=4, units='in')
# ggsave('operating_mode_summer_2023_performance.png',
#        plot = operating_mode_season(i, "6/15/2023 00:00", "9/30/2023 23:59"),
#        path = paste0(wd_out,'daily ops/',i),
#        width=12, height=4, units='in')

print("completed opearting mode daily summary graph")


## Remove data before data stabilizes for each site.
df <- df %>% 
  # Site 2563EH:
  filter(site_ID != "2563EH" | datetime_UTC >= strptime("2023-02-01", "%F", tz="US/Eastern")) %>%
  
  # Site 2896BR:
  filter(site_ID != "2896BR" | datetime_UTC >= strptime("2023-02-03", "%F", tz="US/Eastern")) %>%
  
  # Site 4228VB 
  filter(site_ID != "4228VB" | datetime_UTC >= strptime("2022-12-22", "%F", tz="US/Mountain")) %>%
  
  # Site 5539NO:
  # All data is present from start.
  
  # Site 6112OH:
  filter(site_ID != "6112OH" | datetime_UTC >= strptime("2023-02-14", "%F", tz="US/Eastern")) %>%
  
  # Site 6950NE:
  filter(site_ID != "6950NE" | datetime_UTC >= strptime("2022-12-10", "%F", tz="US/Central")) %>%
  
  # Site 8220XE:
  filter(site_ID != "8220XE" | datetime_UTC >= strptime("2022-12-13", "%F", tz="US/Central")) %>%
  
  # Site 8726VB:
  filter(site_ID != "8726VB" | datetime_UTC >= strptime("2023-02-12", "%F", tz="US/Eastern")) %>%

  # Site 9944LD:
  filter(site_ID != "9944LD" | datetime_UTC >= strptime("2023-01-07", "%F", tz="US/Mountain"))


print(paste("completed diagnostic graphs for site", i, "now writing file",sep = " "))

## Print csv with calculated columns ##
write_csv(df, paste0(wd_out, "calculated_data/", i, ".csv"))
rm(df)

print(paste("completed site", i, sep = " "))

}

