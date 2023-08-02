#####################################################
# Author: Kevin Keene
# Company: Pacific Northwest National Laboratory
# Created on: 2023-08-02
# Description: This script pulls the data files created
# by the "cleaning" file and adds calculated columns 
# and prints out diagnostic graphs.
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
} else if(Sys.info()[7] = "keen930"){
  source("C:/Users/keen930/OneDrive - PNNL/Documents/CCHP/hpanalysis/functions_for_use.R") 
  wd <- "Q:/"
  wd_out <- "R:/"
} else if(Sys.info()[7] = "zhan682"){
  ## Yiting to add for her file paths 
}


## Load data
site_IDs <- unique(substr(list.files(path = paste0(wd, "clean/1_min/")), 6, 11))
metadata <- read_csv(file = paste0(wd, "site-metadata.csv"))

for (i in site_IDs){
  print(paste("beginning site", i, sep = " "))
  
  ## Load data from 'clean' folder ----
    # Pulling in 1-min for now, but should change to 1-sec with RC
  df <- list.files(path = paste0(wd, "clean/1_min/"), pattern = i, full.names = T) %>%
    map_df(~read_csv(.))
  
  
  
## Calculate columns 

df <- df %>% mutate(
  # Add column that determines the number of aux legs
    # Rheem site 5539N0 has larger unit, the first leg measures around 9 kW. Only one unit.
    # Will need to update for Solon, NY once we have data.
  number_aux_legs = ifelse(auxheat_pwr_kW < 0.1, NA,
                           ifelse(site_ID == "5539NO" & auxheat_pwr_kW > 8, 1,
                                  # All other sites
                                  ifelse(auxheat_pwr_kW > 18, 4,
                                         ifelse(auxheat_pwr_kW > 13, 3,
                                                ifelse(auxheat_pwr_kW > 8, 2, 1))))))


## Operating mode ##
  # Objective: Create a column with operating mode: 
    # Heating-HP Only
    # Heating-Aux/HP
    # Heating-Aux Only
    # System Off
    # Defrost
    # Cooling

df <- df %>% mutate(operating_mode =
  # 1. If any of the key parameters are NA, Operating Mode is NA
    ifelse(is.na(fan_pwr_kW) | is.na(ODU_pwr_kW) | is.na(auxheat_pwr_kW), NA,
           
  # 2. Identify defrost mode                      
      # For Michaels sites, 0V on RV indicates heating mode and 27V indicates cooling/defrost.
      # NOTE: It looks like site 7083LM, greater than 20V means heating mode, and between 0.4 and 0.6V is system off, and 0.8 to 1.1V might be defrost
    ifelse(site_ID %in% c("2563EH", "2896BR", "6950NE", "8220XE", "9944LD", "6112OH", "8726VB") & reversing_valve_signal_V > 25 & ODU_pwr_kW > 0.1, "Defrost",
    ifelse(site_ID == "7083LM" & reversing_valve_signal_V > 0.8 & reversing_valve_signal_V < 1.1 & ODU_pwr_kW > 0.1, "Defrost", 
      # For site 4228VB, Trane provided RV data but there are some gaps which require secondary indicators
    ifelse(site_ID == "4228VB" & 
             (datetime_UTC >= strptime("2023-01-16", "%F", tz="US/Mountain") & datetime_UTC <= strptime("2023-01-30","%F",tz="US/Mountain") |
              datetime_UTC >= strptime("2023-02-22", "%F", tz="US/Mountain") & datetime_UTC <= strptime("2023-03-04","%F",tz="US/Mountain")) &
             auxheat_pwr_kW > 4.0 & ODU_pwr_kW > 0.1 & ODU_pwr_kW < 1.25 & fan_pwr_kW > 0.35, "Defrost",
        ifelse(site_ID == "4228VB" & !is.na(DEFROST_ON_1) & DEFROST_ON_1==1 & ODU_pwr_kW > 0.1, "Defrost",
      # For site 5539NO, RV between 0.6 V and 3.0 V indicates defrost mode
    ifelse(site_ID == "5539NO" & !is.na(reversing_valve_signal_V) & reversing_valve_signal_V > 0.6 & reversing_valve_signal_V < 3 & ODU_pwr_kW > 0.1, "Defrost",
      # For site 2458CE, secondary indicators are used
    ifelse(site_ID=="2458CE" & ODU_pwr_kW > 0.1 & ODU_pwr_kW < 1.75 & fan_pwr_kW > 0.4 & auxheat_pwr_kW > 4, "Defrost",
      # For site 5291QJ, secondary indicators are used
    ifelse(site_ID=="5291QJ" & ODU_pwr_kW > 0.5 & fan_pwr_kW > 0.02 & fan_pwr_kW < 0.06 , "Defrost",
  
  # 3. If not defrost mode, use power data to determine the mode         
    ifelse(ODU_pwr_kW > 0.1 & auxheat_pwr_kW < 0.1, "Heating-HP Only",
         ifelse(ODU_pwr_kW < 0.1 & auxheat_pwr_kW > 0.1, "Heating-Aux Only",
                ifelse(ODU_pwr_kW > 0.1 & auxheat_pwr_kW > 0.1, "Heating-Aux/HP",
                       "System Off")))))))))))) %>%
 
  # 4. Correct for cooling mode to differentiate from defrost mode
    # Indicators: Operating Mode is Defrost, Aux Power is less than 0.1 kW and OA Temp is greater than 50F
    mutate(operating_mode=ifelse(
      # Michaels sites
      site_ID %in% c("2563EH", "2896BR", "6950NE", "8220XE", "9944LD", "6112OH", "8726VB", "7083LM") & operating_mode=="Defrost" & auxheat_pwr_kW < 0.1 & OA_temp_F > 50,
             "Cooling", operating_mode))


# Heat pump status
  # Used to calculate the length of heat pump cycles and to determine if the 
  # HP ODU is on or off at every timestamp.
df <- df %>% mutate(HP_status = ifelse(is.na(fan_pwr_kW), NA, ifelse(ODU_pwr_kW > 0.1, "On", "Off")))



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
df$HP_cycle_runtimes <- run_cycle_calc(df$site_ID, df$datetime_UTC, df$HP_status, "On")
df$defrost_cycle_runtimes <- run_cycle_calc(df$site_ID, df$datetime_UTC, df$operating_mode, "Defrost")


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
    (-0.000000001546*SA_TempF^6 + 
       0.000000516256*SA_TempF^5 - 
       0.000020306966*SA_TempF^4 + 
       0.002266035021*SA_TempF^3 + 
       0.190010315225*SA_TempF^2 + 
       6.715900713408*SA_TempF + 
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
      (SA_TempF - RA_temp_F)),                                 # Temperature delta
  
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
      (SA_TempF - RA_temp_F) /
      (1 + supply_humidity_ratio))
)



### Diagnostic Graphs ----


# Vector storing every date in the dataframe
dates <- unique(df$date_local)

# Room temperature time series
  # Loop through dates (skip the first one to not have a partial day)
for(d in dates[-1]){
  # d1 = d + one day
  d1 = substr(as.character(strptime(d, "%F", tz=metadata$Timezone[metadata$site_ID==i]) + 60*60*24), 1, 10)
  ggsave(paste0(i, '_Daily-Room-Temp_',d,'.png'),
         plot = daily_room_temperature_comparison(i, d, d1),
         path = paste0(wd_out,'/daily_ops/',i, '/daily_room_temps/'),
         width=12, height=4, units='in')
}



# Supply temperature time series
  # Loop through dates (skip the first one to not have a partial day)
for(d in dates[-1]){
  # d1 = d + one day
  d1 = substr(as.character(strptime(d, "%F", tz=metadata$Timezone[metadata$site_ID==i]) + 60*60*24), 1, 10)
  ggsave(paste0(i, '_Daily-Supply-Temp_',d,'.png'),
         plot = daily_supply_temperature_comparison(i, d, d1),
         path = paste0(wd_out,'/daily_ops/',i, '/daily_SATs/'),
         width=12, height=4, units='in')
}



# Defrost cycle time series
  # Loop through dates (skip the first one to not have a partial day)
for(d in dates[-1]){
  # d1 = d + one day
  d1 = substr(as.character(strptime(d, "%F", tz=metadata$Timezone[metadata$site_ID==i]) + 60*60*24), 1, 10)
  ggsave(paste0(i, '_Daily-Defrost_',d,'.png'),
         plot = daily_defrost_plot(i, d, d1),
         path = paste0(wd_out,'/daily_ops/',i, '/daily_defrost/'),
         width=12, height=4, units='in')
}



# Daily operation power and temperature time series
  # Loop through dates (skip the first one to not have a partial day)
for(d in dates[-1]){
  # d1 = d + one day
  d1 = substr(as.character(strptime(d, "%F", tz=metadata$Timezone[metadata$site_ID==i]) + 60*60*24), 1, 10)
  ggsave(paste0(i, '_Daily-Operation_',d,'.png'),
         plot = daily_operation_plot(i, d, d1),
         path = paste0(wd_out,'/daily_ops/',i, '/daily_operation/'),
         width=12, height=4, units='in')
}


daily_COP_plot
# COP, heat and cooling output, and power time series
  # Loop through dates (skip the first one to not have a partial day)
for(d in dates[-1]){
  # d1 = d + one day
  d1 = substr(as.character(strptime(d, "%F", tz=metadata$Timezone[metadata$site_ID==i]) + 60*60*24), 1, 10)
  ggsave(paste0(i, '_Daily-COP_',d,'.png'),
         plot = daily_COP_plot(i, d, d1),
         path = paste0(wd_out,'/daily_ops/',i, '/daily_COPs/'),
         width=12, height=4, units='in')
}




# Operating mode daily summary
  ## Make one for winter performance and one for should performance and one for summer performance ##
  # Look at a time series graph of each day to see fraction of time in each operating mode
write.csv(df %>% mutate(datetime_UTC = datetime_UTC %>% with_tz(metadata$Timezone[metadata$site_ID==sitename]),
                        operating_mode = ifelse(is.na(operating_mode), "Data Unavailable", operating_mode)) %>%
            group_by(site_ID, Date, operating_mode) %>%
            summarize(Mode_Time = n()),
          file=paste0(wd, "/Graphs/Graph Data/Operating Mode Daily/", sitename, ".csv"),
          row.names=F)
OperatingModeTime <- function(site, timestart, timeend){
  list.files(path = paste0(wd, "/Graphs/Graph Data/Operating Mode Daily/"),pattern="*.csv", full.names=T) %>% 
    map_df(~read.csv(.)) %>%
    filter(site_ID == site) %>%
    ggplot(aes(x=as.POSIXct(Date, format="%F", tz=metadata$Timezone[metadata$site_ID==site]), fill=operating_mode, y=Mode_Time)) +
    geom_bar(position="fill", stat="identity") +
    scale_x_datetime(date_breaks = "1 week", 
                     date_labels = "%F",
                     limits=c(as.POSIXct(strptime(timestart,"%m/%d/%Y %H:%M", tz=metadata$Timezone[metadata$site_ID==site])),
                              as.POSIXct(strptime(timeend,"%m/%d/%Y %H:%M", tz=metadata$Timezone[metadata$site_ID==site])))) +
    scale_fill_manual(name = "Operating Mode",
                      breaks = c("Defrost","Heating-HP Only","Heating-Aux Only","Heating-Aux/HP","Cooling","System Off","Data Unavailable"),
                      values = c("#009E73","#F0E442","#CC3300","#E69F00","#3333FF","#666666","lightgrey")) +
    labs(title=paste0("Fraction of time in each operating mode per day for site ", site),x="", 
         y="Fraction of Time") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          panel.grid.major = element_line(size = 0.9),
          panel.grid.minor = element_line(size = 0.1),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.text.x = element_text(family = "Times New Roman", angle=-70, hjust=-0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5)) 
}
# OperatingModeTime(sitename, "12/15/2022 00:00", "3/30/2023 23:59")
# Print graph to folder--manually adjust date.
for(site in substr(list.files(path = paste0(wd, "/Graphs/Graph Data/Operating Mode Daily/"),pattern="*.csv", full.names=T),
                   nchar(list.files(path = paste0(wd, "/Graphs/Graph Data/Operating Mode Daily/"),pattern="*.csv", full.names=T)) - 9,
                   nchar(list.files(path = paste0(wd, "/Graphs/Graph Data/Operating Mode Daily/"),pattern="*.csv", full.names=T)) - 4)){
  ggsave(paste0(site, '_operating_mode_Percent_Time.png'),
       plot = OperatingModeTime(site, "12/15/2022 00:00", "3/30/2023 23:59"),
       path = paste0(wd,'/Graphs/',site, '/'),
       width=12, height=4, units='in')
}



## Remove data before data stabilizes for each site.
## Data removed in the middle of the period should be NA not removed!
df <- df %>% 
  # Site 2458CE:
  # Manufacturer visited site and Mar 21, 2023 8:30am - 5pm
  filter(site_ID != "2458CE" | datetime_UTC < strptime("2023-03-03 8:30:00", "%F %T", tz="Canada/Eastern") | datetime_UTC > strptime("2023-03-03 17:00:00", "%F %T", tz="Canada/Eastern")) %>%
  
  # Site 2563EH:
  # Data doesn't stabilize until Feb 1st, use this day as starting point.
  filter(site_ID != "2563EH" | datetime_UTC >= strptime("2023-02-01", "%F", tz="US/Eastern")) %>%
  
  # Site 2896BR:
  # Data doesn't stabilize until Feb 1st, use this day as starting point.
  filter(site_ID != "2896BR" | datetime_UTC >= strptime("2023-02-03", "%F", tz="US/Eastern")) %>%
  
  # Site 4228VB 
  # Appears to be missing Aux power data before Dec 20, 2022, doesn't stabilize until evening of 21st. Use Dec 22nd as starting point.
  filter(site_ID != "4228VB" | datetime_UTC >= strptime("2022-12-22", "%F", tz="US/Mountain")) %>%
  
  # Site 5291QJ:
  # Manufacturer visited site and was playing with controls Mar 21, 2023 8:30am - 5pm
  filter(site_ID != "5291QJ" | datetime_UTC < strptime("2023-03-03 8:30:00", "%F %T", tz="Canada/Eastern") | datetime_UTC > strptime("2023-03-03 17:00:00", "%F %T", tz="Canada/Eastern")) %>%
  
  # Site 5539NO:
  # All data is present from start.
  
  # Site 6112OH:
  # Data looks regular starting Feb 14--using this as starting point
  filter(site_ID != "6112OH" | datetime_UTC >= strptime("2023-02-14", "%F", tz="US/Eastern")) %>%
  
  # Site 6950NE:
  # Data doesn't stabilize until December 10th, use this day as starting point.
  filter(site_ID != "6950NE" | datetime_UTC >= strptime("2022-12-10", "%F", tz="US/Central")) %>%
  
  # Site 8220XE:
  # Data doesn't stabilize until Dec 12th at 12:00, use Dec 13th as starting point.
  filter(site_ID != "8220XE" | datetime_UTC >= strptime("2022-12-13", "%F", tz="US/Central")) %>%
  
  # Site 8726VB:
  # Data doesn't stabilize until Feb 11.
  filter(site_ID != "8726VB" | datetime_UTC >= strptime("2023-02-12", "%F", tz="US/Eastern")) %>%
  # No OA Temp and issues with SAT Mar 09 11AM to Mar 10 12PM. Set key parameters to NA.
  mutate_at(vars(ODU_pwr_kW, auxheat_pwr_kW, fan_pwr_kW, OA_temp_F, SA_TempF),
            ~ ifelse(site_ID == "8726VB" & datetime_UTC > strptime("2023-03-09 11:00:00", "%F %T", tz="US/Eastern") & datetime_UTC < strptime("2023-03-10 12:00:00", "%F %T", tz="US/Eastern"), NA, .)) %>%
  
  # Site 9944LD:
  # Data doesn't stabilize until Jan 7th, use this day as starting point.
  filter(site_ID != "9944LD" | datetime_UTC >= strptime("2023-01-07", "%F", tz="US/Mountain")) %>%
  # NA Fan power and temperature data from Jan 14 - 16 due to eGauge offline.
  mutate_at(vars(ODU_pwr_kW, auxheat_pwr_kW, fan_pwr_kW, OA_temp_F, SA_TempF),
            ~ ifelse(site_ID != "9944LD" & datetime_UTC > strptime("2023-01-14 16:00:00", "%F %T", tz="US/Mountain") & datetime_UTC < strptime("2023-01-16 20:00:00", "%F %T", tz="US/Mountain"), NA, .)) %>%
  # Missing temp data and HP Power and Aux default to 0 kW from Feb 15 noon to Feb 16 noon
  mutate_at(vars(ODU_pwr_kW, auxheat_pwr_kW, fan_pwr_kW, OA_temp_F, SA_TempF),
            ~ ifelse(site_ID != "9944LD" & datetime_UTC > strptime("2023-02-15 12:00:00", "%F %T", tz="US/Mountain") & datetime_UTC < strptime("2023-02-16 12:00:00", "%F %T", tz="US/Mountain"), NA, .))


## Print csv with calculated columns ##
write.csv(df, paste0(), row.names = F)

}

