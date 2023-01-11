# Clear Global Environment
rm(list=ls())

# Open libraries
library(tidyverse)
library(lubridate)
library(stringr)

# Set working library
  # As a string variable to make it easier to change to folders within directory
wd <- "C:/Users/keen930/PNNL/CCHP - General/Field Demonstration (Task 2)/R Data Analysis"
wd <- "/Users/rose775/Library/CloudStorage/OneDrive-PNNL/Desktop/Projects/General/Field Demonstration (Task 2)/R Data Analysis"

# Read data
  # Read_csv (tidyverse) is crashing RStudio, trying fread (data.table) which is 
  # supposed to be better with large files
library(data.table)
read_plus <- function(file) {fread(file) %>% mutate(filename=file)}

# Read Michaels/E350 data separately
df_michaels <- list.files(path = paste0(wd, "/Raw Data/Michaels"),pattern="*.csv", full.names=T) %>% 
  map_df(~read_plus(.)) %>% 
  # Modify filename so that it is the Site ID
  mutate(Site_ID = substr(filename, nchar(filename)-13,nchar(filename)-8)) %>%
  as.data.frame()

  # E350 data read one-minute and one-second data separately
df_e350_min <- list.files(path = paste0(wd, "/Raw Data/Energy350/1-Minute"),pattern="*.csv", full.names=T) %>% 
  map_df(~read_plus(.)) %>% 
  # Modify filename so that it is the Site ID
  mutate(Site_ID = substr(filename, 111, 116)) %>%
  as.data.frame()
df_e350_sec <- list.files(path = paste0(wd, "/Raw Data/Energy350/1-Second"),pattern="*.csv", full.names=T) %>% 
  map_df(~read_plus(.)) %>% 
  # Modify filename so that it is the Site ID
  mutate(Site_ID = substr(filename, 111, 116)) %>%
  as.data.frame()


# Metadata file to append time zone and other characteristics
metadata <- read_csv(file = paste0(wd, "/site-metadata.csv"))


# Convert timestamp from UTC to local time zone
  # Michaels sites are coming in UTC, E350 data is in local time zone
  # I think the best solution is to convert all to UTC so that there aren't any
  # issues binding the data that is stored in multiple timezones.
  # I added a field to the meta data ("Timezone") which will have the local timezone
  # for each site.

# Michaels data
  # data.table automatically converted the timestamp to POSIXct, so just need to force TZ
df_michaels$Timestamp <- df_michaels$index %>% force_tz(tzone = "UTC")

# Energy 350 data (1-minute and 1-second)
  # Convert to POSIXct and force TZ to UTC
df_e350_min$Timestamp = as.POSIXct(strptime(df_e350_min$`Timestamp (UTC)`, tz="UTC","%m/%d/%Y %H:%M"))
df_e350_sec$Timestamp = as.POSIXct(strptime(df_e350_sec$`Timestamp (UTC)`, tz="UTC","%m/%d/%Y %H:%M:%S"))



## Bind dataframes together into one
df_e350 <- rbind(
  # Mintue-level data
  df_e350_min %>% 
    rename(RV_Volts=`ReversingValveSignal [V]`,
           HP_Power=`HP_Power [kW]`,
           Fan_Power=`Fan_Power [kW]`,
           AHU_Power=`AHU_Power [kW]`,
           Aux_Power=`AuxHeat_Power [kW]`,
           OA_TempF=`OA_Temp [°F]`,
           OA_RH=`OA_RH [%]`,
           SA1_TempF=`SA_Duct1_Temp [°F]`,
           SA2_TempF=`SA_Duct2_Temp [°F]`,
           SA1_RH=`SA_Duct1_RH [%]`,
           SA2_RH=`SA_Duct2_RH [%]`,
           RA_TempF=`RA_Temp [°F]`,
           RA_RH=`RA_RH [%]`,
           AHU_TempF=`AHU_Ambient_Temp [°F]`,
           AHU_RH=`AHU_RH [%]`,
           Room1_TempF=`Room1_Temp [°F]`,
           Room1_RH=`Room1_RH [%]`,
           Room2_TempF=`Room2_Temp [°F]`,
           Room2_RH=`Room2_RH [%]`,
           Room3_TempF=`Room3_Temp [°F]`,
           Room3_RH=`Room3_RH [%]`,
           Room4_TempF=`Room4_Temp [°F]`,
           Room4_RH=`Room4_RH [%]`) %>%
    select(Site_ID, Timestamp, RV_Volts, HP_Power, Fan_Power, AHU_Power, Aux_Power,
           OA_TempF, OA_RH, SA1_TempF, SA2_TempF, SA1_RH, SA2_RH, RA_TempF, 
           RA_RH, AHU_TempF, AHU_RH, Room1_TempF, Room1_RH, Room2_TempF, Room2_RH,
           Room3_TempF, Room3_RH, Room4_TempF, Room4_RH) %>%
    mutate(SA3_TempF=NA, SA3_RH=NA, SA4_TempF=NA, SA4_RH=NA),
  # Second-level data
  df_e350_sec %>%
    filter(second(Timestamp) != 0) %>%    # Remove 0 second times to avoid duplicates from minute data
    rename(RV_Volts=`Reversing_Valve_Signal [VDC]`,
           HP_Power=`HP_Power [kW]`,
           Fan_Power=`FanPower [kW]`,
           AHU_Power=`AHU_Power [kW]`,
           Aux_Power=`Aux_Heat_Power [kW]`) %>%
    select(Site_ID, Timestamp, RV_Volts, HP_Power, Fan_Power, AHU_Power, Aux_Power) %>%
    mutate(OA_TempF=NA, OA_RH=NA, SA1_TempF=NA, SA2_TempF=NA, SA1_RH=NA, SA2_RH=NA, 
           RA_TempF=NA, RA_RH=NA, AHU_TempF=NA, AHU_RH=NA, Room1_TempF=NA, Room1_RH=NA, 
           Room2_TempF=NA, Room2_RH=NA, Room3_TempF=NA, Room3_RH=NA, Room4_TempF=NA, 
           Room4_RH=NA, SA3_TempF=NA, SA3_RH=NA, SA4_TempF=NA, SA4_RH=NA)) %>% 
  arrange(Site_ID, Timestamp)

rm(df_e350_min, df_e350_sec)


# Fill in missing temperature data for E350 data (only minute level)
  # Assumes minute data (when seconds are zero) has temperature data and
  # second data is missing it.
  # Important that the data is sorted by site and then timestamp, which it should be.
fillMissingTemp <- function(time, temp){
  t = NA # Initialize temperature counter
  
  for(row in 1:length(time)){
    if(second(time[row])==0){
    # At the minute level, make a record of the temperatures
      t = temp[row]
    } else {
    # And for all other data, record as the stored value
      temp[row] = t
    }
  }
  temp
}
df_e350$SA1_TempF <- fillMissingTemp(df_e350$Timestamp, df_e350$SA1_TempF)
df_e350$SA2_TempF <- fillMissingTemp(df_e350$Timestamp, df_e350$SA2_TempF)
df_e350$OA_TempF <- fillMissingTemp(df_e350$Timestamp, df_e350$OA_TempF)
df_e350$RA_TempF <- fillMissingTemp(df_e350$Timestamp, df_e350$RA_TempF)

# Operating mode
  # For Michaels/Lennox, 0V indicates heating mode and 2.7-3.0V indicates cooling mode,
  # but defrost is also cooling and needs to be separated. Because, if it is in 
  # defrost mode, we don't want to be calculating cooling capacity, we want that
  # to count against the heating capacity, I would think. So, I'm making
  # three modes: heating, cooling, and defrost for accurately pinpoint 
  # what the operation is, using outdoor air temperature to help.
  
  # For Energy350: DC Voltage signal to heat pump reversing valve. +1.5 should 
  # equal no signal. A -12V pulse (900 ms) is expected for cooling signal and a 
  # +3V pulse (900 ms) is expected for heating signal.
  # We only have OAT at 1-minute interval, so there will be many NA values. Heating
  # capacity will be the same because SA and RA temperature.
df_michaels <- df_michaels %>% mutate(
  Operating_Mode = ifelse(HP_Power < 0.01 & OA_TempF < 45, "Heating-Off",
                          ifelse(RV_Volts < 0.1, "Heating",
                            ifelse(OA_TempF < 45, "Defrost",
                              ifelse(HP_Power < 0.01, "Cooling-Off",
                                   "Cooling")))))
df_e350 <- df_e350 %>% mutate(
  Operating_Mode = ifelse(HP_Power < 0.01 & OA_TempF <45, "Heating-Off",
                          ifelse(RV_Volts > 2.5, "Heating",
                            ifelse(RV_Volts < -10 & OA_TempF < 40, "Defrost",
                              ifelse(RV_Volts < -10 & HP_Power < 0.01, "Cooling-Off",
                                ifelse(RV_Volts < -10, "Cooling",
                                    NA))))))

# Calculate Aux_Power for Michaels data (E350 is already calculated)
df_michaels <- df_michaels %>% mutate(
  Aux_Power = rowSums(cbind(Aux1_Power, Aux2_Power, Aux3_Power, Aux4_Power), na.rm=T))

# Row bind e350 and Michaels data together
df <- rbind(
  df_e350,
  df_michaels %>% 
    select(Site_ID, Timestamp, RV_Volts, HP_Power, Fan_Power, AHU_Power, Aux_Power,
           OA_TempF, OA_RH, SA1_TempF, SA2_TempF, SA1_RH, SA2_RH, RA_TempF, 
           RA_RH, AHU_TempF, AHU_RH, Room1_TempF, Room1_RH, Room2_TempF, Room2_RH,
           Room3_TempF, Room3_RH, SA3_TempF, SA3_RH, SA4_TempF, SA4_RH, Operating_Mode) %>%
    mutate(Room4_TempF = NA, Room4_RH = NA)) %>% arrange(Site_ID, Timestamp)

rm(df_michaels, df_e350)


# Add fields for date, hour, and week day
df <- df %>% mutate(
  date = date(Timestamp),
  hour = hour(Timestamp),
  day = wday(Timestamp))



  # Supply temperature and humidity calculated as the average of the four quadrants
df <- df %>% mutate(
  SA_RH = rowMeans(cbind(SA1_RH, SA2_RH, SA3_RH, SA4_RH), na.rm=T),
  SA_TempF = rowMeans(cbind(SA1_TempF, SA2_TempF, SA3_TempF, SA4_TempF), na.rm=T))


# Calculate fields
  # Humidity ratio of supply air (0.62198 * Pw / (P - Pw))
    # P = 97,717 Pascals at 1,000 ft elevation (we could use a more accurate look up for each location)
    # Pw = RH * Pws; Pws = f(T); T in Fahrenheit, based on curve fit
    # 6th order poly fit: Pws = -0.000000001546*T^6 + 0.000000516256*T^5 - 0.000020306966*T^4 + 0.002266035021*T^3 + 0.190010315225*T^2 + 6.715900713408*T + 125.349159019000
df <- df %>% mutate(
      Partial_Water_Pressure_Supply = SA_RH / 100 * 
            (-0.000000001546*SA_TempF^6 + 
             0.000000516256*SA_TempF^5 - 
             0.000020306966*SA_TempF^4 + 
             0.002266035021*SA_TempF^3 + 
             0.190010315225*SA_TempF^2 + 
             6.715900713408*SA_TempF + 
             125.349159019000),
       Supply_Humidity_Ratio = 
              0.62198 * Partial_Water_Pressure_Supply / (97717 - Partial_Water_Pressure_Supply))



  # Air supply (based on fan power curve)
fan_power_curve <- function(fan_power, siteid){
  # Correlate fan power (kW) to volumetric flow rate (CFM) based on initial testing
  # Function will output the air supply in CFM for an input power in Watts
  # We will need to add a line for each site in this function.
  # Note: Michaels gave curves in W and E350 in kW, so Michaels' sites need * 1,000
  supply_flow_rate_CFM = ifelse(siteid=="6950NE", 116.48 * (fan_power*1000) ^ 0.3866,
                         ifelse(siteid=="8220XE", 152.27 * (fan_power*1000) ^ 0.3812,
                         ifelse(siteid=="4228VB", 1647.7 * fan_power^0.394,
                         ifelse(siteid=="9944LD", 115.09 * (fan_power*1000)^0.3926,
                                    NA))))
}
df$supply_flow_rate_CFM <- fan_power_curve(df$Fan_Power, df$Site_ID)

 
  # Total power
df$Total_Power = df$AHU_Power + df$HP_Power


  # Energy use
    # Calculate energy use at each timestamp as the power multiplied by the interval
    # since the last power reading.
    # This assumes that if there is missing data, the eGauge will report the first
    # point after a gap as the average of the gap.
    # Important that the data is sorted by site id and then timestamp, which it 
    # should be from previous code "arrange".

energyCalc <- function(site, timestamp, power){
  
  index <- which(!is.na(power))[1] + 1    # Second non-NA row
  ts <- timestamp[index]                  # Timestamp at first non-NA row
  energy <- rep(NA, length(site))         # Initialize vector for energy
  ct <- site[index]                       # Initialize counter to detect new sites
  
  for(row in index:length(site)){
    
    if(!is.na(power[row])){              # If the power is not NA, calculate energy from last time step
      if(site[row] == ct){               # Only calculate energy if there is not a new site
        energy[row] = power[row] * difftime(timestamp[row], ts, units="hours")
      }
      ts <- timestamp[row]
      ct <- site[row]                    # Record of last site with non-NA
    }
  }
  energy    # Return energy vector as output
}
df$Energy_kWh <- energyCalc(df$Site_ID, df$Timestamp, df$Total_Power)


  # Heat Pump Heat Output
    # Q-heating = (dry air density) * (blower airflow rate) * (specific heat) * (delta Temp)
df <- df %>% mutate(
  HP_Heat_Output_Btu_h = ifelse(
    Operating_Mode == "Cooling", NA,
      0.0765 *                                                # Density of air at 15C (lb/ft3)
      supply_flow_rate_CFM * 60 *                             # CFM * min/hour
      (0.24 + 0.444 *  Supply_Humidity_Ratio) *               # Specific heat capacity (Btu/F-lb)
      (SA_TempF - RA_TempF)) -                                # Temperature delta
    Aux_Power * 3412,                                         # Subtract auxiliary power, convert kW to btu/hr

  # Auxiliary Heat Output
    # Electric resistance heating is expected to have one unit of power in to one unit of heat output
  Aux_Heat_Output_Btu_h = Aux_Power * 3412,

  # Cooling output
    # Q-cooling = (dry air density) * (blower airflow rate) * (specific heat) * (delta Temp) / (1 + Humidity Ratio)
  HP_Cool_Output_Btu_h = ifelse(
    Operating_Mode == "Heating", NA,
      0.0765 *                                                          # Density of air at 15C (lb/ft3)
      supply_flow_rate_CFM * 60 *                                       # CFM * min/hour
      (0.24 + 0.444 *  Supply_Humidity_Ratio) *                         # Specific heat capacity (Btu/F-lb)
      (SA_TempF - RA_TempF) /
      (1 + Supply_Humidity_Ratio)),
  
  # Heating load
  Heating_Load_Btu_h = ifelse(
    Operating_Mode == "Cooling", NA,
    (AHU_Power + HP_Power) * 3412),                           # Convert total system power from kW to Btu/hr

  # Cooling load
  Cooling_Load_Btu_h = ifelse(
    Operating_Mode == "Heating"|Operating_Mode=="Defrost", NA,
    (AHU_Power + HP_Power) * 3412),                           # Convert total system power from kW to Btu/hr


  # COP heating
    # VM: wondering if we should report COP with and without supplemental heat.
    # I personally think that supplemental heat should not be part of the COP
    # but we need to align this with the challenge spec.
    # KK: Right now heating capacity subtracts auxiliary heat component.
  HP_COP_Heating = HP_Heat_Output_Btu_h / 
    Heating_Load_Btu_h,

# COP cooling
  HP_COP_Cooling = (-1 * HP_Cool_Output_Btu_h) / 
    Cooling_Load_Btu_h)


# Calculate heat and defrost run cycle duration for heat pump.
  # The "mode" input should be "Heating" or "Defrost".
  # The script is set up to calculate EITHER heating run time or defrost runtime (or any other mode, e.g., cooling)
runCycleCalc <- function(site, timestamp, operate, mode){
  
  index <- which(operate == mode)[1]         # First row in heating/defrost mode
  ts <- timestamp[index]                     # Timestamp at first non-NA row
  cycle <- rep(NA, length(site))             # Initialize vector for cycle runtimes
  ct <- site[index]                          # Initialize counter to detect new site
  track <- TRUE                              # Tracker for heating cycle
  
  for(row in (index+1):length(site)){

    if(site[row] != ct){
      # If there is a new site, do not calculate previous cycle. Reset trackers
      ct <- site[row]                        # Update record of site
      
      if(operate[row] != mode | is.na(operate[row])){
        track <- FALSE                       # Timestamp will be reset once it re-enters heat/defrost mode
      } else {
        ts <- timestamp[row]                 # Reset timestamp
        track <- TRUE
      }

    } else if(is.na(operate[row])){
      next     # Skip NA rows, I think this makes the most sense. 
               # Other option is to end cycle if it was in heating before the NA.
      
    } else if(operate[row] != mode & track==TRUE){
      # Heating/defrost cycle ends: If the previous row was heating/defrost and this row is not, record runtime.
      cycle[row-1] <- difftime(timestamp[row-1], ts, units="mins")
      track <- FALSE
      
    } else if(operate[row] == mode & track == FALSE){
      # Heating/defrost cycle begins: If the previous row was not heating/defrost and this row is, reset timestamp counter
      ts <- timestamp[row]                   # Reset timestamp
      track <- TRUE
    }
  }
  
  cycle    # Return cycle vector as output
}

df$Heat_Cycle_Runtimes <- runCycleCalc(df$Site_ID, df$Timestamp, df$Operating_Mode, "Heating")
df$Defrost_Cycle_Runtimes <- runCycleCalc(df$Site_ID, df$Timestamp, df$Operating_Mode, "Defrost")



### Diagnostic Tables ----

# Run diagnostics on missing power data
  # Not sure how helpful this is... keeping it here in case we want to update
  # with a statistics table of some sort as an output.
  # error on Mac OS: "Error in file(file, ifelse(append, "a", "w")) : 'mode' for the clipboard must be 'r' on Unix"
powerDiagnostics <- function(site){
  pwr_diag <- df %>% filter(Site_ID==site) %>% group_by(date) %>% summarize(
    Percent_NA_HP = sum(is.na(HP_Power))/length(HP_Power),
    Percent_NA_Aux = sum(is.na(Aux_Power))/length(Aux_Power))
  
  pwr_diag  # Return diagnostics table to write to clipboard
}
view(powerDiagnostics("6950NE"))
write.table(powerDiagnostics("6950NE"), "clipboard",sep="\t",row.names = F)
# 4228VB
# 6950NE
# 8220XE

OATDiagnostics <- function(){
  oat_diag <- df %>% group_by(Site_ID) %>% summarize(
    Percent_Less_Than_Neg25 = sum(OA_TempF <= -25, na.rm=T)/sum(!is.na(OA_TempF)),
    Percent_Neg25_to_Neg20 = sum(OA_TempF > -25 & OA_TempF <= -20, na.rm=T)/sum(!is.na(OA_TempF)),
    Percent_Neg20_to_Neg15 = sum(OA_TempF > -20 & OA_TempF <= -15, na.rm=T)/sum(!is.na(OA_TempF)),
    Percent_Neg15_to_Neg10 = sum(OA_TempF > -15 & OA_TempF <= -10, na.rm=T)/sum(!is.na(OA_TempF)),
    Percent_Neg10_to_Neg5 = sum(OA_TempF > -10 & OA_TempF <= -5, na.rm=T)/sum(!is.na(OA_TempF)),
    Percent_Neg5_to_0 = sum(OA_TempF > -5 & OA_TempF <= 0, na.rm=T)/sum(!is.na(OA_TempF)),
    Percent_0_to_5 = sum(OA_TempF > 0 & OA_TempF <= 5, na.rm=T)/sum(!is.na(OA_TempF)),
    Percent_5_to_10 = sum(OA_TempF > 5 & OA_TempF <= 10, na.rm=T)/sum(!is.na(OA_TempF)),
    Percent_10_to_15 = sum(OA_TempF > 10 & OA_TempF <= 15, na.rm=T)/sum(!is.na(OA_TempF)),
    Percent_15_to_20 = sum(OA_TempF > 15 & OA_TempF <= 20, na.rm=T)/sum(!is.na(OA_TempF)),
    Percent_20_to_25 = sum(OA_TempF > 20 & OA_TempF <= 25, na.rm=T)/sum(!is.na(OA_TempF)),
    Percent_25_to_30 = sum(OA_TempF > 25 & OA_TempF <= 30, na.rm=T)/sum(!is.na(OA_TempF)),
    Percent_30_to_35 = sum(OA_TempF > 30 & OA_TempF <= 35, na.rm=T)/sum(!is.na(OA_TempF)),
    Percent_35_to_40 = sum(OA_TempF > 35 & OA_TempF <= 40, na.rm=T)/sum(!is.na(OA_TempF)),
    Percent_40_to_45 = sum(OA_TempF > 40 & OA_TempF <= 45, na.rm=T)/sum(!is.na(OA_TempF)),
    Percent_More_Than_45 = sum(OA_TempF > 45, na.rm=T)/sum(!is.na(OA_TempF))
  )
  
  oat_diag  # Return diagnostics table to write to clipboard
}
view(OATDiagnostics())
write.table(OATDiagnostics(), "clipboard",sep="\t",row.names = F)





### Time Series Graphs ----

# Investigate time series for any variable
TimeSeries <- function(site, parameter, interval, timestart, timeend){
  # Look at a time series graph for a given parameter, time interval (e.g, 5-minute), time period, and site
  # Interval is in units of minutes, and so the maximum interval would be one hour.
  # The site can be a list of multiple sites if would like to compare.
  # The time start and end should be a date-time string in format for example "4/01/2022 08:00".
  df %>% mutate(Timestamp = Timestamp %>% with_tz(metadata$Timezone[metadata$Site_ID==site]),
                Interval = minute(Timestamp) %/% interval) %>% 
    filter(Site_ID %in% site &
            Timestamp >= strptime(timestart,"%m/%d/%Y %H:%M") &
            Timestamp <= strptime(timeend,"%m/%d/%Y %H:%M")) %>%
    group_by(Site_ID,date, hour, Interval) %>% 
    summarize(Timestamp = Timestamp[1],
              Parameter = mean(!!as.name(parameter),na.rm=T)) %>%
    ggplot(aes(x=as.POSIXct(Timestamp),y=Parameter, color=Site_ID)) +
    geom_line(size=0.3) + 
    geom_hline(aes(yintercept = 0)) +
    labs(title="Time series plot",x="Timestamp",y=parameter, color="Site ID") +
    theme(axis.ticks.y=element_blank(),
          panel.border = element_rect(colour = "black",fill=NA)) +
    guides(color=guide_legend(override.aes=list(size=3)))
}
# TimeSeries("6950NE", "OA_RH", 5, "12/01/2022 00:00", "01/30/2023 00:00")
# "6950NE"


# Temperature time series comparison chart
TempTimeSeries <- function(site, interval, timestart, timeend){
  # Look at a time series graph for all temperature monitors, time interval (e.g, 5-minute), time period, and site
  # Interval is in units of minutes, and so the maximum interval would be one hour.
  # The time start and end should be a date string in format for example "4/01/2022".
  df %>% mutate(Timestamp = Timestamp %>% with_tz(metadata$Timezone[metadata$Site_ID==site]),
                Interval = minute(Timestamp) %/% interval) %>% 
    filter(Site_ID == site &
             Timestamp >= strptime(timestart,"%m/%d/%Y %H:%M") &
             Timestamp <= strptime(timeend,"%m/%d/%Y %H:%M")) %>%
    group_by(Site_ID,date, hour, Interval) %>% 
    summarize(Timestamp = Timestamp[1],
              SA_TempF = mean(SA_TempF,na.rm=T),
              RH_TempF = mean(RA_TempF,na.rm=T),
              Room1_TempF = mean(Room1_TempF,na.rm=T),
              Room2_TempF = mean(Room2_TempF,na.rm=T),
              Room3_TempF = mean(Room3_TempF,na.rm=T),
              AHU_TempF = mean(AHU_TempF,na.rm=T)) %>%
    ggplot(aes(x=as.POSIXct(Timestamp))) +
    geom_line(aes(y=SA_TempF, color = "Supply Air"),size=0.3) + 
    geom_line(aes(y=RH_TempF, color = "Return Air"),size=0.3) + 
    geom_line(aes(y=Room1_TempF, color = "Room 1"),size=0.3) + 
    geom_line(aes(y=Room2_TempF, color = "Room 2"),size=0.3) + 
    geom_line(aes(y=Room3_TempF, color = "Room 3"),size=0.3) + 
    geom_line(aes(y=AHU_TempF, color = "AHU Ambient"),size=0.3) + 
    scale_y_continuous(breaks = seq(0,200, by=10), minor_breaks = seq(0, 200, by=1)) +
    scale_color_manual(name = "", values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
    labs(title=paste0("Temperature time series plot for site ", site),x="",y="Temperature (F)") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          panel.grid.major = element_line(size = 0.9),
          panel.grid.minor = element_line(size = 0.1),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5),) +
    guides(color=guide_legend(override.aes=list(size=3)))
}
# TempTimeSeries("4228VB", 5, "12/01/2022 00:00", "12/31/2022 00:00")
# "8220XE"
# 6950NE

# Supply temperature time series comparison chart
SupplyTempTimeSeries <- function(site, interval, timestart, timeend){
  # Look at a time series graph for the four supply temperature monitors, time interval (e.g, 5-minute), time period, and site
  # Interval is in units of minutes, and so the maximum interval would be one hour.
  # The time start and end should be a date string in format for example "4/01/2022".
  df %>% mutate(Timestamp = Timestamp %>% with_tz(metadata$Timezone[metadata$Site_ID==site]),
                Interval = minute(Timestamp) %/% interval) %>% 
    filter(Site_ID == site &
             Timestamp >= strptime(timestart,"%m/%d/%Y %H:%M") &
             Timestamp <= strptime(timeend,"%m/%d/%Y %H:%M")) %>%
    group_by(Site_ID,date, hour, Interval) %>% 
    summarize(Timestamp = Timestamp[1],
              SA1_TempF = mean(SA1_TempF,na.rm=T),
              SA2_TempF = mean(SA2_TempF,na.rm=T),
              SA3_TempF = mean(SA3_TempF,na.rm=T),
              SA4_TempF = mean(SA4_TempF,na.rm=T)) %>%
    ggplot(aes(x=as.POSIXct(Timestamp))) +
    geom_line(aes(y=SA1_TempF, color = "SA1"),size=0.3) + 
    geom_line(aes(y=SA2_TempF, color = "SA2"),size=0.3) + 
    geom_line(aes(y=SA3_TempF, color = "SA3"),size=0.3) + 
    geom_line(aes(y=SA4_TempF, color = "SA4"),size=0.3) + 
    scale_y_continuous(breaks = seq(0,200, by=10), minor_breaks = seq(0,200, by=1)) +
    scale_color_manual(name = "", values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
    labs(title=paste0("Supply temperature time series plot for site ", site),x="",y="Temperature (F)") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          panel.grid.major = element_line(size = 0.9),
          panel.grid.minor = element_line(size = 0.1),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5),) +
    guides(color=guide_legend(override.aes=list(size=3)))
}
# SupplyTempTimeSeries("4228VB", 5, "12/01/2022 00:00", "12/31/2022 00:00")
#6950NE



# Power time series comparison chart with OAT
PowerTimeSeriesOAT <- function(site, interval, timestart, timeend){
  # Look at a time series graph for all temperature monitors, time interval (e.g, 5-minute), time period, and site
  # Interval is in units of minutes, and so the maximum interval would be one hour.
  # The time start and end should be a date string in format for example "4/01/2022".
  # Scale for the secondary axis may need to be adjusted manually, and will get more
  # complicated once we have outdoor values.
  df %>% mutate(Timestamp = Timestamp %>% with_tz(metadata$Timezone[metadata$Site_ID==site]),
                Interval = minute(Timestamp) %/% interval) %>% 
    filter(Site_ID == site &
             Timestamp >= strptime(timestart,"%m/%d/%Y %H:%M") &
             Timestamp <= strptime(timeend,"%m/%d/%Y %H:%M")) %>%
    group_by(Site_ID,date, hour, Interval) %>% 
    summarize(Timestamp = Timestamp[1],
              HP_Power = mean(HP_Power,na.rm=T),
              Fan_Power = mean(Fan_Power,na.rm=T),
              Aux_Power = mean(Aux_Power,na.rm=T),
              Total_Power = mean(Total_Power, na.rm=T),
              OA_TempF = mean(OA_TempF,na.rm=T)) %>%
    ggplot(aes(x=as.POSIXct(Timestamp))) +
    geom_line(aes(y=OA_TempF/2.5, color = "Outdoor Temperature"),size=0.3) + 
    geom_line(aes(y=Total_Power, color = "Total Power"),size=0.3) +
    geom_line(aes(y=HP_Power, color = "Heat Pump Power"),size=0.3) + 
    geom_line(aes(y=Fan_Power, color = "Fan Power"),size=0.3) +
    geom_line(aes(y=Aux_Power, color = "Auxiliary Power"),size=0.3) + 
    scale_y_continuous(name = "Power (kW)",
                       sec.axis = sec_axis(~.*2.5, name ="Outdoor Air Temperature (F)")) +
    scale_color_manual(name = "", values = c("#E69F00", "#56B4E9","#009E73", "black", "#CC79A7", "#F0E442", "#0072B2", "#D55E00")) +
    labs(title=paste0("Power and OA temp time series plot for site ", site),x="") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          panel.grid.major = element_line(size = 0.9),
          panel.grid.minor = element_line(size = 0.1),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5),) +
    guides(color=guide_legend(override.aes=list(size=3)))
}
# PowerTimeSeriesOAT("4228VB", 5, "12/23/2022 0:00", "12/24/2022 0:00")


# Power time series comparison chart with supply temperature
PowerTimeSeriesSA <- function(site, interval, timestart, timeend){
  # Look at a time series graph for all temperature monitors, time interval (e.g, 5-minute), time period, and site
  # Interval is in units of minutes, and so the maximum interval would be one hour.
  # The time start and end should be a date string in format for example "4/01/2022".
  # Scale for the secondary axis may need to be adjusted manually, and will get more
  # complicated once we have outdoor values.
  df %>% mutate(Timestamp = Timestamp %>% with_tz(metadata$Timezone[metadata$Site_ID==site]),
                Interval = minute(Timestamp) %/% interval) %>% 
    filter(Site_ID == site &
             Timestamp >= strptime(timestart,"%m/%d/%Y %H:%M") &
             Timestamp <= strptime(timeend,"%m/%d/%Y %H:%M")) %>%
    group_by(Site_ID,date, hour, Interval) %>% 
    summarize(Timestamp = Timestamp[1],
              HP_Power = mean(HP_Power,na.rm=T),
              Fan_Power = mean(Fan_Power,na.rm=T),
              Aux_Power = mean(Aux_Power,na.rm=T),
              Total_Power = mean(Total_Power, na.rm=T),
              SA_TempF = mean(SA_TempF,na.rm=T)) %>%
    ggplot(aes(x=as.POSIXct(Timestamp))) +
    geom_line(aes(y=SA_TempF/5, color = "Supply Air Temperature"),size=0.3) + 
    geom_line(aes(y=Total_Power, color = "Total Power"),size=0.3) + 
    geom_line(aes(y=HP_Power, color = "Heat Pump Power"),size=0.3) + 
    geom_line(aes(y=Fan_Power, color = "Fan Power"),size=0.3) + 
    geom_line(aes(y=Aux_Power, color = "Auxiliary Power"),size=0.3) + 
    scale_y_continuous(name = "Power (kW)",
                       sec.axis = sec_axis(~.*5, name ="Supply Air Temperature (F)")) +
    scale_color_manual(name = "", values = c("#E69F00", "#56B4E9","#009E73", "black", "#CC79A7", "#F0E442", "#0072B2", "#D55E00")) +
    labs(title=paste0("Power and SA temp time series plot for site ", site),x="") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          panel.grid.major = element_line(size = 0.9),
          panel.grid.minor = element_line(size = 0.1),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5),) +
    guides(color=guide_legend(override.aes=list(size=3)))
}
# PowerTimeSeriesSA("4228VB", 5, "12/23/2022 16:00", "12/26/2022 12:00")


# Reversing valve voltage chart with supply temperature
RevValveTimeSeries <- function(site, interval, timestart, timeend){
  # Look at a time series graph for all temperature monitors, time interval (e.g, 5-minute), time period, and site
  # Interval is in units of minutes, and so the maximum interval would be one hour.
  # The time start and end should be a date string in format for example "4/01/2022".
  # Scale for the secondary axis may need to be adjusted manually, and will get more
  # complicated once we have outdoor values.
  df %>% mutate(Timestamp = Timestamp %>% with_tz(metadata$Timezone[metadata$Site_ID==site]),
                Interval = minute(Timestamp) %/% interval) %>% 
    filter(Site_ID == site &
             Timestamp >= strptime(timestart,"%m/%d/%Y %H:%M") &
             Timestamp <= strptime(timeend,"%m/%d/%Y %H:%M")) %>%
    group_by(Site_ID,date, hour, Interval) %>% 
    summarize(Timestamp = Timestamp[1],
              HP_Power = mean(HP_Power,na.rm=T),
              Aux_Power = mean(Aux_Power, na.rm=T),
              RV_Volts = mean(Fan_Power,na.rm=T),
              SA_TempF = mean(SA_TempF,na.rm=T)) %>%
    ggplot(aes(x=as.POSIXct(Timestamp))) +
    geom_line(aes(y=SA_TempF/20, color = "Supply Air Temperature"),size=0.3) + 
    geom_line(aes(y=HP_Power, color = "Heat Pump Power"),size=0.3) + 
    geom_line(aes(y=Aux_Power, color = "Aux Power"),size=0.3) + 
    geom_line(aes(y=RV_Volts, color = "Fan Power"),size=0.3) + 
    geom_hline(yintercept=0) +
    scale_y_continuous(name = "Power (kW)/Volts",
                       sec.axis = sec_axis(~.*20, name ="Supply Air Temperature (F)")) +
    scale_color_manual(name = "", values = c("#E69F00", "#56B4E9","#009E73", "black", "#CC79A7", "#F0E442", "#0072B2", "#D55E00")) +
    labs(title=paste0("RV Voltage and SA temp time series plot for site ", site),x="") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          panel.grid.major = element_line(size = 0.9),
          panel.grid.minor = element_line(size = 0.1),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5),) +
    guides(color=guide_legend(override.aes=list(size=3)))
}
RevValveTimeSeries("4228VB", 1, "01/07/2023 0:00", "01/19/2023 0:00")


# Heating output (Btu/h) and heating load with outdoor air temperature as timeseries
HeatOutputTimeSeries <- function(site, interval, timestart, timeend){
  df %>% mutate(Timestamp = Timestamp %>% with_tz(metadata$Timezone[metadata$Site_ID==site]),
                Interval = minute(Timestamp) %/% interval) %>% 
    filter(Site_ID == site &
             Timestamp >= strptime(timestart,"%m/%d/%Y %H:%M") &
             Timestamp <= strptime(timeend,"%m/%d/%Y %H:%M")) %>%
    group_by(Site_ID, date, hour, Interval) %>% 
    summarize(Timestamp = Timestamp[1],
              HP_Heating_Output = mean(HP_Heat_Output_Btu_h,na.rm=T),
              Heating_Load = mean(Heating_Load_Btu_h,na.rm=T),
              Sys_Heating_Output = mean(HP_Cool_Output_Btu_h + Aux_Heat_Output_Btu_h, na.rm=T),
              OA_Temp = mean(OA_TempF,na.rm=T)) %>%
    ggplot(aes(x = as.POSIXct(Timestamp))) + 
    geom_line(size = 0.3, aes(y = HP_Heating_Output, color="HP Heating Output")) +
    geom_line(size = 0.3, aes(y = Heating_Load, color="System Heating Load")) +
    geom_line(size = 0.3, aes(y = Sys_Heating_Output, color="System Heating Output")) +
    geom_line(size = 0.3, aes(y = OA_Temp*1000, color="Outdoor Air Temperature")) +
    scale_color_manual(name = "", values = c("#E69F00", "black", "#56B4E9", "#009E73", "#CC79A7", "#F0E442", "#0072B2", "#D55E00")) +
    scale_y_continuous(name = "Heating Output/Load (Btu/hr)",
                       sec.axis = sec_axis(~./1000, name ="Outdoor Air Temperature (F)")) +
    labs(title=paste0("Heating Output/Heating Load and Outdoor Air Temperature Time Series for site ",site),
         x="") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          legend.title = element_blank(),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5)) +
    guides(color=guide_legend(override.aes=list(size=3)))
}
# HeatOutputTimeSeries("4228VB", 5, "12/15/2022 00:00", "12/31/2022 00:00")


# Percent of time that heat pump and aux heat are running as timeseries
SystemOperationTimeSeries <- function(site, timestart, timeend){
  # Look at a time series graph to see for every hour, what percentage of that
  # hour is the heat pump operating and the auxiliary heat.
  df %>% mutate(Timestamp = Timestamp %>% with_tz(metadata$Timezone[metadata$Site_ID==site])) %>%
    filter(Site_ID == site &
             Timestamp >= strptime(timestart,"%m/%d/%Y %H:%M") &
             Timestamp <= strptime(timeend,"%m/%d/%Y %H:%M")) %>%
    group_by(Site_ID, date, hour) %>% 
    summarize(Timestamp = Timestamp[1],
              Aux_Perc = sum(Aux_Power > 0.1,na.rm=T)*100/sum(!is.na(Aux_Power)),
              HP_Perc = sum(HP_Power > 0.1,na.rm=T)*100/sum(!is.na(HP_Power)),
              OA_Temp = mean(OA_TempF,na.rm=T)) %>%
    ggplot(aes(x=as.POSIXct(Timestamp))) +
    geom_line(aes(y=OA_Temp*2, color = "Outdoor Air Temperature"),size=0.3) + 
    geom_line(aes(y=Aux_Perc, color = "Auxiliary"),size=0.3) + 
    geom_line(aes(y=HP_Perc, color = "Heat Pump"),size=0.3) + 
    scale_y_continuous(name = "Percent (%)",
                       sec.axis = sec_axis(~./2, name ="Outdoor Air Temperature (F)")) +
    scale_color_manual(name = "", values = c("#E69F00", "#56B4E9", "black","#009E73", "#CC79A7", "#F0E442", "#0072B2", "#D55E00")) +
    labs(title=paste0("Percent of time system is operating per hour for site ", site),x="") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          panel.grid.major = element_line(size = 0.9),
          panel.grid.minor = element_line(size = 0.1),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5),) +
    guides(color=guide_legend(override.aes=list(size=3)))
}
# SystemOperationTimeSeries("4228VB", "12/01/2022 00:00", "12/31/2022 00:00")


# Graph to calculate number of heat and defrost run cycles and average length of cycle per day
RunTimesTimeSeries <- function(site, timestart, timeend){
  # Look at a time series graph to see for every day, how many run cycles there are
  # and the average length of a cycle is. Plot against outdoor air temperature and humidity.
  df %>% mutate(Timestamp = Timestamp %>% with_tz(metadata$Timezone[metadata$Site_ID==site])) %>%
    filter(Site_ID == site &
             Timestamp >= strptime(timestart,"%m/%d/%Y %H:%M") &
             Timestamp <= strptime(timeend,"%m/%d/%Y %H:%M")) %>%
    group_by(Site_ID, date) %>% 
    summarize(Timestamp = Timestamp[1],
              Num_Heat_Cycles = sum(!is.na(Heat_Cycle_Runtimes)),
              Average_Heat_Runtime = mean(Heat_Cycle_Runtimes,na.rm=T),
              Num_Defrost_Cycles = sum(!is.na(Defrost_Cycle_Runtimes)),
              Average_Defrost_Runtime = mean(Defrost_Cycle_Runtimes,na.rm=T),
              OA_Temp = mean(OA_TempF,na.rm=T),
              OA_RH = mean(OA_RH,na.rm=T)) %>%
    ggplot(aes(x=as.POSIXct(Timestamp))) +
    geom_line(size = 0.5, linetype="dashed",aes(y = OA_Temp, color="Outdoor Temperature", group=1)) +
    geom_line(size = 0.5, linetype="dashed",aes(y = OA_RH, color="Outdoor Humidity", group=1)) +
    geom_line(size = 0.75, aes(y = Num_Heat_Cycles, color="Number of Heat Cycles", group=1)) +
    geom_line(size = 0.75, aes(y = Average_Heat_Runtime, color="Average Heat Cycle Length", group=1)) +
    geom_line(size = 0.75, aes(y = Num_Defrost_Cycles, color="Number of Defrost Cycles", group=1)) +
    geom_line(size = 0.75, aes(y = Average_Defrost_Runtime, color="Average Defrost Cycle Length", group=1)) +
    geom_point(size=1.5, aes(y = Num_Heat_Cycles), color="#E69F00") +
    geom_point(size=1.5, aes(y = Average_Heat_Runtime), color="#CC79A7") +
    geom_point(size=1.5, aes(y = Num_Defrost_Cycles), color="#009E73") +
    geom_point(size=1.5, aes(y = Average_Defrost_Runtime), color="#D55E00") +
    scale_y_continuous(name = "Number of Cycles/Average Cycle Length (mins)",
                       sec.axis = sec_axis(~.*1, name ="Humidity (%)/Temperature (F)")) +
    scale_color_manual(name = "", values = c("#D55E00","#CC79A7","#009E73","#E69F00", "black","grey","#F0E442")) +
    labs(title=paste0("Heat and defrost runtime length and count per day and outdoor temperature and humidity for site ", site),x="") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          panel.grid.major = element_line(size = 0.9),
          panel.grid.minor = element_line(size = 0.1),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5),) +
    guides(color=guide_legend(override.aes=list(size=3)))
}
# RunTimesTimeSeries("4228VB", "12/01/2022 00:00", "12/31/2022 00:00")

# Operating mode daily summary
OperatingModeTime <- function(site, timestart, timeend){
  # Look at a time series graph of each day to see percent of time in each operating mode
  df %>% mutate(Timestamp = Timestamp %>% with_tz(metadata$Timezone[metadata$Site_ID==site]),
                Operating_Mode = replace(Operating_Mode, !is.na(Operating_Mode) & 
                                           (Operating_Mode=="Heating-Off" | Operating_Mode=="Cooling-Off"), "Off")) %>%
    filter(Site_ID == site &
             Timestamp >= strptime(timestart,"%m/%d/%Y %H:%M") &
             Timestamp <= strptime(timeend,"%m/%d/%Y %H:%M")) %>%
    ggplot(aes(x=date, fill=Operating_Mode, y=1)) +
    geom_bar(position="fill", stat="identity") +
    labs(title=paste0("Percent of time in each operating mode per day for site ", site),x="", y="", fill="Operating Mode") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          panel.grid.major = element_line(size = 0.9),
          panel.grid.minor = element_line(size = 0.1),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5),) 
}
# OperatingModeTime("6950NE", "12/01/2022 00:00", "01/30/2023 00:00")


# Electricity usage vs. outdoor temperature
# The graph in the powerpoint has it grouped into 3-month intervals, but I'm
# trying one day intervals for now because our timeframe isn't as long
# only one site can be entered at a time
ElecUsage <- function(site, timestart, timeend){
  tempdf <- df %>% mutate(Timestamp = Timestamp %>% with_tz(metadata$Timezone[metadata$Site_ID==site])) %>%    
    filter(Site_ID == site &
             Timestamp >= strptime(timestart,"%m/%d/%Y %H:%M") &
             Timestamp <= strptime(timeend,"%m/%d/%Y %H:%M")) %>%
    group_by(date) %>% 
    summarize(AirTemp = mean(OA_TempF, na.rm=T),
              ElecUse = sum(Energy_kWh, na.rm=T))
  
  # Create transformation factor for secondary axis
  # Complicated because there can be positive and negative values for temp
  scale_factor <- (max(tempdf$ElecUse, na.rm=T) - min(tempdf$ElecUse, na.rm=T))/
    (max(tempdf$AirTemp, na.rm=T) - min(tempdf$AirTemp, na.rm=T))
  adj <- max(tempdf$ElecUse / scale_factor, na.rm=T) - max(tempdf$AirTemp, na.rm=T)
  
  ggplot(tempdf, aes(x = date)) + 
    geom_line(size = 1, aes(y = ElecUse / scale_factor - adj, color="Electricity Usage")) +
    geom_line(size = 1, aes(y = AirTemp, color="Average Outdoor Temperature")) +
    geom_point(size=2, aes(y = ElecUse / scale_factor - adj), color="red") +
    geom_point(size=2, aes(y = AirTemp), color="black") +
    scale_color_manual(values=c("black","red")) +
    scale_y_continuous(name = "Outdoor Temperature (F)",
                       sec.axis = sec_axis(~(. + adj)*scale_factor, name ="Electricity Use (kWh)")) +
    labs(title=paste0("Electricity Usage vs Outdoor Temperature for Site ",site),
         x="") +
    theme_bw() +
    theme(axis.ticks.y=element_blank(),
          panel.border = element_rect(colour = "black",fill=NA),
          legend.title = element_blank(),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5),)
  
}
# ElecUsage("4228VB", "12/01/2022 00:00", "12/30/2022 00:00")





### Outdoor Air Bin Graphs ----

# Heating capacity and output (Btu/h) vs outdoor air temperature
  # Only can select one site for this function
HeatCapacityOAT <- function(site, timestart, timeend){
  df %>% mutate(Timestamp = Timestamp %>% with_tz(metadata$Timezone[metadata$Site_ID==site])) %>%
    filter(Site_ID == site &
             Timestamp >= strptime(timestart,"%m/%d/%Y %H:%M") &
             Timestamp <= strptime(timeend,"%m/%d/%Y %H:%M") &
             OA_TempF <= 55) %>%
    group_by(temp_int = cut(OA_TempF,
                                     breaks=c(-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40,45,50,55))) %>% 
    filter(!is.na(temp_int)) %>%
    summarize(Heating_Load = mean(Heating_Load_Btu_h, na.rm=T),
              # HP_Heating_Capacity = mean(HP_Heating_Capacity_Btu_h, na.rm=T),
              HP_Heating_Output = mean(HP_Heat_Output_Btu_h, na.rm=T),
              Sys_Heating_Output = mean(HP_Heat_Output_Btu_h + Aux_Heat_Output_Btu_h, na.rm=T),
              Outdoor_Temp = mean(OA_TempF)) %>%
    ggplot(aes(x = temp_int)) + 
    # geom_line(size = 1, aes(y = HP_Heating_Capacity, color="HP Heating Capacity", group=1)) +
    geom_line(size = 1, aes(y = Heating_Load, color="Heating Load", group=1)) +
    geom_line(size = 1, aes(y = HP_Heating_Output, color="HP Heating Output", group=1)) +
    geom_line(size = 1, aes(y = Sys_Heating_Output, color="System Heating Output", group=1)) +
    # geom_point(size=2, aes(y = HP_Heating_Capacity), color="#D55E00") +
    geom_point(size=2, aes(y = Heating_Load), color="#CC79A7") +
    geom_point(size=2, aes(y = HP_Heating_Output), color="#009E73") +
    geom_point(size=2, aes(y = Sys_Heating_Output), color="#E69F00") +
    scale_color_manual(values=c("#CC79A7","#009E73","#E69F00")) +
    labs(title=paste0("Heating Capacity/Heating Load vs. Outdoor Air Temperature for Site ",site),
         x="Outdoor Temperature (F)",
         y="Load, Output, and Capcity (Btu/h)") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          legend.title = element_blank(),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5)) +
    guides(color=guide_legend(override.aes=list(size=3)))
}
# HeatCapacityOAT("4228VB", "12/01/2022 00:00", "12/30/2022 00:00")




### Site Comparison Graphs ----

# COP vs outdoor air temperature, hourly averages
  # This graph compares mutliple sites, so the timestart and timeend should be
  # entered in UTC.
Heat_COP <- function(site, timestart, timeend){
  df %>%     
    filter(Site_ID %in% site &
             Timestamp >= strptime(timestart,"%m/%d/%Y %H:%M") &
             Timestamp <= strptime(timeend,"%m/%d/%Y %H:%M") &
             OA_TempF <= 55) %>%
    group_by(Site_ID, temp_int = cut(OA_TempF,
                                     breaks=c(-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40,45,50,55))) %>% 
    summarize(COP = mean(HP_COP_Heating, na.rm=T)) %>%
  ggplot(aes(x = temp_int, y = COP, color = as.character(Site_ID), group=Site_ID)) + 
    geom_line(size = 1) +
    geom_point(size=2) +
    labs(title="Demonstrated COP vs. Outdoor Air Temperature",
         x="Outdoor Temperature (F)",
         y="COP",
         color = "Site") +
    theme_bw() +
    theme(axis.ticks.y=element_blank(),
          panel.border = element_rect(colour = "black",fill=NA),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5),)
}
# Heat_COP(unique(df$Site_ID), "12/01/2022 00:00", "12/30/2022 00:00")



# Supplemental resistance heat use compared to total energy use by temperature bin
  # Using 5 degree F intervals like the graph in the powerpoint.
  # This graph compares mutliple sites, so the timestart and timeend should be
  # entered in UTC.
AuxHeatUse <- function(site, timestart, timeend){
  df %>%     
    filter(Site_ID %in% site &
             Timestamp >= strptime(timestart,"%m/%d/%Y %H:%M") &
             Timestamp <= strptime(timeend,"%m/%d/%Y %H:%M") &
             OA_TempF <= 55) %>%
    group_by(Site_ID, temp_int = cut(OA_TempF,
                                     breaks=c(-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40,45,50,55))) %>% 
    summarize(SuppHeat = mean(Aux_Power / (AHU_Power + HP_Power), na.rm=T)) %>%
    filter(!is.na(temp_int)) %>%
    ggplot(aes(x = temp_int, y = SuppHeat, color = as.character(Site_ID), group = Site_ID)) + 
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(title="Supplement Heat Energy Ratio per Outdoor Temperature Bin",
         x="Outdoor Temperature (F)",
         y="Ratio of Supplemental Heat to Total Heat Energy (%)",
         color = "Site") +
    theme_bw() +
    theme(axis.ticks.y=element_blank(),
          panel.border = element_rect(colour = "black",fill=NA),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5),)
  
}
# AuxHeatUse(unique(df$Site_ID), "12/01/2022 00:00", "12/30/2022 00:00")


# Heat pump return and supply temperature for outdoor temperature bins
  # This graph compares mutliple sites, so the timestart and timeend should be
  # entered in UTC.
SupplyReturnTemp <- function(site, timestart, timeend){
  df %>%     
    filter(Site_ID %in% site &
             Timestamp >= strptime(timestart,"%m/%d/%Y %H:%M") &
             Timestamp <= strptime(timeend,"%m/%d/%Y %H:%M") &
             OA_TempF <= 55) %>%
    group_by(Site_ID, temp_int = cut(OA_TempF, breaks=c(-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40,45,50,55))) %>% 
    summarize(SupplyTemp = mean(SA_TempF, na.rm=T),
              ReturnTemp = mean(RA_TempF, na.rm=T)) %>%
    filter(!is.na(temp_int)) %>%
    ggplot(aes(x = temp_int, color = Site_ID)) + 
    geom_line(size = 1, aes(y = SupplyTemp, linetype="Supply Temperature", group=Site_ID)) +
    geom_line(size = 1, aes(y = ReturnTemp, linetype="Return Temperature", group=Site_ID)) +
    geom_point(size=2, aes(y = SupplyTemp), linetype="solid") +
    geom_point(size=2, aes(y = ReturnTemp), linetype="dashed") +
    scale_linetype_manual(values=c("dashed","solid")) +
    labs(title="Supply and Return Temperature per Outdoor Temperature Bin",
         x="Outdoor Temperature Bin (F)",
         y="Indoor Temperature (F)") +
    theme_bw() +
    theme(axis.ticks.y=element_blank(),
          panel.border = element_rect(colour = "black",fill=NA),
          legend.title = element_blank(),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5),)
  
}
# SupplyReturnTemp(unique(df$Site_ID), "12/01/2022 00:00", "12/30/2022 00:00")









## Loop through all graphs and print for each site
for (id in metadata$Site_ID){
  timestart = "12/01/2022 00:00"
  timeend = "01/31/2023 00:00"
  
  temp_time_series <- TempTimeSeries(id, 5, timestart, timeend)
  supply_temp_time_series <- SupplyTempTimeSeries(id, 5, timestart, timeend)
  power_time_series_oat <- PowerTimeSeriesOAT(id, 5, timestart, timeend)
  power_time_series_sa <- PowerTimeSeriesSA(id, 5, timestart, timeend)
  reverse_valve_time_series <- RevValveTimeSeries(id, 5, timestart, timeend)
  heat_capacity_oat <- HeatCapacityOAT(id, timestart, timeend)
  heat_capacity_time_series <- HeatOutputTimeSeries(id, 60, timestart, timeend)
  elec_usage <- ElecUsage(id, timestart, timeend)
  system_operation <- SystemOperationTimeSeries(id, timestart, timeend)
  runtime_time_series <- RunTimesTimeSeries(id, timestart, timeend)
  
  ggsave('Temperature_TimeSeries.png', plot=temp_time_series, path=paste0(wd,'/Graphs/',id), width=12, height=4, units='in')
  ggsave('Supply_Temperature_TimeSeries.png', plot=supply_temp_time_series, path=paste0(wd,'/Graphs/',id), width=12, height=4, units='in')
  ggsave('Power_TimeSeries_OAT.png', plot=power_time_series_oat, path=paste0(wd,'/Graphs/',id), width=12, height=4, units='in')
  ggsave('Power_TimeSeries_SA.png', plot=power_time_series_sa, path=paste0(wd,'/Graphs/',id), width=12, height=4, units='in')
  ggsave('Reverse_Valve_TimeSeries.png', plot=reverse_valve_time_series, path=paste0(wd,'/Graphs/',id), width=12, height=4, units='in')
  ggsave('HeatCapacity_HeatLoad_v_OAT.png', plot=heat_capacity_oat, path=paste0(wd,'/Graphs/',id), width=12, height=4, units='in')
  ggsave('HeatCapacity_HeatLoad_TimeSeries.png', plot=heat_capacity_time_series, path=paste0(wd,'/Graphs/',id), width=12, height=4, units='in')
  ggsave('Elec_Use_v_OAT.png', plot=elec_usage, path=paste0(wd,'/Graphs/',id), width=12, height=4, units='in')
  ggsave('Aux_HP_System_Operation_TimeSeries.png', plot=system_operation, path=paste0(wd,'/Graphs/',id), width=12, height=4, units='in')
  ggsave('Runtime_TimeSeries.png', plot=runtime_time_series, path=paste0(wd,'/Graphs/',id), width=12, height=4, units='in')
}


# Produce site comparison graphs
ggsave('HeatCOP_v_OAT.png', plot=Heat_COP(unique(df$Site_ID), "12/01/2022 00:00", "12/31/2022 00:00"), path=paste0(wd,'/Graphs/Site Comparison'), width=12, height=4, units='in')
ggsave('AuxHeatPercent_v_OAT.png', plot=AuxHeatUse(unique(df$Site_ID), "12/01/2022 00:00", "12/31/2022 00:00"), path=paste0(wd,'/Graphs/Site Comparison'), width=12, height=4, units='in')
ggsave('SA_RA_v_OAT.png', plot=SupplyReturnTemp(unique(df$Site_ID), "12/01/2022 00:00", "12/31/2022 00:00"), path=paste0(wd,'/Graphs/Site Comparison'), width=12, height=4, units='in')


