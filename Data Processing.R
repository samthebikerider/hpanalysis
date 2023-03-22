# Clear Global Environment
rm(list=ls())

# Open libraries
library(tidyverse)
library(lubridate)
library(stringr)
library(data.table)


### Data Load and Cleaning ----

# Set working library
  # As a string variable to make it easier to change to folders within directory
wd <- "C:/Users/keen930/PNNL/CCHP - Project Management - Project Management/Data Analysis"
# wd <- "/Users/rose775/Library/CloudStorage/OneDrive-PNNL/Desktop/Projects/Project Management/Data Analysis"

# Read data
  # Read_csv (tidyverse) is crashing RStudio, trying fread (data.table) which is 
  # supposed to be better with large files
read_plus_michaels <- function(file) {fread(file) %>% 
    select(index, RV_Volts, HP_Power, Fan_Power, AHU_Power, Aux1_Power, Aux2_Power, 
           Aux3_Power, Aux4_Power, OA_TempF, OA_RH, SA1_TempF, SA2_TempF, SA1_RH, 
           SA2_RH, RA_TempF, RA_RH, AHU_TempF, AHU_RH, Room1_TempF, Room1_RH, Room2_TempF, 
           Room2_RH, Room3_TempF, Room3_RH, SA3_TempF, SA3_RH, SA4_TempF, SA4_RH) %>% 
    mutate(Site_ID = substr(file, nchar(file)-20,nchar(file)-15),
           Timestamp =  force_tz(index, tzone = "UTC")) %>%
    filter(Site_ID %in% sites & 
           Timestamp >= timeframe[1] &
           Timestamp <= timeframe[2])}

read_plus_e350_min <- function(file) {fread(file) %>%
  mutate(Site_ID = substr(file, 112, 117),
         Timestamp = as.POSIXct(strptime(`Timestamp (UTC)`, tz="UTC","%m/%d/%Y %H:%M"))) %>%
    select(Site_ID, Timestamp,
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
  filter(Site_ID %in% sites &
         Timestamp >= timeframe[1] &
           Timestamp <= timeframe[2])
  }
read_plus_e350_sec <- function(file) {fread(file) %>%
    mutate(Site_ID = substr(file, 112, 117),
           Timestamp = as.POSIXct(strptime(`Timestamp (UTC)`, tz="UTC","%m/%d/%Y %H:%M:%S"))) %>%
    select(Site_ID,
           Timestamp,
           RV_Volts=`Reversing_Valve_Signal [VDC]`,
           HP_Power=`HP_Power [kW]`,
           Fan_Power=`FanPower [kW]`,
           AHU_Power=`AHU_Power [kW]`,
           Aux_Power=`Aux_Heat_Power [kW]`) %>%
    filter(Site_ID %in% sites & 
             Timestamp >= timeframe[1] &
             Timestamp <= timeframe[2])}

read_plus_nrcan_sec <- function(file) {fread(file) %>%
    # Modify filename so that it is the Site ID
    mutate(Site_ID = substr(file, 108, 113),
           # Convert to POSIXct and force TZ to US/Eastern (local), then change to UTC
           Timestamp = with_tz(as.POSIXct(strptime(Timestamp, tz="Canada/Eastern","%m/%d/%Y %H:%M:%S")),"UTC"),
           RV_Volts=`Leg 1 Voltage` + `Leg 2 Voltage`,
           HP_Power=`CCHP Outdoor Unit Leg 1 Instantaneous Power` + `CCHP Outdoor Unit Leg 2 Instantaneous Power`,
           Fan_Power=`CCHP Blower Leg 1 Instantaneous Power` + `CCHP Blower Leg 2 Instantaneous Power`,
           Aux_Power=`CCHP Heat Bank Leg 1 Instantaneous Power` + `CCHP Heat Bank Leg 2 Instantaneous Power`) %>%
    # select(Site_ID, Timestamp,RV_Volts,HP_Power,Fan_Power,Aux_Power) %>%
    filter(Site_ID %in% sites & 
             Timestamp >= timeframe[1] &
             Timestamp <= timeframe[2])}
read_plus_nrcan_min <- function(file) {fread(file) %>%
    # Modify filename so that it is the Site ID
    mutate(Site_ID = substr(file, 108, 113),
           # Convert to POSIXct and force TZ to US/Eastern (local), then change to UTC
           Timestamp = with_tz(as.POSIXct(strptime(Timestamp, tz="Canada/Eastern","%m/%d/%Y %H:%M:%S")),"UTC")) %>%
    select(Site_ID, Timestamp,
           SA1_TempC=`Supply T (oC)`,
           SA1_RH=`Supply RH (%RH)`,
           RA_TempC=`Return T (oC)`,
           RA_RH=`Return RH (%RH)`,
           AHU_TempC=`Ambient T (oC)`,
           AHU_RH=`Ambient RH (%RH)`,
           Room1_TempC=`Main Floor T-stat T (oC)`,
           Room1_RH=`Main Floor T-stat RH (%RH)`) %>%
    filter(Site_ID %in% sites & 
             Timestamp >= timeframe[1] &
             Timestamp <= timeframe[2])}

# Select sites to read
sites <- c(
           "2563EH",
           # "2896BR",
           # "4228VB",
           # "5291QJ",
           # "6950NE",
           # "8220XE",
           # "9944LD",
           # "5539NO",
           "")
timeframe <- c(strptime("1/10/2022", format="%m/%d/%Y", tz="UTC"), strptime("3/20/2023", format="%m/%d/%Y", tz="UTC"))

# Read Michaels/E350/NRCan data separately
df_michaels <- list.files(path = paste0(wd, "/Raw Data/Michaels"),pattern="*.csv", full.names=T) %>% 
  map_df(~read_plus_michaels(.)) %>% 
  as.data.frame()

  # E350 data read one-minute and one-second data separately
df_e350_min <- list.files(path = paste0(wd, "/Raw Data/Energy350/1-Minute"),pattern="*.csv", full.names=T) %>% 
  map_df(~read_plus_e350_min(.)) %>% 
  as.data.frame()
df_e350_sec <- list.files(path = paste0(wd, "/Raw Data/Energy350/1-Second"),pattern="*.csv", full.names=T) %>% 
  map_df(~read_plus_e350_sec(.)) %>% 
  as.data.frame()

  # NRCan data read one-minute and five-second separately
df_nrcan_min <- list.files(path = paste0(wd, "/Raw Data/NRCan/1-Minute"),pattern="*.csv", full.names=T) %>% 
  map_df(~read_plus_nrcan_min(.)) %>% 
  as.data.frame()
df_nrcan_sec <- list.files(path = paste0(wd, "/Raw Data/NRCan/5-Second"),pattern="*.csv", full.names=T) %>% 
  map_df(~read_plus_nrcan_sec(.)) %>% 
  as.data.frame()


# Metadata file to append time zone and other characteristics
metadata <- read_csv(file = paste0(wd, "/site-metadata.csv"))

# RV data from Trane for Site 4228VB
  # Note: Will need to pull site ID from filename if we get data from multiple sites.
  # This takes a little while, but could be sped up by creating a function that can filter
    # during each CSV reading instead of pulling all the data then filtering at the end.
trane_rv <- list.files(path = paste0(wd, "/Trane-RV-Thermostat-data/TraneTech_Nampa"),pattern="*.csv", full.names=T) %>% 
  map_df(~fread(.)) %>%
  as.data.frame() %>%
  mutate(Site_ID = "4228VB",
         Timestamp = as.POSIXct(strptime(DateTime, tz="US/Mountain", format="%m/%d/%Y %I:%M:%S %p") %>%
                                  with_tz(tzone="UTC"))) %>%
  select(Site_ID, Timestamp, DEFROST_ON_1) %>%
  filter(Site_ID %in% sites &
           Timestamp >= timeframe[1] &
           Timestamp <= timeframe[2]) %>%
  unique()
# Trane RV data investigation
# trane_NA_summary_table <- trane_rv %>%
#   group_by(Site_ID, date(with_tz(Timestamp, tzone="US/Mountain"))) %>%
#   summarize(Perc_NA_values = round(sum(is.na(DEFROST_ON_1))*100/ n(), 1),
#             Perc_complete_1sec_data = round(n()*100 / 86400, 1),
#             Perc_duplicated = round(100 - length(unique(Timestamp))*100 / n(), 1))
# write.csv(trane_NA_summary_table,
#           file=paste0(wd, "/Graphs/Trane_RV_Data_Summary.csv"),
#           row.names=F)

# Pull OAT data from Sam's python script
  # Will need to change if we get multiple sites
eew <- list.files(path = paste0(wd, "/ee_weather_data"),pattern="*.csv", full.names=T) %>% 
  map_df(~fread(.)) %>% 
  as.data.frame() %>%
  mutate(Site_ID = "5539NO",
         Timestamp = with_tz(force_tz(index, tzone="US/Eastern"), tz="UTC"),
         OA_TempF_EEW = .[[3]] * 9/5 + 32) %>%
  select(Site_ID, Timestamp, OA_TempF_EEW) %>%
  filter(Site_ID %in% sites &
           Timestamp >= timeframe[1] &
           Timestamp <= timeframe[2])




## Merge E350 dataframes together into one and clean
df_e350 <- merge(
  # Second-level data
  df_e350_sec %>%
  filter(!is.na(Timestamp)),
  # Minute-level data
  df_e350_min, 
  by=c("Site_ID", "Timestamp"), all.x=T, all.y=F) %>%
    # Merge in Trane RV data
  merge(trane_rv, by=c("Site_ID", "Timestamp"), all.x=T, all.y=F) %>%
    # Merge weather data
  merge(eew, by=c("Site_ID", "Timestamp"), all.x=T, all.y=F) %>%
  arrange(Site_ID, Timestamp) %>%
  mutate(SA3_TempF = NA, SA3_RH = NA, SA4_TempF = NA, SA4_RH = NA)

rm(df_e350_min, df_e350_sec)

  # Trane RV data is every four seconds for site 4228VB, so need to fill in gaps to defrost mode
    # There is some missing data, so we need to add a counter as to not interpolate
    # more than four missing rows in a row.
# trane_defrost_interp <- function(time, defrost_on, site){
#   counter = 0
#   for(row in 2:length(time)){
#     if(time[row] < trane_rv$Timestamp[1] | 
#        time[row] > trane_rv$Timestamp[nrow(trane_rv)] |
#        site[row] != "4228VB"){
#       # If outside of trane_rv time range or not the 4228VB site, skip
#       next
#     } else if (!is.na(defrost_on[row])){
#       # If is not NA, leave row as is and reset counter
#       counter = 0
#     } else if (counter >= 4){
#       # If counter is greater than or equal to four seconds, then keep skipping until we get a non-NA row
#       next
#     } else {
#       df_e350$DEFROST_ON_1[row] <- df_e350$DEFROST_ON_1[row-1]
#       counter = counter + 1
#     }
#   }
# }
# This function is taking way too long for me, so I'm making a much faster
# group_by option, not quite as accurate but I think good enough for our purposes.
  #df_e350$DEFROST_ON_1 <- trane_defrost_interp(df_e350$Timestamp, df_e350$DEFROST_ON_1, df_e350$Site_ID)
# Ideally, we would do 4-5 second breaks, but with data sometimes reporting less frequently,
  # 20 second breaks are safer for not missing any defrost points.
df_e350 <- df_e350 %>% group_by(Site_ID, Break=cut(Timestamp, breaks="20 secs")) %>%
  mutate(DEFROST_ON_1=ceiling(mean(DEFROST_ON_1, na.rm=T))) %>% ungroup() %>%
  select(-Break)

# Fill in missing OAT data for site 5539NO
df_e350 <- df_e350 %>% group_by(Site_ID, Break=cut(Timestamp, breaks="1 hour")) %>%
  mutate(OA_TempF_EEW = mean(OA_TempF_EEW, na.rm=T)) %>% ungroup() %>%
  mutate(OA_TempF = ifelse(is.na(OA_TempF) & Site_ID=="5539NO", OA_TempF_EEW, OA_TempF)) %>%
  select(-OA_TempF_EEW, -Break)

rm(trane_rv, eew)


## Merge NRCan dataframes together into one and clean
df_nrcan <- merge(
  # Second-level data
  df_nrcan_sec,
  # Minute-level data
  df_nrcan_min %>% 
    mutate(RA_TempF = 9/5*RA_TempC+32, SA1_TempF = 9/5*SA1_TempC+32, 
           Room1_TempF = 9/5*Room1_TempC+32, AHU_TempF = 9/5*AHU_TempC+32) %>%
    select(-RA_TempC, -SA1_TempC, -Room1_TempC, -AHU_TempC),
  by=c("Site_ID", "Timestamp"), all.x=T, all.y=F) %>% 
  arrange(Site_ID, Timestamp) %>%
  mutate(Room2_TempF = NA, Room2_RH = NA, Room3_TempF = NA, Room3_RH = NA, 
         Room4_TempF = NA, Room4_RH = NA, SA2_RH = NA, SA2_TempF = NA, 
         SA3_TempF = NA, SA3_RH = NA, SA4_TempF = NA, SA4_RH = NA, AHU_Power = NA,
         OA_TempF = NA, OA_RH = NA)

rm(df_nrcan_min, df_nrcan_sec)


## Remove dates with missing critical data
  # Note: filter is what we are keeping, so needs to be opposite of what removing.
  # I think it is best to remove entire days, to make energy calcs comparable on daily basis.
df_e350 <- df_e350 %>% 
  # Site 4228VB 
    # Appears to be missing Aux power data before Dec 20, 2022, doesn't stabilize until evening of 21st. 
  filter(Site_ID != "4228VB" | Timestamp >= strptime("2022-12-22", "%Y-%m-%d", tz="US/Mountain")) %>%
    # December 30th 18:00 to January 2nd 18:00, the HP Power, and possibly at times Aux Power, 
    # is missing or too low to be reasonable. Delete these days completely?
    # HP had "operational issues" 3/02/23 - 03/06/23 and should be removed
  filter(Site_ID != "4228VB" | Timestamp < strptime("2023-03-03", "%Y-%m-%d", tz="US/Mountain") | Timestamp > strptime("2023-03-06", "%Y-%m-%d", tz="US/Mountain")) %>%
  # Site 5539NO
    # All data is not present until 2/14/23 afternoon (still no OAT but using other data source as substitute)
  filter(Site_ID != "5539NO" | Timestamp >= strptime("2023-02-15", "%Y-%m-%d", tz="US/Eastern"))
  
df_michaels <- df_michaels %>%
  # Site 8220XE:
    # Data doesn't stabilize until Dec 12th at 12:00, use Dec 13th as starting point.
  filter(Site_ID != "8220XE" | Timestamp >= strptime("2022-12-13", "%Y-%m-%d", tz="US/Central")) %>%
  # Site 6950NE:
    # Data doesn't stabilize until December 10th, use this day as starting point.
  filter(Site_ID != "6950NE" | Timestamp >= strptime("2022-12-10", "%Y-%m-%d", tz="US/Central")) %>%
    # Site 6950NE has one very high OAT, apply filter for all sites:
  mutate(OA_TempF=replace(OA_TempF, OA_TempF > 150, NA)) %>%
  # Site 2896BR:
    # Data doesn't stabilize until Feb 1st, use this day as starting point.
  # Site 2563EH:
    # Data doesn't stabilize until Feb 1st, use this day as starting point.
  # Site 7083LM:
  # Site 61120H:
  # Site 9944LD:
    # Data doesn't stabilize until Jan 7th, use this day as starting point.
    # The indoor unit power data at site 9944LD is flipped negative between 1/7/23 and 1/12/23
  mutate(Fan_Power = ifelse(Site_ID == "9944LD" & Fan_Power < 0, - Fan_Power, Fan_Power),
         AHU_Power = ifelse(Site_ID == "9944LD" & AHU_Power < 0, - AHU_Power, AHU_Power),
         Aux1_Power = ifelse(Site_ID == "9944LD" & Aux1_Power < 0, - Aux1_Power, Aux1_Power),
         Aux2_Power = ifelse(Site_ID == "9944LD" & Aux2_Power < 0, - Aux2_Power, Aux2_Power),
         Aux3_Power = ifelse(Site_ID == "9944LD" & Aux3_Power < 0, - Aux3_Power, Aux3_Power),
         Aux4_Power = ifelse(Site_ID == "9944LD" & Aux4_Power < 0, - Aux4_Power, Aux4_Power))

# df_nrcan <- df_nrcan %>%
  # Site 5291QJ:




# Fill in missing temperature data for E350 and NRCan data (only have minute level)
  # Minute data (when seconds are zero) has temperature data and every second between minutes is NA.
  # Important that the data is sorted by site and then timestamp, which it should be.
  # The first process with a loop was taking too much time so I commented it out
  # and used grouping to make it faster.
# fillMissingTemp <- function(time, temp){
#   t = NA # Initialize temperature counter
#   
#   for(row in 1:length(time)){
#     if(second(time[row])==0){
#     # At the minute level, make a record of the temperatures
#       t = temp[row]
#     } else {
#     # And for all other data, record as the stored value
#       temp[row] = t
#     }
#   }
#   temp
# }
# df_e350$SA1_TempF <- fillMissingTemp(df_e350$Timestamp, df_e350$SA1_TempF)

df_e350 <- df_e350 %>% 
  group_by(Site_ID, Break=cut(Timestamp, breaks="1 min")) %>%
  mutate(SA1_TempF=mean(SA1_TempF, na.rm=T),
         SA2_TempF=mean(SA2_TempF, na.rm=T),
         OA_TempF=mean(OA_TempF, na.rm=T),
         RA_TempF=mean(RA_TempF, na.rm=T),
         SA1_RH=mean(SA1_RH, na.rm=T),
         SA2_RH=mean(SA2_RH, na.rm=T)) %>% 
  ungroup() %>%
  select(-Break)

  # NRCan
# Note: Only two SA monnits at the first site, but future sites may have four.
df_nrcan <- df_nrcan %>% 
  group_by(Site_ID, Break=cut(Timestamp, breaks="1 min")) %>%
  mutate(SA1_TempF=mean(SA1_TempF, na.rm=T),
         SA1_RH=mean(SA1_RH, na.rm=T),
         RA_TempF=mean(RA_TempF, na.rm=T)) %>% 
  ungroup() %>%
  select(-Break)



## Operating mode and defrost cycles ##
  # Objective: Create a column with operating mode: 
    # Heating-HP Only
    # Heating-Aux/HP
    # Heating-Aux Only
    # Heating-Off
    # Defrost
    # Note: Only applicable for winter--to determine cooling mode, will 
      # need to revisit this part and edit, maybe using OAT.
  # And then a column with the HP heat cycle run times and a column with the 
  # Defrost mode run times, which will be a length in time in minutes at the last
  # row of the respective cycle.

  # For Michaels/Lennox, 0V on RV indicates heating mode and 27V indicates cooling/defrost.
  # For the first data dump of the IA sites (before 2022-12-23 17:30 UTC), the RV_Volts seems to 
  # be off by a factor of 0.1, so that needs to be corrected manually here.
  # Use OAT to differentiate cooling and defrost mode, and HP and aux power to 
  # determine which type of heating mode or cooling mode it is in.
  # Also, calculate Aux_Power for Michaels data (E350 is already calculated)

df_michaels <- df_michaels %>% mutate(
  RV_Volts = ifelse(Timestamp < strptime("2022-12-23 17:30:00", format="%Y-%m-%d %H:%M:%S", tz="UTC"),
                    RV_Volts * 10, RV_Volts),
  Aux_Power = rowSums(cbind(Aux1_Power, Aux2_Power, Aux3_Power, Aux4_Power), na.rm=T),
  Operating_Mode = ifelse(RV_Volts < 25 | HP_Power < 0.1, 
                          ifelse(HP_Power > 0.1 & Aux_Power < 0.1, "Heating-HP Only",
                          ifelse(HP_Power < 0.1 & Aux_Power > 0.1, "Heating-Aux Only",
                          ifelse(HP_Power > 0.1 & Aux_Power > 0.1, "Heating-Aux/HP",
                          "Heating-Off"))),
                              "Defrost"))

  # For Energy350: "~24V is expected when reversing valve is in heating position"
    # Based on data collected, it looks like 0.6-3.0 V indicates defrost mode
    # For 4228VB, the pulse seems to be too short to be picked up consistently at the
    # 1-second level, so need to use dataset provided by Trane with "DEFROST_ON_1"
df_e350 <- df_e350 %>% mutate(
  Operating_Mode = 
    # Site 4228VB has different logic than other E350 sites
    ifelse(Site_ID == "4228VB",
           ifelse(  
             # Trane RV data is missing during the following time periods, so use secondary indicators
             (Timestamp >= strptime("2023-01-16", "%F", tz="US/Mountain") & Timestamp <= strptime("2023-01-30","%F",tz="US/Mountain") |
                Timestamp >= strptime("2023-02-22", "%F", tz="US/Mountain") & Timestamp <= strptime("2023-03-04","%F",tz="US/Mountain")) &
               # Secondary indicators for defrost mode
               Aux_Power > 4.0 & HP_Power > 0.1 & HP_Power < 1.25 & Fan_Power > 0.35, "Defrost",
             # For all other time periods, use RV indicator for defrost mode
             ifelse(!is.na(DEFROST_ON_1) & DEFROST_ON_1==1 & HP_Power > 0.1, "Defrost",
                    # For all time periods, use power to determine which heating mode
                    ifelse(HP_Power > 0.1 & Aux_Power < 0.1, "Heating-HP Only",
                           ifelse(HP_Power < 0.1 & Aux_Power > 0.1, "Heating-Aux Only",
                                  ifelse(HP_Power > 0.1 & Aux_Power > 0.1, "Heating-Aux/HP",
                                         "Heating-Off"))))),
    # Logic for non-4228VB sites
           ifelse(RV_Volts > 0.6 & RV_Volts < 3 & HP_Power > 0.2, "Defrost",
                  ifelse(HP_Power > 0.1 & Aux_Power < 0.1, "Heating-HP Only",
                      ifelse(HP_Power > 0.1 & Aux_Power > 0.1, "Heating-Aux/HP",
                         ifelse(HP_Power < 0.1 & Aux_Power > 0.1, "Heating-Aux Only",
                             ifelse(HP_Power < 0.1 & Aux_Power < 0.1, "Heating-Off",
                                    NA)))))))


df_nrcan <- df_nrcan %>% mutate(
  Operating_Mode = "Heating-HP Only"
)


# Row bind all data together
df <- rbind(
  df_e350 %>%
    select(Site_ID, Timestamp, RV_Volts, HP_Power, Fan_Power, AHU_Power, Aux_Power,
           OA_TempF, OA_RH, SA1_TempF, SA2_TempF, SA1_RH, SA2_RH, RA_TempF, 
           RA_RH, AHU_TempF, AHU_RH, Room1_TempF, Room1_RH, Room2_TempF, Room2_RH,
           Room3_TempF, Room3_RH, SA3_TempF, SA3_RH, SA4_TempF, SA4_RH, Operating_Mode,
           Room4_TempF, Room4_RH),
  # df_nrcan,
  df_michaels %>% 
    select(Site_ID, Timestamp, RV_Volts, HP_Power, Fan_Power, AHU_Power, Aux_Power,
           OA_TempF, OA_RH, SA1_TempF, SA2_TempF, SA1_RH, SA2_RH, RA_TempF, 
           RA_RH, AHU_TempF, AHU_RH, Room1_TempF, Room1_RH, Room2_TempF, Room2_RH,
           Room3_TempF, Room3_RH, SA3_TempF, SA3_RH, SA4_TempF, SA4_RH, Operating_Mode) %>%
    mutate(Room4_TempF = NA, Room4_RH = NA)) %>% arrange(Site_ID, Timestamp)

rm(df_michaels, df_e350, df_nrcan)


# Add fields for date, hour, and week day
  # Timestamp is in UTC. For hour and weekday, we want them to be in local time.
  # For date, we also want local time, but the date column cannot be stored with 
  # multiple timezones, so need to read it in the local timezone, and then force
  # it into a character vector so that we have it in local time. This will be 
  # important for the daily summary graphs.
  # This loop is a little slow.
for(id in unique(df$Site_ID)){ 
  df$Date[df$Site_ID==id] <- date(df$Timestamp[df$Site_ID==id] %>% 
                                    with_tz(tzone=metadata$Timezone[metadata$Site_ID==id])) %>%
   as.character()
  
  df$Hour[df$Site_ID==id] <- hour(df$Timestamp[df$Site_ID==id] %>% 
                                    with_tz(tzone=metadata$Timezone[metadata$Site_ID==id]))
  df$Weekday[df$Site_ID==id] <- lubridate::wday(df$Timestamp[df$Site_ID==id] %>% 
                                   with_tz(tzone=metadata$Timezone[metadata$Site_ID==id]),
                                   label=T) %>%
    as.character()
}


  # Supply temperature and humidity calculated as the average of the four quadrants
df <- df %>% mutate(
  SA_RH = rowMeans(cbind(SA1_RH, SA2_RH, SA3_RH, SA4_RH), na.rm=T),
  SA_TempF = rowMeans(cbind(SA1_TempF, SA2_TempF, SA3_TempF, SA4_TempF), na.rm=T))

  # Add column that determines the number of aux legs (when not in defrost mode)
df <- df %>% mutate(
  Number_Aux_Legs = ifelse(Operating_Mode == "Defrost" | Aux_Power < 0.1, NA,
                           ifelse(Aux_Power > 18, 4,
                                  ifelse(Aux_Power > 13, 3,
                                         ifelse(Aux_Power > 8, 2, 1)))))

# Calculate heat and defrost run cycle duration for heat pump.
  # The "mode" input should be "Heating" or "Defrost".
  # The script is set up to calculate EITHER heating run time or defrost runtime (or any other mode, e.g., cooling)
runCycleCalc <- function(site, timestamp, operate, mode){
  
  index <- which(operate == mode)[1]         # First row in heating/defrost mode
  ts <- timestamp[index]                     # Timestamp at first non-NA row
  cycle <- rep(NA, length(site))             # Initialize vector for cycle runtimes
  ct <- site[index]                          # Initialize counter to detect new site
  track <- TRUE                              # Tracker for new cycle
  
  for(row in (index+1):length(site)){
    
    if(site[row] != ct){
      # If there is a new site, do not calculate previous cycle. Update 'ct' with new site.
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

# df$HP_Cycle_Runtimes <- runCycleCalc(df$Site_ID, df$Timestamp, df$Operating_Mode, "Heating-HP Only")
df$Defrost_Cycle_Runtimes <- runCycleCalc(df$Site_ID, df$Timestamp, df$Operating_Mode, "Defrost")



### Calculations ----

  # Air supply (based on fan power curve)
df <- df %>% mutate(supply_flow_rate_CFM = 
  # Correlate fan power (kW) to volumetric flow rate (CFM) based on initial testing
  # Function will output the air supply in CFM for an input power in Watts
  # We will need to add a line for each site in this function.
  # Note: Michaels gave curves in W and E350 in kW, so Michaels' sites need * 1,000
    ifelse(Site_ID=="6950NE", 116.48 * (Fan_Power*1000) ^ 0.3866,
    ifelse(Site_ID=="8220XE", 152.27 * (Fan_Power*1000) ^ 0.3812,
    ifelse(Site_ID=="4228VB", 1647.7 * Fan_Power^0.394,
    ifelse(Site_ID=="9944LD", 115.09 * (Fan_Power*1000)^0.3926,
    ifelse(Site_ID=="2563EH", 169.1 * (Fan_Power*1000)^0.2978,
    ifelse(Site_ID=="2896BR", 108.64 * (Fan_Power*1000)^0.4405,
    ifelse(Site_ID=="6112OH", 98.457 * (Fan_Power*1000)^0.4346,
    ifelse(Site_ID=="7083LM", 120.66 * (Fan_Power*1000)^0.3657,
      NA)))))))))
 

  # Energy use
    # Calculate energy use at each timestamp as the power multiplied by the interval
    # since the last power reading.
    # This assumes that if there is missing data, the eGauge will report the first
    # point after a gap as the average of the gap.
## WE SHOULD CONFIRM IF THIS IS TRUE ^^^
    # Important that the data is sorted by site id and then timestamp, which it 
    # should be from previous code "arrange".

## I think there is an easier way to handle energy by dividing the sum by the timeframe
  # within the graph function instead of calculating here. This loop is time consuming,
  # So best to avoid if possible. ##
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
# df$Energy_kWh <- energyCalc(df$Site_ID, df$Timestamp, df$AHU_Power + df$HP_Power)


## Heating and cooling related calculations:
df <- df %>% mutate(
  
  # Partial Water Pressure (Pw) = RH * Pws; 
  # Pws = f(T); T in Fahrenheit, based on curve fit
  # 6th order poly fit: Pws = -0.000000001546*T^6 + 0.000000516256*T^5 - 0.000020306966*T^4 + 0.002266035021*T^3 + 0.190010315225*T^2 + 6.715900713408*T + 125.349159019000
  Partial_Water_Pressure_Supply = SA_RH / 100 * 
    (-0.000000001546*SA_TempF^6 + 
       0.000000516256*SA_TempF^5 - 
       0.000020306966*SA_TempF^4 + 
       0.002266035021*SA_TempF^3 + 
       0.190010315225*SA_TempF^2 + 
       6.715900713408*SA_TempF + 
       125.349159019000),
 
  # Humidity ratio of supply air (0.62198 * Pw / (P - Pw))
  # P = 97,717 Pascals at 1,000 ft elevation (we could use a more accurate look up for each location)
  Supply_Humidity_Ratio = 
    0.62198 * Partial_Water_Pressure_Supply / (97717 - Partial_Water_Pressure_Supply),
  
  # Heat Pump Heat Output
    # Q-heating = (dry air density) * (blower airflow rate) * (specific heat) * (delta Temp)
  HP_Heat_Output_Btu_h = ifelse(
    Operating_Mode == "Cooling", NA,
      0.0715 *                                                # Density of air at 35C (lb/ft3)
      supply_flow_rate_CFM * 60 *                             # CFM * min/hour
      (0.24 + 0.444 *  Supply_Humidity_Ratio) *               # Specific heat capacity (Btu/F-lb)
      (SA_TempF - RA_TempF)) -                                # Temperature delta
    Aux_Power * 3412,                                         # Subtract auxiliary power, convert kW to btu/hr

  # Auxiliary Heat Output
    # Electric resistance heating is expected to have one unit of power in to one unit of heat output
  Aux_Heat_Output_Btu_h = ifelse(
    Operating_Mode == "Cooling", NA, Aux_Power * 3412),

  # Cooling output
    # Q-cooling = (dry air density) * (blower airflow rate) * (specific heat) * (delta Temp) / (1 + Humidity Ratio)
  # HP_Cool_Output_Btu_h = ifelse(
  #   Operating_Mode == "Heating", NA,
  #     0.0765 *                                                          # Density of air at 15C (lb/ft3)
  #     supply_flow_rate_CFM * 60 *                                       # CFM * min/hour
  #     (0.24 + 0.444 *  Supply_Humidity_Ratio) *                         # Specific heat capacity (Btu/F-lb)
  #     (SA_TempF - RA_TempF) /
  #     (1 + Supply_Humidity_Ratio)),
  
  # Heating load
  Heating_Load_Btu_h = ifelse(
    Operating_Mode == "Cooling", NA,
    (AHU_Power + HP_Power) * 3412),                           # Convert total system power from kW to Btu/hr

  # Cooling load
  # Cooling_Load_Btu_h = ifelse(
  #   Operating_Mode == "Heating"|Operating_Mode=="Defrost", NA,
  #   (AHU_Power + HP_Power) * 3412),                           # Convert total system power from kW to Btu/hr


  # COP heating
    # VM: wondering if we should report COP with and without supplemental heat.
    # I personally think that supplemental heat should not be part of the COP
    # but we need to align this with the challenge spec.
    # KK: Right now heating capacity subtracts auxiliary heat component.
  HP_COP_Heating = HP_Heat_Output_Btu_h / 
    Heating_Load_Btu_h,

# COP cooling
  # HP_COP_Cooling = (-1 * HP_Cool_Output_Btu_h) / 
  #   Cooling_Load_Btu_h
)









### Miscellaneous Investigation Graphs ----
  # These are intended for custom analysis, not to print to folder for summary.
  # The time series are too difficult to view for full time frames, and 
  # these are not key parameters.

# Investigate time series for any variable
TimeSeries <- function(site, parameter, interval, timestart, timeend){
  # Look at a time series graph for a given parameter, time interval (e.g, 5-minute), time period, and site
  # Interval is in units of minutes, and so the maximum interval would be one hour.
  # The time start and end should be a date-time string in 24-hr format for example "4/01/2022 16:00".
  df %>% mutate(Timestamp = Timestamp %>% with_tz(metadata$Timezone[metadata$Site_ID==site]),
                Interval = minute(Timestamp) %/% interval) %>% 
    filter(Site_ID == site &
             Timestamp >= strptime(timestart,"%m/%d/%Y %H:%M", tz=metadata$Timezone[metadata$Site_ID==site]) &
             Timestamp <= strptime(timeend,"%m/%d/%Y %H:%M", tz=metadata$Timezone[metadata$Site_ID==site])) %>%
    group_by(Site_ID, Date, Hour, Interval) %>% 
    summarize(Timestamp = Timestamp[1],
              Parameter = mean(!!as.name(parameter),na.rm=T)) %>%
    ggplot(aes(x=as.POSIXct(Timestamp),y=Parameter, color=Site_ID)) +
    geom_line(size=0.3) + 
    # geom_hline(aes(yintercept = 0)) +
    labs(title="Time series plot",x="Timestamp",y=parameter, color="Site ID") +
    theme(axis.ticks.y=element_blank(),
          panel.border = element_rect(colour = "black",fill=NA)) +
    guides(color=guide_legend(override.aes=list(size=3)))
}
# TimeSeries("8220XE", "Room1_TempF", 1, "12/20/2022 0:00", "12/30/2022 0:00")


# Investigate NA values for any variable
NATimeSeries <- function(site, parameter, timestart, timeend){
  # Look at a time series graph for a given parameter, time period, and site.
  # The site can be a list of multiple sites if would like to compare.
  # The time start and end should be a date-time string in format for example "4/01/2022 08:00".
  df %>% mutate(Timestamp = Timestamp %>% with_tz(metadata$Timezone[metadata$Site_ID==site])) %>% 
    filter(Site_ID %in% site &
             Timestamp >= strptime(timestart,"%m/%d/%Y %H:%M", tz=metadata$Timezone[metadata$Site_ID==site]) &
             Timestamp <= strptime(timeend,"%m/%d/%Y %H:%M", tz=metadata$Timezone[metadata$Site_ID==site])) %>%
    mutate(NA_1 = ifelse(is.na(get(parameter)), 1, 0)) %>%
    ggplot(aes(x=as.POSIXct(Timestamp),y=NA_1, color=Site_ID)) +
    geom_point() + 
    ylim(c(-0.1, 1.1)) +
    labs(title=paste0("Time series plot of NA ", parameter, " values"),x="Timestamp",y="NA=1, Non-NA=0", color="Site ID") +
    theme(axis.ticks.y=element_blank(),
          panel.border = element_rect(colour = "black",fill=NA)) +
    guides(color=guide_legend(override.aes=list(size=3)))
}
NATimeSeries("4228VB", "SA_RH", "12/23/2022 00:00", "12/24/2022 00:00")


# Temperature time series comparison chart
TempTimeSeries <- function(site, interval, timestart, timeend){
  # Look at a time series graph for all temperature monitors, time interval (e.g, 5-minute), time period, and site
  # Interval is in units of minutes, and so the maximum interval would be one hour.
  # The time start and end should be a date string in format for example "4/01/2022".
  df %>% mutate(Timestamp = Timestamp %>% with_tz(metadata$Timezone[metadata$Site_ID==site]),
                Interval = minute(Timestamp) %/% interval) %>% 
    filter(Site_ID == site &
             Timestamp >= strptime(timestart,"%m/%d/%Y %H:%M", tz=metadata$Timezone[metadata$Site_ID==site]) &
             Timestamp <= strptime(timeend,"%m/%d/%Y %H:%M", tz=metadata$Timezone[metadata$Site_ID==site])) %>%
    group_by(Site_ID,Date, hour, Interval) %>% 
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


# Supply temperature time series comparison chart
SupplyTempTimeSeries <- function(site, interval, timestart, timeend){
  # Look at a time series graph for the four supply temperature monitors, time interval (e.g, 5-minute), time period, and site
  # Interval is in units of minutes, and so the maximum interval would be one hour.
  # The time start and end should be a Date string in format for example "4/01/2022".
  df %>% mutate(Timestamp = Timestamp %>% with_tz(metadata$Timezone[metadata$Site_ID==site]),
                Interval = minute(Timestamp) %/% interval) %>% 
    filter(Site_ID == site &
             Timestamp >= strptime(timestart,"%m/%d/%Y %H:%M", tz=metadata$Timezone[metadata$Site_ID==site]) &
             Timestamp <= strptime(timeend,"%m/%d/%Y %H:%M", tz=metadata$Timezone[metadata$Site_ID==site])) %>%
    group_by(Site_ID,Date, hour, Interval) %>% 
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




### NA Data Summary ----

for(id in unique(df$Site_ID)){
  write.csv(
    df %>%
      filter(Site_ID==id) %>%
      group_by(Site_ID, Date, Weekday) %>%
      summarize(HP_Power_NA = round(sum(is.na(HP_Power))*100/ n(), 1),
                Aux_Power_NA = round(sum(is.na(Aux_Power))*100/ n(),1),
                Fan_Power_NA = round(sum(is.na(Fan_Power))*100/ n(),1),
                RV_Volts_NA = round(sum(is.na(RV_Volts))*100/ n(),1),
                Duplicated_timestamps = round(sum(duplicated(Timestamp))*100/ n(),1),
                Duplicated_rows = round(100 - n_distinct(Timestamp, HP_Power, Aux_Power, Fan_Power)*100/ n(),1),
                Data_missing = round(100 - (n() - sum(duplicated(Timestamp)))*100/ 86400, 1)),
            file=paste0(wd, "/Graphs/", id, "/Missing_Power_Data_Summary_", id, ".csv"),
            row.names=F)
}


### Time Series Daily Investigation Plots ----

# Investigate defrost cycles for every day
DefrostCycleTimeSeries <- function(site, timestart, timeend){
  # Look at a time series graph for defrost run times and total duration
  # as compared to HP, Aux, and Fan power and SA temperature
  # The time start and end should be character with format "%Y-%m-%d".
  df %>% mutate(Timestamp = Timestamp %>% with_tz(metadata$Timezone[metadata$Site_ID==site]),
                Defrost = ifelse(Operating_Mode=="Defrost", 5, NA)) %>% 
    filter(Site_ID == site &
             Timestamp >= strptime(timestart,"%Y-%m-%d", tz=metadata$Timezone[metadata$Site_ID==site]) &
             Timestamp <= strptime(timeend,"%Y-%m-%d", tz=metadata$Timezone[metadata$Site_ID==site])) %>%
    ggplot(aes(x=as.POSIXct(Timestamp))) +
    geom_line(aes(y=HP_Power, color = "Outdoor Unit Power"),size=0.3) + 
    geom_line(aes(y=Fan_Power, color = "Supply Fan Power"),size=0.3) +
    geom_line(aes(y=Aux_Power, color = "Auxiliary Power"),size=0.3) + 
    geom_point(aes(y=Defrost, color = "Defrost Mode On"),size=2) + 
    geom_point(aes(y=Defrost_Cycle_Runtimes/2, color = "Defrost Cycle Length"),size=3,shape=8) +
    scale_y_continuous(name = "Power (kW)",
                       limits = c(-0.5, 21),
                       sec.axis = sec_axis(~.*2, name ="Defrost Cycle Length (mins)")) +
    scale_color_manual(name = "", breaks = c("Auxiliary Power","Outdoor Unit Power","Supply Fan Power","Defrost Mode On","Defrost Cycle Length"),
                       values = c("#E69F00","black","#CC79A7","#009E73","#56B4E9", "gray", "#F0E442", "#0072B2", "#D55E00")) +
    labs(title=paste0("Defrost runtime chart for site ", site),x="") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          panel.grid.major = element_line(size = 0.5),
          panel.grid.minor = element_line(size = 0.1),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5)) +
    guides(color=guide_legend(override.aes = list(shape=c(NA,NA,NA,16,8), 
                                                  size=c(1,1,1,3,3),
                                                  linetype=c(1,1,1,NA,NA))))
}
# DefrostCycleTimeSeries("2896BR", "2023-02-05", "2023-02-06")


# Power time series comparison chart with OAT and SAT
OperationTimeSeries <- function(site, timestart, timeend){
  # Look at a time series graph for all temperature monitors, time interval (e.g, 5-minute), time period, and site
  # Interval is in units of minutes, and so the maximum interval would be one hour.
  # The time start and end should be character with format "%Y-%m-%d".
  df %>% mutate(Timestamp = Timestamp %>% with_tz(metadata$Timezone[metadata$Site_ID==site])) %>% 
    filter(Site_ID == site &
             Timestamp >= strptime(timestart,"%Y-%m-%d", tz=metadata$Timezone[metadata$Site_ID==site]) &
             Timestamp <= strptime(timeend,"%Y-%m-%d", tz=metadata$Timezone[metadata$Site_ID==site])) %>%
    ggplot(aes(x=as.POSIXct(Timestamp))) +
    geom_line(aes(y=OA_TempF/5, color = "Outdoor Air Temperature"),size=0.3) + 
    geom_line(aes(y=SA_TempF/5, color = "Supply Air Temperature"),size=0.3) +
    geom_line(aes(y=HP_Power, color = "Outdoor Unit Power"),size=0.3) + 
    geom_line(aes(y=Fan_Power, color = "Supply Fan Power"),size=0.3) +
    geom_line(aes(y=Aux_Power, color = "Auxiliary Power"),size=0.3) + 
    scale_y_continuous(name = "Power (kW)",
                       limits = c(-4, 25),
                       sec.axis = sec_axis(~.*5, name ="Temperature (F)")) +
    scale_color_manual(name = "", breaks = c("Auxiliary Power","Outdoor Unit Power","Supply Fan Power","Outdoor Air Temperature","Supply Air Temperature"),
                       values = c("#E69F00","gray","#009E73","black","#56B4E9","#CC79A7", "#F0E442", "#0072B2", "#D55E00")) +
    labs(title=paste0("System operation time series plot for site ", site),x="") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          panel.grid.major = element_line(size = 0.5),
          panel.grid.minor = element_line(size = 0.1),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5)) +
    guides(color=guide_legend(override.aes=list(size=3)))
}
# OperationTimeSeries("6950NE", "2023-01-25", "2023-01-26")


# Heating output (Btu/h) and heating load with outdoor air temperature as timeseries
HeatOutputTimeSeries <- function(site, timestart, timeend){
  df %>% mutate(Timestamp = Timestamp %>% with_tz(metadata$Timezone[metadata$Site_ID==site])) %>% 
    filter(Site_ID == site &
             Timestamp >= strptime(timestart,"%Y-%m-%d", tz=metadata$Timezone[metadata$Site_ID==site]) &
             Timestamp <= strptime(timeend,"%Y-%m-%d", tz=metadata$Timezone[metadata$Site_ID==site])) %>%
    ggplot(aes(x = as.POSIXct(Timestamp))) + 
    geom_line(size = 0.3, aes(y = HP_Heat_Output_Btu_h, color="HP Heating Output")) +
    geom_line(size = 0.3, aes(y = Heating_Load_Btu_h, color="Total Heating Load")) +
    geom_line(size = 0.3, aes(y = Aux_Heat_Output_Btu_h, color="Aux Heating Output")) +
    geom_line(size = 0.3, aes(y = OA_TempF*1000, color="Outdoor Air Temperature")) +
    scale_color_manual(name = "", values = c("#E69F00", "#56B4E9", "black", "#009E73", "#CC79A7", "#F0E442", "#0072B2", "#D55E00")) +
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
# HeatOutputTimeSeries("4228VB", "2023-02-06", "2023-02-08")





### Time Series Long Term Graphs ----


# Number of defrost run cycles and average length of cycle per day
RunTimesTimeSeries <- function(site, timestart, timeend){
  # Look at a time series graph to see for every day, how many run cycles there are
  # and the average length of a cycle is. Plot against outdoor air temperature and humidity.
  df %>% mutate(Timestamp = Timestamp %>% with_tz(metadata$Timezone[metadata$Site_ID==site])) %>%
    filter(Site_ID == site &
             Timestamp >= strptime(timestart,"%m/%d/%Y %H:%M", tz=metadata$Timezone[metadata$Site_ID==site]) &
             Timestamp <= strptime(timeend,"%m/%d/%Y %H:%M", tz=metadata$Timezone[metadata$Site_ID==site])) %>%
    group_by(Date) %>% 
    summarize(Timestamp = Timestamp[1],
              Num_Defrost_Cycles = sum(Defrost_Cycle_Runtimes, na.rm=T),
              Average_Defrost_Runtime = mean(Defrost_Cycle_Runtimes,na.rm=T),
              OA_Temp = mean(OA_TempF,na.rm=T),
              OA_RH = mean(OA_RH,na.rm=T)) %>%
    ggplot(aes(x=as.POSIXct(Timestamp))) +
    geom_line(aes(y = OA_Temp/2, color="Outdoor Temperature", group=1)) +
    geom_line(aes(y = OA_RH/2, color="Outdoor Humidity", group=1)) +
    geom_point(size = 2, aes(y = Num_Defrost_Cycles, color="Number of Defrost Cycles", group=1)) +
    geom_point(size = 2, aes(y = Average_Defrost_Runtime, color="Average Defrost Cycle Length", group=1)) +
    scale_y_continuous(name = "Number of Cycles/Average Cycle Length (mins)",
                       sec.axis = sec_axis(~.*2, name ="Humidity (%)/Temperature (F)")) +
    scale_color_manual(name = "", values = c("#D55E00","#009E73","grey", "black","#CC79A7","#E69F00","#F0E442")) +
    labs(title=paste0("Defrost cycles and outdoor temperature and humidity for site ", site),x="") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          panel.grid.major = element_line(size = 0.9),
          panel.grid.minor = element_line(size = 0.1),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5),) +
    guides(color=guide_legend(override.aes=list(size=3)))
}
# RunTimesTimeSeries("2563EH", "2/01/2023 00:00", "2/10/2023 00:00")

# Operating mode daily summary
OperatingModeTime <- function(site, timestart, timeend){
  # Look at a time series graph of each day to see percent of time in each operating mode
  df %>% mutate(Timestamp = Timestamp %>% with_tz(metadata$Timezone[metadata$Site_ID==site]),
                Operating_Mode = replace(Operating_Mode, !is.na(Operating_Mode) & 
                                           (Operating_Mode=="Heating-Off" | Operating_Mode=="Cooling-Off"), "Off")) %>%
    filter(Site_ID == site &
             Timestamp >= strptime(timestart,"%m/%d/%Y %H:%M", tz=metadata$Timezone[metadata$Site_ID==site]) &
             Timestamp <= strptime(timeend,"%m/%d/%Y %H:%M", tz=metadata$Timezone[metadata$Site_ID==site])) %>%
    ggplot(aes(x=Date, fill=Operating_Mode, y=1)) +
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
# OperatingModeTime("2563EH", "2/01/2023 00:00", "2/10/2023 00:00")


# Electricity usage vs. outdoor temperature
# The graph in the powerpoint has it grouped into 3-month intervals, but I'm
# trying one day intervals for now because our timeframe isn't as long
# only one site can be entered at a time
ElecUsage <- function(site, timestart, timeend){
  tempdf <- df %>% mutate(Timestamp = Timestamp %>% with_tz(metadata$Timezone[metadata$Site_ID==site])) %>%    
    filter(Site_ID == site &
             Timestamp >= strptime(timestart,"%m/%d/%Y %H:%M", tz=metadata$Timezone[metadata$Site_ID==site]) &
             Timestamp <= strptime(timeend,"%m/%d/%Y %H:%M", tz=metadata$Timezone[metadata$Site_ID==site])) %>%
    group_by(Date) %>% 
    summarize(AirTemp = mean(OA_TempF, na.rm=T),
              ElecUse = sum(Energy_kWh, na.rm=T))
  
  # Create transformation factor for secondary axis
  # Complicated because there can be positive and negative values for temp
  scale_factor <- (max(tempdf$ElecUse, na.rm=T) - min(tempdf$ElecUse, na.rm=T))/
    (max(tempdf$AirTemp, na.rm=T) - min(tempdf$AirTemp, na.rm=T))
  adj <- max(tempdf$ElecUse / scale_factor, na.rm=T) - max(tempdf$AirTemp, na.rm=T)
  
  ggplot(tempdf, aes(x = Date)) + 
    geom_line(size = 1, aes(y = ElecUse / scale_factor - adj, color="Electricity Usage", group = 1)) +
    geom_line(size = 1, aes(y = AirTemp, color="Average Outdoor Temperature", group = 1)) +
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
# ElecUsage("2563EH", "2/01/2023 00:00", "2/10/2023 00:00")





### Outdoor Air Bin Graphs ----

# Heating load and output (Btu/h) vs outdoor air temperature
HeatOutputOAT <- function(site, timestart, timeend){
  df %>% mutate(Timestamp = Timestamp %>% with_tz(metadata$Timezone[metadata$Site_ID==site])) %>%
    filter(Site_ID == site &
             Timestamp >= strptime(timestart,"%m/%d/%Y %H:%M", tz=metadata$Timezone[metadata$Site_ID==site]) &
             Timestamp <= strptime(timeend,"%m/%d/%Y %H:%M", tz=metadata$Timezone[metadata$Site_ID==site]) &
             OA_TempF <= 55) %>%
    group_by(temp_int = cut(OA_TempF,
                                     breaks=c(-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40,45,50,55))) %>% 
    filter(!is.na(temp_int)) %>%
    summarize(Heating_Load = mean(Heating_Load_Btu_h, na.rm=T),
              HP_Heating_Output = mean(HP_Heat_Output_Btu_h, na.rm=T),
              Aux_Heating_Output = mean(Aux_Heat_Output_Btu_h, na.rm=T),
              Outdoor_Temp = mean(OA_TempF)) %>%
    ggplot(aes(x = temp_int)) + 
    geom_line(size = 1, aes(y = Heating_Load, color="Heating Load", group=1)) +
    geom_line(size = 1, aes(y = HP_Heating_Output, color="HP Heating Output", group=1)) +
    geom_line(size = 1, aes(y = Aux_Heating_Output, color="Aux Heating Output", group=1)) +
    scale_color_manual(values=c("#CC79A7","#009E73","#E69F00")) +
    labs(title=paste0("Heating Output/Heating Load vs. Outdoor Air Temperature for Site ",site),
         x="Outdoor Temperature (F)",
         y="Heating Load and Output (Btu/h)") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          legend.title = element_blank(),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5)) +
    guides(color=guide_legend(override.aes=list(size=3)))
}
# HeatOutputOAT("2563EH", "2/01/2023 00:00", "2/10/2023 00:00")

# Heating load and output (Btu/h) vs outdoor air temperature
   # This one needs work
# COPAuxHPOAT <- function(site, timestart, timeend){
#   df %>% mutate(Timestamp = Timestamp %>% with_tz(metadata$Timezone[metadata$Site_ID==site])) %>%
#     filter(Site_ID == site &
#              Timestamp >= strptime(timestart,"%m/%d/%Y %H:%M", tz=metadata$Timezone[metadata$Site_ID==site]) &
#              Timestamp <= strptime(timeend,"%m/%d/%Y %H:%M", tz=metadata$Timezone[metadata$Site_ID==site]) &
#              OA_TempF <= 55) %>%
#     group_by(temp_int = cut(OA_TempF,
#                             breaks=c(-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40,45,50,55))) %>% 
#     filter(!is.na(temp_int)) %>%
#     summarize(COP_HP = mean(HP_COP_Heating[Operating_Mode=="Heating-HP Only" | Operating_Mode=="Defrost"], na.rm=T),
#               COP_HP_Aux = mean(HP_COP_Heating[Operating_Mode=="Defrost" | Operating_Mode=="Heating-Aux/HP"], na.rm=T),
#               COP_System = mean(HP_COP_Heating[Operating_Mode != "Heating-Off" & !is.na(Operating_Mode)], na.rm=T),
#               Outdoor_Temp = mean(OA_TempF)) %>%
#     ggplot(aes(x = temp_int)) + 
#     geom_line(size = 1, aes(y = COP_HP, color="COP - HP Only", group=1)) +
#     geom_line(size = 1, aes(y = COP_HP_Aux, color="COP - HP & Aux Only", group=1)) +
#     geom_line(size = 1, aes(y = COP_System, color="COP - System Total", group=1)) +
#     scale_color_manual(values=c("#CC79A7","#009E73","#E69F00")) +
#     labs(title=paste0("COP By Mode vs. Outdoor Air Temperature for Site ",site),
#          x="Outdoor Temperature (F)",
#          y="COP") +
#     theme_bw() +
#     theme(panel.border = element_rect(colour = "black",fill=NA),
#           legend.title = element_blank(),
#           plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
#           axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
#           axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5)) +
#     guides(color=guide_legend(override.aes=list(size=3)))
# }
# COPAuxHPOAT("2563EH", "2/01/2023 00:00", "2/10/2023 00:00")



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
# Heat_COP(unique(df$Site_ID), "2/01/2023 00:00", "2/10/2023 00:00")



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
# AuxHeatUse(unique(df$Site_ID), "2/01/2023 00:00", "2/10/2023 00:00")


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
# SupplyReturnTemp(unique(df$Site_ID), "2/01/2023 00:00", "2/10/2023 00:00")





### Print Graphs to Folder ----

# Loop to print daily operation time series graphs, one for each day for each site
for(id in unique(df$Site_ID)){
  for(d in unique(df$Date[df$Site_ID==id])){
    d1 = substr(as.character(strptime(d, "%Y-%m-%d", tz=metadata$Timezone[metadata$Site_ID==id]) + 60*60*24), 1, 10) # Date plus one day
    ggsave(paste0(id, '_Daily-Operation_',d,'.png'),
           plot = OperationTimeSeries(id, d, d1),
           path = paste0(wd,'/Graphs/',id, '/Daily Operation/'),
           width=12, height=4, units='in')
  }
}
rm(d1,d,id)

# Loop to print daily defrost time series graphs, one for each day for each site
for(id in unique(df$Site_ID)){
  for(d in unique(df$Date[df$Site_ID==id])){
    d1 = substr(as.character(strptime(d, "%Y-%m-%d", tz=metadata$Timezone[metadata$Site_ID==id]) + 60*60*24), 1, 10) # Date plus one day
    ggsave(paste0(id, '_Daily-Defrost-Cycles_',d,'.png'),
           plot = DefrostCycleTimeSeries(id, d, d1),
           path = paste0(wd,'/Graphs/',id, '/Daily Defrost Cycles/'),
           width=12, height=4, units='in')
  }
}
rm(d1,d,id)







## Loop through individual site diagnostic graphs
# for (id in metadata$Site_ID){
#   timestart = "12/01/2022 00:00"
#   timeend = "01/31/2023 00:00"
#   
#   heat_capacity_oat <- HeatCapacityOAT(id, timestart, timeend)
#   heat_capacity_time_series <- HeatOutputTimeSeries(id, 60, timestart, timeend)
#   elec_usage <- ElecUsage(id, timestart, timeend)
#   system_operation <- SystemOperationTimeSeries(id, timestart, timeend)
#   runtime_time_series <- RunTimesTimeSeries(id, timestart, timeend)
#   
#   ggsave('HeatCapacity_HeatLoad_v_OAT.png', plot=heat_capacity_oat, path=paste0(wd,'/Graphs/',id), width=12, height=4, units='in')
#   ggsave('HeatCapacity_HeatLoad_TimeSeries.png', plot=heat_capacity_time_series, path=paste0(wd,'/Graphs/',id), width=12, height=4, units='in')
#   ggsave('Elec_Use_v_OAT.png', plot=elec_usage, path=paste0(wd,'/Graphs/',id), width=12, height=4, units='in')
#   ggsave('Aux_HP_System_Operation_TimeSeries.png', plot=system_operation, path=paste0(wd,'/Graphs/',id), width=12, height=4, units='in')
#   ggsave('Runtime_TimeSeries.png', plot=runtime_time_series, path=paste0(wd,'/Graphs/',id), width=12, height=4, units='in')
# }


# Produce site comparison graphs
ggsave('HeatCOP_v_OAT.png', plot=Heat_COP(unique(df$Site_ID), "12/01/2022 00:00", "12/31/2022 00:00"), path=paste0(wd,'/Graphs/Site Comparison'), width=12, height=4, units='in')
ggsave('AuxHeatPercent_v_OAT.png', plot=AuxHeatUse(unique(df$Site_ID), "12/01/2022 00:00", "12/31/2022 00:00"), path=paste0(wd,'/Graphs/Site Comparison'), width=12, height=4, units='in')
ggsave('SA_RA_v_OAT.png', plot=SupplyReturnTemp(unique(df$Site_ID), "12/01/2022 00:00", "12/31/2022 00:00"), path=paste0(wd,'/Graphs/Site Comparison'), width=12, height=4, units='in')


