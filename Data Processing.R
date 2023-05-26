# Clear Global Environment
rm(list=ls())

# Open libraries
library(tidyverse)
library(lubridate)
library(stringr)
library(data.table)

# df <- read.csv(paste0(wd, "/Raw Data/PNNL_ccASHP__2896BR_FEB-MAR.csv")) %>% 
#   select(index, RV_Volts, HP_Power, Fan_Power, AHU_Power, Aux1_Power, Aux2_Power, 
#   Aux3_Power, Aux4_Power, OA_TempF, OA_RH, SA1_TempF, SA2_TempF, SA1_RH, 
#   SA2_RH, RA_TempF, RA_RH, AHU_TempF, AHU_RH, Room1_TempF, Room1_RH, Room2_TempF, 
#   Room2_RH, Room3_TempF, Room3_RH, SA3_TempF, SA3_RH, SA4_TempF, SA4_RH)
# write.csv(df, paste0(wd, "/Raw Data/PNNL_ccASHP__2896BR_2023-03-13.csv"))

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
           RV_Volts=NA,
           HP_Power=`CCHP Outdoor Unit Leg 1 Instantaneous Power`+`CCHP Outdoor Unit Leg 2 Instantaneous Power`,
           Fan_Power=`CCHP Blower Leg 1 Instantaneous Power`+`CCHP Blower Leg 2 Instantaneous Power`,
           Aux1_Power=`CCHP Heat Bank Stage 1 Leg 1 Inst Power`*2,
           Aux2_Power=`CCHP Heat Bank Stage 2 Leg 1 Inst Power`*2,
           Aux3_Power=`CCHP Heat Bank Stage 3 Leg 1 Inst Power`*2) %>%
    select(Site_ID, Timestamp,RV_Volts,HP_Power,Fan_Power,Aux1_Power,Aux2_Power,Aux3_Power) %>%
    filter(Site_ID %in% sites &
             Timestamp >= timeframe[1] &
             Timestamp <= timeframe[2])}
read_plus_nrcan_min <- function(file) {fread(file) %>%
    # Modify filename so that it is the Site ID
    mutate(Site_ID = substr(file, 108, 113),
           # Convert to POSIXct and force TZ to US/Eastern (local), then change to UTC
           Timestamp = with_tz(as.POSIXct(strptime(Timestamp, tz="Canada/Eastern","%m/%d/%Y %H:%M")),"UTC"),
           SA1_TempF=9/5*`Supply T (oC)`+32,
           SA1_RH=`Supply RH (%RH)`,
           RA_TempF=9/5*`Return T (oC)`+32,
           RA_RH=`Return RH (%RH)`,
           OA_TempF=9/5*`Ambient T (oC)`+32,
           OA_RH=`Ambient RH (%RH)`,
           Room1_TempF=9/5*`Main Floor T-stat T (oC)`+32,
           Room1_RH=`Main Floor T-stat RH (%RH)`,
           supply_flow_rate_CFM=`Airflow (CFM)`) %>%
    select(Site_ID,Timestamp,SA1_TempF,SA1_RH,RA_TempF,RA_RH,OA_TempF,OA_RH,Room1_TempF,Room1_RH,supply_flow_rate_CFM) %>%
    filter(!is.na(Timestamp) & Site_ID %in% sites &
             Timestamp >= timeframe[1] &
             Timestamp <= timeframe[2])}

# Metadata file to append time zone and other characteristics
metadata <- read_csv(file = paste0(wd, "/site-metadata.csv"))

# Select sites to read
sites <- c(
  # "2563EH",
  # "2896BR",
  # "6112OH",
  # "6950NE",
  # "7083LM",  # Still no data for this site.
  # "8220XE",
  # "8726VB",
  "9944LD",
  # "4228VB",
  # "5539NO",
  # "5291QJ",
  # "2458CE",
  "")
timeframe <- c(strptime("12/10/2022", format="%m/%d/%Y", tz=metadata$Timezone[metadata$Site_ID==sites[1]]), 
               strptime("4/01/2023", format="%m/%d/%Y", tz=metadata$Timezone[metadata$Site_ID==sites[1]]))

# Read Michaels/E350/NRCan data separately
df_michaels <- list.files(path = paste0(wd, "/Raw Data/Michaels"),pattern=sites[1], full.names=T) %>% 
  map_df(~read_plus_michaels(.)) %>% 
  as.data.frame() %>% mutate(
    # Correct RV Volts for before Dec 23--off by a factor of 10
  RV_Volts = ifelse(Timestamp < strptime("2022-12-23 17:30:00", format="%Y-%m-%d %H:%M:%S", tz="UTC"), RV_Volts * 10, RV_Volts),
    # The indoor unit power data at site 9944LD is flipped negative between 1/7/23 and 1/12/23
    # Aux power is flipped negative at 8726VB except Mar 10 - 11
  Fan_Power = ifelse(Site_ID == "9944LD" & Fan_Power < 0, - Fan_Power, Fan_Power),
  AHU_Power = ifelse(Site_ID == "9944LD" & AHU_Power < 0, - AHU_Power, AHU_Power),
  Aux1_Power = ifelse(Site_ID %in% c("9944LD","8726VB") & Aux1_Power < 0, - Aux1_Power, Aux1_Power),
  Aux2_Power = ifelse(Site_ID %in% c("9944LD","8726VB") & Aux2_Power < 0, - Aux2_Power, Aux2_Power),
  Aux3_Power = ifelse(Site_ID %in% c("9944LD","8726VB") & Aux3_Power < 0, - Aux3_Power, Aux3_Power),
  Aux4_Power = ifelse(Site_ID %in% c("9944LD","8726VB") & Aux4_Power < 0, - Aux4_Power, Aux4_Power),
    # Create Aux_Power as sum of individual legs (rowSums defaults to zero if all NA I think)
  Aux_Power = ifelse(is.na(Aux1_Power) & is.na(Aux2_Power) & is.na(Aux3_Power) & is.na(Aux4_Power), NA,
                     rowSums(cbind(Aux1_Power, Aux2_Power, Aux3_Power, Aux4_Power), na.rm=T)),
  Room4_TempF=NA, Room4_RH=NA, supply_flow_rate_CFM=NA, DEFROST_ON_1 = NA) %>%
    # There are some overlaps in data from their data dumps--remove duplicated rows (note this function take a bit of time)
  distinct(Timestamp, HP_Power, Aux_Power, Fan_Power, .keep_all = T) %>%
    # Some rows have duplicated timestamps with one having NAs for HP_Power or Fan_Power--remove the NAs
  arrange(Site_ID, Timestamp, HP_Power, Fan_Power) %>% filter(!duplicated(Timestamp) | (!is.na(Fan_Power) & !is.na(HP_Power)))

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
  as.data.frame() %>% mutate(
    # Create Aux_Power as sum of individual legs
    Aux_Power = ifelse(is.na(Aux1_Power) & is.na(Aux2_Power) & is.na(Aux3_Power), NA,
                       rowSums(cbind(Aux1_Power, Aux2_Power, Aux3_Power), na.rm=T)))


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

# Pull OAT data from Sam's python script
  # Will need to change if we get multiple sites
eew <- list.files(path = paste0(wd, "/ee_weather_data"),pattern="*.csv", full.names=T) %>% 
  map_df(~fread(.)) %>% 
  as.data.frame() %>%
  mutate(Site_ID = "5539NO",
         Timestamp = with_tz(force_tz(timestamp, tzone="UTC"), tz="UTC"),
         OA_TempF_EEW = outdoor_air_temp_degC * 9/5 + 32) %>%
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
  mutate(SA3_TempF = NA, SA3_RH = NA, SA4_TempF = NA, SA4_RH = NA,
         supply_flow_rate_CFM = NA)

rm(df_e350_min, df_e350_sec)

## Merge NRCan dataframes together into one and clean
df_nrcan <- merge(
  # Second-level data
  df_nrcan_sec,
  # Minute-level data
  df_nrcan_min,
  by=c("Site_ID", "Timestamp"), all.x=T, all.y=F) %>% 
  arrange(Site_ID, Timestamp) %>%
  mutate(Room2_TempF = NA, Room2_RH = NA, Room3_TempF = NA, Room3_RH = NA, 
         Room4_TempF = NA, Room4_RH = NA, SA2_RH = NA, SA2_TempF = NA, 
         SA3_TempF = NA, SA3_RH = NA, SA4_TempF = NA, SA4_RH = NA, AHU_Power = NA,
         AHU_TempF = NA, AHU_RH = NA, DEFROST_ON_1 = NA)

rm(df_nrcan_min, df_nrcan_sec)

  
# For site 4228VB only:
  # Trane RV data is every four seconds for site 4228VB (and sometimes up to 16 seconds), 
  # so need to fill in gaps to defrost mode
df_e350 <- df_e350 %>% group_by(Site_ID, Break=cut(Timestamp, breaks="20 secs")) %>%
  mutate(DEFROST_ON_1=ceiling(mean(DEFROST_ON_1, na.rm=T))) %>% ungroup() %>%
  select(-Break)

# For site 5539NO only:
  # OAT data is only every hour, so need to interpolate to 1-second
df_e350 <- df_e350 %>% group_by(Site_ID, Break=cut(Timestamp, breaks="1 hour")) %>%
  mutate(OA_TempF_EEW = mean(OA_TempF_EEW, na.rm=T)) %>% ungroup() %>%
  mutate(OA_TempF = ifelse(is.na(OA_TempF) & Site_ID=="5539NO", OA_TempF_EEW, OA_TempF)) %>%
  select(-OA_TempF_EEW, -Break)

rm(trane_rv, eew)

# For all E350 sites, all 1-minute data needs to be interpolated to 1-second
df_e350 <- df_e350 %>% 
  group_by(Site_ID, Break=cut(Timestamp, breaks="1 min")) %>%
  mutate(SA1_TempF=mean(SA1_TempF, na.rm=T),
         SA2_TempF=mean(SA2_TempF, na.rm=T),
         OA_TempF=mean(OA_TempF, na.rm=T),
         OA_RH=mean(OA_RH, na.rm=T),
         RA_TempF=mean(RA_TempF, na.rm=T),
         SA1_RH=mean(SA1_RH, na.rm=T),
         SA2_RH=mean(SA2_RH, na.rm=T),
         Room1_TempF=mean(Room1_TempF, na.rm=T),
         Room2_TempF=mean(Room2_TempF, na.rm=T),
         Room3_TempF=mean(Room3_TempF, na.rm=T),
         Room4_TempF=mean(Room4_TempF, na.rm=T),
         AHU_TempF=mean(AHU_TempF, na.rm=T)) %>% 
  ungroup() %>%
  select(-Break)

# For all NRCan sites, all 1-minute data needs to be interpolated to 5-second
df_nrcan <- df_nrcan %>% 
  group_by(Site_ID, Break=cut(Timestamp, breaks="1 min")) %>%
  mutate(SA1_TempF=mean(SA1_TempF, na.rm=T),
         SA1_RH=mean(SA1_RH, na.rm=T),
         RA_TempF=mean(RA_TempF, na.rm=T),
         OA_TempF=mean(OA_TempF, na.rm=T),
         OA_RH=mean(OA_RH, na.rm=T),
         supply_flow_rate_CFM=mean(supply_flow_rate_CFM, na.rm=T)) %>% 
  ungroup() %>%
  select(-Break)




## Merge data into one dataframe ----
df <- df_e350
df <- df_michaels
df <- df_nrcan
df <- rbind(
  df_e350 %>%
    select(Site_ID, Timestamp, RV_Volts, HP_Power, Fan_Power, AHU_Power, Aux_Power,
           OA_TempF, OA_RH, SA1_TempF, SA2_TempF, SA1_RH, SA2_RH, RA_TempF, 
           RA_RH, AHU_TempF, AHU_RH, Room1_TempF, Room1_RH, Room2_TempF, Room2_RH,
           Room3_TempF, Room3_RH, SA3_TempF, SA3_RH, SA4_TempF, SA4_RH, Operating_Mode,
           Room4_TempF, Room4_RH, supply_flow_rate_CFM, DEFROST_ON_1),
  df_nrcan %>%
    select(Site_ID, Timestamp, RV_Volts, HP_Power, Fan_Power, AHU_Power, Aux_Power,
           OA_TempF, OA_RH, SA1_TempF, SA2_TempF, SA1_RH, SA2_RH, RA_TempF,
           RA_RH, AHU_TempF, AHU_RH, Room1_TempF, Room1_RH, Room2_TempF, Room2_RH,
           Room3_TempF, Room3_RH, SA3_TempF, SA3_RH, SA4_TempF, SA4_RH, Operating_Mode,
           Room4_TempF, Room4_RH, supply_flow_rate_CFM, DEFROST_ON_1),
  df_michaels %>% 
    select(Site_ID, Timestamp, RV_Volts, HP_Power, Fan_Power, AHU_Power, Aux_Power,
           OA_TempF, OA_RH, SA1_TempF, SA2_TempF, SA1_RH, SA2_RH, RA_TempF, 
           RA_RH, AHU_TempF, AHU_RH, Room1_TempF, Room1_RH, Room2_TempF, Room2_RH,
           Room3_TempF, Room3_RH, SA3_TempF, SA3_RH, SA4_TempF, SA4_RH, Operating_Mode,
           Room4_TempF, Room4_RH, supply_flow_rate_CFM, DEFROST_ON_1)) %>%
    arrange(Site_ID, Timestamp)

rm(df_michaels, df_e350, df_nrcan)


# Supply temperature and humidity calculated as the average of the four quadrants
df <- df %>% mutate(
  SA_RH = rowMeans(cbind(SA1_RH, SA2_RH, SA3_RH, SA4_RH), na.rm=T),
  SA_TempF = rowMeans(cbind(SA1_TempF, SA2_TempF, SA3_TempF, SA4_TempF), na.rm=T),
  Room_TempF = rowMeans(cbind(Room1_TempF, Room2_TempF, Room3_TempF, Room4_TempF), na.rm=T),
    # Fan Power is the only parameter used in both heat capacity and COP, so make it NA if any of the key
    # parameters are NA so that heat and COP calculations are not based on differenct rows.
  Fan_Power = ifelse(is.na(HP_Power) | is.na(Aux_Power) | is.na(Fan_Power) | is.na(SA_TempF) | is.na(RA_TempF), NA, Fan_Power),
  Total_Power = HP_Power + Aux_Power + Fan_Power,
  
  # Add column that determines the number of aux legs
  # Rheem site 5539N0 has larger unit, the first leg measures around 9 kW. Only one unit.
  # Will need to update for Solon, NY once we have data.
  Number_Aux_Legs = ifelse(Aux_Power < 0.1, NA,
                           ifelse(Site_ID == "5539NO" & Aux_Power > 8, 1,
                                  # All other sites
                                  ifelse(Aux_Power > 18, 4,
                                         ifelse(Aux_Power > 13, 3,
                                                ifelse(Aux_Power > 8, 2, 1))))))


## Operating mode and defrost cycles ##
  # Objective: Create a column with operating mode: 
    # Heating-HP Only
    # Heating-Aux/HP
    # Heating-Aux Only
    # System Off
    # Defrost
    # Cooling

df <- df %>% mutate(Operating_Mode = 
  # 1. Identify defrost mode                      
      # For Michaels sites, 0V on RV indicates heating mode and 27V indicates cooling/defrost.
    ifelse(Site_ID %in% c("2563EH", "2896BR", "6950NE", "8220XE", "9944LD", "6112OH", "8726VB") & RV_Volts > 25 & HP_Power > 0.1, "Defrost",
      # For site 4228VB, Trane provided RV data but there are some gaps which require secondary indicators
    ifelse(Site_ID == "4228VB" & 
             (Timestamp >= strptime("2023-01-16", "%F", tz="US/Mountain") & Timestamp <= strptime("2023-01-30","%F",tz="US/Mountain") |
              Timestamp >= strptime("2023-02-22", "%F", tz="US/Mountain") & Timestamp <= strptime("2023-03-04","%F",tz="US/Mountain")) &
             Aux_Power > 4.0 & HP_Power > 0.1 & HP_Power < 1.25 & Fan_Power > 0.35, "Defrost",
        ifelse(Site_ID == "4228VB" & !is.na(DEFROST_ON_1) & DEFROST_ON_1==1 & HP_Power > 0.1, "Defrost",
      # For site 5539NO, RV between 0.6 V and 3.0 V indicates defrost mode
    ifelse(Site_ID == "5539NO" & !is.na(RV_Volts) & RV_Volts > 0.6 & RV_Volts < 3 & HP_Power > 0.1, "Defrost",
      # For site 2458CE, secondary indicators are used
    ifelse(Site_ID=="2458CE" & HP_Power > 0.1 & HP_Power < 1.75 & Fan_Power > 0.4 & Aux_Power > 4, "Defrost",
      # For site 5291QJ, secondary indicators are used
    ifelse(Site_ID=="5291QJ" & HP_Power > 0.5 & Fan_Power > 0.02 & Fan_Power < 0.06 , "Defrost",
  # 2. If not defrost mode, use power data to determine the mode         
    ifelse(HP_Power > 0.1 & Aux_Power < 0.1, "Heating-HP Only",
         ifelse(HP_Power < 0.1 & Aux_Power > 0.1, "Heating-Aux Only",
                ifelse(HP_Power > 0.1 & Aux_Power > 0.1, "Heating-Aux/HP",
                       "System Off")))))))))) %>%
  # 3. If Fan_Power is NA, make operating mode NA, because we don't want to evaluate 
    # time periods with key parameters missing.
  mutate(Operating_Mode=ifelse(is.na(Fan_Power), NA, Operating_Mode)) %>%

  # 4. Correct for cooling mode
  mutate(Operating_Mode=ifelse(
    # Michaels sites
    Site_ID %in% c("2563EH", "2896BR", "6950NE", "8220XE", "9944LD", "6112OH", "8726VB") & Operating_Mode=="Defrost" & Aux_Power < 0.1 & OA_TempF > 60,
           "Cooling", Operating_Mode))

## Remove data before data stabilizes for each site.
  # If there are key parameters missing during any timeframes, set all key parameters to NA
  # so that heating capacity, COP, and other calculated variables are based on the same data.
  # Do not run this part for daily operation graphs, we want to see the original data for that.
df <- df %>% 
  # Site 2458CE:
    # Manufacturer visited site and Mar 21, 2023 8:30am - 5pm
  filter(Site_ID != "2458CE" | Timestamp < strptime("2023-03-03 8:30:00", "%F %T", tz="Canada/Eastern") | Timestamp > strptime("2023-03-03 17:00:00", "%F %T", tz="Canada/Eastern")) %>%
  
  # Site 2563EH:
    # Data doesn't stabilize until Feb 1st, use this day as starting point.
  filter(Site_ID != "2563EH" | Timestamp >= strptime("2023-02-01", "%F", tz="US/Eastern")) %>%
  
  # Site 2896BR:
    # Data doesn't stabilize until Feb 1st, use this day as starting point.
  filter(Site_ID != "2896BR" | Timestamp >= strptime("2023-02-03", "%F", tz="US/Eastern")) %>%
  
  # Site 4228VB 
    # Appears to be missing Aux power data before Dec 20, 2022, doesn't stabilize until evening of 21st. Use Dec 22nd as starting point.
  filter(Site_ID != "4228VB" | Timestamp >= strptime("2022-12-22", "%F", tz="US/Mountain")) %>%

  # Site 5291QJ:
    # Manufacturer visited site and was playing with controls Mar 21, 2023 8:30am - 5pm
  filter(Site_ID != "5291QJ" | Timestamp < strptime("2023-03-03 8:30:00", "%F %T", tz="Canada/Eastern") | Timestamp > strptime("2023-03-03 17:00:00", "%F %T", tz="Canada/Eastern")) %>%
  
  # Site 5539NO:
    # All data is present from start.

  # Site 6112OH:
    # Data looks regular starting Feb 14--using this as starting point
  filter(Site_ID != "6112OH" | Timestamp >= strptime("2023-02-14", "%F", tz="US/Eastern")) %>%
  
  # Site 6950NE:
    # Data doesn't stabilize until December 10th, use this day as starting point.
  filter(Site_ID != "6950NE" | Timestamp >= strptime("2022-12-10", "%F", tz="US/Central")) %>%

  # Site 8220XE:
    # Data doesn't stabilize until Dec 12th at 12:00, use Dec 13th as starting point.
  filter(Site_ID != "8220XE" | Timestamp >= strptime("2022-12-13", "%F", tz="US/Central")) %>%
  
  # Site 8726VB:
    # Data doesn't stabilize until Feb 11.
  filter(Site_ID != "8726VB" | Timestamp >= strptime("2023-02-12", "%F", tz="US/Eastern")) %>%
    # No OA Temp and issues with SAT Mar 09 11AM to Mar 10 12PM. Set key parameters to NA.
  mutate_at(vars(HP_Power, Aux_Power, Fan_Power, OA_TempF, SA_TempF),
            ~ ifelse(Site_ID == "8726VB" & Timestamp > strptime("2023-03-09 11:00:00", "%F %T", tz="US/Eastern") & Timestamp < strptime("2023-03-10 12:00:00", "%F %T", tz="US/Eastern"), NA, .)) %>%
  
  # Site 9944LD:
    # Data doesn't stabilize until Jan 7th, use this day as starting point.
  filter(Site_ID != "9944LD" | Timestamp >= strptime("2023-01-07", "%F", tz="US/Mountain")) %>%
    # NA Fan power and temperature data from Jan 14 - 16 due to eGauge offline.
  mutate_at(vars(HP_Power, Aux_Power, Fan_Power, OA_TempF, SA_TempF),
            ~ ifelse(Site_ID != "9944LD" & Timestamp > strptime("2023-01-14 16:00:00", "%F %T", tz="US/Mountain") & Timestamp < strptime("2023-01-16 20:00:00", "%F %T", tz="US/Mountain"), NA, .)) %>%
    # Missing temp data and HP Power and Aux default to 0 kW from Feb 15 noon to Feb 16 noon
  mutate_at(vars(HP_Power, Aux_Power, Fan_Power, OA_TempF, SA_TempF),
            ~ ifelse(Site_ID != "9944LD" & Timestamp > strptime("2023-02-15 12:00:00", "%F %T", tz="US/Mountain") & Timestamp < strptime("2023-02-16 12:00:00", "%F %T", tz="US/Mountain"), NA, .))




## Flag time periods with operational issues
    # We can use this column to filter data for COP graphs for showing performance 
    # without operational issues and variation of performance due to reliability.
    # A "0" denotes no operational issues and a "1" indicates issues
    # See CCHP Analysis Notebook for more information on these events.
df <- df %>% mutate(
  Operational_Issue = ifelse(
    (Site_ID == "5539NO" & Timestamp >= strptime("2023-04-03", "%F", tz="US/Central")) |
      (Site_ID == "9944LD" & Timestamp > strptime("2023-01-26", "%F", tz="US/Mountain") & Timestamp < strptime("2023-01-27 12:00:00", "%F %T", tz="US/Mountain")) |
      (Site_ID == "9944LD" & Timestamp > strptime("2023-02-01 12:00:00", "%F %T", tz="US/Mountain")) |
      (Site_ID == "4228VB" & Timestamp > strptime("2022-12-30", "%F", tz="US/Mountain") & Timestamp < strptime("2023-01-02 18:00:00", "%F %T", tz="US/Mountain")) |
      (Site_ID == "4228VB" & Timestamp > strptime("2023-01-05", "%F", tz="US/Mountain") & Timestamp < strptime("2023-01-06 21:00:00", "%F %T", tz="US/Mountain")) |
      (Site_ID == "4228VB" & Timestamp > strptime("2023-01-07 15:00:00", "%F %T", tz="US/Mountain") & Timestamp < strptime("2023-01-08 12:00:00", "%F %T", tz="US/Mountain")) |
      (Site_ID == "4228VB" & Timestamp > strptime("2023-03-02 6:00:00", "%F %T", tz="US/Mountain") & Timestamp < strptime("2023-03-06 12:00:00", "%F %T", tz="US/Mountain")),
    1, 0))


# Heat pump status
  # Used to calculate the length of heat pump cycles
df <- df %>% mutate(HP_Status = ifelse(is.na(Fan_Power), NA, ifelse(HP_Power > 0.1, "On", "Off")))

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

df$HP_Cycle_Runtimes <- runCycleCalc(df$Site_ID, df$Timestamp, df$HP_Status, "On")
df$Defrost_Cycle_Runtimes <- runCycleCalc(df$Site_ID, df$Timestamp, df$Operating_Mode, "Defrost")


## Corrections needed for defrost modes with secondary indicators
  # See notes for each site in the CCHP Analysis Notebook for more details.
df <- df %>% mutate(
  Defrost_Cycle_Runtimes = ifelse((Site_ID=="4228VB" | Site_ID=="5291QJ" | Site_ID=="2458CE") & 
                                    Defrost_Cycle_Runtimes < 1, NA, 
                                  ifelse(Site_ID=="6221OH" & Defrost_Cycle_Runtimes < 0.5, NA,
                                  ifelse(Site_ID=="4228VB" & Defrost_Cycle_Runtimes > 60, NA,
                                         Defrost_Cycle_Runtimes))))




### Calculations ----

df <- df %>%
  
  # Air supply (based on fan power curve)
  mutate(supply_flow_rate_CFM = 
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
    ifelse(Site_ID=="5539NO", 1415.9 * Fan_Power^0.4138,
    ifelse(Site_ID=="8726VB", 109.14 * (Fan_Power*1000)^0.424,
                  supply_flow_rate_CFM)))))))))),
 

## Heating and cooling related calculations:

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

  # Air pressure (merge from metadata)
  # Pressure_pa = df %>% 
  #   merge(metadata %>% select(Site_ID, Pressure), by="Site_ID", all.x=T, all.y=F) %>%
  #   pull(Pressure),
 
  # Humidity ratio of supply air (0.62198 * Pw / (P - Pw))
  # P = 97,717 Pascals at 1,000 ft elevation (we could use a more accurate look up for each location)
  Supply_Humidity_Ratio = 
    0.62198 * Partial_Water_Pressure_Supply / (97717 - Partial_Water_Pressure_Supply),
  
  # Heat Output
    # Q-heating = (dry air density) * (blower airflow rate) * (specific heat) * (delta Temp)
  Heat_Output_Btu_h = ifelse(is.na(Operating_Mode), NA,
      0.0715 *                                                # Density of air at 35C (lb/ft3)
      supply_flow_rate_CFM * 60 *                             # CFM * min/hour
      (0.24 + 0.444 *  Supply_Humidity_Ratio) *               # Specific heat capacity (Btu/F-lb)
      (SA_TempF - RA_TempF)),                                 # Temperature delta
  
  # Adjusted Heat Output
    # Many of the sites are significantly affected by SAT sensor placement, which can be
    # observed from the heat output differing from aux power in aux only mode. The aux heat
    # is causing the air to not fully mix at the sensors. This factor will adjust the heat
    # output from the aux contribution only.
  Heat_Output_Btu_h_adjusted = 
    # Heat output from HP
    Heat_Output_Btu_h - Aux_Power * 3412 +
    # Adjusted aux power
    Aux_Power * 3412 * ifelse(Site_ID=="5539NO", 1.5,
                              ifelse(Site_ID=="8220XE", 0.8,
                                     ifelse(Site_ID=="6950NE", 0.8,
                                            ifelse(Site_ID=="9944LD", 0.5,
                                                   ifelse(Site_ID=="4228VB", 0.95,
                                                          ifelse(Site_ID=="2896BR", 1,
                                                                 ifelse(Site_ID=="2563EH", 1.5,
                                                                        ifelse(Site_ID=="6112OH", 1.2,
                                                                               1))))))))

  # Cooling output
    # Q-cooling = (dry air density) * (blower airflow rate) * (specific heat) * (delta Temp) / (1 + Humidity Ratio)
  # HP_Cool_Output_Btu_h = 0.0765 *                                       # Density of air at 15C (lb/ft3)
  #     supply_flow_rate_CFM * 60 *                                       # CFM * min/hour
  #     (0.24 + 0.444 *  Supply_Humidity_Ratio) *                         # Specific heat capacity (Btu/F-lb)
  #     (SA_TempF - RA_TempF) /
  #     (1 + Supply_Humidity_Ratio),
)



## Set sitename to not have to update for each graph:
sitename = "9944LD"

## Set minimum temperature for temperature bins for when sample size is too small
temp_min = ifelse(sitename=="4228VB", 5, ifelse(sitename=="9944LD", -20, ifelse(sitename=="8220XE", -15,
           ifelse(sitename=="2563EH", -10, ifelse(sitename=="5291QJ", -20, ifelse(sitename=="2896BR", -10,
           ifelse(sitename=="6112OH", 10, ifelse(sitename=="6950NE", -15, ifelse(sitename=="5539NO", 10,
           ifelse(sitename=="2458CE", 15, ifelse(sitename=="8726VB", 5, NA)))))))))))
temp_max = ifelse(sitename=="4228VB", 55, ifelse(sitename=="9944LD", 45, ifelse(sitename=="8220XE", 55,
           ifelse(sitename=="2563EH", 55, ifelse(sitename=="5291QJ", 45, ifelse(sitename=="2896BR", 55,
           ifelse(sitename=="6112OH", 55, ifelse(sitename=="6950NE", 55, ifelse(sitename=="5539NO", 55,
           ifelse(sitename=="2458CE", 45, ifelse(sitename=="8726VB", 55, NA)))))))))))


### Miscellaneous Investigation Graphs ----
  # These are intended for custom analysis, not to print to folder for summary.
  # The time series are too difficult to view for full time frames, and 
  # these are not key parameters.

# Investigate time series for any variable
TimeSeries <- function(parameter, interval, timestart, timeend){
  # Look at a time series graph for a given parameter, time interval (e.g, 5-minute), time period, and site
  # Interval is in units of minutes, and so the maximum interval would be one hour.
  # The time start and end should be a date-time string in 24-hr format for example "4/01/2022 16:00".
  df %>% mutate(Timestamp = Timestamp %>% with_tz(metadata$Timezone[metadata$Site_ID==sitename])
                # ,
                # Interval = minute(Timestamp) %/% interval
                ) %>% 
    filter(Site_ID == sitename &
             Timestamp >= strptime(timestart,"%F %H:%M", tz=metadata$Timezone[metadata$Site_ID==sitename]) &
             Timestamp <= strptime(timeend,"%F %H:%M", tz=metadata$Timezone[metadata$Site_ID==sitename])) %>%
    # group_by(Site_ID, Date, Hour, Interval) %>% 
    # summarize(Timestamp = Timestamp[1],
              mutate(Parameter = !!as.name(parameter)
                # Parameter = mean(!!as.name(parameter),na.rm=T)
                ) %>%
    ggplot(aes(x=as.POSIXct(Timestamp),y=Parameter, color=Site_ID)) +
    geom_point(size=0.3) + 
    # geom_hline(aes(yintercept = 0)) +
    labs(title="Time series plot",x="Timestamp",y=parameter, color="Site ID") +
    theme(axis.ticks.y=element_blank(),
          panel.border = element_rect(colour = "black",fill=NA)) +
    guides(color=guide_legend(override.aes=list(size=3)))
}
# TimeSeries("6112OH", "HP_Power", 1,  "2023-02-14 6:00", "2023-02-14 17:00")


# Investigate NA values for any variable
NATimeSeries <- function(site, parameter, timestart, timeend){
  # Look at a time series graph for a given parameter, time period, and site.
  # The site can be a list of multiple sites if would like to compare.
  # The time start and end should be a date-time string in format for example "4/01/2022 08:00".
  df %>% mutate(Timestamp = Timestamp %>% with_tz(metadata$Timezone[metadata$Site_ID==site])) %>% 
    filter(Site_ID %in% site &
             Timestamp >= strptime(timestart,"%F %H:%M", tz=metadata$Timezone[metadata$Site_ID==site]) &
             Timestamp <= strptime(timeend,"%F %H:%M", tz=metadata$Timezone[metadata$Site_ID==site])) %>%
    mutate(NA_1 = ifelse(is.na(get(parameter)), 1, 0)) %>%
    ggplot(aes(x=as.POSIXct(Timestamp),y=NA_1, color=Site_ID)) +
    geom_point() + 
    ylim(c(-0.1, 1.1)) +
    labs(title=paste0("Time series plot of NA ", parameter, " values"),x="Timestamp",y="NA=1, Non-NA=0", color="Site ID") +
    theme(axis.ticks.y=element_blank(),
          panel.border = element_rect(colour = "black",fill=NA)) +
    guides(color=guide_legend(override.aes=list(size=3)))
}
# NATimeSeries("6950NE", "HP_Power", "2023-02-14 0:00", "2023-02-15 0:00")


# Temperature time series comparison chart
RoomTempTimeSeries <- function(interval, timestart, timeend){
  # Look at a time series graph for all temperature monitors, time interval (e.g, 5-minute), time period, and site
  # Interval is in units of minutes, and so the maximum interval would be 60 minutes/1 hour.
  # The time start and end should be a date string in format for example "4/01/2022 0:00".
  df %>% mutate(Timestamp = Timestamp %>% with_tz(metadata$Timezone[metadata$Site_ID==sitename]),
                Interval = minute(Timestamp) %/% interval) %>% 
    filter(Site_ID == sitename &
             Timestamp >= strptime(timestart,"%F %H:%M", tz=metadata$Timezone[metadata$Site_ID==sitename]) &
             Timestamp <= strptime(timeend,"%F %H:%M", tz=metadata$Timezone[metadata$Site_ID==sitename])) %>%
    group_by(Site_ID, Date, Hour, Interval) %>% 
    summarize(Timestamp = Timestamp[1],
              Room1_TempF = mean(Room1_TempF,na.rm=T),
              Room2_TempF = mean(Room2_TempF,na.rm=T),
              Room3_TempF = mean(Room3_TempF,na.rm=T),
              Room4_TempF = mean(Room4_TempF,na.rm=T),
              AHU_TempF = mean(AHU_TempF,na.rm=T)) %>%
    ggplot(aes(x=as.POSIXct(Timestamp))) +
    geom_line(aes(y=Room1_TempF, color = "Room 1"),size=0.5) + 
    geom_line(aes(y=Room2_TempF, color = "Room 2"),size=0.5) +
    geom_line(aes(y=Room3_TempF, color = "Room 3"),size=0.5) +
    geom_line(aes(y=Room4_TempF, color = "Room 4"),size=0.5) +
    geom_line(aes(y=AHU_TempF, color = "AHU Ambient"),size=0.5) +
    scale_y_continuous(breaks = seq(0,200, by=10), minor_breaks = seq(0, 200, by=1)) +
    scale_color_manual(name = "", values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
    labs(title=paste0("Room temperature time series plot for site ", sitename),x="",y="Temperature (F)") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          panel.grid.major = element_line(size = 0.9),
          panel.grid.minor = element_line(size = 0.1),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5),) +
    guides(color=guide_legend(override.aes=list(size=3)))
}
# RoomTempTimeSeries(5, "2023-02-23 00:00", "2023-02-26 00:00")
  # Save a sample of data to the folder for each site
    # Adjust dates manually
ggsave(paste0(sitename, '_Room_Temperature_Comparison.png'),
       plot = RoomTempTimeSeries(5, "2023-02-16 00:00", "2023-02-17 00:00"),
       path = paste0(wd,'/Graphs/',sitename, '/'),
       width=12, height=4, units='in')


# Supply temperature time series comparison chart
SupplyTempTimeSeries <- function(interval, timestart, timeend){
  # Look at a time series graph for the four supply temperature monitors, time interval (e.g, 5-minute), time period, and site
  # Interval is in units of minutes, and so the maximum interval would be 60 minutes/1 hour.
  # The time start and end should be a date string in format for example "4/01/2022 0:00".
  df %>% mutate(Timestamp = Timestamp %>% with_tz(metadata$Timezone[metadata$Site_ID==sitename]),
                Interval = minute(Timestamp) %/% interval) %>% 
    filter(Site_ID == sitename &
             Timestamp >= strptime(timestart,"%F %H:%M", tz=metadata$Timezone[metadata$Site_ID==sitename]) &
             Timestamp <= strptime(timeend,"%F %H:%M", tz=metadata$Timezone[metadata$Site_ID==sitename])) %>%
    group_by(Site_ID, Date, Hour, Interval) %>% 
    summarize(Timestamp = Timestamp[1],
              SA1_TempF = mean(SA1_TempF,na.rm=T),
              SA2_TempF = mean(SA2_TempF,na.rm=T),
              SA3_TempF = mean(SA3_TempF,na.rm=T),
              SA4_TempF = mean(SA4_TempF,na.rm=T)) %>%
    ggplot(aes(x=as.POSIXct(Timestamp))) +
    geom_line(aes(y=SA1_TempF, color = "SA1"),size=0.5) + 
    geom_line(aes(y=SA2_TempF, color = "SA2"),size=0.5) + 
    geom_line(aes(y=SA3_TempF, color = "SA3"),size=0.5) + 
    geom_line(aes(y=SA4_TempF, color = "SA4"),size=0.5) + 
    scale_y_continuous(breaks = seq(0,200, by=10), minor_breaks = seq(0,200, by=1)) +
    scale_color_manual(name = "", values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
    labs(title=paste0("Supply temperature time series plot for site ", sitename),x="",y="Temperature (F)") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          panel.grid.major = element_line(size = 0.9),
          panel.grid.minor = element_line(size = 0.1),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5),) +
    guides(color=guide_legend(override.aes=list(size=3)))
}
# SupplyTempTimeSeries(5, "2023-03-16 00:00", "2023-03-19 00:00")
# Save a sample week of data to the folder for each site
  # Adjust date manually
ggsave(paste0(sitename, '_Supply_Temperature_Comparison.png'),
       plot = SupplyTempTimeSeries(5, "2023-03-16 00:00", "2023-03-19 00:00"),
       path = paste0(wd,'/Graphs/',sitename, '/'),
       width=12, height=4, units='in')





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
                OA_Temp_NA = round(sum(is.na(OA_TempF))*100/ n(),1),
                SA_Temp_NA = round(sum(is.na(SA_TempF))*100/ n(),1),
                Duplicated_timestamps = round(sum(duplicated(Timestamp))*2*100/ n(),1),
                Duplicated_rows = round((n() - n_distinct(Timestamp, HP_Power, Aux_Power, Fan_Power))*100/ n_distinct(Timestamp, HP_Power, Aux_Power, Fan_Power),1),
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
             Timestamp >= strptime(timestart,"%F", tz=metadata$Timezone[metadata$Site_ID==site]) &
             Timestamp <= strptime(timeend,"%F", tz=metadata$Timezone[metadata$Site_ID==site])) %>%
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
# DefrostCycleTimeSeries("6112OH", "2023-02-25", "2023-02-26")
# Loop to print daily defrost time series graphs, one for each day for each site
for(id in unique(df$Site_ID)){
  dates <- unique(df$Date[df$Site_ID==id])
  for(d in dates[-1]){
    d1 = substr(as.character(strptime(d, "%F", tz=metadata$Timezone[metadata$Site_ID==id]) + 60*60*24), 1, 10) # Date plus one day
    ggsave(paste0(id, '_Daily-Defrost-Cycles_',d,'.png'),
           plot = DefrostCycleTimeSeries(id, d, d1),
           path = paste0(wd,'/Graphs/',id, '/Daily Defrost Cycles/'),
           width=12, height=4, units='in')
  }
}
rm(dates,d1,d,id)



# Power time series comparison chart with OAT and SAT
OperationTimeSeries <- function(site, timestart, timeend){
  # The time start and end should be character with format "%Y-%m-%d".
  df %>% mutate(Timestamp = Timestamp %>% with_tz(metadata$Timezone[metadata$Site_ID==site])) %>% 
    filter(Site_ID == site &
             Timestamp >= strptime(timestart,"%F", tz=metadata$Timezone[metadata$Site_ID==site]) &
             Timestamp <= strptime(timeend,"%F", tz=metadata$Timezone[metadata$Site_ID==site])) %>%
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
# OperationTimeSeries(sitename, "2023-02-12", "2023-02-13")
# Loop to print daily operation time series graphs, one for each day for each site
for(id in unique(df$Site_ID)){
  dates <- unique(df$Date[df$Site_ID==id])  
  for(d in dates[-1]){
    d1 = substr(as.character(strptime(d, "%F", tz=metadata$Timezone[metadata$Site_ID==id]) + 60*60*24), 1, 10) # Date plus one day
    ggsave(paste0(id, '_Daily-Operation_',d,'.png'),
           plot = OperationTimeSeries(id, d, d1),
           path = paste0(wd,'/Graphs/',id, '/Daily Operation/'),
           width=12, height=4, units='in')
  }
}
rm(dates,d1,d,id)



# Power, capacity, and COP time series comparison chart with OAT and RAT
COPTimeSeries <- function(site, timestart, timeend, interval){
  # The time start and end should be character with format "%Y-%m-%d".
  # Interval is in minutes, at a maximum of 60 mins.
  df %>% mutate(Timestamp = Timestamp %>% with_tz(metadata$Timezone[metadata$Site_ID==site]),
                Interval = minute(Timestamp) %/% interval) %>% 
    filter(Site_ID == site &
             Timestamp >= strptime(timestart,"%F", tz=metadata$Timezone[metadata$Site_ID==site]) &
             Timestamp <= strptime(timeend,"%F", tz=metadata$Timezone[metadata$Site_ID==site])) %>%
    group_by(Site_ID, Date, Hour, Interval) %>%
    summarize(Timestamp = Timestamp[1],
              SA_TempF=mean(SA_TempF, na.rm=T),
              RA_TempF=mean(RA_TempF, na.rm=T),
              HP_Power=mean(HP_Power, na.rm=T),
              # Fan_Power=mean(Fan_Power, na.rm=T),
              Aux_Power=mean(Aux_Power, na.rm=T),
              Heat_Output=mean(Heat_Output_Btu_h/3412, na.rm=T),
              COP_Heating=sum(Heat_Output_Btu_h/3412, na.rm=T)/sum(Total_Power, na.rm=T)) %>%
    ggplot(aes(x=as.POSIXct(Timestamp))) +
    geom_line(aes(y=SA_TempF/2, linetype = "Supply Air Temperature"),color="black",size=0.3) + 
    geom_line(aes(y=RA_TempF/2, linetype = "Return Air Temperature"),color="black",size=0.3) +
    geom_line(aes(y=HP_Power, color = "Outdoor Unit Power"),size=0.3) + 
    # geom_line(aes(y=Fan_Power, color = "Supply Fan Power"),size=0.3) +
    geom_line(aes(y=Aux_Power, color = "Auxiliary Power"),size=0.3) + 
    geom_line(aes(y=Heat_Output, color = "Heat Output"),size=0.3) +
    geom_line(aes(y=COP_Heating, color = "COP"),size=1) + 
    scale_y_continuous(name = "Power (kW)",
                       # limits = c(-4, 25),
                       sec.axis = sec_axis(~.*2, name ="Temperature (F)")) +
    scale_color_manual(name = "Power/COP", breaks = c("Auxiliary Power","Outdoor Unit Power", "Heat Output", "COP"),
                       values = c("#E69F00","#009E73","#F0E442","#CC79A7")) +
    scale_linetype_manual(name = "Temperature", values = c("solid","dashed")) +
    labs(title=paste0("COP and power investigation time series plot for site ", site),x="") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          panel.grid.major = element_line(size = 0.5),
          panel.grid.minor = element_line(size = 0.1),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5)) +
    guides(color=guide_legend(override.aes=list(size=3)))
}
# COPTimeSeries(sitename, "2023-03-17", "2023-03-17", 5)
# Loop to print daily operation time series graphs, one for each day for each site
for(id in unique(df$Site_ID)){
  dates <- unique(df$Date[df$Site_ID==id])     
  for(d in dates[-1]){
    d1 = substr(as.character(strptime(d, "%F", tz=metadata$Timezone[metadata$Site_ID==id]) + 60*60*24), 1, 10) # Date plus one day
    ggsave(paste0(id, '_Daily-COP_',d,'.png'),
           plot = COPTimeSeries(id, d, d1, 5),
           path = paste0(wd,'/Graphs/',id, '/Daily COP/'),
           width=12, height=4, units='in')
  }
}
rm(dates,d1,d,id)



### Time Series Long Term Graphs ----



# 0a. Time flagged as operational issue or M&V issue
  # Hours for NRCan sites should be multiplied by five.
print_operational_issues <- function(site){
  ft <- ifelse(df$Site_ID[1] %in% c("2458CE", "5291QJ"), 5, 1)
  temp <- read.csv(paste0(wd, '/Graphs/Site Comparison/Operational Issues.csv')) %>%
    filter(Site_ID != site) %>%
    rbind(df %>% filter(Site_ID == site) %>% group_by(Site_ID) %>%
            summarize(Total_Hours = round(n()*ft/3600),
                      Oper_Issue_Hours = sum(Operational_Issue==1, na.rm=T)*ft/3600,
                      Oper_Issue_Perc = round(sum(Operational_Issue==1, na.rm=T)*100/n(),1),
                      MV_Issue_Hours = sum(is.na(Fan_Power), na.rm=T)*ft/3600,
                      MV_Issue_Perc = round(sum(is.na(Fan_Power), na.rm=T)*100/n(),1)))
  
  write.csv(temp, paste0(wd, '/Graphs/Site Comparison/Operational Issues.csv'), row.names = F)
}
print_operational_issues(sitename)

# 0b. Table of amount of time spent in each OAT bin
  # Hours for NRCan sites should be multiplied by five.
  # Need to transpose the data manually to fit in the table for the report.
print_OAT_summary <- function(site){
  ft <- ifelse(df$Site_ID[1] %in% c("2458CE", "5291QJ"), 5, 1)
  temp <- read.csv(paste0(wd, '/Graphs/Site Comparison/OAT Bin Summary.csv')) %>%
    filter(Site_ID != site) %>% rbind(
      df %>% filter(OA_TempF <= 55 & Site_ID == site) %>%
        mutate(temp_int = cut(OA_TempF,breaks=c(-50,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40,45,50,55))) %>%
        group_by(Site_ID, temp_int) %>%
        summarize(Hours = round(n()*ft/3600)))
  
   write.csv(temp, paste0(wd, '/Graphs/Site Comparison/OAT Bin Summary.csv'), row.names = F)
}
print_OAT_summary(sitename)



# 1a. Operating mode daily summary
  # Look at a time series graph of each day to see fraction of time in each operating mode
write.csv(df %>% mutate(Timestamp = Timestamp %>% with_tz(metadata$Timezone[metadata$Site_ID==sitename]),
                        Operating_Mode = ifelse(is.na(Operating_Mode), "Data Unavailable", Operating_Mode)) %>%
            group_by(Site_ID, Date, Operating_Mode) %>%
            summarize(Mode_Time = n()),
          file=paste0(wd, "/Graphs/Graph Data/Operating Mode Daily/", sitename, ".csv"),
          row.names=F)
OperatingModeTime <- function(site, timestart, timeend){
  list.files(path = paste0(wd, "/Graphs/Graph Data/Operating Mode Daily/"),pattern="*.csv", full.names=T) %>% 
    map_df(~read.csv(.)) %>%
    filter(Site_ID == site) %>%
    ggplot(aes(x=as.POSIXct(Date, format="%F", tz=metadata$Timezone[metadata$Site_ID==site]), fill=Operating_Mode, y=Mode_Time)) +
    geom_bar(position="fill", stat="identity") +
    scale_x_datetime(date_breaks = "1 week", 
                     date_labels = "%F",
                     limits=c(as.POSIXct(strptime(timestart,"%m/%d/%Y %H:%M", tz=metadata$Timezone[metadata$Site_ID==site])),
                              as.POSIXct(strptime(timeend,"%m/%d/%Y %H:%M", tz=metadata$Timezone[metadata$Site_ID==site])))) +
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
  ggsave(paste0(site, '_Operating_Mode_Percent_Time.png'),
       plot = OperatingModeTime(site, "12/15/2022 00:00", "3/30/2023 23:59"),
       path = paste0(wd,'/Graphs/',site, '/'),
       width=12, height=4, units='in')
}

# 1b. Table of 1a with temperature and RH
  # Save to file long table with row for every day
  # NRCan sites need to multiple hours by five
write.csv(
  df %>% group_by(Date) %>%
    summarize(Hours_Defrost=sum(Operating_Mode=="Defrost", na.rm=T)/3600,
              Hours_HP_Only=sum(Operating_Mode=="Heating-HP Only", na.rm=T)/3600,
              Hours_Aux_Only=sum(Operating_Mode=="Heating-Aux Only", na.rm=T)/3600,
              Hours_HP.Aux=sum(Operating_Mode=="Heating-Aux/HP", na.rm=T)/3600,
              Hours_System_Off=sum(Operating_Mode=="System Off", na.rm=T)/3600,
              Hours_Data_Unavailable=sum(is.na(Operating_Mode))/3600,
              Min_Temp=min(OA_TempF, na.rm=T),
              Avg_Temp=mean(OA_TempF, na.rm=T),
              Max_Temp=max(OA_TempF, na.rm=T),
              Avg_RH=mean(OA_RH, na.rm=T)), 
  file=paste0(wd, "/Graphs/", sitename, "/Daily_Operation_Summary_", sitename, ".csv"), row.names=F)
# Print summary site comparison
print_operating_summary <- function(site){
  ft <- ifelse(df$Site_ID[1] %in% c("2458CE", "5291QJ"), 5, 1)
  temp <- read.csv(paste0(wd, '/Graphs/Site Comparison/Operating Mode Summary.csv')) %>%
    filter(Site_ID != site) %>% rbind(df %>% filter(Site_ID==site) %>% group_by(Site_ID) %>%
      summarize(Hours_Defrost=round(sum(Operating_Mode=="Defrost", na.rm=T)*ft/3600),
                Hours_HP_Only=round(sum(Operating_Mode=="Heating-HP Only", na.rm=T)*ft/3600),
                Hours_Aux_Only=round(sum(Operating_Mode=="Heating-Aux Only", na.rm=T)*ft/3600),
                Hours_HP.Aux=round(sum(Operating_Mode=="Heating-Aux/HP", na.rm=T)*ft/3600),
                Hours_System_Off=round(sum(Operating_Mode=="System Off", na.rm=T)*ft/3600),
                Hours_Data_Unavailable=round(sum(is.na(Operating_Mode))*ft/3600),
                Min_Temp=round(quantile(OA_TempF, 0.05, na.rm=T)),
                Median_Temp=round(median(OA_TempF, na.rm=T)),
                Max_Temp=round(quantile(OA_TempF, 0.95, na.rm=T)),
                Median_RH=round(median(OA_RH, na.rm=T))))
  
  write.csv(temp, paste0(wd, '/Graphs/Site Comparison/Operating Mode Summary.csv'), row.names = F)
}
print_operating_summary(sitename)



# 1c. Operating mode summary by OAT bin
  # Look at a time series graph of each day to see fraction of time in each operating mode
write.csv(df %>% filter(OA_TempF <= temp_max & OA_TempF > temp_min) %>%
            mutate(temp_int = cut(OA_TempF,breaks=c(-50,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40,45,50,55)),
                   Operating_Mode = ifelse(is.na(Operating_Mode), "Data Unavailable", Operating_Mode)) %>%
            group_by(Site_ID, temp_int, Operating_Mode) %>%
            summarize(Mode_Time=n()),
          file=paste0(wd, "/Graphs/Graph Data/Operating Mode OAT/", sitename, ".csv"),
          row.names=F)
OperatingModeOAT <- function(site){
  list.files(path = paste0(wd, "/Graphs/Graph Data/Operating Mode OAT/"),pattern="*.csv", full.names=T) %>% 
    map_df(~read.csv(.)) %>%
    filter(Site_ID == site) %>%
    ggplot(aes(x=temp_int, fill=Operating_Mode, y=Mode_Time)) +
    geom_bar(position="fill", stat="identity") +
    scale_fill_manual(name = "Operating Mode", 
                      breaks = c("Defrost","Heating-HP Only","Heating-Aux Only","Heating-Aux/HP","Cooling","System Off","Data Unavailable"),
                      values = c("#009E73","#F0E442","#CC3300","#E69F00","#3333FF","#666666","lightgrey")) +
    labs(title=paste0("Fraction of time in each operating mode by OAT bin for site ", site),x="", 
         y="Fraction of Time") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          panel.grid.major = element_line(size = 0.9),
          panel.grid.minor = element_line(size = 0.1),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5)) 
}
# OperatingModeOAT(sitename)
# Print graph to folder--manually adjust date.
for(site in substr(list.files(path = paste0(wd, "/Graphs/Graph Data/Operating Mode OAT/"),pattern="*.csv", full.names=T),
                   nchar(list.files(path = paste0(wd, "/Graphs/Graph Data/Operating Mode OAT/"),pattern="*.csv", full.names=T)) - 9,
                   nchar(list.files(path = paste0(wd, "/Graphs/Graph Data/Operating Mode OAT/"),pattern="*.csv", full.names=T)) - 4)){
  ggsave(paste0(site, '_Operating_Mode_Percent_Time_by_OAT.png'),
       plot = OperatingModeOAT(site),
       path = paste0(wd,'/Graphs/',site, '/'),
       width=12, height=4, units='in')
}

# 1d. Operating mode summary for all sites 0-10F
  # Print out data for each site for 0-10F range.
  # Note that sites 5539NO, 6112OH, and 2458CE do not have data in this range and should not save data.
write.csv(df %>% 
            filter(OA_TempF < 10 & OA_TempF > 0) %>%
            mutate(Operating_Mode = ifelse(is.na(Operating_Mode), "Data Unavailable", Operating_Mode)) %>%
            group_by(Site_ID, Operating_Mode) %>%
            summarize(Mode_Time=n()),
            file=paste0(wd, "/Graphs/Graph Data/Operating Time 0-10F/", sitename, ".csv"),
          row.names=F)
Operation_5F_all_sites <- function(){
  list.files(path = paste0(wd, "/Graphs/Graph Data/Operating Time 0-10F/"),pattern="*.csv", full.names=T) %>% 
    map_df(~read.csv(.)) %>%
    merge(metadata %>% select(Site_ID, Manufacturer), by="Site_ID", all.x=T, all.y=F) %>%
    mutate(Site_Manufacturer = paste0(Manufacturer, "-", Site_ID)) %>% 
    ggplot(aes(x=Site_Manufacturer, fill=Operating_Mode, y=Mode_Time)) +
    geom_bar(position="fill", stat="identity") +
    scale_fill_manual(name = "Operating Mode", 
                      breaks = c("Defrost","Heating-HP Only","Heating-Aux Only","Heating-Aux/HP","Cooling","System Off","Data Unavailable"),
                      values = c("#009E73","#F0E442","#CC3300","#E69F00","#3333FF","#666666","lightgrey")) +
    labs(title="Fraction of time in each operating mode at 0 to 10F site comparison",x="Site ID-Manufacturer", 
         y="Fraction of Time") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          panel.grid.major = element_line(size = 0.9),
          panel.grid.minor = element_line(size = 0.1),
          axis.text.x = element_text(family = "Times New Roman", angle=-70, hjust=-0.5),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5)) 
}    
# Operation_5F_all_sites()
# Print graph to folder.
ggsave('Operating_Mode_0-10F_Site_Comparison.png',
       plot = Operation_5F_all_sites(),
       path = paste0(wd,'/Graphs/Site Comparison/'),
       width=12, height=4, units='in')
  
# 1e. Operating mode summary for all sites below 32F
# Print out data for each site for 0-10F range and for 32F and below range and below
write.csv(df %>% 
            filter(OA_TempF < 32) %>%
            mutate(Operating_Mode = ifelse(is.na(Operating_Mode), "Data Unavailable", Operating_Mode)) %>%
            group_by(Site_ID, Operating_Mode) %>%
            summarize(Mode_Time=n()),
          file=paste0(wd, "/Graphs/Graph Data/Operating Time 32F Below/", sitename, ".csv"),
          row.names=F)
Operation_32F_all_sites <- function(){
  list.files(path = paste0(wd, "/Graphs/Graph Data/Operating Time 32F Below/"),pattern="*.csv", full.names=T) %>% 
    map_df(~read.csv(.)) %>%
    merge(metadata %>% select(Site_ID, Manufacturer), by="Site_ID", all.x=T, all.y=F) %>%
    mutate(Site_Manufacturer = paste0(Manufacturer, "-", Site_ID)) %>% 
    ggplot(aes(x=Site_Manufacturer, fill=Operating_Mode, y=Mode_Time)) +
    geom_bar(position="fill", stat="identity") +
    scale_fill_manual(name = "Operating Mode", 
                      breaks = c("Defrost","Heating-HP Only","Heating-Aux Only","Heating-Aux/HP","Cooling","System Off","Data Unavailable"),
                      values = c("#009E73","#F0E442","#CC3300","#E69F00","#3333FF","#666666","lightgrey")) +
    labs(title="Fraction of time in each operating mode below 32F site comparison",x="Site ID-Manufacturer", 
         y="Fraction of Time") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          panel.grid.major = element_line(size = 0.9),
          panel.grid.minor = element_line(size = 0.1),
          axis.text.x = element_text(family = "Times New Roman", angle=-70, hjust=-0.5),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5)) 
}    
# Operation_32F_all_sites()
# Print graph to folder.
ggsave('Operating_Mode_32F_Site_Comparison.png',
       plot = Operation_32F_all_sites(),
       path = paste0(wd,'/Graphs/Site Comparison/'),
       width=12, height=4, units='in')







### Outdoor Air Graphs ----


# 2a. Heating mode cycling frequency site comparison
  # To identify short cycling and modulation.
write.csv(df %>% 
            filter(HP_Status=="On") %>%
            group_by(Site_ID) %>%
            summarize(Long_Cycles = sum(HP_Cycle_Runtimes > 15, na.rm=T)*3600/n(),
                      Med_Cycles = sum(HP_Cycle_Runtimes <= 15 & HP_Cycle_Runtimes >= 5, na.rm=T)*3600/n(),
                      Short_Cycles = sum(HP_Cycle_Runtimes < 5, na.rm=T)*3600/n(),
                      Avg_HP_Cycle_Duration = mean(HP_Cycle_Runtimes, na.rm=T)),
            file=paste0(wd, "/Graphs/Graph Data/Heat Pump Cycling/", sitename, ".csv"),
          row.names=F)
HeatCyclingComparison <- function(){
  list.files(path = paste0(wd, "/Graphs/Graph Data/Heat Pump Cycling/"),pattern="*.csv", full.names=T) %>% 
    map_df(~read.csv(.)) %>%
    gather(key = "Cycle_Length", value = "Cycles_Per_Hour", Long_Cycles:Short_Cycles) %>%
    merge(metadata %>% select(Site_ID, Manufacturer), by="Site_ID", all.x=T, all.y=F) %>%
    mutate(Site_Manufacturer = paste0(Manufacturer, "-", Site_ID)) %>% 
    ggplot(aes(x = Site_Manufacturer, y = Cycles_Per_Hour, fill = Cycle_Length)) + 
    geom_bar(stat="identity") +
    scale_fill_manual(limits = c("Long_Cycles", "Med_Cycles", "Short_Cycles"),
                      labels=c("Long (>15 mins)", "Medium (5-10 mins)", "Short (<5 mins)"), 
                      values=c("#F8766D", "#00BFC4", "#C77CFF")) +
    labs(title="Number of HP cycles per operating hour categorized by length site comparison",
         x="Manufacturer-Site ID",
         y="HP Cycles Per Hour") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          legend.title = element_blank(),
          axis.text.x = element_text(family = "Times New Roman", angle=-70, hjust=-0.5),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5))
}
HeatCyclingComparison()
ggsave('Heat_Cycling.png',
       plot = HeatCyclingComparison(),
       path = paste0(wd,'/Graphs/Site Comparison/'),
       width=12, height=4, units='in')

# 2b. Heating mode cycling frequency
# To identify modulation.
write.csv(df %>% 
            filter(HP_Status=="On" & OA_TempF <= temp_max & OA_TempF > temp_min) %>%
            mutate(temp_int = cut(OA_TempF,breaks=c(-50,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40,45,50,55))) %>%
            group_by(Site_ID, temp_int) %>%
            summarize(OA_TempF = median(OA_TempF, na.rm=T),
                      Cycle_Count = sum(!is.na(HP_Cycle_Runtimes)),
                      Frequency = sum(!is.na(HP_Cycle_Runtimes))*3600/n(),
                      Duration = median(HP_Cycle_Runtimes, na.rm=T)),
          file=paste0(wd, "/Graphs/Graph Data/Heat Pump Cycling OAT/", sitename, ".csv"),
          row.names=F)
HeatCyclingOAT <- function(site){
  list.files(path = paste0(wd, "/Graphs/Graph Data/Heat Pump Cycling OAT/"),pattern="*.csv", full.names=T) %>% 
    map_df(~read.csv(.)) %>%
    filter(Site_ID==site & Cycle_Count > 10) %>%
    ggplot(aes(x = OA_TempF)) + 
    geom_point(aes(y = Frequency, color = "Cycles Per Hour")) +
    geom_point(aes(y = Duration, color = "Cycle Duration")) +
    geom_line(aes(y = Frequency, color = "Cycles Per Hour", group=1)) +
    geom_line(aes(y = Duration, color = "Cycle Duration", group=2)) +
    scale_color_manual(values=c("black", "#00BFC4")) +
    scale_y_continuous(name = "Median HP Cycle Duration (mins)",
                       sec.axis = sec_axis(~., name ="Average HP Cycles Per Hour of Operation")) +
    labs(title=paste0("Heat pump cycle frequency and duration by OAT for site ", site),
         x="Outdoor Air Temperature (F)") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          legend.title = element_blank(),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5))
}
HeatCyclingOAT(sitename)
# Print graph to folder.
for(site in substr(list.files(path = paste0(wd, "/Graphs/Graph Data/Heat Pump Cycling OAT/"),pattern="*.csv", full.names=T),
                   nchar(list.files(path = paste0(wd, "/Graphs/Graph Data/Heat Pump Cycling OAT/"),pattern="*.csv", full.names=T)) - 9,
                   nchar(list.files(path = paste0(wd, "/Graphs/Graph Data/Heat Pump Cycling OAT/"),pattern="*.csv", full.names=T)) - 4)){
  ggsave(paste0(site, '_HP_Cycling_vs_OAT.png'),
         plot = HeatCyclingOAT(site),
         path = paste0(wd,'/Graphs/',site, '/'),
         width=12, height=4, units='in')
}

# 2c. Heat pump power variation/modulation
  # To identify ability to modulate to lower HP power.
write.csv(df %>% 
            filter(HP_Status=="On" & OA_TempF > temp_min & OA_TempF <= temp_max) %>%
            group_by(Site_ID, Date, Hour, minute(Timestamp)) %>%
            summarize(OA_TempF = median(OA_TempF, na.rm=T),
                      HP_Power = mean(HP_Power, na.rm=T)) %>%
            mutate(temp_int = cut(OA_TempF,breaks=c(-50,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40,45,50,55))),
          file=paste0(wd, "/Graphs/Graph Data/Heat Pump Power Variation/", sitename, ".csv"),
          row.names=F)
HPPowerVariation <- function(site){
  list.files(path = paste0(wd, "/Graphs/Graph Data/Heat Pump Power Variation/"),pattern="*.csv", full.names=T) %>% 
    map_df(~read.csv(.)) %>%
    filter(Site_ID==site) %>%
    mutate(temp_int = cut(OA_TempF,breaks=c(-50,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40,45,50,55))) %>%
    ggplot(aes(x = temp_int)) + 
    geom_boxplot(aes(y = HP_Power)) +
    labs(title=paste0("Heat pump power variation by OAT for site ", site),
         x="Outdoor Air Temperature Bin (F)",
         y="Heat Pump Power (kW)") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          legend.title = element_blank(),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5))
}
HPPowerVariation(sitename)
# Print graph to folder.
for(site in substr(list.files(path = paste0(wd, "/Graphs/Graph Data/Heat Pump Power Variation/"),pattern="*.csv", full.names=T),
                   nchar(list.files(path = paste0(wd, "/Graphs/Graph Data/Heat Pump Power Variation/"),pattern="*.csv", full.names=T)) - 9,
                   nchar(list.files(path = paste0(wd, "/Graphs/Graph Data/Heat Pump Power Variation/"),pattern="*.csv", full.names=T)) - 4)){
  ggsave(paste0(site, '_HP_Power_Variation_vs_OAT_Bin.png'),
         plot = HPPowerVariation(site),
         path = paste0(wd,'/Graphs/',site, '/'),
         width=12, height=4, units='in')
}



# 3a. Defrost mode cycling frequency by OAT bin
write.csv(df %>% 
            mutate(temp_int = cut(OA_TempF,breaks=c(-50,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40,45,50,55)),
                   Time_Ratio = NA) %>%
            filter(OA_TempF <= temp_max & OA_TempF > temp_min & !is.na(Defrost_Cycle_Runtimes)) %>%
            select(Site_ID, OA_TempF, temp_int, Time_Ratio, Defrost_Cycle_Runtimes) %>%
            rbind(df %>% filter(OA_TempF <= temp_max & OA_TempF > temp_min) %>%
                    group_by(Site_ID, temp_int=cut(OA_TempF,breaks=c(-50,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40,45,50,55))) %>%
                    summarize(OA_TempF = median(OA_TempF, na.rm=T),
                              Time_Ratio = sum(Defrost_Cycle_Runtimes,na.rm=T)*60*100/sum(HP_Status=="On",na.rm=T),
                              Defrost_Cycle_Runtimes = NA)) %>%
            ungroup(),
          file=paste0(wd, "/Graphs/Graph Data/Defrost OAT/", sitename, ".csv"),
          row.names=F)
DefrostCyclingOAT <- function(site){
  list.files(path = paste0(wd, "/Graphs/Graph Data/Defrost OAT/"),pattern="*.csv", full.names=T) %>% 
    map_df(~read.csv(.)) %>%
    filter(Site_ID==site) %>%
    mutate(temp_int = cut(OA_TempF,breaks=c(-50,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40,45,50,55))) %>%
    ggplot(aes(x = temp_int)) + 
    geom_boxplot(aes(y = Defrost_Cycle_Runtimes, color="Cycle Duration (Boxplot)"), show.legend = F) +
    geom_point(aes(y=Time_Ratio, color="Time Ratio"), size=5) +
    scale_color_manual(name="", values=c("black", "#D55E00")) +
    scale_y_continuous(name = "Defrost Cycle Duration (mins)",
                       sec.axis = sec_axis(~., name ="Defrost Time Ratio (%)")) +
    labs(title=paste0("Defrost time ratio and cycle duration per OAT bin for site ",site),
         x="Outdoor Air Temperature Bin (F)") +
    theme_bw() +
    guides(color = guide_legend(override.aes = list(shape=c(0,19)))) +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          legend.title = element_blank(),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5))
}
DefrostCyclingOAT(sitename)
# Print graph to folder.
for(site in substr(list.files(path = paste0(wd, "/Graphs/Graph Data/Defrost OAT/"),pattern="*.csv", full.names=T),
                   nchar(list.files(path = paste0(wd, "/Graphs/Graph Data/Defrost OAT/"),pattern="*.csv", full.names=T)) - 9,
                   nchar(list.files(path = paste0(wd, "/Graphs/Graph Data/Defrost OAT/"),pattern="*.csv", full.names=T)) - 4)){
  ggsave(paste0(site, '_Defrost_Cycling_vs_OAT_Bin.png'),
       plot = DefrostCyclingOAT(site),
       path = paste0(wd,'/Graphs/',site, '/'),
       width=12, height=4, units='in')
}

# 3b. Print values for site comparison table
print_defrost_summary <- function(site){
  ft <- ifelse(df$Site_ID[1] %in% c("2458CE", "5291QJ"), 5, 1)
  temp <- read.csv(paste0(wd, '/Graphs/Site Comparison/Defrost Summary.csv')) %>%
    filter(Site_ID != site) %>% rbind(df %>% filter(Site_ID==site) %>% group_by(Site_ID) %>%
                                        summarize(Defrost_Frequency = round(sum(!is.na(Defrost_Cycle_Runtimes))*3600/ft/sum(HP_Status=="On",na.rm=T),2),
                                                  Avg_Defrost_Duration = round(mean(Defrost_Cycle_Runtimes,na.rm=T),1),
                                                  Med_Defrost_Duration = round(median(Defrost_Cycle_Runtimes,na.rm=T),1),
                                                  Defrost_Time_Ratio = round(sum(Defrost_Cycle_Runtimes,na.rm=T)*60*100/sum(HP_Status=="On",na.rm=T),1),
                                                  HP_Frequency = round(sum(!is.na(HP_Cycle_Runtimes))*3600/ft/sum(HP_Status=="On",na.rm=T),2),
                                                  Avg_HP_Duration = round(mean(HP_Cycle_Runtimes,na.rm=T),1),
                                                  Med_HP_Duration = round(median(HP_Cycle_Runtimes,na.rm=T),1)))
  
  write.csv(temp, paste0(wd, '/Graphs/Site Comparison/Defrost Summary.csv'), row.names = F)
}
print_defrost_summary(sitename)


                                       
# 4. Defrost mode cycling frequency by RH bin
  # ascale is used to match the two axes--can be adjusted manually.
  # Note: 5539NO does not have OA humidity data, so do not save data for this site.
write.csv(df %>% 
            mutate(hum_int = cut(OA_RH,breaks=c(0,10,20,30,40,50,60,70,80,90,100)),
                   Time_Ratio = NA) %>%
            filter(!is.na(Defrost_Cycle_Runtimes) & OA_RH > 0 & OA_RH <= 100) %>%
            select(Site_ID, hum_int, Time_Ratio, Defrost_Cycle_Runtimes) %>%
            rbind(df %>% filter(OA_RH > 0 & OA_RH <= 100) %>%
                    group_by(Site_ID, hum_int=cut(OA_RH,breaks=c(0,10,20,30,40,50,60,70,80,90,100))) %>%
                    summarize(Time_Ratio = sum(Defrost_Cycle_Runtimes,na.rm=T)*60*100/sum(HP_Status=="On",na.rm=T),
                              Defrost_Cycle_Runtimes = NA)) %>%
            ungroup(),
          file=paste0(wd, "/Graphs/Graph Data/Defrost RH/", sitename, ".csv"),
          row.names=F)
DefrostCyclingRH <- function(site){
  list.files(path = paste0(wd, "/Graphs/Graph Data/Defrost RH/"),pattern="*.csv", full.names=T) %>% 
    map_df(~read.csv(.)) %>%
    filter(Site_ID==site) %>%
    ggplot(aes(x = hum_int)) + 
    geom_boxplot(aes(y = Defrost_Cycle_Runtimes, color="Cycle Duration (Boxplot)"), show.legend = F) +
    geom_point(aes(y=Time_Ratio*2, color="Time Ratio"), size=5) +
    scale_color_manual(name="", values=c("black", "#D55E00")) +
    scale_y_continuous(name = "Defrost Cycle Duration (mins)",
                       sec.axis = sec_axis(~./2, name ="Defrost Time Ratio (%)")) +
    labs(title=paste0("Defrost time ratio and cycle duration per RH bin for site ",sitename),
         x="Outdoor Relative Humidity Bin (%)") +
    theme_bw() +
    guides(color = guide_legend(override.aes = list(shape=c(0,19)))) +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          legend.title = element_blank(),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5))
}
DefrostCyclingRH(sitename)
# Print graph to folder.
for(site in substr(list.files(path = paste0(wd, "/Graphs/Graph Data/Defrost RH/"),pattern="*.csv", full.names=T),
                   nchar(list.files(path = paste0(wd, "/Graphs/Graph Data/Defrost RH/"),pattern="*.csv", full.names=T)) - 9,
                   nchar(list.files(path = paste0(wd, "/Graphs/Graph Data/Defrost RH/"),pattern="*.csv", full.names=T)) - 4)){
  ggsave(paste0(site, '_Defrost_Cycling_vs_RH_Bin.png'),
       plot = DefrostCyclingRH(site),
       path = paste0(wd,'/Graphs/',site, '/'),
       width=12, height=4, units='in')
}



# 5a. Aux staging by OAT bin without defrost
  # Save data to file to be able to easily reproduce sites.
write.csv(df %>% 
            filter(OA_TempF <= temp_max & OA_TempF > temp_min & !is.na(Operating_Mode)) %>%
            mutate(temp_int = cut(OA_TempF,breaks=c(-50,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40,45,50,55))) %>%
            group_by(Site_ID, temp_int) %>%
            mutate(OA_TempF = median(OA_TempF, na.rm=T),
                   Temp_Bin_Tim = n()) %>%
            ungroup() %>%
            group_by(Site_ID, temp_int, Number_Aux_Legs) %>%
            summarize(OA_TempF = median(OA_TempF, na.rm=T),
                      Average_Duration_Defrost = n()*100/mean(Temp_Bin_Tim),
                      Average_Duration_No_Defrost = sum(Operating_Mode != "Defrost", na.rm=T)*100/mean(Temp_Bin_Tim)) %>%
            filter(!is.na(Number_Aux_Legs)),
          file=paste0(wd, "/Graphs/Graph Data/Aux Staging/", sitename, ".csv"),
          row.names=F)
AuxStagingNoDefrost <- function(site){
  list.files(path = paste0(wd, "/Graphs/Graph Data/Aux Staging/"),pattern="*.csv", full.names=T) %>% 
    map_df(~read.csv(.)) %>%
    filter(Site_ID==site) %>%
    mutate(temp_int = cut(OA_TempF,breaks=c(-50,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40,45,50,55))) %>%
    ggplot(aes(x=temp_int)) +
    geom_bar(stat="identity", aes(y = Average_Duration_No_Defrost, fill = as.character(Number_Aux_Legs))) +
    scale_y_continuous(name = "Percent of Time",
                       limits = c(0,100)) +
    labs(title=paste0("Auxiliary heat use (excluding defrost) by outdoor temperature bin for site ", site),
         x="Temperature (F)", fill="Aux Stage") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          panel.grid.major = element_line(size = 0.9),
          panel.grid.minor = element_line(size = 0.1),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5))
}
AuxStagingNoDefrost(sitename)
# Print graph to folder in loop for all sites.
for(site in substr(list.files(path = paste0(wd, "/Graphs/Graph Data/Aux Staging/"),pattern="*.csv", full.names=T),
                   nchar(list.files(path = paste0(wd, "/Graphs/Graph Data/Aux Staging/"),pattern="*.csv", full.names=T)) - 9,
                   nchar(list.files(path = paste0(wd, "/Graphs/Graph Data/Aux Staging/"),pattern="*.csv", full.names=T)) - 4)){
  ggsave(paste0(site, '_Aux_Use_vs_OAT_Bin.png'),
         plot = AuxStagingNoDefrost(site),
         path = paste0(wd,'/Graphs/',site, '/'),
         width=12, height=4, units='in')
}
# 5b. Aux staging by OAT bin with defrost
AuxStagingDefrost <- function(site){
  list.files(path = paste0(wd, "/Graphs/Graph Data/Aux Staging/"),pattern="*.csv", full.names=T) %>% 
    map_df(~read.csv(.)) %>%
    filter(Site_ID==site) %>%
    mutate(temp_int = cut(OA_TempF,breaks=c(-50,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40,45,50,55))) %>%
    ggplot(aes(x=temp_int)) +
    geom_bar(stat="identity", aes(y = Average_Duration_Defrost, fill = as.character(Number_Aux_Legs))) +
    scale_y_continuous(name = "Percent of Time",
                       limits = c(0,100)) +
    labs(title=paste0("Auxiliary heat use (including defrost) by outdoor temperature bin for site ", site),
         x="Temperature (F)", fill="Aux Stage") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          panel.grid.major = element_line(size = 0.9),
          panel.grid.minor = element_line(size = 0.1),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5))
}
AuxStagingDefrost(sitename)
# Print graph to folder in loop for all sites.
for(site in substr(list.files(path = paste0(wd, "/Graphs/Graph Data/Aux Staging/"),pattern="*.csv", full.names=T),
                   nchar(list.files(path = paste0(wd, "/Graphs/Graph Data/Aux Staging/"),pattern="*.csv", full.names=T)) - 9,
                   nchar(list.files(path = paste0(wd, "/Graphs/Graph Data/Aux Staging/"),pattern="*.csv", full.names=T)) - 4)){
  ggsave(paste0(site, '_Aux_Use_vs_OAT_Bin_w_Defrost.png'),
         plot = AuxStagingDefrost(site),
         path = paste0(wd,'/Graphs/',site, '/'),
         width=12, height=4, units='in')
}

# 6a. Heating capacity (i.e., heating load) (Btu/h) by OAT bin
# Save data to file to be able to easily reproduce sites.
write.csv(df %>% 
            mutate(temp_int = cut(OA_TempF,breaks=c(-50,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40,45,50,55))) %>%
            filter(OA_TempF <= temp_max & OA_TempF > temp_min) %>%
            group_by(Site_ID, temp_int) %>%
            summarize(HP_Capacity = mean(Heat_Output_Btu_h - Aux_Power*3412, na.rm=T),
                      Aux_Capacity = mean(Aux_Power*3412, na.rm=T)),
          file=paste0(wd, "/Graphs/Graph Data/Heating Capacity/", sitename, ".csv"),
          row.names=F)
HeatCapacityOATBin <- function(site){
  list.files(path = paste0(wd, "/Graphs/Graph Data/Heating Capacity/"),pattern="*.csv", full.names=T) %>% 
    map_df(~read.csv(.)) %>%
    filter(Site_ID==site) %>%
    mutate(temp_int = factor(temp_int, levels=temp_int)) %>%
    gather(Heat_Element, Capacity, HP_Capacity:Aux_Capacity) %>%
    ggplot(aes(x = temp_int, y = Capacity, fill = Heat_Element)) + 
    geom_bar(stat="identity", position="stack") +
    scale_fill_manual(limits = c("HP_Capacity", "Aux_Capacity"),
                      labels=c("Heat Pump", "Auxiliary Heat"),
                      values=c("#00BFC4", "#F8766D")) +
    labs(title=paste0("Delivered heat capacity per OAT bin for site ",site),
         x="Outdoor Air Temperature Bin (F)",
         y="Delivered Heating Capacity (Btu/hr)",
         fill="Heating Element") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          legend.title = element_blank(),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5))
}
HeatCapacityOATBin(sitename)
for(site in substr(list.files(path = paste0(wd, "/Graphs/Graph Data/Heating Capacity/"),pattern="*.csv", full.names=T),
                   nchar(list.files(path = paste0(wd, "/Graphs/Graph Data/Heating Capacity/"),pattern="*.csv", full.names=T)) - 9,
                   nchar(list.files(path = paste0(wd, "/Graphs/Graph Data/Heating Capacity/"),pattern="*.csv", full.names=T)) - 4)){
  ggsave(paste0(site, '_Heat_Capacity_vs_OAT_Bin.png'),
       plot = HeatCapacityOATBin(site),
       path = paste0(wd,'/Graphs/',site, '/'),
       width=12, height=4, units='in')
}


# 6b. Table to show the "maximum" heating capacity in each OAT bin
  # Using the 95th percentile to account for potential outliers.
  # Need to transpose data manually to match table in report.
print_heat_capacity <- function(site){
  temp <- read.csv(paste0(wd, '/Graphs/Site Comparison/Max Heat Capacity.csv')) %>%
    filter(Site_ID != site) %>% 
    rbind(df %>% filter(Site_ID==site & OA_TempF <= temp_max & OA_TempF > temp_min & !is.na(Operating_Mode) & Operating_Mode != "System Off") %>%
            mutate(temp_int = cut(OA_TempF,breaks=c(-50,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40,45,50,55))) %>%
            group_by(Site_ID, temp_int) %>%
            summarize(Max_Capacity = round(quantile(Heat_Output_Btu_h, c(0.95), na.rm = T)/1000),
                      Max_HP_Capacity = round(quantile(Heat_Output_Btu_h[Operating_Mode=="Heating-HP Only"], c(0.95), na.rm = T)/1000),
                      Max_Aux_Capacity = round(quantile(Heat_Output_Btu_h[Operating_Mode=="Heating-Aux Only"], c(0.95), na.rm = T)/1000)))
  
  write.csv(temp, paste0(wd, '/Graphs/Site Comparison/Max Heat Capacity.csv'), row.names = F)
}
print_heat_capacity(sitename)

# 6c. Table to show COP in aux only mode as a comparison to the expected 100% efficiency.
  # Hours will need to be multiplied by 5 for NRCan sites
print_aux_only_cop <- function(site){
  ft <- ifelse(df$Site_ID[1] %in% c("2458CE", "5291QJ"), 5, 1)
  temp <- read.csv(paste0(wd, '/Graphs/Site Comparison/Aux Only COP.csv')) %>%
    filter(Site_ID != site) %>% 
    rbind(df %>% filter(Site_ID==site & !is.na(Operating_Mode) & Operating_Mode=="Heating-Aux Only") %>%
  group_by(Site_ID) %>%
  summarize(Aux_Only_Hours = round(n()*ft/3600,1),
            Avg_Heat_Output = round(mean(Heat_Output_Btu_h, na.rm = T)/3412,2),
            Avg_Power_Input = round(mean(Aux_Power, na.rm = T),2),
            Avg_COP = round(Avg_Heat_Output / Avg_Power_Input,2)))
  
  write.csv(temp, paste0(wd, '/Graphs/Site Comparison/Aux Only COP.csv'), row.names = F)
}
print_aux_only_cop(sitename)

# 7a. COP vs outdoor air temperature for each site
  # When printing individual site graphs, export the datapoints needed to make this graph 
write.csv(df %>% 
            mutate(temp_int = cut(OA_TempF,breaks=c(-50,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40,45,50,55))) %>%
            filter(OA_TempF < temp_max & OA_TempF > temp_min) %>%
            group_by(Site_ID, temp_int) %>%
            summarize(OA_TempF = median(OA_TempF, na.rm=T),
                      COP_Total = sum(Heat_Output_Btu_h, na.rm=T)/sum(Total_Power, na.rm=T)/3412),
          file=paste0(wd, "/Graphs/Graph Data/COP by OAT Bin/", sitename, ".csv"),
          row.names=F)
Heat_COP_all_sites <- function(manufacturers){
  list.files(path = paste0(wd, "/Graphs/Graph Data/COP by OAT Bin/"),pattern="*.csv", full.names=T) %>% 
    map_df(~read.csv(.)) %>%
    merge(metadata %>% select(Site_ID, Manufacturer), by="Site_ID", all.x=T, all.y=F) %>%
    filter(Manufacturer %in% manufacturers) %>%
    mutate(Site_Manufacturer = paste0(Manufacturer, "-", Site_ID)) %>% 
    ggplot(aes(x = OA_TempF)) + 
    geom_point(size = 3, aes(y = COP_Total, color = Site_Manufacturer)) +
    geom_line(aes(y = COP_Total, color = Site_Manufacturer)) +
    scale_x_continuous(breaks = seq(-30, 60, by=10),
                       minor_breaks = seq(-30, 60, by=5)) +
    geom_hline(yintercept=0) +
    labs(title="Overall system COP vs. outdoor air temperature",
         x="Outdoor Temperature (F)",
         y="COP",
         color="Manufacturer-Site ID") +
    theme_bw() +
    theme(axis.ticks.y=element_blank(),
          panel.border = element_rect(colour = "black",fill=NA),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5),)
}
# Heat_COP_all_sites(unique(metadata$Manufacturer))
# Print graph to folder.
ggsave('COP_vs_OAT_Bin.png',
       plot = Heat_COP_all_sites(unique(metadata$Manufacturer)),
       path = paste0(wd,'/Graphs/Site Comparison/'),
       width=12, height=4, units='in')
# Print graph to folder with one for each manufacturer
for(manu in unique(metadata$Manufacturer)){
  ggsave(paste0('COP_vs_OAT_Bin_', manu, '.png'),
       plot = Heat_COP_all_sites(manu),
       path = paste0(wd,'/Graphs/Site Comparison/'),
       width=12, height=4, units='in')
}

# 7b. HP compressor only COP vs outdoor air temperature for each site
  # When printing individual site graphs, export the datapoints needed to make this graph 
write.csv(df %>% 
            mutate(temp_int = cut(OA_TempF,breaks=c(-50,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40,45,50,55))) %>%
            filter(OA_TempF < temp_max & OA_TempF > temp_min & !is.na(Operating_Mode) & 
                     Operating_Mode %in% c("Heating-HP Only","Heating-Aux/HP")) %>%
            group_by(Site_ID, temp_int) %>%
            summarize(OA_TempF = median(OA_TempF, na.rm=T),
                      COP_HP = sum(Heat_Output_Btu_h - 3412*Aux_Power, na.rm=T)/sum(HP_Power + Fan_Power, na.rm=T)/3412),
          file=paste0(wd, "/Graphs/Graph Data/HP COP by OAT Bin/", sitename, ".csv"),
          row.names=F)
Heat_COP_HP_all_sites <- function(manufacturers, spec1, spec2){
  list.files(path = paste0(wd, "/Graphs/Graph Data/HP COP by OAT Bin/"),pattern="*.csv", full.names=T) %>% 
    map_df(~read.csv(.)) %>%
    merge(metadata %>% select(Site_ID, Manufacturer), by="Site_ID", all.x=T, all.y=F) %>%
    filter(Manufacturer %in% manufacturers) %>%
    mutate(Site_Manufacturer = paste0(Manufacturer, "-", Site_ID)) %>% 
    ggplot(aes(x = OA_TempF)) + 
    geom_point(size = 3, aes(y = COP_HP, color = Site_Manufacturer)) +
    geom_line(aes(y = COP_HP, color = Site_Manufacturer)) +
    scale_x_continuous(breaks = seq(-30, 60, by=10),
                       minor_breaks = seq(-30, 60, by=5)) +
    geom_hline(yintercept=spec1, linetype="dashed") +
    geom_hline(yintercept=spec2, linetype="dashed") +
    geom_vline(xintercept=5) +
    geom_hline(yintercept=0) +
    labs(title="Demonstrated heat pump COP (excluding defrost) vs. outdoor air temperature",
         x="Outdoor Temperature (F)",
         y="COP",
         color="Manufacturer-Site ID") +
    theme_bw() +
    theme(axis.ticks.y=element_blank(),
          panel.border = element_rect(colour = "black",fill=NA),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5),)
}
# Heat_COP_HP_all_sites(unique(metadata$Manufacturer), 2.1, 2.4)
# Print graph to folder.
ggsave('HP_COP_vs_OAT_Bin.png',
       plot = Heat_COP_HP_all_sites(unique(metadata$Manufacturer), 2.1, 2.4),
       path = paste0(wd,'/Graphs/Site Comparison/'),
       width=12, height=4, units='in')
# Print graph to folder with one for each manufacturer
for(manu in unique(metadata$Manufacturer)){
  ggsave(paste0('HP_COP_vs_OAT_Bin_', manu, '.png'),
         plot = Heat_COP_HP_all_sites(manu, mean(metadata$COP_Spec[metadata$Manufacturer==manu], na.rm=T), 0),
         path = paste0(wd,'/Graphs/Site Comparison/'),
         width=12, height=4, units='in')
}

# 7c. HP compressor only COP vs outdoor air temperature for each site with defrost
# When printing individual site graphs, export the datapoints needed to make this graph 
write.csv(df %>% 
            mutate(temp_int = cut(OA_TempF,breaks=c(-50,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40,45,50,55))) %>%
            filter(OA_TempF < temp_max & OA_TempF > temp_min & !is.na(Operating_Mode) & 
                     Operating_Mode %in% c("Heating-HP Only","Heating-Aux/HP","Defrost")) %>%
            group_by(Site_ID, temp_int) %>%
            summarize(OA_TempF = median(OA_TempF, na.rm=T),
                      COP_HP = sum(Heat_Output_Btu_h - 3412*Aux_Power, na.rm=T)/sum(HP_Power + Fan_Power, na.rm=T)/3412),
          file=paste0(wd, "/Graphs/Graph Data/HP COP by OAT Bin w Defrost/", sitename, ".csv"),
          row.names=F)
Heat_COP_HP_all_sites_defrost <- function(manufacturers, spec1, spec2){
  list.files(path = paste0(wd, "/Graphs/Graph Data/HP COP by OAT Bin w Defrost/"),pattern="*.csv", full.names=T) %>% 
    map_df(~read.csv(.)) %>%
    merge(metadata %>% select(Site_ID, Manufacturer), by="Site_ID", all.x=T, all.y=F) %>%
    filter(Manufacturer %in% manufacturers) %>%
    mutate(Site_Manufacturer = paste0(Manufacturer, "-", Site_ID)) %>% 
    ggplot(aes(x = OA_TempF)) + 
    geom_point(size = 3, aes(y = COP_HP, color = Site_Manufacturer)) +
    geom_line(aes(y = COP_HP, color = Site_Manufacturer)) +
    scale_x_continuous(breaks = seq(-30, 60, by=10),
                       minor_breaks = seq(-30, 60, by=5)) +
    geom_hline(yintercept=spec1, linetype="dashed") +
    geom_hline(yintercept=spec2, linetype="dashed") +
    geom_vline(xintercept=5) +
    geom_hline(yintercept=0) +
    scale_linetype_manual(values=c("dashed", "dotted"), name="") +
    labs(title="Demonstrated heat pump COP (including defrost) vs. outdoor air temperature",
         x="Outdoor Temperature (F)",
         y="COP",
         color="Manufacturer-Site ID") +
    theme_bw() +
    theme(axis.ticks.y=element_blank(),
          panel.border = element_rect(colour = "black",fill=NA),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5),)
}
# Heat_COP_HP_all_sites_defrost(unique(metadata$Manufacturer), 2.1, 2.4)
# Print graph to folder.
ggsave('HP_COP_vs_OAT_Bin_w_Defrost.png',
       plot = Heat_COP_HP_all_sites_defrost(unique(metadata$Manufacturer), 2.1, 2.4),
       path = paste0(wd,'/Graphs/Site Comparison/'),
       width=12, height=4, units='in')
# Print graph to folder with one for each manufacturer
# for(manu in unique(metadata$Manufacturer)){
#   ggsave(paste0('HP_COP_vs_OAT_Bin_w_Defrost', manu, '.png'),
#          plot = Heat_COP_HP_all_sites_defrost(manu, mean(metadata$COP_Spec[metadata$Manufacturer==manu], na.rm=T), 0),
#          path = paste0(wd,'/Graphs/Site Comparison/'),
#          width=12, height=4, units='in')
# }


# 7d. Adjusted COP vs outdoor air temperature for each site
  # When printing individual site graphs, export the datapoints needed to make this graph 
write.csv(df %>% 
            mutate(temp_int = cut(OA_TempF,breaks=c(-50,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40,45,50,55))) %>%
            filter(OA_TempF < temp_max & OA_TempF > temp_min) %>%
            group_by(Site_ID, temp_int) %>%
            summarize(OA_TempF = median(OA_TempF, na.rm=T),
                      Adjusted_COP_Total = sum(Heat_Output_Btu_h_adjusted, na.rm=T)/sum(Total_Power, na.rm=T)/3412),
          file=paste0(wd, "/Graphs/Graph Data/Adjusted COP by OAT Bin/", sitename, ".csv"),
          row.names=F)
Adjusted_Heat_COP_all_sites <- function(){
  list.files(path = paste0(wd, "/Graphs/Graph Data/Adjusted COP by OAT Bin/"),pattern="*.csv", full.names=T) %>% 
    map_df(~read.csv(.)) %>%
    merge(metadata %>% select(Site_ID, Manufacturer), by="Site_ID", all.x=T, all.y=F) %>%
    mutate(Site_Manufacturer = paste0(Manufacturer, "-", Site_ID)) %>% 
    ggplot(aes(x = OA_TempF)) + 
    geom_point(size = 3, aes(y = Adjusted_COP_Total, color = Site_Manufacturer)) +
    geom_line(aes(y = Adjusted_COP_Total, color = Site_Manufacturer)) +
    scale_x_continuous(breaks = seq(-30, 60, by=10),
                       minor_breaks = seq(-30, 60, by=5)) +
    geom_hline(yintercept=0) +
    labs(title="Adjusted COP vs. outdoor air temperature",
         x="Outdoor Temperature (F)",
         y="Adjusted COP",
         color="Manufacturer-Site ID") +
    theme_bw() +
    theme(axis.ticks.y=element_blank(),
          panel.border = element_rect(colour = "black",fill=NA),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5),)
}
# Adjusted_Heat_COP_all_sites()
# Print graph to folder.
ggsave('Adjusted_COP_vs_OAT_Bin.png',
       plot = Adjusted_Heat_COP_all_sites(),
       path = paste0(wd,'/Graphs/Site Comparison/'),
       width=12, height=4, units='in')


# 8. COP vs outdoor air temperature
write.csv(df %>% 
            group_by(Site_ID, Date, Hour) %>%
            summarize(OA_TempF = median(OA_TempF, na.rm=T),
                      Total_COP_Heating = sum(Heat_Output_Btu_h, na.rm=T)/sum(Total_Power, na.rm=T)/3412,
                      Percent_System_Off = sum(Operating_Mode=="System Off", na.rm=T)/n(),
                      Percent_Defrost = sum(Operating_Mode=="Defrost", na.rm=T)/n(),
                      Percent_HP = sum(Operating_Mode=="Heating-HP Only", na.rm=T)/n(),
                      Percent_HP_Aux = sum(Operating_Mode=="Heating-Aux/HP", na.rm=T)/n(),
                      Percent_Aux = sum(Operating_Mode=="Heating-Aux Only", na.rm=T)/n()),
          file=paste0(wd, "/Graphs/Graph Data/COP/", sitename, ".csv"),
          row.names=F)
COPOAT <- function(site){
  list.files(path = paste0(wd, "/Graphs/Graph Data/COP/"),pattern="*.csv", full.names=T) %>% 
    map_df(~read.csv(.)) %>%
    filter(Site_ID==site) %>%
    filter(Percent_System_Off < 0.9) %>%
    mutate(Dominant_Mode = ifelse(Percent_Defrost > 0.05, "Defrost", 
                                  ifelse(Percent_HP >= Percent_Aux & Percent_HP >= Percent_HP_Aux, "Heat Pump",
                                         ifelse(Percent_HP_Aux >= Percent_Aux, "Heat Pump + Aux Heat", "Aux Heat")))) %>%
  ggplot(aes(x = OA_TempF)) + 
    geom_point(aes(y = Total_COP_Heating, color=Dominant_Mode), size=0.8) +
    geom_hline(yintercept = 0) +
    ylim(0, NA) +
    labs(title=paste0("Hourly COP vs outdoor air temperature for site ", site),
         x="Outdoor Air Temperature (F)",
         y="COP",
         color="Dominant Mode") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5)) +
    guides(color=guide_legend(override.aes=list(size=3)))
}
COPOAT(sitename)
# Print graph to folder. 
for(site in substr(list.files(path = paste0(wd, "/Graphs/Graph Data/COP/"),pattern="*.csv", full.names=T),
                   nchar(list.files(path = paste0(wd, "/Graphs/Graph Data/COP/"),pattern="*.csv", full.names=T)) - 9,
                   nchar(list.files(path = paste0(wd, "/Graphs/Graph Data/COP/"),pattern="*.csv", full.names=T)) - 4)){
  ggsave(paste0(site, '_COP_vs_OAT.png'),
       plot = COPOAT(site),
       path = paste0(wd,'/Graphs/',site, '/'),
       width=12, height=4, units='in')
}



#### Old graphs ----
# COP vs OAT in a point plot
COPOATPoint <- function(site){
  df %>% 
    filter(Site_ID==site) %>%
    group_by(Site_ID, Date, Hour) %>%
    summarize(OA_TempF = mean(OA_TempF, na.rm=T),
              Total_COP_Heating = mean(COP_Heating[Operating_Mode=="Heating-Aux Only" | Operating_Mode=="Heating-Aux/HP"], na.rm=T),
              HP_COP_Heating = mean(COP_Heating[Operating_Mode=="Heating-HP Only"], na.rm=T)) %>%
    ggplot(aes(x = OA_TempF)) + 
    geom_point(aes(y = Total_COP_Heating, color = "Aux or Aux+HP Heating")) +
    geom_point(aes(y = HP_COP_Heating, color = "HP Only Heating")) +
    scale_color_manual(values=c("#CC79A7","#009E73","#E69F00")) +
    labs(title=paste0("COP vs OAT by operating mode without defrost for site ",site),
         x="Outdoor Temperature (F)",
         y="COP") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          legend.title = element_blank(),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5))
}
# COPOATPoint("9944LD")
for(id in unique(df$Site_ID)){
  ggsave(paste0(id, '_COP_vs_OAT.png'),
         plot = COPOATPoint(id),
         path = paste0(wd,'/Graphs/',id, '/'),
         width=12, height=4, units='in')
}


# Number of defrost run cycles and average length of cycle per day
DailyDefrost <- function(site, timestart, timeend){
  df %>% mutate(Timestamp = Timestamp %>% with_tz(metadata$Timezone[metadata$Site_ID==site])) %>%
    filter(Site_ID == site &
             Timestamp >= strptime(timestart,"%m/%d/%Y", tz=metadata$Timezone[metadata$Site_ID==site]) &
             Timestamp <= strptime(timeend,"%m/%d/%Y", tz=metadata$Timezone[metadata$Site_ID==site])) %>%
    group_by(Date) %>%
    summarize(Timestamp = Timestamp[1],
              Percent_Defrost = sum(Defrost_Cycle_Runtimes, na.rm=T)*100*60/n(),
              OA_Temp = mean(OA_TempF,na.rm=T),
              OA_RH = mean(OA_RH,na.rm=T)) %>%
    ggplot(aes(x=as.POSIXct(Timestamp))) +
    geom_line(aes(y = OA_Temp/10, color="Outdoor Temperature", group=1)) +
    geom_line(aes(y = OA_RH/10, color="Outdoor Humidity", group=2)) +
    geom_point(size = 2, aes(y = Percent_Defrost, color="Defrost Duration")) +
    geom_line(aes(y = Percent_Defrost , color="Defrost Duration", group=3)) +
    scale_y_continuous(name = "Percent in Defrost (%)",
                       sec.axis = sec_axis(~.*10, name ="Humidity (%)/Temperature (F)")) +
    scale_color_manual(name = "", values = c("#D55E00","grey", "black","#CC79A7","#E69F00","#009E73","#F0E442")) +
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
# DailyDefrost("4228VB", "2/10/2023", "3/10/2023")
# for(id in unique(df$Site_ID)){
#   ggsave(paste0(id, '_Defrost_Mode_Percent_Time.png'),
#          plot = DailyDefrost(id, "2/20/2023", "3/21/2023"),
#          path = paste0(wd,'/Graphs/',id, '/'),
#          width=12, height=4, units='in')
# }


# Power usage vs. outdoor temperature
PowerUsageOAT <- function(site){
  df %>% filter(OA_TempF <= temp_max & OA_TempF > temp_min) %>%
    mutate(temp_int = cut(OA_TempF,breaks=c(-50,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40,45,50,55))) %>%
    group_by(Site_ID, temp_int) %>% 
    summarize(OA_TempF = median(OA_TempF, na.rm=T),
              HP_Power = sum(HP_Power, na.rm=T) / n(),
              Total_Power = sum(Total_Power, na.rm=T) / n()) %>%
    ggplot(aes(x = OA_TempF)) + 
    geom_line(size=1, aes(y = HP_Power, color="Heat Pump ODU")) +
    geom_line(size=1, aes(y = Total_Power, color="System Total")) +
    geom_point(size=2, aes(y = HP_Power, color="Heat Pump ODU")) +
    geom_point(size=2, aes(y = Total_Power, color="System Total")) +
    scale_color_manual(name="", values=c("#009E73", "black")) +
    scale_x_continuous(breaks = seq(-30, 60, by=10),
                       minor_breaks = seq(-30, 60, by=5)) +
    labs(title=paste0("Average power vs outdoor temperature for site ",site),
         x="Outdoor Temperature (F)", y="Power (kW)") +
    theme_bw() +
    theme(axis.ticks.y=element_blank(),
          panel.border = element_rect(colour = "black",fill=NA),
          legend.title = element_blank(),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5),)
  
}
# PowerUsageOAT(sitename)
# Print graph to folder.
ggsave(paste0(sitename, '_Power_vs_OAT.png'),
       plot = PowerUsageOAT(sitename),
       path = paste0(wd,'/Graphs/',sitename, '/'),
       width=12, height=4, units='in')

# COP vs outdoor air temperature
Heat_COP <- function(){
  df %>% 
    mutate(temp_int = cut(OA_TempF,breaks=c(-50,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40,45,50,55))) %>%
    filter(OA_TempF < temp_max & OA_TempF > temp_min) %>%
    group_by(temp_int) %>% 
    summarize(OA_TempF = median(OA_TempF, na.rm=T),
              COP_HP = sum(Heat_Output_Btu_h[Operating_Mode=="Heating-HP Only"], na.rm=T)/sum(Total_Power[Operating_Mode=="Heating-HP Only"], na.rm=T)/3412,
              COP_HP_Aux = sum(Heat_Output_Btu_h[Operating_Mode=="Heating-Aux/HP"], na.rm=T)/sum(Total_Power[Operating_Mode=="Heating-Aux/HP"], na.rm=T)/3412,
              COP_Aux = sum(Heat_Output_Btu_h[Operating_Mode=="Heating-Aux Only"], na.rm=T)/sum(Total_Power[Operating_Mode=="Heating-Aux Only"], na.rm=T)/3412,
              COP_Total = sum(Heat_Output_Btu_h, na.rm=T)/sum(Total_Power, na.rm=T)/3412) %>%
    ggplot(aes(x = OA_TempF)) + 
    geom_point(size = 3, aes(y = COP_HP, color = "Heat Pump Only")) +
    geom_point(size = 3, aes(y = COP_HP_Aux, color = "Heat Pump and Aux")) +
    geom_point(size = 3, aes(y = COP_Aux, color = "Aux Only")) +
    geom_point(size = 3, aes(y = COP_Total, color = "System Total")) +
    geom_line(aes(y = COP_HP, color = "Heat Pump Only", group=1)) +
    geom_line(aes(y = COP_HP_Aux, color = "Heat Pump and Aux", group=2)) +
    geom_line(aes(y = COP_Aux, color = "Aux Only", group=3)) +
    geom_line(aes(y = COP_Total, color = "System Total", group=4)) +
    scale_color_manual(name="", values = c("#E69F00","#CC79A7","#009E73","black")) +
    scale_x_continuous(breaks = seq(-30, 60, by=10),
                       minor_breaks = seq(-30, 60, by=5)) +
    geom_hline(yintercept=0) +
    labs(title=paste0("Demonstrated COP vs. outdoor air temperature for site", sitename),
         x="Outdoor Temperature (F)--Median of 5F Bin",
         y="COP") +
    theme_bw() +
    theme(axis.ticks.y=element_blank(),
          panel.border = element_rect(colour = "black",fill=NA),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5),)
}
# Heat_COP()
# Print graph to folder.
ggsave(paste0(sitename, '_COP_vs_OAT_Bin.png'),
       plot = Heat_COP(),
       path = paste0(wd,'/Graphs/',sitename, '/'),
       width=12, height=4, units='in')

# Percent of time spent in defrost mode per OAT bin
DefrostOATBin <- function(site){
  df %>% mutate(temp_int = cut(OA_TempF,breaks=c(-50,-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40,45,50,55))) %>%
    filter(Site_ID==site & !is.na(temp_int)) %>%
    group_by(Site_ID, temp_int) %>%
    summarize(Percent_Time_Defrost = sum(Defrost_Cycle_Runtimes, na.rm=T)*100*60/n()) %>%
    ggplot(aes(x=temp_int)) +
    geom_line(aes(y = Percent_Time_Defrost, group=1), size=1) +
    labs(title=paste0("Defrost cycles duration by outdoor temperature bin for site ", site),x="Temperature (F)", y="Percent of Time (%)") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          panel.grid.major = element_line(size = 0.9),
          panel.grid.minor = element_line(size = 0.1),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5),) +
    guides(color=guide_legend(override.aes=list(size=3)))
}
DefrostOATBin("9944LD")
for(id in unique(df$Site_ID)){
  ggsave(paste0(id, '_Defrost_Time_vs_OAT_Bin.png'),
         plot = DefrostOATBin(id),
         path = paste0(wd,'/Graphs/',id, '/'),
         width=12, height=4, units='in')
}

# Percent of time spent in defrost mode per RH bin
DefrostRHBin <- function(site){
  df %>% mutate(humid_int = cut(OA_RH,breaks=c(0,10,20,30,40,50,60,70,80,90,100))) %>%
    filter(Site_ID==site & !is.na(humid_int)) %>%
    group_by(Site_ID, humid_int) %>%
    summarize(Percent_Time_Defrost = sum(Defrost_Cycle_Runtimes, na.rm=T)*100*60/n()) %>%
    ggplot(aes(x=humid_int)) +
    geom_line(aes(y = Percent_Time_Defrost, group=1), size=1) +
    labs(title=paste0("Defrost cycles duration by humidity bin for site ", site),x="Humidity (%)", y="Percent of Time (%)") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          panel.grid.major = element_line(size = 0.9),
          panel.grid.minor = element_line(size = 0.1),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5),) +
    guides(color=guide_legend(override.aes=list(size=3)))
}
DefrostRHBin("9944LD")
for(id in unique(df$Site_ID)){
  ggsave(paste0(id, '_Defrost_Time_vs_RH_Bin.png'),
         plot = DefrostRHBin(id),
         path = paste0(wd,'/Graphs/',id, '/'),
         width=12, height=4, units='in')
}










### Demand Response Graphs ----
  # U.S. Sites
DemandResponseEvents <- data.frame(
  Site_ID = c(rep("4228VB",4), rep("6950NE",4), rep("8220XE",4), rep("9944LD",4)),
  Event_Type = rep(c("GCCW","GCMW","CCMW","CCCW"), 4),
  Start = c(strptime("2023-02-02 09:00:00", format="%F %T", tz="US/Mountain"),strptime("2023-02-06 13:00:00", format="%F %T", tz="US/Mountain"),strptime("2023-02-07 13:00:00", format="%F %T", tz="US/Mountain"),strptime("2023-02-09 09:00:00", format="%F %T", tz="US/Mountain"),
            strptime("2023-02-03 09:00:00", format="%F %T", tz="US/Central"),strptime("2023-02-06 13:00:00", format="%F %T", tz="US/Central"),strptime("2023-02-13 09:00:00", format="%F %T", tz="US/Central"),strptime("2023-02-09 09:00:00", format="%F %T", tz="US/Central"),
            strptime("2023-02-03 09:00:00", format="%F %T", tz="US/Central"),strptime("2023-02-10 09:00:00", format="%F %T", tz="US/Central"),strptime("2023-02-13 09:00:00", format="%F %T", tz="US/Central"),strptime("2023-02-09 09:00:00", format="%F %T", tz="US/Central"),
            strptime("2023-02-13 09:00:00", format="%F %T", tz="US/Mountain"),strptime("2023-02-02 09:00:00", format="%F %T", tz="US/Mountain"),strptime("2023-02-03 09:00:00", format="%F %T", tz="US/Mountain"),strptime("2023-02-10 13:00:00", format="%F %T", tz="US/Mountain")),
  End = c(strptime("2023-02-02 13:00:00", format="%F %T", tz="US/Mountain"),strptime("2023-02-06 17:00:00", format="%F %T", tz="US/Mountain"),strptime("2023-02-07 17:00:00", format="%F %T", tz="US/Mountain"),strptime("2023-02-09 13:00:00", format="%F %T", tz="US/Mountain"),
          strptime("2023-02-03 13:00:00", format="%F %T", tz="US/Central"),strptime("2023-02-06 17:00:00", format="%F %T", tz="US/Central"),strptime("2023-02-13 13:00:00", format="%F %T", tz="US/Central"),strptime("2023-02-09 13:00:00", format="%F %T", tz="US/Central"),
          strptime("2023-02-03 13:00:00", format="%F %T", tz="US/Central"),strptime("2023-02-10 13:00:00", format="%F %T", tz="US/Central"),strptime("2023-02-13 13:00:00", format="%F %T", tz="US/Central"),strptime("2023-02-09 13:00:00", format="%F %T", tz="US/Central"),
          strptime("2023-02-13 13:00:00", format="%F %T", tz="US/Mountain"),strptime("2023-02-02 13:00:00", format="%F %T", tz="US/Mountain"),strptime("2023-02-03 13:00:00", format="%F %T", tz="US/Mountain"),strptime("2023-02-10 17:00:00", format="%F %T", tz="US/Mountain"))
)

  # NRCan sites
DemandResponseEvents <- data.frame(
  Site_ID = c(rep("2458CE",3), rep("5291QJ",3)),
  Event_Type = rep(c("GC","CC","CC"), 2),
  Start = c(strptime("2023-04-04 10:00:00", format="%F %T", tz="Canada/Eastern"),strptime("2023-04-05 12:00:00", format="%F %T", tz="Canada/Eastern"),strptime("2023-04-07 9:00:00", format="%F %T", tz="Canada/Eastern"),
            strptime("2023-04-04 10:00:00", format="%F %T", tz="Canada/Eastern"),strptime("2023-04-05 12:00:00", format="%F %T", tz="Canada/Eastern"),strptime("2023-04-07 9:00:00", format="%F %T", tz="Canada/Eastern")),
  End = c(strptime("2023-04-04 14:00:00", format="%F %T", tz="Canada/Eastern"),strptime("2023-04-05 16:00:00", format="%F %T", tz="Canada/Eastern"),strptime("2023-04-07 15:00:00", format="%F %T", tz="Canada/Eastern"),
          strptime("2023-04-04 14:00:00", format="%F %T", tz="Canada/Eastern"),strptime("2023-04-05 16:00:00", format="%F %T", tz="Canada/Eastern"),strptime("2023-04-07 15:00:00", format="%F %T", tz="Canada/Eastern"))
)

# Demand Reponse Time Series Investigation
DemandResponseTimeSeries <- function(site, timestart, timeend){

  DemRes <-  DemandResponseEvents %>%
    mutate(Start = Start %>% with_tz(metadata$Timezone[metadata$Site_ID==site]),
           End = End %>% with_tz(metadata$Timezone[metadata$Site_ID==site])) %>% 
    filter(Site_ID == site)
  
  Data <- df %>% 
    mutate(Timestamp = Timestamp %>% with_tz(metadata$Timezone[metadata$Site_ID==site])) %>% 
    filter(Site_ID == site &
             Timestamp >= strptime(timestart,"%Y-%m-%d", tz=metadata$Timezone[metadata$Site_ID==site]) &
             Timestamp <= strptime(timeend,"%Y-%m-%d", tz=metadata$Timezone[metadata$Site_ID==site]))
  
  ggplot() +
    geom_rect(data=DemRes, aes(xmin=as.POSIXct(Start), xmax=as.POSIXct(End), ymin=-Inf, ymax=Inf, fill=Event_Type), alpha=0.2) +
    geom_line(data=Data, aes(x=as.POSIXct(Timestamp), y=Room_TempF/5, color = "Room Temperature"),size=0.3) +
    geom_line(data=Data, aes(x=as.POSIXct(Timestamp), y=OA_TempF/5, color = "Outdoor Air Temperature"),size=0.3) + 
    geom_line(data=Data, aes(x=as.POSIXct(Timestamp), y=HP_Power, color = "Outdoor Unit Power"),size=0.3) + 
    geom_line(data=Data, aes(x=as.POSIXct(Timestamp), y=Aux_Power, color = "Auxiliary Power"),size=0.3) + 
    scale_y_continuous(name = "Power (kW)",
                       limits = c(-4, 20),
                       sec.axis = sec_axis(~.*5, name ="Temperature (F)")) +
    scale_color_manual(name = "", breaks = c("Auxiliary Power","Outdoor Unit Power","Outdoor Air Temperature","Room Temperature"),
                       values = c("#E69F00","gray","#009E73","black","#56B4E9","#CC79A7", "#F0E442", "#0072B2", "#D55E00")) +
    labs(title=paste0("Demand response event plot for site ", site),x="") +
    theme_bw() +
    xlim(c(Data$Timestamp[1], Data$Timestamp[nrow(Data)])) +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          panel.grid.major = element_line(size = 0.5),
          panel.grid.minor = element_line(size = 0.1),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5)) +
    guides(color=guide_legend(override.aes=list(size=3)),
           fill=guide_legend(title="Event Type"))
}
DemandResponseTimeSeries("2458CE", "2023-04-04", "2023-04-05")
# Loop to print. Requires manually deleting of days that do not have events in folder.
for(id in unique(df$Site_ID)){
  for(d in unique(df$Date[df$Site_ID==id])){
    d1 = substr(as.character(strptime(d, "%Y-%m-%d", tz=metadata$Timezone[metadata$Site_ID==id]) + 60*60*24), 1, 10) # Date plus one day
    ggsave(paste0(id, '_Daily_Demand_Response_',d,'.png'),
           plot = DemandResponseTimeSeries(id, d, d1),
           path = paste0(wd,'/Graphs/Demand Response/',id,'/'),
           width=12, height=4, units='in')
  }
}
rm(d1,d,id)

# Demand Reponse Daily Time Series Investigation
DemandResponseDaily <- function(site){
  # Create a dataframe manually with DR event periods
  DemRes <- DemandResponseEvents %>%
    mutate(Start = Start %>% with_tz(metadata$Timezone[metadata$Site_ID==site]),
           End = End %>% with_tz(metadata$Timezone[metadata$Site_ID==site])) %>% 
    filter(Site_ID == site)
  
  Data <- df %>% 
    mutate(Timestamp = Timestamp %>% with_tz(metadata$Timezone[metadata$Site_ID==site])) %>% 
    filter(Site_ID == site &
             Hour >= 9 & Hour < 17) %>%
    group_by(Date) %>%
    summarize(Timestamp = Timestamp[length(Timestamp)/2],
              Energy = sum(HP_Power + Aux_Power + Fan_Power, na.rm=T) / 3600,
              OA_Temp = mean(OA_TempF, na.rm=T),
              OA_Temp_low = min(OA_TempF, na.rm=T),
              OA_Temp_high = max(OA_TempF, na.rm=T))
  
  ggplot() +
    geom_rect(data=DemRes, aes(xmin=as.POSIXct(Start), xmax=as.POSIXct(End), ymin=-Inf, ymax=Inf, fill=Event_Type), alpha=0.2) +
    geom_line(data=Data, aes(x=as.POSIXct(Timestamp), y=OA_Temp/2, color = "Outdoor Air Temperature (Avg/High/Low)"),size=1) +
    geom_line(data=Data, aes(x=as.POSIXct(Timestamp), y=Energy, color = "Energy Use"),size=1) + 
    geom_point(data=Data, aes(x=as.POSIXct(Timestamp), y=OA_Temp/2, color = "Outdoor Air Temperature (Avg/High/Low)"),size=3) +
    geom_point(data=Data, aes(x=as.POSIXct(Timestamp), y=Energy, color = "Energy Use"),size=3) + 
    geom_line(data=Data, aes(x=as.POSIXct(Timestamp), y=OA_Temp_low/2, color = "Outdoor Air Temperature (Avg/High/Low)"),size=0.5) +
    geom_line(data=Data, aes(x=as.POSIXct(Timestamp), y=OA_Temp_high/2, color = "Outdoor Air Temperature (Avg/High/Low)"),size=0.5) +
    scale_y_continuous(name = "Energy (kWh)",
                       # limits = c(-4, 20),
                       sec.axis = sec_axis(~.*2, name ="Temperature (F)")) +
    labs(title=paste0("Demand response energy and temperature (9AM-5PM) plot for site ", site),x="", color="") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          panel.grid.major = element_line(size = 0.5),
          panel.grid.minor = element_line(size = 0.1),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5)) +
    guides(color=guide_legend(override.aes=list(size=3)),
           fill=guide_legend(title="Event Type"))
}
# DemandResponseDaily("8220XE")
## Loop to print
for(id in unique(df$Site_ID)){
  ggsave(paste0(id, '_Demand_Reponse_Full_Period.png'),
         plot = DemandResponseDaily(id),
         path = paste0(wd,'/Graphs/Demand Response/',id,'/'),
         width=12, height=4, units='in')
}


# Demand response graph to compare energy use to another, similar day
  # date1 should be the starting timestamp of the DR event and date2 starting time for a different day
DemandResponseComparison <- function(site, date1, date2, event, tempscale){
  df1 <- df %>% 
    mutate(Timestamp = Timestamp %>% with_tz(metadata$Timezone[metadata$Site_ID==site])) %>% 
    filter(!is.na(HP_Power) & Site_ID == site & 
             Timestamp >= strptime(date1, format="%F %T", tz=metadata$Timezone[metadata$Site_ID==site]) &
             Timestamp <= strptime(date1, format="%F %T", tz=metadata$Timezone[metadata$Site_ID==site]) + hours(4)) %>%
    mutate(Energy = cumsum(HP_Power + Aux_Power + Fan_Power) / 3600) 

  df2 <- df %>% 
    mutate(Timestamp = Timestamp %>% with_tz(metadata$Timezone[metadata$Site_ID==site])) %>% 
    filter(!is.na(HP_Power) & Site_ID == site & 
             Timestamp >= strptime(date2, format="%F %T", tz=metadata$Timezone[metadata$Site_ID==site]) &
             Timestamp <= strptime(date2, format="%F %T", tz=metadata$Timezone[metadata$Site_ID==site]) + hours(4)) %>%
    mutate(Energy = cumsum(HP_Power + Aux_Power + Fan_Power) / 3600,
           Timestamp_New = df1$Timestamp)

    ggplot() +
      geom_line(data=df1, aes(x=as.POSIXct(Timestamp), y=Energy, color = event, linetype="Energy"),size=1) + 
      geom_line(data=df2, aes(x=as.POSIXct(Timestamp_New), y=Energy, color = "Comparison Day", linetype="Energy"),size=1) + 
      geom_line(data=df1, aes(x=as.POSIXct(Timestamp), y=Room_TempF/tempscale, color = event, linetype="Room Temperature"),size=1) + 
      geom_line(data=df2, aes(x=as.POSIXct(Timestamp_New), y=Room_TempF/tempscale, color = "Comparison Day", linetype="Room Temperature"),size=1) + 
      geom_line(data=df1, aes(x=as.POSIXct(Timestamp), y=OA_TempF/tempscale, color = event, linetype="OA Temperature"),size=1) + 
      geom_line(data=df2, aes(x=as.POSIXct(Timestamp_New), y=OA_TempF/tempscale, color = "Comparison Day", linetype="OA Temperature"),size=1) + 
      scale_linetype_manual(name="", values=c("solid","dashed","dotted")) +
      scale_y_continuous(name = "Cumulative Energy (kWh)",
                       sec.axis = sec_axis(~.*tempscale, name ="Temperature (F)")) +
      labs(title=paste0("Demand response event comparison plot for site ", site),x="", color="") +
      theme_bw() +
      theme(panel.border = element_rect(colour = "black",fill=NA),
            panel.grid.major = element_line(size = 0.5),
            panel.grid.minor = element_line(size = 0.1),
            plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
            axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
            axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5)) +
      guides(color=guide_legend(override.aes=list(size=3)))
}
DemandResponseComparison("4228VB", "2023-02-02 09:00:00", "2023-02-03 09:00:00", "GCCW Event", 20)
DemandResponseComparison("4228VB", "2023-02-06 13:00:00", "2023-02-05 13:00:00", "GCMW Event", 20)
DemandResponseComparison("4228VB", "2023-02-07 13:00:00", "2023-02-08 13:00:00", "CCMW Event", 20)
DemandResponseComparison("4228VB", "2023-02-09 09:00:00", "2023-02-08 09:00:00", "CCCW Event", 20)
DemandResponseComparison("6950NE", "2023-02-03 09:00:00", "2023-01-31 09:00:00", "GCCW Event", 20)
DemandResponseComparison("6950NE", "2023-02-06 13:00:00", "2023-02-05 13:00:00", "GCMW Event", 20)
# DemandResponseComparison("6950NE", "2023-02-13 09:00:00", "2023-02-12 09:00:00", "CCCW Event", 20)
  # Feb 12-14 at 6950NE has a lot of duplicated rows and it's causing the graph function to fault.
  # I tried using "unique()" and also removing the NA HP Power data, but still couldn't get it to work.
DemandResponseComparison("6950NE", "2023-02-09 09:00:00", "2023-02-07 09:00:00", "CCMW Event", 20)
DemandResponseComparison("8220XE", "2023-02-03 09:00:00", "2023-01-31 09:00:00", "GCCW Event", 5)
DemandResponseComparison("8220XE", "2023-02-10 09:00:00", "2023-02-05 09:00:00", "GCMW Event", 10)
# DemandResponseComparison("8220XE", "2023-02-13 09:00:00", "2023-02-12 09:00:00", "CCMW Event", 10)
  # Feb 12-14 at 8220XE also has a lot of duplicated rows and it's causing the graph function to fault.
DemandResponseComparison("8220XE", "2023-02-09 09:00:00", "2023-02-05 09:00:00", "CCCW Event", 10)
# DemandResponseComparison("9944LD", "2023-02-13 09:00:00", "2023-02-12 09:00:00", "GCCW Event", 20)
  # Feb 12-14 at 9944LD also has a lot of duplicated rows and it's causing the graph function to fault.
DemandResponseComparison("9944LD", "2023-02-02 09:00:00", "2023-02-01 09:00:00", "GCMW Event", 2)
DemandResponseComparison("9944LD", "2023-02-03 09:00:00", "2023-02-05 09:00:00", "CCMW Event", 20)
DemandResponseComparison("9944LD", "2023-02-10 13:00:00", "2023-02-08 13:00:00", "CCCW Event", 10)

#NRCan sites
DemandResponseComparison("2458CE", "2023-04-04 10:00:00", "2023-04-06 10:00:00", "GC Event", 100)
DemandResponseComparison("2458CE", "2023-04-05 12:00:00", "2023-04-02 12:00:00", "CC Event", 25)
DemandResponseComparison("2458CE", "2023-04-07 10:00:00", "2023-04-03 10:00:00", "CC Event", 50)
DemandResponseComparison("5291QJ", "2023-04-04 10:00:00", "2023-04-06 10:00:00", "GC Event", 100)
DemandResponseComparison("5291QJ", "2023-04-05 12:00:00", "2023-03-30 12:00:00", "CC Event", 100)
DemandResponseComparison("5291QJ", "2023-04-07 10:00:00", "2023-03-31 10:00:00", "CC Event", 50)

