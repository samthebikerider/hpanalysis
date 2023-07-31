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
# read_plus <- function(file) {read_csv(file) %>% mutate(filename=file)}
# df <- list.files(path = paste0(wd, "/Raw Data"),pattern="*.csv", full.names=T) %>% 
# map_df(~read_csv(.))

# Read_csv (tidyverse) is crashing RStudio, trying fread (data.table) which is supposed to be better with large files
library(data.table)
read_plus <- function(file) {fread(file) %>% mutate(filename=file)}
df <- list.files(path = paste0(wd, "/Raw Data"),pattern="*.csv", full.names=T) %>% 
  map_df(~read_plus(.)) %>% 
  # Modify filename so that it is the Site ID
  mutate(Site_ID = substr(filename, nchar(filename)-13,nchar(filename)-8)) %>%
  as.data.frame()



metadata <- read_csv(file = paste0(wd, "/site-metadata.csv"))


# Convert timestamp from UTC to local time zone
# Confirm if data will be in local time or UTC (may vary for each site) 
# VM: Michaels sites are coming in UTC
# I added a field to the meta data ("Timezone") which will have the local timezone
# for each site.
# I tried to do this without creating so many temporary variables, but the "tz"
# variable in strptime doesn't let me do dynamic inputs with multiple sites in one df
# KK: It looks like data.table automatically converted the timestamp to POSIXct, so just need to change TZ now
temp <- df[0,] %>% mutate(Timestamp = NA)
for (id in metadata$Site_ID){
  sub <- df %>% filter(Site_ID == id)
  # sub$Timestamp = strptime(substr(sub$index,1,19), tz=metadata$Timezone[metadata$Site_ID==id],"%Y-%m-%d %H:%M:%S")
  sub$Timestamp = sub$index %>% with_tz(tzone = metadata$Timezone[metadata$Site_ID==id])
  temp <- rbind(temp, sub)
}
df <- temp
rm(temp, sub, id)

# Add fields for date, hour, and week day
df <- df %>% mutate(
  date = date(Timestamp),
  hour = hour(Timestamp),
  day = wday(Timestamp))


# Clean data
# Additional cleaning that will need to occur once we see raw data

# Be careful of missing kW data when converting to kWh
# Look into what others have done. Interpolation could be a solution.

# VM: I auggest doing a series of panel plots of the timeseries data 
# as well as BW plots for temp, and power from the raw data as a first step.
# We can look at these charts to determine acceptable ranges etc.

# Supply temperature and humidity calculated as the average of the four quadrants
df <- df %>% mutate(
  SA_RH = rowMeans(cbind(SA1_RH, SA2_RH, SA3_RH, SA4_RH)),
  SA_TempF = rowMeans(cbind(SA1_TempF, SA2_TempF, SA3_TempF, SA4_TempF)))


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
  supply_flow_rate_CFM = ifelse(siteid=="6950NE",
                                152.27 * fan_power ^ 0.3812,
                                ifelse(siteid=="8220XE",
                                       152.27 * fan_power ^ 0.3812,  # This one needs to be updated
                                       NA))
}
df$supply_flow_rate_CFM <- fan_power_curve(df$Fan_Power, df$Site_ID)


# Operating mode
# Need to ask Michaels how to calculate this field
# Looks like the voltage is normal around 0V or 2.7V, so I'm guessing one 
# means heating mode and the other is cooling, but not clear. Setting only 
# to heating for now.
# Also need to consider in following equations how to treat operating mode
# when the system is turned off... should be in the mode that it was just before
# turning off I would think.
# plot(x=df$RV_Volts, y=df$HP_Power)
# plot(x=df$RV_Volts, y=df$SA_TempF)
df <- df %>% mutate(
  Operating_Mode = ifelse(RV_Volts > -1, "Heating",
                          ifelse(RV_Volts > -1, "Cooling",
                                 "None")))

# Auxiliary power
# Is it just "Power" or the sum of "Power" and "Power."?
df <- df %>% mutate(
  Aux_Power = Aux1_Power + Aux2_Power + Aux3_Power + Aux4_Power,
  Total_Power = AHU_Power + HP_Power)


# Energy use
# Calculate energy use at each timestamp as the power multiplied by the interval
# since the last power reading.
# This assumes that if there is missing data, the eGauge will report the first
# point after a gap as the average of the gap.
df  <- df %>% mutate(Energy_Use_kWH = NA)
temp <- df[0,] 
for(id in metadata$Site_ID){
  sub <- df %>% filter(Site_ID == id) %>% arrange(Timestamp)
  index <- which(!is.na(sub$Total_Power))[1] + 1 # Second non-NA row
  ts <- sub$Timestamp[index]                     # Timestamp at first non-NA row
  
  for(row in index:nrow(sub)){
    if(!is.na(sub$Total_Power[row])){              # If the power is not NA, calculate energy from last timestep
      sub$Energy_Use_kWH[row] = sub$Total_Power[row] * difftime(sub$Timestamp[row], ts, units="hours")
      ts <- sub$Timestamp[row]
    }
  }
  temp <- rbind(temp, sub)
}
df <- temp
rm(temp, sub, id, row, index, ts)

# Heating capacity (Q-heating)
# Q-heating = (dry air density) * (blower airflow rate) * (specific heat) * (delta Temp)
df <- df %>% mutate(
  Heating_Capacity_Btu_h = ifelse(
    Operating_Mode == "Cooling", NA,
    0.0765 *                                                # Density of air at 15C (lb/ft3)
      supply_flow_rate_CFM * 60 *                             # CFM * min/hour
      (0.24 + 0.444 *  Supply_Humidity_Ratio) *               # Specific heat capacity (Btu/F-lb)
      (SA_TempF - RA_TempF)) -                                # Temperature delta
    Aux_Power * 3412)                                         # Subtract auxiliary power, convert kW to btu/hr

# Heating load
# VM: Heating load might be a misnomer here because this does not incorporate the COP. 
df <- df %>% mutate(
  Heating_Load_Btu_h = ifelse(
    Operating_Mode == "Cooling", NA,
    (AHU_Power + HP_Power) * 3412))               # Convert total system power from kW to Btu/hr

# Cooling capacity (Q-cooling)
# Q-cooling = (dry air density) * (blower airflow rate) * (specific heat) * (delta Temp) / (1 + Humidity Ratio)
# VM: This will yield -ve values which is what we want here but something to bear in mind during COP calcs.
df <- df %>% mutate(
  Cooling_Capacity_Btu_h = ifelse(
    Operating_Mode == "Heating", NA,
    0.0765 *                                                          # Density of air at 15C (lb/ft3)
      supply_flow_rate_CFM * 60 *                                       # CFM * min/hour
      (0.24 + 0.444 *  Supply_Humidity_Ratio) *                         # Specific heat capacity (Btu/F-lb)
      (SA_TempF - RA_TempF) /
      (1 + Supply_Humidity_Ratio)))

# Cooling load
df <- df %>% mutate(
  Cooling_Load_Btu_h = ifelse(
    Operating_Mode == "Heating", NA,
    (AHU_Power + HP_Power) * 3412))                                   # Convert total system power from kW to Btu/hr


# COP heating
# VM: wondering if we should report COP with and without supplemental heat.
# I personally think that supplemental heat should not be part of the COP
# but we need to align this with the challenge spec.
# KK: To calculate COP of just heat pump we would need to remove all data where the
# auxiliary heat is operating, and so there might be some temperature bins where
# we wouldn't have results.
df <- df %>% mutate(
  COP_Heating = Heating_Capacity_Btu_h / 
    3412 / (AHU_Power + HP_Power))

# COP cooling
df <- df %>% mutate(
  COP_Cooling = (-1 * Cooling_Capacity_Btu_h) / 
    3412 / (AHU_Power + HP_Power))







# Run diagnostics on missing power data
# Not sure how helpful this is...
# error on Mac OS: "Error in file(file, ifelse(append, "a", "w")) : 'mode' for the clipboard must be 'r' on Unix"
pwr_diag <- data.frame(
  Site = df %>% group_by(Site_ID) %>% tally() %>% pull(Site_ID),
  Percent_Missing_HP = df %>% group_by(Site_ID) %>% summarize(col = sum(is.na(HP_Power))/length(HP_Power)) %>% pull(col),
  Percent_Missing_Aux = df %>% group_by(Site_ID) %>% summarize(col = sum(is.na(Aux_Power))/length(Aux_Power)) %>% pull(col)
)
write.table(pwr_diag, "clipboard",sep="\t",row.names = F)

