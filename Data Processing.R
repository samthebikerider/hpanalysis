# Clear Global Environment
rm(list=ls())

# Open libraries
library(tidyverse)
library(lubridate)
library(stringr)

# Set working library
setwd('C:/Users/keen930/PNNL/CCHP - General/Field Demonstration (Task 2)/R Data Analysis')

# Read data
  # Will need to read multiple files from weekly data dumps and bind
data <- read.csv('PNNL_ccASHP__6950NE.csv')
metadata <- read.csv('site-metadata.csv')


# Convert timestamp from UTC to local time zone
  # Confirm if data will be in local time or UTC (may vary for each site) 
    # VM: Michaels sites are coming in UTC
  # I added a field to the meta data ("Timezone") which will have the local timezone
  # for each site.
  # I tried to do this without creating so many temporary variables, but the "tz"
  # variable in strptime doesn't let me do dynamic inputs with multiple sites in one df
temp <- data[0,]
for (id in metadata$Site_ID){
  sub <- data %>% filter(Site_ID == id)
  sub$Timestamp = strptime(substr(sub$index,1,19), tz=metadata$Timezone[metadata$Site_ID==id],"%Y-%m-%d %H:%M:%S")
  temp <- rbind(temp, sub)
}
data <- temp
rm(temp, sub, id)

# Add fields for date, hour, and week day
data <- data %>% mutate(
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
data <- data %>% mutate(
  SA_RH = rowMeans(cbind(SA1_RH, SA2_RH, SA3_RH, SA4_RH)),
  SA_TempF = rowMeans(cbind(SA1_TempF, SA2_TempF, SA3_TempF, SA4_TempF)))


# Calculate fields
  # Humidity ratio of supply air (0.62198 * Pw / (P - Pw))
    # P = 97,717 Pascals at 1,000 ft elevation (we could use a more accurate look up for each location)
    # Pw = RH * Pws; Pws = f(T); T in Fahrenheit, based on curve fit
    # 6th order poly fit: Pws = -0.000000001546*T^6 + 0.000000516256*T^5 - 0.000020306966*T^4 + 0.002266035021*T^3 + 0.190010315225*T^2 + 6.715900713408*T + 125.349159019000
data <- data %>% mutate(
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
  # Correlate fan power to volumetric flow rate based on initial testing
  # Function will output the air supply in CFM for an input power in Watts
  # We will need to add a line for each site in this function.
  supply_flow_rate_CFM = ifelse(siteid=="6950NE",
                      152.27 * fan_power ^ 0.3812,
                      NA)
}
data$supply_flow_rate_CFM <- fan_power_curve(data$Fan_Power, data$Site_ID)


  # Operating mode
    # Need to ask Michaels how to calculate this field
    # Looks like the voltage is normal around 0V or 3.0V, so I'm guessing one 
    # means heating mode and the other is cooling, but not clear. Setting only 
    # to heating for now.
# plot(x=data$RV_Volts, y=data$HP_Power)
# plot(x=data$RV_Volts, y=data$SA_TempF)
data <- data %>% mutate(
  Operating_Mode = ifelse(RV_Volts > -1, "Heating",
                          ifelse(RV_Volts > -1, "Cooling",
                                 "None")))

  # Auxiliary power
    # Is it just "Power" or the sum of "Power" and "Power."?
data <- data %>% mutate(
  Aux_Power = Aux1_Power + Aux2_Power + Aux3_Power + Aux4_Power)


  # Heating capacity (Q-heating)
    # Q-heating = (dry air density) * (blower airflow rate) * (specific heat) * (delta Temp)
data <- data %>% mutate(
  Heating_Capacity_Btu_h = ifelse(
  Operating_Mode == "Cooling", NA,
    0.0765 *                                                # Density of air at 15C (lb/ft3)
    supply_flow_rate_CFM * 60 *                             # CFM * min/hour
    (0.24 + 0.444 *  Supply_Humidity_Ratio) *               # Specific heat capacity (Btu/F-lb)
    (SA_TempF - RA_TempF)) -                                # Temperature delta
  Aux_Power * 3.412)                                        # Subtract auxiliary power, convert Watts to btu/hr

  # Heating load
    # VM: Heating load might be a misnomer here because this does not incorporate the COP. 
data <- data %>% mutate(
  Heating_Load_Btu_h = ifelse(
  Operating_Mode == "Cooling", NA,
  (AHU_Power + HP_Power) * 3.412))               # Convert total system power to Btu/hr

  # Cooling capacity (Q-cooling)
    # Q-cooling = (dry air density) * (blower airflow rate) * (specific heat) * (delta Temp) / (1 + Humidity Ratio)
        # VM: This will yield -ve values which is what we want here but something to bear in mind during COP calcs.
data <- data %>% mutate(
  Cooling_Capacity_Btu_h = ifelse(
  Operating_Mode == "Heating", NA,
    0.0765 *                                                          # Density of air at 15C (lb/ft3)
    supply_flow_rate_CFM * 60 *                                       # CFM * min/hour
    (0.24 + 0.444 *  Supply_Humidity_Ratio) *                         # Specific heat capacity (Btu/F-lb)
    (SA_TempF - RA_TempF) /
    (1 + Supply_Humidity_Ratio)))

# Cooling load
data <- data %>% mutate(
  Cooling_Load_Btu_h = ifelse(
    Operating_Mode == "Heating", NA,
    (AHU_Power + HP_Power) * 3.412))                                   # Convert total system power to Btu/hr


  # COP heating
    # VM: wondering if we should report COP with and without supplemental heat.
    # I personally think that supplemental heat should not be part of the COP
    # but we need to align this with the challenge spec.
    # KK: To calculate COP of just heat pump we would need to remove all data where the
    # auxiliary heat is operating, and so there might be some temperature bins where
    # we wouldn't have results.
data <- data %>% mutate(
  COP_Heating = Heating_Capacity_Btu_h / 
  3.412 / (AHU_Power + HP_Power))

# COP cooling
data <- data %>% mutate(
  COP_Cooling = (-1 * Cooling_Capacity_Btu_h) / 
  3.412 / (AHU_Power + HP_Power))











# Investigate time series

TimeSeries <- function(site, parameter, interval, timestart, timeend){
  # Look at a time series graph for a given parameter, time interval (e.g, 5-minute), time period, and site
  # Interval is in units of minutes, and so the maximum interval would be one hour.
  # The site can be a list of multiple sites if would like to compare.
  # The time start and end should be a date string in format for example "4/01/2022".
  testdata %>% mutate(Interval = minute(Timestamp) %/% interval) %>% 
    filter(Site_ID %in% site &
            Timestamp >= strptime(timestart,"%m/%d/%Y") &
            Timestamp <= strptime(timeend,"%m/%d/%Y")) %>%
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

TimeSeries(1, "Supply_Humidity", 5, "12/15/2022", "12/30/2022")

# Temperature time series comparison chart
TempTimeSeries <- function(site, interval, timestart, timeend){
  # Look at a time series graph for all temperature monitors, time interval (e.g, 5-minute), time period, and site
  # Interval is in units of minutes, and so the maximum interval would be one hour.
  # The time start and end should be a date string in format for example "4/01/2022".
  data %>% mutate(Interval = minute(Timestamp) %/% interval) %>% 
    filter(Site_ID == site &
             Timestamp >= strptime(timestart,"%m/%d/%Y") &
             Timestamp <= strptime(timeend,"%m/%d/%Y")) %>%
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
    scale_y_continuous(breaks = seq(0,120, by=10), minor_breaks = seq(0, 120, by=1)) +
    scale_color_manual(name = "", values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
    labs(title=paste0("Temperature time series plot for site ", site),x="",y="Temperature (F)") +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          panel.grid.major = element_line(size = 0.9),
          panel.grid.minor = element_line(size = 0.1)) +
    guides(color=guide_legend(override.aes=list(size=3)))
}
TempTimeSeries("6950NE", 5, "12/01/2022", "12/31/2022")

# Supply temperature time series comparison chart
SupplyTempTimeSeries <- function(site, interval, timestart, timeend){
  # Look at a time series graph for the four supply temperature monitors, time interval (e.g, 5-minute), time period, and site
  # Interval is in units of minutes, and so the maximum interval would be one hour.
  # The time start and end should be a date string in format for example "4/01/2022".
  data %>% mutate(Interval = minute(Timestamp) %/% interval) %>% 
    filter(Site_ID == site &
             Timestamp >= strptime(timestart,"%m/%d/%Y") &
             Timestamp <= strptime(timeend,"%m/%d/%Y")) %>%
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
    scale_y_continuous(breaks = seq(0,120, by=10), minor_breaks = seq(0, 120, by=1)) +
    scale_color_manual(name = "", values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
    labs(title=paste0("Supply temperature time series plot for site ", site),x="",y="Temperature (F)") +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          panel.grid.major = element_line(size = 0.9),
          panel.grid.minor = element_line(size = 0.1)) +
    guides(color=guide_legend(override.aes=list(size=3)))
}
SupplyTempTimeSeries("6950NE", 5, "12/01/2022", "12/31/2022")


# Power time series comparison chart with OAT
PowerTimeSeries <- function(site, interval, timestart, timeend){
  # Look at a time series graph for all temperature monitors, time interval (e.g, 5-minute), time period, and site
  # Interval is in units of minutes, and so the maximum interval would be one hour.
  # The time start and end should be a date string in format for example "4/01/2022".
  # Scale for the secondary axis may need to be adjusted manually, and will get more
  # complicated once we have outdoor values.
  data %>% mutate(Interval = minute(Timestamp) %/% interval) %>% 
    filter(Site_ID == site &
             Timestamp >= strptime(timestart,"%m/%d/%Y") &
             Timestamp <= strptime(timeend,"%m/%d/%Y")) %>%
    group_by(Site_ID,date, hour, Interval) %>% 
    summarize(Timestamp = Timestamp[1],
              HP_Power = mean(HP_Power,na.rm=T),
              Fan_Power = mean(Fan_Power,na.rm=T),
              Aux_Power = mean(Aux_Power,na.rm=T),
              OA_TempF = mean(OA_TempF,na.rm=T)) %>%
    ggplot(aes(x=as.POSIXct(Timestamp))) +
    geom_line(aes(y=OA_TempF/3, color = "Outdoor Temperature"),size=0.3) + 
    geom_line(aes(y=HP_Power, color = "Heat Pump Power"),size=0.3) + 
    geom_line(aes(y=Fan_Power, color = "Fan Power"),size=0.3) + 
    geom_line(aes(y=Aux_Power, color = "Auxiliary Power"),size=0.3) + 
    scale_y_continuous(name = "Power (W)",
                       sec.axis = sec_axis(~.*3, name ="Outdoor Air Temperature (F)")) +
    scale_color_manual(name = "", values = c("#E69F00", "#56B4E9","#009E73", "black", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
    labs(title=paste0("Power time series plot for site ", site),x="") +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          panel.grid.major = element_line(size = 0.9),
          panel.grid.minor = element_line(size = 0.1)) +
    guides(color=guide_legend(override.aes=list(size=3)))
}
PowerTimeSeries("6950NE", 5, "12/01/2022", "12/31/2022")


# Heating capacity (Btu/h) vs outdoor air temperature
  # Round to nearest 1 degree F (may want to consider larger intervals)
  # Only can select one site for this function
HeatCapacity <- function(site, timestart, timeend){
  data %>%
    filter(Site_ID == site &
             Timestamp >= strptime(timestart,"%m/%d/%Y") &
             Timestamp <= strptime(timeend,"%m/%d/%Y")) %>%
    group_by(temp_int = round(OA_TempF)) %>% 
    filter(!is.na(temp_int)) %>%
    summarize(Heating_Capacity = mean(Heating_Capacity_Btu_h, na.rm=T),
              Heating_Load = mean(Heating_Load_Btu_h, na.rm=T)) %>%
    ggplot(aes(x = temp_int)) + 
    geom_line(size = 1, aes(y = Heating_Capacity, color="Heating Capacity")) +
    geom_line(size = 1, aes(y = Heating_Load, color="Heating Load")) +
    geom_point(size=2, aes(y = Heating_Capacity), color="red") +
    geom_point(size=2, aes(y = Heating_Load), color="blue") +
    scale_color_manual(values=c("red","blue")) +
    labs(title="Heating Capacity/Heating Load vs. Outdoor Air Temperature",
         x="Outdoor Temperature (F)",
         y="Load and Capcity (Btu/h)") +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          legend.title = element_blank())
}
HeatCapacity("6950NE", "12/01/2022", "12/30/2022")




# COP vs outdoor air temperature, hourly averages
  # The powerpoint has this grouped by stage (low, medium, high) as well.
  # I'm not sure how we will get the stage of operation.
      # VM: These should all be continously varying units, so we shouldn't have to worry about stages.
COP <- function(site, timestart, timeend){
  data %>%     
    filter(Site_ID %in% site &
             Timestamp >= strptime(timestart,"%m/%d/%Y") &
             Timestamp <= strptime(timeend,"%m/%d/%Y")) %>%
    group_by(Site_ID, temp_int = round(OA_TempF)) %>% 
    summarize(COP = mean(COP_Heating, na.rm=T)) %>%
  ggplot(aes(x = temp_int, y = COP, color = as.character(Site_ID))) + 
    geom_line(size = 1) +
    geom_point(size=2) +
    labs(title="Heating COP vs. Outdoor Air Temperature",
         x="Outdoor Temperature (F)",
         y="COP",
         color = "Site") +
    theme(axis.ticks.y=element_blank(),
          panel.border = element_rect(colour = "black",fill=NA))
}
COP("6950NE", "12/01/2022", "12/30/2022")


## Need to figure out how to do staging charts and defrost mode run time chart
  # There should be a flag for defrost mode
  # Staging will have to be determined based on power consumption

# Supplemental resistance heat use compared to total energy use by temperature bin
  # Using 5 degree F intervals like the graph in the powerpoint
SuppHeatUse <- function(site, timestart, timeend){
  data %>%     
    filter(Site_ID %in% site &
             Timestamp >= strptime(timestart,"%m/%d/%Y") &
             Timestamp <= strptime(timeend,"%m/%d/%Y")) %>%
    group_by(Site_ID, temp_int = cut(OA_TempF,
                                     breaks=c(-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40,45))) %>% 
    summarize(SuppHeat = mean(Aux_Power / (AHU_Power + HP_Power), na.rm=T)) %>%
    filter(!is.na(temp_int)) %>%
    ggplot(aes(x = temp_int, y = SuppHeat, color = as.character(Site_ID), group = Site_ID)) + 
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(title="Supplement Heat Energy Ratio per Outdoor Temperature Bin",
         x="Outdoor Temperature (F)",
         y="Ratio of Supplemental Heat to Total Heat Energy (%)",
         color = "Site") +
    theme(axis.ticks.y=element_blank(),
          panel.border = element_rect(colour = "black",fill=NA))
  
}
SuppHeatUse("6950NE", "12/01/2022", "12/30/2022")


# Heat pump return and supply temperature for outdoor temperature bins
  # Only one site can be entered at a time
  # I assume we want to filter out when supplemental heat is on, so added a filter for that
SupplyReturnTemp <- function(site, timestart, timeend){
  data %>%     
    filter(Site_ID == site &
             Timestamp >= strptime(timestart,"%m/%d/%Y") &
             Timestamp <= strptime(timeend,"%m/%d/%Y") &
             Aux_Power == 0) %>%
    group_by(temp_int = cut(OA_TempF, breaks=c(-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35,40,45))) %>% 
    summarize(SupplyTemp = mean(SA_TempF, na.rm=T),
              ReturnTemp = mean(RA_TempF, na.rm=T)) %>%
    filter(!is.na(temp_int)) %>%
    ggplot(aes(x = temp_int)) + 
    geom_line(size = 1, aes(y = SupplyTemp, color="Supply Temperature", group=1)) +
    geom_line(size = 1, aes(y = ReturnTemp, color="Return Temperature", group=1)) +
    geom_point(size=2, aes(y = SupplyTemp), color="red") +
    geom_point(size=2, aes(y = ReturnTemp), color="blue") +
    scale_color_manual(values=c("blue","red")) +
    labs(title="Supply and Return Temperature per Outdoor Temperature Bin",
         x="Outdoor Temperature Bin (F)",
         y="Indoor Temperature (F)") +
    theme(axis.ticks.y=element_blank(),
          panel.border = element_rect(colour = "black",fill=NA),
          legend.title = element_blank())
  
}
SupplyReturnTemp("6950NE", "12/01/2022", "12/30/2022")


## Skipping the cooling mode graphs for now, not a priority



# Electricity usage vs. outdoor temperature
  # The graph in the powerpoint has it grouped into 3-month intervals, but I'm
  # trying one day intervals for now because our timeframe isn't as long
  # only one site can be entered at a time
  # I did heat pump + supplemental power, I think that makes more sense than just heat pump power
  # Need to think about how to convert kW to kWh when there could be missing data or uneven time intervals
ElecUsage <- function(site, timestart, timeend){
  tempdf <- data %>%     
    filter(Site_ID == site &
             Timestamp >= strptime(timestart,"%m/%d/%Y") &
             Timestamp <= strptime(timeend,"%m/%d/%Y")) %>%
    group_by(date) %>% 
    summarize(AirTemp = mean(OA_TempF, na.rm=T),
              ElecUse = mean(AHU_Power + HP_Power, na.rm=T))
  
  # Create transformation factor for secondary axis
    # Complicated because there can be positive and negative values for temp
  scale_factor <- (max(tempdf$ElecUse, na.rm=T) - min(tempdf$ElecUse, na.rm=T))/
    (max(tempdf$AirTemp, na.rm=T) - min(tempdf$AirTemp, na.rm=T))
  adj <- max(tempdf$ElecUse / scale_factor, na.rm=T) - max(tempdf$AirTemp, na.rm=T)
  
  ggplot(tempdf, aes(x = date)) + 
    geom_line(size = 1, aes(y = ElecUse / scale_factor - adj, color="Electricity Usage")) +
    geom_line(size = 1, aes(y = AirTemp, color="Average Outdoor Temperature")) +
    geom_point(size=2, aes(y = ElecUse / scale_factor - adj), color="black") +
    geom_point(size=2, aes(y = AirTemp), color="red") +
    scale_color_manual(values=c("red","black")) +
    scale_y_continuous(name = "Outdoor Temperature (F)",
                       sec.axis = sec_axis(~(. + adj)*scale_factor, name ="Electricity Use (W)")) +
    labs(title="Electricity Usage vs Outdoor Temperature",
         x="") +
    theme(axis.ticks.y=element_blank(),
          panel.border = element_rect(colour = "black",fill=NA),
          legend.title = element_blank())
  
}
ElecUsage("6950NE", "12/01/2022", "12/30/2022")





