#####################################################
# Author: Samuel Rosenberg
# Company: Pacific Northwest National Laboratory
# Created on: 2023-07-26
# Description: Functions to be widely used in the CCHP data cleaning and analysis
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

# fill missing rows with an appropriate timestamp and NA 
fill_missing_timestamps <- function(df, timestamp_col, format, interval){
  
  df[[timestamp_col]] <- as.POSIXct(df[[timestamp_col]], format = format)
  
  ts <- seq.POSIXt(min(df[[timestamp_col]]), max(df[[timestamp_col]]), by= interval)
  ts <- seq.POSIXt(as.POSIXct(min(df[[timestamp_col]]), format), as.POSIXct(max(df[[timestamp_col]]), format), by = interval)
  
  ts <- seq.POSIXt(min(df[[timestamp_col]]), max(df[[timestamp_col]]), by = interval)
  ts <- format.POSIXct(ts, format)
  
  df_ts <- data.frame(timestamp=ts)
  df_ts[[timestamp_col]] <- df_ts$timestamp
  df_ts[[timestamp_col]] <- as.POSIXct(df_ts[[timestamp_col]], format = format)
  
  df_out <- full_join(df_ts, df)
  
  return(df_out)
}


# Calculate heat and defrost run cycle duration for heat pump.
  # The "mode" input should be "Heating" or "Defrost".
  # The script is set up to calculate EITHER heating run time or defrost runtime 
  # (or any other mode, e.g., cooling)
run_cycle_calc <- function(site, timestamp, operate, mode){
  
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




### Daily Operation Graphs ----

# Room temperature time series comparison chart
daily_room_temperature_comparison <- function(site, timestart, timeend){

  df %>% mutate(datetime_UTC = datetime_UTC %>% with_tz(metadata$Timezone[metadata$Site_ID==site])) %>% 
    filter(datetime_UTC >= strptime(timestart,"%F", tz=metadata$Timezone[metadata$Site_ID==site]) &
           datetime_UTC <= strptime(timeend,"%F", tz=metadata$Timezone[metadata$Site_ID==site])) %>%
    ggplot(aes(x=as.POSIXct(datetime_UTC))) +
    geom_line(aes(y=room1_temp_F, color = "Room 1"),size=0.5) + 
    geom_line(aes(y=room2_temp_F, color = "Room 2"),size=0.5) +
    geom_line(aes(y=room3_temp_F, color = "Room 3"),size=0.5) +
    geom_line(aes(y=room4_temp_F, color = "Room 4"),size=0.5) +
    geom_line(aes(y=AHU_ambient_temp_F, color = "AHU Ambient"),size=0.5) +
    scale_y_continuous(breaks = seq(0,200, by=10), minor_breaks = seq(0, 200, by=1)) +
    scale_color_manual(name = "", values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
    labs(title=paste0("Room temperature daily plot for site ", site),x="",y="Temperature (F)") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          panel.grid.major = element_line(size = 0.9),
          panel.grid.minor = element_line(size = 0.1),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5),) +
    guides(color=guide_legend(override.aes=list(size=3)))
}


# Supply and return temperature time series comparison chart
daily_supply_temperature_comparison <- function(site, timestart, timeend){

  df %>% mutate(datetime_UTC = datetime_UTC %>% with_tz(metadata$Timezone[metadata$Site_ID==site])) %>% 
    filter(datetime_UTC >= strptime(timestart,"%F", tz=metadata$Timezone[metadata$Site_ID==site]) &
           datetime_UTC <= strptime(timeend,"%F", tz=metadata$Timezone[metadata$Site_ID==site])) %>%
    summarize(datetime_UTC = datetime_UTC[1],
              SA_temp_duct1_F = mean(SA_temp_duct1_F,na.rm=T),
              SA_temp_duct2_F = mean(SA_temp_duct2_F,na.rm=T),
              SA_temp_duct3_F = mean(SA_temp_duct3_F,na.rm=T),
              SA_temp_duct4_F = mean(SA_temp_duct4_F,na.rm=T),
              RA_temp_F = mean(RA_temp_F, na.rm = T)) %>%
    ggplot(aes(x=as.POSIXct(datetime_UTC))) +
    geom_line(aes(y=SA_temp_duct1_F, color = "SA1"),size=0.5) + 
    geom_line(aes(y=SA_temp_duct2_F, color = "SA2"),size=0.5) + 
    geom_line(aes(y=SA_temp_duct3_F, color = "SA3"),size=0.5) + 
    geom_line(aes(y=SA_temp_duct4_F, color = "SA4"),size=0.5) + 
    geom_line(aes(y=RA_temp_F, color = "RA"),size=0.5) + 
    scale_y_continuous(breaks = seq(0,200, by=10), minor_breaks = seq(0,200, by=1)) +
    scale_color_manual(name = "", values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
    labs(title=paste0("Supply temperature daily plot for site ", site),x="",y="Temperature (F)") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          panel.grid.major = element_line(size = 0.9),
          panel.grid.minor = element_line(size = 0.1),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5),) +
    guides(color=guide_legend(override.aes=list(size=3)))
}


# Investigate defrost cycles for every day
daily_defrost_plot <- function(site, timestart, timeend){

  df %>% mutate(datetime_UTC = datetime_UTC %>% with_tz(metadata$Timezone[metadata$Site_ID==site]),
                # If defrost mode, set marker to 5 (arbitrary number) to show activity
                Defrost = ifelse(operating_mode=="Defrost", 5, NA)) %>% 
    filter(datetime_UTC >= strptime(timestart,"%F", tz=metadata$Timezone[metadata$Site_ID==site]) &
           datetime_UTC <= strptime(timeend,"%F", tz=metadata$Timezone[metadata$Site_ID==site])) %>%
    ggplot(aes(x=as.POSIXct(datetime_UTC))) +
    geom_line(aes(y=ODU_pwr_kW, color = "Outdoor Unit Power"),size=0.3) + 
    geom_line(aes(y=fan_pwr_kW, color = "Supply Fan Power"),size=0.3) +
    geom_line(aes(y=auxheat_pwr_kW, color = "Auxiliary Power"),size=0.3) + 
    geom_point(aes(y=Defrost, color = "Defrost Mode On"),size=2) + 
    geom_point(aes(y=Defrost_Cycle_Runtimes/2, color = "Defrost Cycle Length"),size=3,shape=8) +
    scale_y_continuous(name = "Power (kW)",
                       limits = c(-0.5, 21),
                       sec.axis = sec_axis(~.*2, name ="Defrost Cycle Length (mins)")) +
    scale_color_manual(name = "", breaks = c("Auxiliary Power","Outdoor Unit Power","Supply Fan Power","Defrost Mode On","Defrost Cycle Length"),
                       values = c("#E69F00","black","#CC79A7","#009E73","#56B4E9", "gray", "#F0E442", "#0072B2", "#D55E00")) +
    labs(title=paste0("Defrost cycle daily plot for site ", site),x="") +
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


# Power time series comparison chart with OAT and SAT
daily_operation_plot <- function(site, timestart, timeend){

  df %>% mutate(datetime_UTC = datetime_UTC %>% with_tz(metadata$Timezone[metadata$Site_ID==site])) %>% 
    filter(datetime_UTC >= strptime(timestart,"%F", tz=metadata$Timezone[metadata$Site_ID==site]) &
           datetime_UTC <= strptime(timeend,"%F", tz=metadata$Timezone[metadata$Site_ID==site])) %>%
    ggplot(aes(x=as.POSIXct(datetime_UTC))) +
    geom_line(aes(y=OA_temp_F/5, color = "Outdoor Air Temperature"),size=0.3) + 
    geom_line(aes(y=SA_temp_F/5, color = "Supply Air Temperature"),size=0.3) +
    geom_line(aes(y=ODU_pwr_kW, color = "Outdoor Unit Power"),size=0.3) + 
    geom_line(aes(y=fan_pwr_kW, color = "Supply Fan Power"),size=0.3) +
    geom_line(aes(y=auxheat_pwr_kW, color = "Auxiliary Power"),size=0.3) + 
    scale_y_continuous(name = "Power (kW)",
                       limits = c(-4, 25),
                       sec.axis = sec_axis(~.*5, name ="Temperature (F)")) +
    scale_color_manual(name = "", breaks = c("Auxiliary Power","Outdoor Unit Power","Supply Fan Power","Outdoor Air Temperature","Supply Air Temperature"),
                       values = c("#E69F00","gray","#009E73","black","#56B4E9","#CC79A7", "#F0E442", "#0072B2", "#D55E00")) +
    labs(title=paste0("System operation daily plot for site ", site),x="") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          panel.grid.major = element_line(size = 0.5),
          panel.grid.minor = element_line(size = 0.1),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5)) +
    guides(color=guide_legend(override.aes=list(size=3)))
}


# COP time series comparison chart with SAT and RAT
daily_COP_plot <- function(site, timestart, timeend){

  df %>% mutate(datetime_UTC = datetime_UTC %>% with_tz(metadata$Timezone[metadata$Site_ID==site])) %>% 
    filter(datetime_UTC >= strptime(timestart,"%F", tz=metadata$Timezone[metadata$Site_ID==site]) &
           datetime_UTC <= strptime(timeend,"%F", tz=metadata$Timezone[metadata$Site_ID==site])) %>%
    summarize(datetime_UTC = datetime_UTC[1],
              SA_Temp_F=mean(SA_Temp_F, na.rm=T),
              RA_temp_F=mean(RA_temp_F, na.rm=T),
              ODU_pwr_kW=mean(ODU_pwr_kW, na.rm=T),
              # fan_pwr_kW=mean(fan_pwr_kW, na.rm=T), Removing fan power because there are too many lines and fan power is small
              auxheat_pwr_kW=mean(auxheat_pwr_kW, na.rm=T),
              Heat_Output=mean(heat_output_btu_h/3412, na.rm=T),
              COP_Heating=sum(heat_output_btu_h/3412, na.rm=T)/sum(HP_system_pwr_kW, na.rm=T),
              Cooling_Output=mean(cooling_output_btu_h/3412, na.rm=T),
              COP_Cooling=sum(cooling_output_btu_h/3412, na.rm=T)/sum(HP_system_pwr_kW, na.rm=T)) %>%
    ggplot(aes(x=as.POSIXct(datetime_UTC))) +
    geom_line(aes(y=SA_TempF/2, linetype = "Supply Air Temperature"),color="black",size=0.3) + 
    geom_line(aes(y=RA_temp_F/2, linetype = "Return Air Temperature"),color="black",size=0.3) +
    geom_line(aes(y=ODU_pwr_kW, color = "Outdoor Unit Power"),size=0.3) + 
    # geom_line(aes(y=fan_pwr_kW, color = "Supply Fan Power"),size=0.3) +
    geom_line(aes(y=auxheat_pwr_kW, color = "Auxiliary Power"),size=0.3) + 
    geom_line(aes(y=Heat_Output, color = "Heat Output"),size=0.3) +
    geom_line(aes(y=COP_Heating, color = "COP Heating"),size=1) + 
    geom_line(aes(y=Cooling_Output, color = "Cooling Output"),size=0.3) +
    geom_line(aes(y=COP_Cooling, color = "COP Cooling"),size=1) + 
    scale_y_continuous(name = "Power (kW)",
                       # limits = c(-4, 25),
                       sec.axis = sec_axis(~.*2, name ="Temperature (F)")) +
    scale_color_manual(name = "Power/COP", breaks = c("Auxiliary Power","Outdoor Unit Power", "Heat Output", "COP"),
                       values = c("#E69F00","gray","#009E73","black","#56B4E9","#CC79A7", "#F0E442", "#0072B2", "#D55E00")) +
    scale_linetype_manual(name = "Temperature", values = c("solid","dashed")) +
    labs(title=paste0("COP and power daily plot for site ", site),x="") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          panel.grid.major = element_line(size = 0.5),
          panel.grid.minor = element_line(size = 0.1),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5)) +
    guides(color=guide_legend(override.aes=list(size=3)))
}


# Operating mode summary for each day in a large timeframe
operating_mode_season <- function(site, timestart, timeend){
  df %>%
    mutate(datetime_UTC = datetime_UTC %>% with_tz(metadata$Timezone[metadata$Site_ID==site]),
           operating_mode = ifelse(is.na(operating_mode), "Data Unavailable", operating_mode)) %>%
    group_by(date_local, operating_mode) %>%
    summarize(Mode_Time = n()) %>%
    ggplot(aes(x=as.POSIXct(date_local, format="%F", tz=metadata$Timezone[metadata$Site_ID==site]), fill=operating_mode, y=Mode_Time)) +
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




