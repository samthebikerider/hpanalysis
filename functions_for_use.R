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
# rm(list = ls())
# comment for test

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
run_cycle_calc <- function(timestamp, operate, mode){
  
    # index records the first row in heating/defrost mode to start the for loop
  index <- which(operate == mode)[1]         
  
    # ts is a value to store the timestamp at the beginning of a new cycle,
      # in order to be able to calculate the length of time when the cycle end
  ts <- timestamp[index]                     # Timestamp at first non-NA row
  
    # cycle is a vector to record the cycle runtimes, and will be returned at the end
  cycle <- rep(NA, length(timestamp))        # Initialize vector for cycle runtimes
  
    # track is a tracker to record if there is currently a cycle happening or not
  track <- TRUE                              # at index, we begin in a cycle
  
  if(!is.na(index)){
    # Do not run loop if system does not enter the specified mode (i.e., index is NA)
    for(row in (index+1):length(timestamp)){
      
      # if(is.na(operate[row])){
      #   next     # Skip NA row 
  
      if((is.na(operate[row]) | operate[row] != mode) & track == TRUE){
        # Heating/defrost cycle ends: If the previous row was heating/defrost and this row is not, record runtime.
        cycle[row-1] <- difftime(timestamp[row-1], ts, units="mins")
        track <- FALSE
        
      } else if(!is.na(operate[row]) & operate[row] == mode & track == FALSE){
        # Heating/defrost cycle begins: If the previous row was not heating/defrost and this row is, reset timestamp counter
        ts <- timestamp[row]                   # Reset timestamp
        track <- TRUE
      }
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
    scale_y_continuous(breaks = seq(0,200, by=10), minor_breaks = seq(0, 200, by=1), limits = c(55, 85)) +
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
# daily_defrost_plot <- function(site, timestart, timeend){
# 
#   df %>% mutate(datetime_UTC = datetime_UTC %>% with_tz(metadata$Timezone[metadata$Site_ID==site]),
#                 # If defrost mode, set marker to 5 (arbitrary number) to show activity
#                 Defrost = ifelse(operating_mode=="Defrost", 5, NA)) %>% 
#     filter(datetime_UTC >= strptime(timestart,"%F", tz=metadata$Timezone[metadata$Site_ID==site]) &
#            datetime_UTC <= strptime(timeend,"%F", tz=metadata$Timezone[metadata$Site_ID==site])) %>%
#     ggplot(aes(x=as.POSIXct(datetime_UTC))) +
#     geom_line(aes(y=ODU_pwr_kW, color = "Outdoor Unit Power"),size=0.3) + 
#     geom_line(aes(y=fan_pwr_kW, color = "Supply Fan Power"),size=0.3) +
#     geom_line(aes(y=auxheat_pwr_kW, color = "Auxiliary Power"),size=0.3) + 
#     geom_point(aes(y=Defrost, color = "Defrost Mode On"),size=2) + 
#     geom_point(aes(y=defrost_cycle_runtimes/2, color = "Defrost Cycle Length"),size=3,shape=8) +
#     scale_y_continuous(name = "Power (kW)",
#                        limits = c(-0.5, 21),
#                        sec.axis = sec_axis(~.*2, name ="Defrost Cycle Length (mins)")) +
#     scale_color_manual(name = "", breaks = c("Auxiliary Power","Outdoor Unit Power","Supply Fan Power","Defrost Mode On","Defrost Cycle Length"),
#                        values = c("#E69F00","black","#CC79A7","#009E73","#56B4E9", "gray", "#F0E442", "#0072B2", "#D55E00")) +
#     labs(title=paste0("Defrost cycle daily plot for site ", site),x="") +
#     theme_bw() +
#     theme(panel.border = element_rect(colour = "black",fill=NA),
#           panel.grid.major = element_line(size = 0.5),
#           panel.grid.minor = element_line(size = 0.1),
#           plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
#           axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
#           axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5)) +
#     guides(color=guide_legend(override.aes = list(shape=c(NA,NA,NA,16,8), 
#                                                   size=c(1,1,1,3,3),
#                                                   linetype=c(1,1,1,NA,NA))))
# }


# Power time series comparison chart with OAT and SAT
daily_operation_plot <- function(site, timestart, timeend){

  grph_data <- df %>% mutate(datetime_UTC = datetime_UTC %>% with_tz(metadata$Timezone[metadata$Site_ID==site])) %>% 
    filter(datetime_UTC >= strptime(timestart,"%F", tz=metadata$Timezone[metadata$Site_ID==site]) &
           datetime_UTC <= strptime(timeend,"%F", tz=metadata$Timezone[metadata$Site_ID==site]))
  
  # If the data is all NA, the plot will fail because of the secondary axis; need a solution to avoid this
  if(sum(!is.na(grph_data$OA_temp_F)) > 0 & 
     sum(!is.na(grph_data$SA_temp_F)) > 0 &
     sum(!is.na(grph_data$ODU_pwr_kW)) > 0 &
     sum(!is.na(grph_data$fan_pwr_kW)) > 0 &
     sum(!is.na(grph_data$auxheat_pwr_kW)) > 0){
    
    plot <- ggplot(grph_data, aes(x=as.POSIXct(datetime_UTC))) +
      # Power and temperature
      geom_line(aes(y=OA_temp_F/5, color = "Outdoor Air Temperature"),size=0.3) + 
      geom_line(aes(y=SA_temp_F/5, color = "Supply Air Temperature"),size=0.3) +
      geom_line(aes(y=ODU_pwr_kW, color = "Outdoor Unit Power"),size=0.3) + 
      geom_line(aes(y=fan_pwr_kW, color = "Supply Fan Power"),size=0.3) +
      geom_line(aes(y=auxheat_pwr_kW, color = "Auxiliary Power"),size=0.3) + 
      # Operation mode
      geom_point(aes(y=defrost_cycle_runtimes, shape = "Defrost Cycle Length"),size=3,color="purple") +
      geom_line(aes(y=ifelse(operating_mode == "Cooling", -2, as.numeric(NA)), linetype="Cooling"), color = "blue", size = 5) +
      geom_line(aes(y=ifelse(operating_mode == "Defrost", -2, as.numeric(NA)), linetype="Defrost"), color = "purple", size = 5) +
      geom_line(aes(y=ifelse(operating_mode %in% c("Heating-HP Only", "Heating-Aux Only", "Heating-Aux/HP") , -2, as.numeric(NA)), 
                    linetype="Heating"), color = "red", size = 5) +
      geom_line(aes(y=ifelse(operating_mode == "System Off", -2, as.numeric(NA)), linetype="System Off"), color = "gray", size = 5) +
      scale_y_continuous(name = "Power (kW)",
                         limits = c(-4, 25),
                         sec.axis = sec_axis(~.*5, name ="Temperature (F)")) +
      scale_color_manual(name = "", breaks = c("Auxiliary Power","Outdoor Unit Power","Supply Fan Power","Outdoor Air Temperature","Supply Air Temperature"),
                         values = c("#E69F00","gray","#009E73","black","#56B4E9","#CC79A7", "#F0E442", "#0072B2", "#D55E00")) +
      scale_linetype_manual(name = "Operating Mode (Bottom)", breaks = c("Cooling","Defrost","Heating","System Off"),
                         values = c("solid", "solid","solid","solid")) +
      scale_shape_manual(name = "", values = 8) + 
      labs(title=paste0("System operation daily plot for site ", site),x="") +
      theme_bw() +
      theme(panel.border = element_rect(colour = "black",fill=NA),
            panel.grid.major = element_line(size = 0.5),
            panel.grid.minor = element_line(size = 0.1),
            plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
            axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
            axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5)) +
      guides(color=guide_legend(override.aes=list(size=3)),
             linetype=guide_legend(override.aes=list(color=c("blue","purple","red","gray"))))
  } else { plot <- ggplot()}
  plot 
}


# COP time series comparison chart with SAT and RAT
daily_COP_plot <- function(site, timestart, timeend){

  grph_data <- df %>% mutate(datetime_UTC = datetime_UTC %>% with_tz(metadata$Timezone[metadata$Site_ID==site])) %>% 
    filter(datetime_UTC >= strptime(timestart,"%F", tz=metadata$Timezone[metadata$Site_ID==site]) &
             datetime_UTC <= strptime(timeend,"%F", tz=metadata$Timezone[metadata$Site_ID==site])) %>%
    mutate(COP_Heating = heat_output_btu_h / HP_system_pwr_kW,
           COP_Cooling = cooling_output_btu_h / HP_system_pwr_kW)
  
  # If the data is all NA, the plot will fail because of the secondary axis; need a solution to avoid this
  if(sum(!is.na(grph_data$SA_temp_F)) > 0 & 
     sum(!is.na(grph_data$RA_temp_F)) > 0 &
     sum(!is.na(grph_data$ODU_pwr_kW)) > 0 &
     sum(!is.na(grph_data$auxheat_pwr_kW)) > 0 &
     sum(!is.na(grph_data$heat_output_btu_h)) > 0 &
     sum(!is.na(grph_data$cooling_output_btu_h)) > 0){
    
    plot <- ggplot(grph_data, aes(x=as.POSIXct(datetime_UTC))) +
      geom_line(aes(y=SA_temp_F/2, linetype = "Supply Air Temperature"),color="black",size=0.3) + 
      geom_line(aes(y=RA_temp_F/2, linetype = "Return Air Temperature"),color="black",size=0.3) +
      geom_line(aes(y=ODU_pwr_kW, color = "Outdoor Unit Power"),size=0.3) + 
      # geom_line(aes(y=fan_pwr_kW, color = "Supply Fan Power"),size=0.3) +
      geom_line(aes(y=auxheat_pwr_kW, color = "Auxiliary Power"),size=0.3) + 
      geom_line(aes(y=heat_output_btu_h, color = "Heat Output"),size=0.3) +
      geom_line(aes(y=COP_Heating, color = "COP Heating"),size=1) + 
      geom_line(aes(y=cooling_output_btu_h, color = "Cooling Output"),size=0.3) +
      geom_line(aes(y=COP_Cooling, color = "COP Cooling"),size=1) + 
      scale_y_continuous(name = "Power (kW)",
                         limits = c(-4, 25),
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
    } else { plot <- ggplot()}
  plot 
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

# 1b. 
# Write long table for Operating mode daily summary
generate_operating_mode_summary_table <- function(site, wd_out){
  sum_table <- df %>% 
    filter(site_ID == site) %>%
    group_by(date_local) %>%
    summarize(Hours_Defrost=sum(operating_mode=="Defrost", na.rm=T)/3600,
              Hours_HP_Only=sum(operating_mode=="Heating-HP Only", na.rm=T)/3600,
              Hours_Aux_Only=sum(operating_mode=="Heating-Aux Only", na.rm=T)/3600,
              Hours_HP.Aux=sum(operating_mode=="Heating-Aux/HP", na.rm=T)/3600,
              Hours_System_Off=sum(operating_mode=="System Off", na.rm=T)/3600,
              Hours_Data_Unavailable=sum(is.na(operating_mode))/3600,
              Min_Temp=min(OA_temp_F, na.rm=T),
              Median_Temp=median(OA_temp_F, na.rm=T),
              Max_Temp=max(OA_temp_F, na.rm=T),
              Median_RH=median(OA_RH, na.rm=T))
  output_file <- paste0(wd_out, "/daily_ops/", site, "/Daily_Operation_Summary.csv")
  write.csv(sum_table, output_file)
}
# Print summary site comparison (To Do)
print_operating_summary <- function(site, wd_out){
  ft <- ifelse(df$Site_ID[1] %in% c("2458CE", "5291QJ"), 5, 1)
  temp <- read.csv(paste0(wd_out, "/daily_ops/", site, "/Daily_Operation_Summary.csv")) %>%
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

# 1c. Operating mode summary by OAT bin
  # the OA_temp_F,breaks have been updated for spring data
operating_mode_OAT <- function(site){
  df %>%
    filter(OA_temp_F <= temp_max & OA_temp_F > temp_min) %>%
    mutate(temp_int = cut(OA_temp_F,breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)),
           operating_mode = ifelse(is.na(operating_mode), "Data Unavailable", operating_mode)) %>%
    group_by(site_ID, temp_int, operating_mode) %>%
    summarize(Mode_Time=n()) %>%
    # mutate(temp_int = factor(temp_int, levels = unique(temp_int))) %>% # comment this line
    ggplot(aes(x=temp_int, fill=operating_mode, y=Mode_Time)) +
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

# 1d. Operating mode summary for all sites 0-32F
  # Print out data for each site for 0-32F range.
  # Note that sites 5539NO, 6112OH, and 2458CE do not have data in this range and should not save data.
operation_32F_all_sites <- function(){
  # Check column names in df and metadata
  if (!"site_ID" %in% colnames(df)) {
    stop("Column 'site_ID' not found in df.")
  }
  if (!"Site_ID" %in% colnames(metadata)) {
    stop("Column 'Site_ID' not found in metadata.")
  }
  
  filtered_df <- df %>%
    filter(OA_temp_F < 32 & OA_temp_F > 0) %>%
    mutate(operating_mode = ifelse(is.na(operating_mode), "Data Unavailable", operating_mode))
  
  summarized_df <- filtered_df %>%
    group_by(site_ID, operating_mode) %>%
    summarize(Mode_Time=n())
  
  # Merging step with error checks
  if (!"site_ID" %in% colnames(summarized_df)) {
    stop("Column 'site_ID' not found in summarized_df.")
  }
  if (!"Site_ID" %in% colnames(metadata)) {
    stop("Column 'Site_ID' not found in metadata.")
  }
  
  merged_df <- left_join(summarized_df, metadata %>% select(Site_ID, Manufacturer), by = c("site_ID" = "Site_ID"))
  # print(head(merged_df))
  
  final_plot <- merged_df %>%
    mutate(Site_Manufacturer = paste0(Manufacturer, "-", site_ID)) %>% 
    ggplot(aes(x=Site_Manufacturer, fill=operating_mode, y=Mode_Time)) +
    geom_bar(position="fill", stat="identity") +
    scale_fill_manual(name = "Operating Mode", 
                      breaks = c("Defrost","Heating-HP Only","Heating-Aux Only","Heating-Aux/HP","Cooling","System Off","Data Unavailable"),
                      values = c("#009E73","#F0E442","#CC3300","#E69F00","#3333FF","#666666","lightgrey")) +
    labs(title="Fraction of time in each operating mode at 0 to 32F site comparison",x="Site ID-Manufacturer", 
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


# 1e. Operating mode summary for all sites below 100F

operation_100F_all_sites <- function(){
  # Check column names in df and metadata
  if (!"site_ID" %in% colnames(df)) {
    stop("Column 'site_ID' not found in df.")
  }
  if (!"Site_ID" %in% colnames(metadata)) {
    stop("Column 'Site_ID' not found in metadata.")
  }
  
  filtered_df <- df %>%
    filter(OA_temp_F < 100) %>%
    mutate(operating_mode = ifelse(is.na(operating_mode), "Data Unavailable", operating_mode))
  
  summarized_df <- filtered_df %>%
    group_by(site_ID, operating_mode) %>%
    summarize(Mode_Time=n())
  
  # Merging step with error checks
  if (!"site_ID" %in% colnames(summarized_df)) {
    stop("Column 'site_ID' not found in summarized_df.")
  }
  if (!"Site_ID" %in% colnames(metadata)) {
    stop("Column 'Site_ID' not found in metadata.")
  }
  
  merged_df <- left_join(summarized_df, metadata %>% select(Site_ID, Manufacturer), by = c("site_ID" = "Site_ID"))
  # print(head(merged_df))
  
  final_plot <- merged_df %>%
    mutate(Site_Manufacturer = paste0(Manufacturer, "-", site_ID)) %>% 
    ggplot(aes(x=Site_Manufacturer, fill=operating_mode, y=Mode_Time)) +
    geom_bar(position="fill", stat="identity") +
    scale_fill_manual(name = "Operating Mode", 
                      breaks = c("Defrost","Heating-HP Only","Heating-Aux Only","Heating-Aux/HP","Cooling","System Off","Data Unavailable"),
                      values = c("#009E73","#F0E442","#CC3300","#E69F00","#3333FF","#666666","lightgrey")) +
    labs(title="Fraction of time in each operating mode below 100F site comparison",x="Site ID-Manufacturer", 
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



### Outdoor Air Graphs ----

# 2a. Heating mode cycling frequency site comparison
# 2b. Heating mode cycling frequency
# 2c. Heat pump power variation/modulation
# 3b. Print values for site comparison table

# ++++5a. Aux staging by OAT bin without defrost
aux_staging_no_defrost <- function(site){
  df %>%
    filter(OA_temp_F <= temp_max & OA_temp_F > temp_min & !is.na(operating_mode)) %>%
    mutate(temp_int = cut(OA_temp_F,breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)),
           operating_mode = ifelse(is.na(operating_mode), "Data Unavailable", operating_mode)) %>%
    group_by(site_ID, temp_int) %>%
    mutate(OA_temp_F = median(OA_temp_F, na.rm=T),
           Temp_Bin_Tim = n()) %>%
    ungroup() %>%
    group_by(site_ID, temp_int, number_aux_legs) %>%
    summarize(OA_temp_F = median(OA_temp_F, na.rm=T),
              average_duration_defrost = n()*100/mean(Temp_Bin_Tim),
              average_duration_no_defrost = sum(operating_mode != "Defrost", na.rm=T)*100/mean(Temp_Bin_Tim)) %>%
    filter(!is.na(number_aux_legs)) %>%
    mutate(temp_int = cut(OA_temp_F,breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100))) %>%
    ggplot(aes(x=temp_int)) +
    geom_bar(stat="identity", aes(y = average_duration_no_defrost, fill = as.character(number_aux_legs))) +
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

# ++++6a. Heating capacity (i.e., heating load) (Btu/h) by OAT bin (+ cooling capacity, both)
heat_cool_capacity_OAT_Bin <- function(site){
  df %>%
    mutate(temp_int = cut(OA_temp_F,breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100))) %>%
    filter(OA_temp_F <= temp_max & OA_temp_F > temp_min) %>%
    group_by(site_ID, temp_int) %>%
    summarize(heating_capacity = mean(heat_output_btu_h - auxheat_pwr_kW*3412, na.rm=T),
              aux_capacity = mean(auxheat_pwr_kW*3412, na.rm=T),
              cooling_capacity = mean(cooling_output_btu_h, na.rm=T)) %>%
    # mutate(temp_int = factor(temp_int, levels=temp_int)) %>%
    gather(Heat_Element, Capacity, heating_capacity:cooling_capacity) %>%
    ggplot(aes(x = temp_int, y = Capacity, fill = Heat_Element)) +
    geom_bar(stat="identity", position="stack") +
    scale_fill_manual(limits = c("heating_capacity", "aux_capacity", "cooling_capacity"),
                      labels=c("Heating", "Auxiliary", "Cooling"),
                      values=c("#00BFC4", "#F8766D", "#E69F00")) +
    labs(title=paste0("Delivered heating and cooling capacity per OAT bin for site ",site),
         x="Outdoor Air Temperature Bin (F)",
         y="Delivered Heating and Cooling Capacity (Btu/hr)",
         fill="Heating Element") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          legend.title = element_blank(),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5))
}


# ++++6b. Table to show the "maximum" heating (+ cooling) capacity in each OAT bin
  # debugged
print_heat_cool_capacity <- function(site){
  # temp1 <- df %>%
  #   filter(site_ID != site)
  temp2 <- df %>%
    filter(site_ID == site & OA_temp_F <= temp_max & OA_temp_F > temp_min & !is.na(operating_mode) & operating_mode != "System Off") %>%
    mutate(temp_int = cut(OA_temp_F,breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100))) %>%
    group_by(site_ID, temp_int) %>%
    summarize(max_heat_capacity = round(quantile(heat_output_btu_h, c(0.95), na.rm = T)/1000),
              max_HP_capacity = round(quantile(heat_output_btu_h[operating_mode=="Heating-HP Only"], c(0.95), na.rm = T)/1000),
              max_Aux_capacity = round(quantile(heat_output_btu_h[operating_mode=="Heating-Aux Only"], c(0.95), na.rm = T)/1000),
              max_cool_capacity = round(quantile(cooling_output_btu_h, c(0.95), na.rm = T)/1000) # [operating_mode=="Cooling"]?
              )
  return(temp2)
  # write.csv(temp2, paste0(wd_out, '/Graphs/Site Comparison/Max Heat and Cool Capacity.csv'), row.names = F)
}

# 6c. Table to show COP in aux only mode as a comparison to the expected 100% efficiency.


# ++++7a. COP vs outdoor air temperature for each site
heat_cool_COP_all_sites <- function(manufacturers){
  merged_df <- df %>%
    mutate(temp_int = cut(OA_temp_F, breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)),
           total_power = HP_system_pwr_kW + auxheat_pwr_kW + fan_pwr_kW) %>%
    filter(OA_temp_F < temp_max & OA_temp_F > temp_min) %>%
    group_by(site_ID, temp_int) %>%
    summarize(OA_temp_F = median(OA_temp_F, na.rm = TRUE),
              COP_Total = (sum(cooling_output_btu_h, na.rm = TRUE) + sum(heat_output_btu_h, na.rm = TRUE)) / sum(total_power, na.rm = TRUE) / 3412)
  
  plot_df <- left_join(merged_df, metadata %>% select(Site_ID, Manufacturer), by = c("site_ID" = "Site_ID")) %>%
    filter(Manufacturer %in% manufacturers) %>%
    mutate(Site_Manufacturer = paste0(Manufacturer, "-", site_ID))
  
  ggplot(plot_df, aes(x = OA_temp_F)) + 
    geom_point(size = 3, aes(y = COP_Total, color = Site_Manufacturer)) +
    geom_line(aes(y = COP_Total, color = Site_Manufacturer)) +
    scale_x_continuous(breaks = seq(0, 100, by = 10), minor_breaks = seq(0, 100, by = 5)) +
    geom_hline(yintercept = 0) +
    labs(title = "Overall system COP vs. outdoor air temperature",
         x = "Outdoor Temperature (F)",
         y = "COP",
         color = "Manufacturer-Site ID") +
    theme_bw() +
    theme(axis.ticks.y = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5))
}

# 7b. HP compressor only COP vs outdoor air temperature for each site
# 7c. HP compressor only COP vs outdoor air temperature for each site with defrost
# 7d. Adjusted COP vs outdoor air temperature for each site

# ++++8. COP vs outdoor air temperature
COP_vs_OAT <- function(site){
  df %>% 
    group_by(site_ID, date_local, hour_local) %>%
    mutate(total_power = HP_system_pwr_kW + auxheat_pwr_kW + fan_pwr_kW) %>%
    summarize(OA_temp_F = median(OA_temp_F, na.rm=T),
              total_COP = (sum(cooling_output_btu_h, na.rm = TRUE) + sum(heat_output_btu_h, na.rm = TRUE)) / sum(total_power, na.rm = TRUE) / 3412,
              percent_system_Off = sum(operating_mode=="System Off", na.rm=T)/n(),
              percent_defrost = sum(operating_mode=="Defrost", na.rm=T)/n(),
              percent_HP = sum(operating_mode=="Heating-HP Only", na.rm=T)/n(),
              percent_HP_aux = sum(operating_mode=="Heating-Aux/HP", na.rm=T)/n(),
              percent_aux = sum(operating_mode=="Heating-Aux Only", na.rm=T)/n()) %>%
    filter(site_ID==site) %>%
    filter(percent_system_Off < 0.9) %>%
    mutate(dominant_mode = ifelse(percent_defrost > 0.05, "Defrost", 
                                  ifelse(percent_HP >= percent_aux & percent_HP >= percent_HP_aux, "Heat Pump",
                                         ifelse(percent_HP_aux >= percent_aux, "Heat Pump + Aux Heat", "Aux Heat")))) %>%
    ggplot(aes(x = OA_temp_F)) + 
    geom_point(aes(y = total_COP, color=dominant_mode), size=0.8) +
    geom_hline(yintercept = 0) +
    ylim(0, 5) +
    scale_color_manual(name="Dominant Mode",
                       breaks=c("Heat Pump", "Heat Pump + Aux Heat", "Defrost", "Aux Heat"),
                       values=c("#009E73","#E69F00","#CC79A7", "coral2")) +
    labs(title=paste0("Hourly COP vs outdoor air temperature for site ", site),
         x="Outdoor Air Temperature (F)",
         y="COP") +
    theme_bw() +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5)) +
    guides(color=guide_legend(override.aes=list(size=3)))
}

### Demand Response Graphs (Not use for now)----

# Demand Reponse Time Series Investigation
DemandResponseTimeSeries <- function(site, timestart, timeend){
  
  DemRes <-  DemandResponseEvents %>%
    mutate(Start = Start %>% with_tz(metadata$Timezone[metadata$Site_ID==site]),
           End = End %>% with_tz(metadata$Timezone[metadata$Site_ID==site])) %>% 
    filter(Site_ID == site)
  
  Data <- df %>% 
    mutate(datetime_UTC = datetime_UTC %>% with_tz(metadata$Timezone[metadata$Site_ID==site])) %>% 
    filter(site_ID == site &
             datetime_UTC >= strptime(timestart,"%Y-%m-%d", tz=metadata$Timezone[metadata$Site_ID==site]) &
             datetime_UTC <= strptime(timeend,"%Y-%m-%d", tz=metadata$Timezone[metadata$Site_ID==site]))
  
  ggplot() +
    geom_rect(data=DemRes, aes(xmin=as.POSIXct(Start), xmax=as.POSIXct(End), ymin=-Inf, ymax=Inf, fill=Event_Type), alpha=0.2) +
    # no 'Room_TempF' in df
    geom_line(data=Data, aes(x=as.POSIXct(datetime_UTC), y=Room_TempF/5, color = "Room Temperature"),size=0.3) +
    geom_line(data=Data, aes(x=as.POSIXct(datetime_UTC), y=OA_temp_F/5, color = "Outdoor Air Temperature"),size=0.3) + 
    geom_line(data=Data, aes(x=as.POSIXct(datetime_UTC), y=HP_system_pwr_kW, color = "Outdoor Unit Power"),size=0.3) + 
    geom_line(data=Data, aes(x=as.POSIXct(datetime_UTC), y=auxheat_pwr_kW, color = "Auxiliary Power"),size=0.3) + 
    scale_y_continuous(name = "Power (kW)",
                       limits = c(-4, 20),
                       sec.axis = sec_axis(~.*5, name ="Temperature (F)")) +
    scale_color_manual(name = "", breaks = c("Auxiliary Power","Outdoor Unit Power","Outdoor Air Temperature","Room Temperature"),
                       values = c("#E69F00","gray","#009E73","black","#56B4E9","#CC79A7", "#F0E442", "#0072B2", "#D55E00")) +
    labs(title=paste0("Demand response event plot for site ", site),x="") +
    theme_bw() +
    # xlim(c(Data$datetime_UTC[1], Data$datetime_UTC[nrow(Data)])) +
    xlim(c(min(Data$datetime_UTC), max(Data$datetime_UTC))) +
    theme(panel.border = element_rect(colour = "black",fill=NA),
          panel.grid.major = element_line(size = 0.5),
          panel.grid.minor = element_line(size = 0.1),
          plot.title = element_text(family = "Times New Roman", size = 11, hjust = 0.5),
          axis.title.x = element_text(family = "Times New Roman",  size = 11, hjust = 0.5),
          axis.title.y = element_text(family = "Times New Roman", size = 11, hjust = 0.5)) +
    guides(color=guide_legend(override.aes=list(size=3)),
           fill=guide_legend(title="Event Type"))
}

# Demand Reponse Daily Time Series Investigation

# Demand response graph to compare energy use to another, similar day