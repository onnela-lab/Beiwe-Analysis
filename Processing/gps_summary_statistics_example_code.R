##Code to generate daily GPS summary metrics
# Author: Harrison Reeder

library(dplyr)
library(tidyr)
library(ggplot2)
library(MASS)
library(RcppRoll)
library(zoo)
library(lubridate)
library(stringr)
library(ggpubr)
library(rcompanion)

##**********************##
#### Initialize Paths ####
##**********************##

#' NOTE: Directory paths should end in a "/" so that appending filename to string yields valid path.

#' This is the path to the base directory containing all code, input and output directories
path =  "[ Path to directory where you've unzipped the scripts and data folder, ending in / ]"

#' This is the directory containing the code files necessary to analyse GPS data
#' In particular it should have the following three files in it:
#' "GPS_preprocessing.R"
#' "gps_imputation.R"
#' "gps_survey_communication_dailyfeatures.R"
scripts = path


#' This is the folder containing downloaded patient GPS data.
#' In this folder, each subject should have their own subfolder downloaded from Beiwe.
#' There should be no other subfolders of data in this directory besides the individual patient subfolders.
input = paste0(path,"GPSExample/")

#This is the directory where temporary RData files are written.
temp = path

# e.g., 
# temp = paste0(path,"Temp/")

#This is the directory where final summary output csv files are written.
output = path

# e.g.,
# output = paste0(path,"Output/")


##************************************************##
#### Source necessary functions from Ian's code ####
##************************************************##

#As long as the filepaths are defined as above, this should source the three necessary files.
source(paste0(scripts,"GPS_preprocessing.R"))
source(paste0(scripts,"gps_imputation.R"))
source(paste0(scripts,"gps_survey_communication_dailyfeatures.R"))

##*************************************##
#### Read in and analyze patient data####
##*************************************##

#### Cleaning Process
# Following Barnett & Onnela (2018), for the data processing I reviewed and used Ian Barnett's code to prepare the GPS data as follows:
# 
# * Read in all available GPS data in raw form--(time of reading, latitude, longitude)
# * Project latitude and longitude into 2D space (in meters)
# * Convert data into flights and pauses
#   + Each row of data is a time interval, either with two endpoints of a "flight" of motion, or the single location of a pause.
# * Impute data for missing time intervals
# * Calculate metrics based on full flight/pause data


# 'input' is a filepath to a directory of subfolders.
# Each subfolder is named with a unique subject_id, 
# so this command creates a list of relevant patient ids.
patient_ids <- list.dirs(input,full.names = F,recursive = F)


# create an empty list object to store the output
# this will become a list of lists, where each element is a patient, 
# and for each patient there is a list of summary outputs.
#results <- list()

#loop through each patient id in the data directory
for(i in 1:length(patient_ids)){
  print(paste0("Patient: ",i, " ", patient_ids[i]))
  print(Sys.time())
  
  
  ##First, we look in the patient's "identifier" folder if it is available,
  ##to store their device type
  ##If that data isn't downloaded, we skip it.
  
  
  #check if the patient has the "identifier" dataset downloaded
  identifier_input = paste0(input,patient_ids[i],"/identifiers/")
  if(dir.exists(identifier_input)){
    filelist <- list.files(path = identifier_input, pattern = "\\.csv$")
    file <- filelist[1] #if the patient has multiple identifier data files, take the first one.
    
    #read in identifier dataset
    identifier_temp_data <- read.csv(paste0(identifier_input,file),
                                     fileEncoding="UTF-8",
                                     row.names = NULL,
                                     stringsAsFactors = F)
    
    #note that sometimes the file is messed up for some reason,
    #but the seventh column should always be the device type so it's fine.
    device_type <- identifier_temp_data[,7]
  } else{
    device_type = "Missing"
  }
  
  ##Next, we calculate the GPS summaries as follows:
  
  #build directory path to gps datasets
  #in keeping with convention used in function, no final slash
  gps_input = paste0(input,patient_ids[i],"/gps")
  fildir=gps_input
  
  #This is the patient_id, which is used to identify the output for this individual.
  filename=patient_ids[i]
  
  if(dir.exists(fildir)){ #if the patient_id does not exist as a folder, then skip it
    
    #This function call generates the list with all GPS summary outputs!
    #Currently HARDCODED that patients live on the eastern time-zone
    mout=MobilityFeatures(filename,fildir,tz = "America/New_York")
  } else{ mout= NULL}
  
  if(is.null(mout)){
    mout <- list()
  }
  
  mout[["patient_id"]] <- patient_ids[i]
  mout[["device_type"]] <- device_type
  
  results[[i]] <- mout
}



##****************************************##
#### Format and export patient summaries####
##****************************************##

#### Metrics Calculated
# We identify "significant locations" at which the individual spends substantial amounts of time. We label the location at which they spend the most time "home."
# The metrics then calculated (aggregated daily) are as follows:
# 
# * Time Spent at Home, in Hours (Hometime_hrs)
# * Total Distance Traveled, in Kilometers (DistTravelled_km)
# * Radius of Gyration, in Lilometers (RoG_km)
#   + This is a summary metric loosely capturing average distance from the "center" of a persons typical location.
# * Maximum Diameter, in Kilometers (MaxDiam_km)
#   + This is the largest distance between two points observed that day. 
# * Maximum Distance from Home, in Kilometers (MaxHomeDist_km)
# * Number of Significant Locations Visited (SigLocsVisited)
# * Average Flight Length, in Kilometers (AvgFlightLen_km)
#   + This is the average distance of a "flight" or travel event.
# * SD Flight Length, in Kilometers (StdFlightLen_km)
#   + This is the standard deviation of distance of a "flight" or travel event.
# * Average Flight Duration, in Minutes (AvgFlightDur_min)
#   + This is the average time of a "flight" or travel event.
# * SD Flight Duration, in Minutes (StdFlightDur_min)
#   + This is the standard deviation of time of a "flight" or travel event.
# * Proportion of Day spent Paused (ProbPause)
#   + This is the proportion of the day spent not traveling.
# * Entropy of Significant Location (SigLocEntropy)
#   + A metric measuring the amount of uncertainty we have about whether this person will travel to a significant location. It is highest when this person visits their significant locations either quite often or quite rarely. 
# * Minutes Missing (MinsMissing)
# * Circadian Routine Metric (CircdnRtn)
#   + Probability that, on any another day at around the same time, the person is within 200 meters of where they are on the day.
# * Weekday/Weekend Routine Metric (WkEndDayRtn)
#   + Probability that, on another weekday/weekend day at around the same time, the person is within 200 meters of where they are on the day.


#First, add the time, utc timecode, and patient_id to the data
metric_summary_temp <- data.frame()
for(i in 1:length(patient_ids)){
  if(is.null(results[[i]][["featavg"]])){
    next
  } else{
    temp_data <- as.data.frame(results[[i]][["featavg"]])
    temp_data$local_date <- rownames(results[[i]][["featavg"]])
    temp_time_dates <- as.POSIXct(temp_data$local_date,tz="America/New_York")
    attr(temp_time_dates, "tzone") <- "UTC"
    temp_data$utc_timecode <- as.numeric(temp_time_dates)*1000 #stored in milliseconds
    temp_data$days_from_start <- ceiling((temp_data$utc_timecode - min(temp_data$utc_timecode))/1000/60/60/24)
    temp_data$patient_id <- as.factor(results[[i]][["patient_id"]])
    metric_summary_temp <- rbind(metric_summary_temp,temp_data)
  }
}


#Next, reformat the data so that it is in more interpretable units
metric_summary <- metric_summary_temp %>% mutate(Hometime_hrs = Hometime/60,
                                                 DistTravelled_km = DistTravelled/1000,
                                                 MaxDiam_km = MaxDiam/1000,
                                                 MaxHomeDist_km = MaxHomeDist/1000,
                                                 AvgFlightLen_km = AvgFlightLen/1000,
                                                 StdFlightLen_km = StdFlightLen/1000,
                                                 AvgFlightDur_min = AvgFlightDur/60,
                                                 StdFlightDur_min = StdFlightDur/60,
                                                 RoG_km = RoG / 1000)


#Calculate lagged versions of each summary statistic as well.
metric_summary_daily_lags <- metric_summary %>%
  group_by(patient_id) %>%
  mutate(DistTravelled_km_lag1 = lag(DistTravelled_km,n=1),
         MaxDiam_km_lag1 = lag(MaxDiam_km,n=1),
         MaxHomeDist_km_lag1 = lag(MaxHomeDist_km,n=1),
         AvgFlightLen_km_lag1 = lag(AvgFlightLen_km,n=1),
         StdFlightLen_km_lag1 = lag(StdFlightLen_km,n=1),
         AvgFlightDur_min_lag1 = lag(AvgFlightDur_min,n=1),
         StdFlightDur_min_lag1 = lag(StdFlightDur_min,n=1),
         Hometime_hrs_lag1 = lag(Hometime_hrs,n = 1),
         SigLocsVisited_lag1 = lag(SigLocsVisited,n=1),
         SigLocEntropy_lag1 = lag(SigLocEntropy,n=1),
         ProbPause_lag1 = lag(ProbPause,n=1),
         CircdnRtn_lag1 = lag(CircdnRtn,n=1),
         WkEndDayRtn_lag1 = lag(WkEndDayRtn,n=1),
         RoG_km_lag1 = lag(RoG_km,n=1)) %>%
  dplyr::select(patient_id,contains("_km"),
                contains("_min"),contains("_hrs"),
                contains("SigLocsVisited"),contains("SigLocEntropy"),
                contains("ProbPause"),contains("RoG"),contains("CircdnRtn"),
                contains("WkEndDayRtn"),
                local_date,days_from_start,utc_timecode)


#' Finally, calculate seven-day rolling lags of each summary.
#' Aggregate the prior seven days three different ways:
#' 1. minimum
#' 2. maximum
#' 3. median

metric_summary_temp2 <- metric_summary_daily_lags %>%
  group_by(patient_id) %>%
  mutate( DistTravelled_km_max_7day = rollapplyr(DistTravelled_km,width=7,max,partial=T,na.rm=T),
          MaxDiam_km_max_7day = rollapplyr(MaxDiam_km,width=7,max,partial=T,na.rm=T),
          MaxHomeDist_km_max_7day = rollapplyr(MaxHomeDist_km,width=7,max,partial=T,na.rm=T),
          AvgFlightLen_km_max_7day = rollapplyr(AvgFlightLen_km,width=7,max,partial=T,na.rm=T),
          StdFlightLen_km_max_7day = rollapplyr(StdFlightLen_km,width=7,max,partial=T,na.rm=T),
          AvgFlightDur_min_max_7day = rollapplyr(AvgFlightDur_min,width=7,max,partial=T,na.rm=T),
          StdFlightDur_min_max_7day = rollapplyr(StdFlightDur_min,width=7,max,partial=T,na.rm=T),
          Hometime_hrs_max_7day = rollapplyr(Hometime_hrs,width=7,max,partial=T,na.rm=T),
          SigLocsVisited_max_7day = rollapplyr(SigLocsVisited,width=7,max,partial=T,na.rm=T),
          SigLocEntropy_max_7day = rollapplyr(SigLocEntropy,width=7,max,partial=T,na.rm=T),
          ProbPause_max_7day = rollapplyr(ProbPause,width=7,max,partial=T,na.rm=T),
          CircdnRtn_max_7day = rollapplyr(CircdnRtn,width=7,max,partial=T,na.rm=T),
          WkEndDayRtn_max_7day = rollapplyr(WkEndDayRtn,width=7,max,partial=T,na.rm=T),
          RoG_km_max_7day = rollapplyr(RoG_km,width=7,max,partial=T,na.rm=T)
  ) 

metric_summary_temp3 <- metric_summary_temp2 %>%
  group_by(patient_id) %>%
  mutate(DistTravelled_km_min_7day = rollapplyr(DistTravelled_km,width=7,min,partial=T,na.rm=T),
         MaxDiam_km_min_7day = rollapplyr(MaxDiam_km,width=7,min,partial=T,na.rm=T),
         MaxHomeDist_km_min_7day = rollapplyr(MaxHomeDist_km,width=7,min,partial=T,na.rm=T),
         AvgFlightLen_km_min_7day = rollapplyr(AvgFlightLen_km,width=7,min,partial=T,na.rm=T),
         StdFlightLen_km_min_7day = rollapplyr(StdFlightLen_km,width=7,min,partial=T,na.rm=T),
         AvgFlightDur_min_min_7day = rollapplyr(AvgFlightDur_min,width=7,min,partial=T,na.rm=T),
         StdFlightDur_min_min_7day = rollapplyr(StdFlightDur_min,width=7,min,partial=T,na.rm=T),
         Hometime_hrs_min_7day = rollapplyr(Hometime_hrs,width=7,min,partial=T,na.rm=T),
         SigLocsVisited_min_7day = rollapplyr(SigLocsVisited,width=7,min,partial=T,na.rm=T),
         SigLocEntropy_min_7day = rollapplyr(SigLocEntropy,width=7,min,partial=T,na.rm=T),
         ProbPause_min_7day = rollapplyr(ProbPause,width=7,min,partial=T,na.rm=T),
         CircdnRtn_min_7day = rollapplyr(CircdnRtn,width=7,min,partial=T,na.rm=T),
         WkEndDayRtn_min_7day = rollapplyr(WkEndDayRtn,width=7,min,partial=T,na.rm=T),
         RoG_km_min_7day = rollapplyr(RoG_km,width=7,min,partial=T,na.rm=T)) 

metric_summary_weekly_lags <- metric_summary_temp3 %>%
  group_by(patient_id) %>%
  mutate(DistTravelled_km_med_7day = rollapplyr(DistTravelled_km,width=7,median,partial=T,na.rm=T),
         MaxDiam_km_med_7day = rollapplyr(MaxDiam_km,width=7,median,partial=T,na.rm=T),
         MaxHomeDist_km_med_7day = rollapplyr(MaxHomeDist_km,width=7,median,partial=T,na.rm=T),
         AvgFlightLen_km_med_7day = rollapplyr(AvgFlightLen_km,width=7,median,partial=T,na.rm=T),
         StdFlightLen_km_med_7day = rollapplyr(StdFlightLen_km,width=7,median,partial=T,na.rm=T),
         AvgFlightDur_min_med_7day = rollapplyr(AvgFlightDur_min,width=7,median,partial=T,na.rm=T),
         StdFlightDur_min_med_7day = rollapplyr(StdFlightDur_min,width=7,median,partial=T,na.rm=T),
         Hometime_hrs_med_7day = rollapplyr(Hometime_hrs,width=7,median,partial=T,na.rm=T),
         SigLocsVisited_med_7day = rollapplyr(SigLocsVisited,width=7,median,partial=T,na.rm=T),
         SigLocEntropy_med_7day = rollapplyr(SigLocEntropy,width=7,median,partial=T,na.rm=T),
         ProbPause_med_7day = rollapplyr(ProbPause,width=7,median,partial=T,na.rm=T),
         CircdnRtn_med_7day = rollapplyr(CircdnRtn,width=7,median,partial=T,na.rm=T),
         WkEndDayRtn_med_7day = rollapplyr(WkEndDayRtn,width=7,median,partial=T,na.rm=T),
         RoG_km_med_7day = rollapplyr(RoG_km,width=7,median,partial=T,na.rm=T)) %>%
  dplyr::select(patient_id,contains("DistTravelled"),contains("MaxDiam"),
                contains("MaxHomeDist"),contains("AvgFlightLen"),contains("StdFlightLen"),
                contains("AvgFlightDur"),contains("StdFlightDur"),contains("Hometime"),
                contains("SigLocsVisited"),contains("SigLocEntropy"),
                contains("ProbPause"),contains("CircdnRtn"),
                contains("WkEndDayRtn"),contains("RoG"),
                local_date,days_from_start,utc_timecode)


##*************************##
####Export Resulting Data####
##*************************##

#export a unique csv file for each patient.
for(patient_id_cur in unique(as.character(metric_summary5$patient_id))){
  patient_sub_data <- metric_summary5 %>% filter(patient_id == patient_id_cur)
  write.csv(patient_sub_data,file=paste0(output,patient_id_cur,"_gps_summaries.csv"))
}



#Save results in RData file so that summary computation does not need to be repeated.
save(results,metric_summary,metric_summary_daily_lags,metric_summary_weekly_lags,
     file=paste0(temp,"Example_GPS_summary.RData"))
write.csv(metric_summary_weekly_lags,file=paste0(output,"GPS daily summaries with lags.csv"))
