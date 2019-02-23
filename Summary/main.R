## Main.R
## Usage: Rscript main.R data_root_dir output_root_dir

#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("ggplot2")
#install.packages("MASS")
#install.packages("RcppRoll")
#install.packages("zoo")
#install.packages("lubridate")
#install.packages("stringr")
#install.packages("ggpubr")
#install.packages("rcompanion")

# Required libraries
library(dplyr)
library(purrr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(MASS)
library(RcppRoll)
library(zoo)
library(stringr)
library(ggpubr)
library(rcompanion)

# Handling command line arguements
args = commandArgs(trailingOnly=TRUE)
if (length(args) != 2) {
  stop("ERROR: missing arguments", call.=FALSE)
}
data_filepath = args[1]
output_filepath = args[2]

# Global variables
timezone = "America/New_York"
SIDs <- list.dirs(data_filepath, full.names = F, recursive = F)

# Import helper functions
source("GPS_preprocessing.R")
source("gps_imputation.R")
source("gps_survey_communication_dailyfeatures.R")
source("summary_helper_functions.R")

# Data Processing
for(i in 1:length(SIDs)){
  print(paste0("Subject: ", i, " ", SIDs[i]))
  print(Sys.time())
  
  # Create output directories
  dir.create(file.path(output_filepath, SIDs[[i]]), showWarnings = FALSE)
  patient_output_path = paste0(output_filepath, SIDs[[i]], "/")
  
  # Generate GPS Summary
  gps_summary(data_filepath, patient_output_path, SIDs[[i]], timezone)
  print(paste(SIDs[i], "GPS Summary successfully generated!", sep=" "))
  
  # Generate Texts summary
  text_summary(data_filepath, patient_output_path, SIDs[[i]], timezone)
  print(paste(SIDs[i], "Text Summary successfully generated!", sep=" "))

  # Generate Calls summary  
  call_summary(data_filepath, patient_output_path, SIDs[[i]], timezone)
  print(paste(SIDs[i], "Call Summary successfully generated!", sep=" "))
  
  # Generate Power state summary
  powerstate_summary(data_filepath, patient_output_path, SIDs[[i]], timezone)
  print(paste(SIDs[i], "Power State Summary successfully generated!", sep=" "))
  
  # Generate Accelerometer summary
  #accelerometer_summary(data_filepath, patient_output_path, SIDs[[i]], timezone)
  #print(paste(SIDs[i], "Accelerometer Summary successfully generated!", sep=" "))
}
