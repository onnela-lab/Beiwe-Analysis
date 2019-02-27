## Main.R
## Usage: Rscript main.R patient_id data_root_dir output_root_dir

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
if (length(args) != 3) {
  stop("ERROR: missing arguments", call.=FALSE)
}
patient_id = args[1]
data_filepath = args[2]
output_filepath = args[3]

# Global variables
timezone = "America/New_York"
#SIDs <- list.dirs(data_filepath, full.names = F, recursive = F)

# Import helper functions
source("GPS_preprocessing.R")
source("gps_imputation.R")
source("gps_survey_communication_dailyfeatures.R")
source("summary_helper_functions.R")

# Data Processing
print(paste0("Subject: ", patient_id))
print(Sys.time())

# Create output directories
dir.create(file.path(output_filepath, patient_id), showWarnings = FALSE)
patient_output_path = paste0(output_filepath, patient_id, "/")

# Generate GPS Summary
gps_summary(data_filepath, patient_output_path, patient_id, timezone)
#print(paste(SIDs[i], "GPS Summary successfully generated!", sep=" "))

# Generate Texts summary
text_summary(data_filepath, patient_output_path, patient_id, timezone)
#print(paste(SIDs[i], "Text Summary successfully generated!", sep=" "))

# Generate Calls summary
call_summary(data_filepath, patient_output_path, patient_id, timezone)
#print(paste(SIDs[i], "Call Summary successfully generated!", sep=" "))

# Generate Power state summary
powerstate_summary(data_filepath, patient_output_path, patient_id, timezone)
#print(paste(SIDs[i], "Power State Summary successfully generated!", sep=" "))

# Generate Accelerometer summary
#accelerometer_summary(data_filepath, patient_output_path, SIDs[[i]], timezone)
#print(paste(SIDs[i], "Accelerometer Summary successfully generated!", sep=" "))

