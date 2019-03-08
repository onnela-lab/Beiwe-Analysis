## Main.R
## Usage: Rscript main.R patient_id data_root_dir output_root_dir timestamp_str

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
suppressMessages(library(dplyr))
suppressMessages(library(purrr))
suppressMessages(library(lubridate))
suppressMessages(library(tidyr))
suppressMessages(library(ggplot2))
suppressMessages(library(MASS))
suppressMessages(library(RcppRoll))
suppressMessages(library(zoo))
suppressMessages(library(stringr))
suppressMessages(library(ggpubr))
suppressMessages(library(rcompanion))

# Handling command line arguements
args = commandArgs(trailingOnly=TRUE)
if (length(args) != 4) {
  stop("ERROR: missing arguments", call.=FALSE)
}
patient_id = args[1]
data_filepath = args[2]
output_filepath = args[3]
timestamp_str = args[4]

# Global variables
timezone = "America/New_York"

# Get Beiwe_Analysis directory path
BA_PATH <- Sys.getenv("BEIWE_ANALYSIS_PROJECT_PATH")

# Import helper functions
source(paste0(BA_PATH, "/Summary/GPS_preprocessing.R"))
source(paste0(BA_PATH, "/Summary/gps_imputation.R"))
source(paste0(BA_PATH, "/Summary/gps_survey_communication_dailyfeatures.R"))
source(paste0(BA_PATH, "/Summary/summary_helper_functions.R"))

# Data Processing
print(paste0("Subject: ", patient_id))
print(Sys.time())

# Create output directories
dir.create(file.path(output_filepath, patient_id), showWarnings = FALSE)
patient_output_path = paste0(output_filepath, "/", patient_id, "/")

# Generate GPS Summary
try(gps_summary(data_filepath, patient_output_path, patient_id, timestamp_str, timezone), silent=T)

# Generate Texts summary
text_summary(data_filepath, patient_output_path, patient_id, timestamp_str, timezone)

# Generate Calls summary
call_summary(data_filepath, patient_output_path, patient_id, timestamp_str, timezone)

# Generate Power state summary
powerstate_summary(data_filepath, patient_output_path, patient_id, timestamp_str, timezone)
