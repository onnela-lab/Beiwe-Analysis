library(dplyr)
library(purrr)
library(lubridate)

data_filepath = "/Users/method/Zagaran/patient_data/all_data/data2"
source_filepath = "/Users/method/Zagaran/Beiwe-Analysis"
output_filepath = "/Users/method/Zagaran/patient_data/all_data/output"
patient_name = "tolsewzt"
verbose = TRUE
stream = "accelerometer"
minutes = 5 # ALI: a.k.a. acc_binsize
acc_binsize=5 # width of bin in minutes used for combining accelerometer data
acc_millisecond_divider = 30*1000
acc_frequency = 10
acc_burst_duration = 60
acc_break_duration = 60
frequency = 10
burst_duration = 60
break_duration = 60
#gps_millisecond_divider = 30*1000
#gps_frequency = 1
#gps_burst_duration = 60
#gps_break_duration = 60*10

# Preprocessing Data
if(length(list.files(paste(data_filepath, patient_name, "accelerometer", sep="/")))==0){
  cat("No files to preprocess.")
}else{
  if(verbose) cat("Preprocessing accelerometer data\n")
  if(verbose) cat(paste("Aggregating by",minutes,"minutes\n"))
  accelerometer_python = paste(source_filepath,"Preprocessing", "accelerometer_preprocessing.py",sep="/")
  
  patient_data_filepath = paste(output_filepath, "/Preprocessed_Data/Individual/", patient_name, sep="")
  patient_data_filename_TXT = paste(patient_data_filepath, "/appended_sheared_file_acc_",minutes,".txt",sep="")
  patient_data_filename_RDS = paste(patient_data_filepath, "/appended_sheared_file_acc_",minutes,".rds", sep="")
  
  if(file.exists(patient_data_filename_RDS)){
    if(verbose) cat("Already preprocessed.\n")
  }else{
    if(verbose) cat("Preprocessing...\n")
    system(paste("python", accelerometer_python, data_filepath, output_filepath, patient_name, minutes, sep=" "))
    accmat = read.table(patient_data_filename_TXT, header=T)
    accmat[,1] = accmat[,1] / 1000
    accmat = accmat[,-2]
    accmat[,c("hours","days")] = hour(as.POSIXlt(accmat[,"timestamp"], origin = "1970-01-01"))#hours(accmat[,"timestamp"])
    saveRDS(accmat, patient_data_filename_RDS)
    #file.remove(patient_data_filename_TXT)
  }
}

# Find bursts
find_bursts_python = paste(source_filepath,"Preprocessing", "find_bursts.py",sep="/")
patient_data_filepath = paste(output_filepath, "/Preprocessed_Data/Individual/", patient_name, sep="")
patient_data_filename_TXT = paste(patient_data_filepath, "/", stream, "_bursts.txt",sep="")
patient_data_filename_RDS = paste(patient_data_filepath, "/", stream, "_bursts.rds",sep="")

if(file.exists(patient_data_filename_RDS)){
  if(verbose) cat(paste(stream, "bursts file already exists.\n"))
}else{
  system(paste("python", find_bursts_python, data_filepath, patient_data_filename_TXT, patient_name, stream, acc_millisecond_divider))
  data = read.csv2(patient_data_filename_TXT, sep=",", header=T)
  saveRDS(data, patient_data_filename_RDS)
  #file.remove(patient_data_filename_TXT)
}



#summarize_data_quality(stream = "accelerometer", acc_burst_duration, acc_break_duration, acc_frequency)



data = list()
patient_names = list(patient_name)
for(patient_name in patient_names){  
  patient_data_filepath = paste(output_filepath, "/Preprocessed_Data/Individual/", patient_name, sep="")
  patient_data_filename_TXT = paste(patient_data_filepath, "/", stream, "_bursts.txt",sep="")
  patient_data_filename_RDS = paste(patient_data_filepath, "/", stream, "_bursts.rds",sep="")
  
  if(!file.exists(patient_data_filename_RDS)){
    if(verbose) cat(paste("The", stream, "bursts file does not yet exist for patient",patient_name, "\n"))
  }else{
    data[[patient_name]] = readRDS(patient_data_filename_RDS)
  }
}

data = do.call(rbind, data)
patients = data[,"patient"] %>% unique %>% unlist %>% as.vector %>% sort
bursts   = data %>% group_by(patient, date) %>% summarize(
  num_bursts=n(),
  avg_pings = mean(pings),
  sum_pings = sum(pings),
  avg_within_burst_duration  = mean(end-start)/1000,
  avg_between_burst_duration = mean(diff(start)/1000
  )
) %>%
  mutate(numeric_date = as.numeric(date))

bursts = bursts %>% group_by(patient, date) %>%
  mutate(avg_within_burst_frequency = sum_pings/(num_bursts*avg_within_burst_duration))

bursts = bursts %>% group_by(patient, date) %>%
  mutate(total_coverage = pmin(1,sum_pings/((burst_duration+break_duration)*burst_duration*frequency)),
         num_bursts_coverage = num_bursts/(24*60*60/(burst_duration+break_duration)),
         within_burst_length_coverage = avg_within_burst_duration/(burst_duration),
         within_burst_frequency_coverage = avg_within_burst_frequency/frequency
  )

mins = bursts %>% group_by(patient) %>% summarise(min_date = min(numeric_date))
for(pat in patients){
  sub = which(bursts[,"patient"]==pat)
  bursts[sub,"zeroed"] = bursts[sub,"numeric_date"]-unlist((mins %>% filter(patient == pat))[,"min_date"])
}
bursts = bursts %>% data.frame
saveRDS(bursts, paste(output_filepath,"/Processed_Data/Group/", stream, "_bursts.rds",sep=""))
