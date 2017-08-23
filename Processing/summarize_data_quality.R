summarize_data_quality = function(stream,
								  burst_duration,
								  break_duration,
								  frequency,
								  code_filepath = paste(source_filepath, "/Preprocessing/find_bursts.py",sep=""),
								  verbose = TRUE,
								  ...)
{						  
  data = list()
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
}

# data_quality(stream = "accelerometer",
#              burst_duration = 60,
#              break_duration = 60,
#              frequency = 10,
#              millisecond_divider = 30*1000)

# data_quality(stream = "gps",
#                burst_duration = 60,
#                break_duration = 60*10,
#                frequency = 1,
#                millisecond_divider = 120*1000)






