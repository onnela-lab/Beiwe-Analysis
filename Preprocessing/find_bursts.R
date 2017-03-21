find_bursts = function(patient_name,
						stream,
						millisecond_divider,
						code_filepath = paste(source_filepath, "/Preprocessing/find_bursts.py",sep=""),
						verbose = TRUE,
						...)
{						  
  patient_data_filepath = paste(output_filepath, "/Preprocessed_Data/Individual/", patient_name, sep="")
  patient_data_filename_TXT = paste(patient_data_filepath, "/", stream, "_bursts.txt",sep="")
  patient_data_filename_RDS = paste(patient_data_filepath, "/", stream, "_bursts.rds",sep="")
  
  if(file.exists(patient_data_filename_RDS)){
  if(verbose) cat(paste(stream, "bursts file already exists.\n"))
  }else{
    system(paste("python", code_filepath, data_filepath, patient_data_filename_TXT, patient_name, stream, millisecond_divider))
	data = read.csv2(patient_data_filename_TXT, sep=",", header=T)
	saveRDS(data, patient_data_filename_RDS)
	file.remove(patient_data_filename_TXT)
  }
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






