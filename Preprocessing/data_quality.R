data_quality = function(stream,
						pulse_duration,
						break_duration,
						frequency,
						millisecond_divider,
						code_filepath = paste(source_filepath, "/Preprocessing/find_pulses.py",sep=""),
						verbose = TRUE,
						...)
{						
  output_filepath = paste(output_filepath,"/Preprocessed_Data/Group",sep="")
  output_filename = paste(output_filepath,"/", stream, "_pulses.txt",sep="")
  if(file.exists(output_filename)){
  if(verbose) cat("Pulses file already exists.\n")
  }else{
    cat("finding pulses... written in Python, takes ~30 seconds per patient\n")
    system(paste("python", code_filepath, data_filepath, output_filepath, stream, millisecond_divider))
  }
  if(verbose) cat("finding counts (last step)\n")
  stream_file = paste(output_filepath, "/",stream,"_pulses.txt",sep="")
  data = read.csv2(stream_file,sep=",",header=T)
  
  patients = data[,"patient"] %>% unique %>% unlist %>% as.vector %>% sort
  #days     = data %>% select(date)    %>% unique %>% unlist %>% as.vector %>% sort
  counts   = data %>% group_by(patient, date) %>% summarize(count=n(), avg_pings = mean(pings),sum_pings = sum(pings),
                                                            avg_within_pulse_duration = mean(end-start)/1000, avg_between_pulse_duration = mean(diff(start)/1000)) %>%
  mutate(numeric_date = as.numeric(date))
  counts = counts %>% group_by(patient, date) %>%
  mutate(coverage = pmin(1,sum_pings/(24*60*60/(pulse_duration+break_duration)*frequency*60)))
  mins = counts %>% group_by(patient) %>% summarise(min_date = min(numeric_date))
  
  for(pat in patients){
    sub = which(counts[,"patient"]==pat)
    counts[sub,"zeroed"] = counts[sub,"numeric_date"]-unlist((mins %>% filter(patient == pat))[,"min_date"])
  }
  
  saveRDS(counts, paste(output_filepath,"/", stream, "_pulses.rds",sep=""))
}

# data_quality(code_filepath = "C:/Phoenix/School/Harvard/Research/Beiwe/Code/find_pulses.py",
#              stream = "accelerometer",
#              pulse_duration = 60,
#              break_duration = 60,
#              frequency = 10,
#              millisecond_divider = 60*1000)

# data_quality(code_filepath = "C:/Phoenix/School/Harvard/Research/Beiwe/Code/find_pulses.py", 
#              stream = "gps",
#              pulse_duration = 60,
#              break_duration = 60*10,
#              frequency = 1,
#              millisecond_divider = 60*1000)

