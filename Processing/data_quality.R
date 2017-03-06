data_quality = function(data_filepath, code_filepath, stream, pulse_duration, break_duration, frequency, millisecond_divider){
  output_filepath = paste(data_filepath, "/output", sep="")
  if(!file.exists(paste(output_filepath,"/", stream, "_pulses.txt",sep=""))){
    print("finding pulses... written in Python, takes ~30 seconds per patient")
    system(paste("python", code_filepath, data_filepath, stream, millisecond_divider))
  }
  print("finding counts (last step)")
  output_filepath = paste(data_filepath, "output",sep="/")
  stream_file = paste(output_filepath, "/",stream,"_pulses.txt",sep="")
  data = read.csv2(stream_file,sep=",",header=T)
  
  patients = data[,"patient"] %>% unique %>% unlist %>% as.vector %>% sort
  #days     = data %>% select(date)    %>% unique %>% unlist %>% as.vector %>% sort
  counts   = data %>% group_by(patient, date) %>% summarize(count=n(), avg_pings = mean(pings),sum_pings = sum(pings),
                                                            avg_within_pulse_duration = mean(end-start)/1000, avg_between_pulse_duration = mean(diff(start)/1000)) %>%
    mutate(numeric_date = as.numeric(date))
  counts = counts %>% group_by(patient, date) %>%
    mutate(coverage = pmin(1,sum_pings/(24*60*60/(pulse_duration+break_duration)*frequency*60)))
  
  
  colors = cols(length(patients),transparency=1-light_alpha)
  person_colors = colors[counts %>% data.frame %>% dplyr::select(patient) %>% unlist %>% as.numeric]
  names = counts %>% data.frame %>% dplyr::select(patient) %>% unique %>% unlist %>% as.character
  
  mins = counts %>% group_by(patient) %>% summarise(min_date = min(numeric_date))
  
  for(pat in patients){
    sub = which(counts[,"patient"]==pat)
    counts[sub,"zeroed"] = counts[sub,"numeric_date"]-unlist((mins %>% filter(patient == pat))[,"min_date"])
  }
  
  return(counts)
}

# data_quality(data_filepath, code_filepath = "C:/Phoenix/School/Harvard/Research/Beiwe/Code/find_pulses.py",
#              stream = "accelerometer",
#              pulse_duration = 60,
#              break_duration = 60,
#              frequency = 10,
#              millisecond_divider = 60*1000)

# data_quality(data_filepath, code_filepath = "C:/Phoenix/School/Harvard/Research/Beiwe/Code/find_pulses.py", 
#              stream = "gps",
#              pulse_duration = 60,
#              break_duration = 60*10,
#              frequency = 1,
#              millisecond_divider = 60*1000)

