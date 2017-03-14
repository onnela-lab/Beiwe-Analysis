data_quality = function(stream,
						burst_duration,
						break_duration,
						frequency,
						millisecond_divider,
						code_filepath = paste(source_filepath, "/Preprocessing/find_bursts.py",sep=""),
						verbose = TRUE,
						...)
{						
  results_filepath = paste(output_filepath,"/Preprocessed_Data/Group",sep="")
  results_filename = paste(results_filepath,"/", stream, "_bursts.txt",sep="")
  if(file.exists(results_filename)){
  if(verbose) cat("Bursts file already exists.\n")
  }else{
    cat("finding bursts... written in Python, takes ~30 seconds per patient\n")
    system(paste("python", code_filepath, data_filepath, results_filepath, stream, millisecond_divider))
  }
  if(verbose) cat("making coverage statistics (last step)\n")
  stream_file = paste(results_filepath, "/",stream,"_bursts.txt",sep="")
  data = read.csv2(stream_file,sep=",",header=T)
  
  patients = data[,"patient"] %>% unique %>% unlist %>% as.vector %>% sort
  bursts   = data %>% group_by(patient, date) %>% summarize(
    num_bursts=n(),
    avg_pings = mean(pings),
    sum_pings = sum(pings),
    avg_within_burst_duration = mean(end-start)/1000,
    avg_between_burst_duration = mean(diff(start)/1000 # interesting, but not necessary.
    )
  ) %>%
    mutate(numeric_date = as.numeric(date))

  bursts = bursts %>% group_by(patient, date) %>%
    mutate(avg_within_burst_frequency = sum_pings/(num_bursts*avg_within_burst_duration))

  bursts = bursts %>% group_by(patient, date) %>%
    mutate(total_coverage = pmin(1,sum_pings/(24*60*60/(burst_duration+break_duration)*frequency*60)),
         num_bursts_coverage = num_bursts/(24*60*60/(burst_duration+break_duration)),
         within_burst_length_coverage = avg_within_burst_duration/(burst_duration),
         within_burst_frequency_coverage = avg_within_burst_frequency/frequency
    )

  mins = bursts %>% group_by(patient) %>% summarise(min_date = min(numeric_date))
  for(pat in patients){
    sub = which(bursts[,"patient"]==pat)
    bursts[sub,"zeroed"] = bursts[sub,"numeric_date"]-unlist((mins %>% filter(patient == pat))[,"min_date"])
  }

  saveRDS(bursts, paste(results_filepath,"/", stream, "_bursts.rds",sep=""))
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






