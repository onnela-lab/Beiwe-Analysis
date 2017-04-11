coverage_over_time = function(stream,
                              verbose = TRUE,
                              ...)
{
  input_filename = paste(output_filepath,   "/Processed_Data/Group/",stream, "_bursts.rds", sep="")
  results_filename = paste(output_filepath, "/Processed_Data/Group/",stream, "_coverage.rds", sep="")
  
  if(!file.exists(input_filename)){
    if(verbose) cat("Bursts file does not exist.\n")
  }else{
    bursts   = readRDS(input_filename)
    n_patients = bursts %>% data.frame %>% dplyr::select(patient) %>% unique %>% unlist %>% length
    coverage_over_time = bursts %>%
      group_by(zeroed) %>%
      summarize(
        total_coverage_across_all_patients = sum(total_coverage,na.rm=T)/n_patients,
        total_coverage_across_active_patients = mean(total_coverage,na.rm=T),
        num_bursts_coverage = mean(num_bursts_coverage,na.rm=T),
        within_burst_length_coverage    = mean(within_burst_length_coverage,na.rm=T),
        within_burst_frequency_coverage = mean(within_burst_frequency_coverage,na.rm=T)
      )
	  coverage_over_time = coverage_over_time %>% data.frame
	  coverage_over_time[is.infinite(coverage_over_time[,"within_burst_frequency_coverage"]),"within_burst_frequency_coverage"] = 1
    saveRDS(coverage_over_time, results_filename)
  }
}