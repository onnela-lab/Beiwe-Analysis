coverage_over_time = function(stream,
							  verbose = TRUE,
							  ...)
{
	input_filename = paste(output_filepath, "/Preprocessed_Data/Group/",stream, "_pulses.rds", sep="")
	output_filename = paste(output_filepath, "/Preprocessed_Data/Group/",stream, "_coverage.rds", sep="")
	
	if(!file.exists(input_filename)){
		if(verbose) cat("Pulses file does not exist.\n")
	}else{
	  counts = readRDS(paste(output_filepath, "/Preprocessed_Data/Group/",stream, "_pulses.rds", sep=""))
	  patients = counts %>% data.frame %>% dplyr::select(patient) %>% unique %>% as.vector
	  coverage_over_time = counts %>%
		group_by(zeroed) %>%
		summarize(
			total = sum(coverage,na.rm=T)/length(patients),
			mean = mean(coverage,na.rm=T),
			q10=quantile(coverage, 0.1, na.rm=T),
			median = quantile(coverage, 0.5, na.rm=T),
			q90=quantile(coverage, 0.9, na.rm=T)
		)
	saveRDS(coverage_over_time, output_filename)
	}
}

