model_data_quality_predictiveness = function(questions_filter, SHIFT){
  patient_names = list.files(data_filepath)[-grep("\\.",list.files(data_filepath))]
  weekly_coverage = function(stream, shift, ...){
    data = readRDS(paste(output_filepath, "/Preprocessed_Data/Group/", stream, "_bursts.rds", sep="")) %>% data.frame %>% subset(complete.cases(.))
    data[,"zeroed_week"] = data[,"zeroed"] %/% 7 + shift
    return(
      data %>% group_by(patient, zeroed_week) %>% summarize(
        #mean_num_bursts_coverage = mean(num_bursts_coverage),
        #mean_within_burst_length_coverage = mean(within_burst_length_coverage),
        #mean_within_burst_frequency_coverage = mean(within_burst_frequency_coverage)
        mean_total_coverage = sum(total_coverage)/7
      )
    )
  }
  create_zeroed_columns = function(data, shift){
    data[,"zeroed"] = 0
    mins = data %>% group_by(patient) %>% summarise(min_date = min(numeric_date))
    for(pat in patient_names){
      sub = which(data[,"patient"]==pat)
      data[sub,"zeroed"] = data[sub,"numeric_date"]-unlist((mins %>% filter(patient == pat))[,"min_date"])
    }
    data[,"zeroed_week"] = data[,"zeroed"] %/% 7 + shift
    return(data)
  }
  
  
  weekly_accelerometer = weekly_coverage("accelerometer", shift = SHIFT)
  weekly_gps = weekly_coverage("gps", shift = SHIFT)
  
  timings = readRDS(paste(output_filepath, "/Processed_Data/Group/survey_timings.rds", sep="")) %>% data.frame %>% subset(complete.cases(.))
  timings[,"numeric_date"] =as.numeric(as.Date(as.POSIXct(timings[,"Notified"],origin="1970-01-01")))
  colnames(timings)[which(colnames(timings) == "Person")] = "patient"
  timings = timings %>% data.frame
  timings[,"time_to_present"] = log10(timings[,"Present"]-timings[,"Notified"])
  timings[,"time_to_submitted"] = log10(timings[,"Submitted"]-timings[,"Present"])
  timings = create_zeroed_columns(timings, shift = SHIFT)
  timings = timings[complete.cases(timings)&(!is.infinite(timings[,"time_to_present"]))&(!is.infinite(timings[,"time_to_submitted"])),]
  weekly_timings = timings %>% group_by(patient, zeroed_week) %>% summarize(
    mean_time_to_present = mean(time_to_present),
    mean_time_to_submitted = mean(time_to_submitted)
  ) %>% data.frame
  
  surveys = list()
  for(patient_name in patient_names){
    patient_survey_filename = paste(output_filepath, "/Preprocessed_Data/Individual/",patient_name, "/survey_data.rds",sep="")
    if(file.exists(patient_survey_filename))
      surveys[[patient_name]] = readRDS(patient_survey_filename) %>% dplyr::filter(question.text %in% questions_filter) %>% group_by(survey_id, timestamp) %>% summarize(count=n(), mean_score = mean(as.numeric(answer),na.rm=T), completion = sum(!is.na(as.numeric(answer)))) %>% data.frame %>% mutate(patient = patient_name)
  }
  surveys = do.call(rbind, surveys)
  surveys[,"date"] = as.factor(as.Date(as.POSIXct(surveys[,"timestamp"],origin="1970-01-01")))
  surveys[,"numeric_date"] = as.numeric(surveys[,"date"])
  surveys = create_zeroed_columns(surveys, shift = 0)
  weekly_surveys = surveys %>% group_by(patient, zeroed_week) %>% summarize(mean_score = mean(mean_score), completion = sum(completion))
  
  
  
  data=Reduce(function(...) merge(..., all = TRUE, by = c("patient", "zeroed_week")), 
              list(weekly_accelerometer, weekly_gps, weekly_timings, weekly_surveys))
  #data[which(is.infinite(data[,"mean_within_burst_frequency_coverage.y"])),"mean_within_burst_frequency_coverage.y"] = NA
  
  
  
  mod = lmer(mean_score~mean_total_coverage.x + mean_total_coverage.y + 
               mean_time_to_present + mean_time_to_submitted + completion + (1|patient), data=data)
  return(mod)
}

