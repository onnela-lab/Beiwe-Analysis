plot_survey_completion = function(legend = TRUE, ...){
  
  plot_filename = paste(output_filepath,"/Results/Group/survey_completion.pdf", sep="")
  pdf(plot_filename,width=8,height=6)
  par(mgp=c(2.5,1,0),cex.main=1.5,cex.lab=1.5)
  
  surveys = list()
  for(patient_name in patient_names){
    patient_survey_filename = paste(output_filepath, "/Preprocessed_Data/Individual/",patient_name, "/survey_data.rds",sep="")
    if(file.exists(patient_survey_filename))
      surveys[[patient_name]] = readRDS(patient_survey_filename) %>% group_by(survey_id, timestamp) %>% summarize(count=n()) %>% data.frame %>% mutate(patient = patient_name)
  }
  surveys = do.call(rbind, surveys)
  surveys[,"numeric_date"] = as.numeric(as.Date(as.POSIXct(surveys[,"timestamp"],origin="1970-01-01")))
  
  mins = surveys %>% group_by(patient) %>% summarise(min_date = min(numeric_date))
  patient_names = mins %>% dplyr::select(patient) %>% unique %>% data.frame %>% unlist
  surveys = surveys[with(surveys, order(patient, timestamp)),]
  (as.POSIXlt(surveys[,"timestamp"],origin="1970-01-01"))$wday
  for(patient_name in patient_names){
    sub = which(surveys[,"patient"]==patient_name)
    surveys[sub,"zeroed"] = surveys[sub,"numeric_date"]-unlist((mins %>% filter(patient == patient_name))[,"min_date"])
    surveys[sub,"cumsum"] = 1:length(sub)
  }
  
  surveys[,"week"] = surveys[,"zeroed"]%/%7
  surveys_week = surveys %>% group_by(patient, week) %>% summarize(num_surveys=n()) %>% data.frame
  
  patients = surveys %>% data.frame %>% dplyr::select(patient) %>% unique %>% unlist %>% as.character %>% sort
  names = surveys %>% data.frame %>% dplyr::select(patient) %>% unique %>% unlist %>% as.character
  dark_colors = cols(length(patients),transparency=1-light_alpha)
  lite_colors = cols(length(patients),dirtiness=0.8,darkness=.1)
  person_dark_colors = dark_colors[surveys_week %>% data.frame %>% dplyr::select(patient) %>% unlist %>% as.factor %>% as.numeric]
  person_lite_colors = lite_colors[surveys_week %>% data.frame %>% dplyr::select(patient) %>% unlist %>% as.factor %>% as.numeric]
  
  
  plot(log10(surveys_week[,"num_surveys"]),yaxt="n",col=person_dark_colors,pch=16,
       xlim=c(0,nrow(surveys_week)*ifelse(legend,1.3,1)),xlab="Week", ylab="Surveys Taken",main="Surveys Taken")
  axis(2,at=log10(c(1,2,3,4,5,15)),label=c(1,2,3,4,5,15))
  axis(2,at=log10(8),label="...",tick=F)
  if(legend) legend("topright",legend=names, col=dark_colors, pch=16,ncol=1,bg="white")
  
  plot(surveys_week[,"week"],log10(surveys_week[,"num_surveys"]),yaxt="n",col=person_lite_colors,pch=16,
       xlim=c(0,max(surveys_week[,"week"],na.rm=T)*ifelse(legend,1.3,1)), xlab="Week", ylab="Surveys Taken",main="Surveys Taken")
  lines(lowess(surveys_week[,"week"],log10(surveys_week[,"num_surveys"])),col="red",lwd=3.5, lty=lowess_lty)
  axis(2,at=log10(c(1,2,3,4,5,15)),label=c(1,2,3,4,5,15))
  axis(2,at=log10(8),label="...",tick=F)
  if(legend) legend("topright",legend=names, col=dark_colors, pch=16,ncol=1,bg="white")
  dev.off()
}
