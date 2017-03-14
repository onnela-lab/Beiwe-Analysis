survey_responsiveness_plotting = function(...){
  plot_filename = paste(output_filepath,"/Results/Group/survey_responsiveness.pdf", sep="")
  pdf(plot_filename,width=8,height=6)
  
  curated_total = readRDS(paste(output_filepath, "/Processed_Data/Group/surveys_responsiveness.rds", sep="")) %>% data.frame %>% subset(complete.cases(.))
  
  patient_names = curated_total %>% data.frame %>% dplyr::select(Person) %>% unique %>% unlist %>% as.character %>% sort
  dark_colors = cols(length(patient_names),transparency=1-light_alpha)
  lite_colors = cols(length(patient_names),transparency=0.8)
  person_dark_colors = dark_colors[curated_total %>% data.frame %>% dplyr::select(Person) %>% unlist %>% as.factor %>% as.numeric]
  person_lite_colors = lite_colors[curated_total %>% data.frame %>% dplyr::select(Person) %>% unlist %>% as.factor %>% as.numeric]
  
  curated_total[,"zeroed"] = 0
  mins = curated_total %>% group_by(Person) %>% summarise(min_date = min(Notified))
  for(pat in patient_names){
    sub = which(curated_total[,"Person"]==pat)
    curated_total[sub,"zeroed"] = curated_total[sub,"Notified"]-unlist((mins %>% filter(Person == pat))[,"min_date"])
  }
  
  curated_total[,"zeroed"] = curated_total[,"zeroed"]/(60*60*24)
  x_min = min(unlist(curated_total["Notified"]))
  x_max = max(unlist(curated_total["Submitted"]))
  x_range = c(x_min, x_max+(x_max-x_min)*.2)
  x_range = c(0, max(curated_total[,"zeroed"]*1.2))
  date_seq = seq(min(curated_total[,"Notified"]), max(curated_total[,"Notified"]), length.out=5)
  
  time_to_response   = log10(1+curated_total[,"Present"]-curated_total[,"Notified"])
  time_to_completion = log10(1+curated_total[,"Submitted"]-curated_total[,"Present"])
  complete = as.logical(complete.cases(time_to_response)*complete.cases(time_to_completion))
  time_to_response = time_to_response[complete]
  time_to_completion = time_to_completion[complete]
  curated_total = curated_total[complete,]
  
  plot(time_to_response, pch=16,xlim=c(0,nrow(curated_total)*1.3),
       col=person_dark_colors,main="Time to First Response",#xaxt="n",
       xlab="Unique Daily Measurement",ylab="Time to First Response (Seconds)",yaxt="n",ylim=c(0,6))
  axis(2,at=0:6,labels=c(expression(1),expression(10^1),expression(10^2),expression(10^3),expression(10^4),expression(10^5),expression(10^6)))
  legend("topright",legend=patient_names, col=dark_colors, pch=16,ncol=1,bg="white")
  
  plot(curated_total[,"zeroed"], time_to_response, pch=16,xlim=x_range,
       col=person_lite_colors,main="Time to First Response",#xaxt="n",
       xlab="Day",ylab="Time to First Response (Seconds)",yaxt="n",ylim=c(0,6))
  #axis(1, at=date_seq, label = as.Date(as.POSIXct(date_seq,origin="1970-01-01")))
  lines(lowess(curated_total[,"zeroed"], time_to_response,f=.9),col="red",lwd=2,lty=2)
  axis(2,at=0:6,labels=c(expression(1),expression(10^1),expression(10^2),expression(10^3),expression(10^4),expression(10^5),expression(10^6)))
  legend("topright",legend=patient_names, col=dark_colors, pch=16,ncol=1,bg="white")
  
  
  
  plot(time_to_completion, pch=16,main="Time to Complete After First Response",
       col=person_dark_colors,xlim=c(0,nrow(curated_total)*1.3),
       xlab="Unique Daily Measurement",ylab="Time to First Response (Seconds)",yaxt="n",ylim=c(0,6))
  axis(2,at=0:6,labels=c(expression(1),expression(10^1),expression(10^2),expression(10^3),expression(10^4),expression(10^5),expression(10^6)))
  legend("topright",legend=patient_names, col=dark_colors, pch=16,ncol=1,bg="white")
  
  plot(curated_total[,"zeroed"], time_to_completion, pch=16,main="Time to Complete After First Response",
       col=person_lite_colors,xlim=x_range,#xaxt="n",
       xlab="Day",ylab="Time to First Response (Seconds)",yaxt="n",ylim=c(0,6))
  #axis(1, at=date_seq, label = as.Date(as.POSIXct(date_seq,origin="1970-01-01")))
  lines(lowess(curated_total[,"zeroed"], time_to_completion),col="red",lwd=2,lty=2)
  axis(2,at=0:6,labels=c(expression(1),expression(10^1),expression(10^2),expression(10^3),expression(10^4),expression(10^5),expression(10^6)))
  legend("topright",legend=patient_names, col=dark_colors, pch=16,ncol=1,bg="white")
  
  
  
  dev.off()
}