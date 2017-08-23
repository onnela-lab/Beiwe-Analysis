plot_survey_responsiveness = function(legend = TRUE, ...){
  plot_filename = paste(output_filepath,"/Results/Group/survey_responsiveness.pdf", sep="")
  pdf(plot_filename,width=8,height=6)
  par(mgp=c(2.5,1,0),cex.main=1.5,cex.lab=1.5)
  
  timings = readRDS(paste(output_filepath, "/Processed_Data/Group/survey_responsiveness.rds", sep="")) %>% data.frame %>% subset(complete.cases(.))
  
  patient_names = timings %>% data.frame %>% dplyr::select(Person) %>% unique %>% unlist %>% as.character %>% sort
  dark_colors = cols(length(patient_names),transparency=1-light_alpha)
  lite_colors = cols(length(patient_names),dirtiness=0.8,darkness=.1)
  person_dark_colors = dark_colors[timings %>% data.frame %>% dplyr::select(Person) %>% unlist %>% as.factor %>% as.numeric]
  person_lite_colors = lite_colors[timings %>% data.frame %>% dplyr::select(Person) %>% unlist %>% as.factor %>% as.numeric]
  
  timings[,"zeroed"] = 0
  mins = timings %>% group_by(Person) %>% summarise(min_date = min(Notified))
  for(pat in patient_names){
    sub = which(timings[,"Person"]==pat)
    timings[sub,"zeroed"] = timings[sub,"Notified"]-unlist((mins %>% filter(Person == pat))[,"min_date"])
  }
  
  timings[,"zeroed"] = timings[,"zeroed"]/(60*60*24)
  x_min = min(unlist(timings["Notified"]))
  x_max = max(unlist(timings["Submitted"]))
  x_range = c(x_min, x_max+(x_max-x_min)*.2)
  x_range = c(0, max(timings[,"zeroed"]*ifelse(legend,1.2,1)))
  date_seq = seq(min(timings[,"Notified"]), max(timings[,"Notified"]), length.out=5)
  
  time_to_response   = log10(1+timings[,"Present"]-timings[,"Notified"])
  time_to_completion = log10(1+timings[,"Submitted"]-timings[,"Present"])
  complete = as.logical(complete.cases(time_to_response)*complete.cases(time_to_completion))
  time_to_response = time_to_response[complete]
  time_to_completion = time_to_completion[complete]
  timings = timings[complete,]
  
  plot(time_to_response, pch=16,xlim=c(0,nrow(timings)*ifelse(legend,1.3,1)),
       col=person_dark_colors,main="Time to First Response",#xaxt="n",
       xlab="Unique Daily Measurement",ylab="Time to First Response (Seconds)",yaxt="n",ylim=c(0,6))
  axis(2,at=0:6,labels=c(expression(1),expression(10^1),expression(10^2),expression(10^3),expression(10^4),expression(10^5),expression(10^6)))
  if(legend) legend("topright",legend=patient_names, col=dark_colors, pch=16,ncol=1,bg="white")
  
  plot(timings[,"zeroed"], time_to_response, pch=16,xlim=x_range,
       col=person_lite_colors,main="Time to First Response",#xaxt="n",
       xlab="Day",ylab="Time to First Response (Seconds)",yaxt="n",ylim=c(0,6))
  #axis(1, at=date_seq, label = as.Date(as.POSIXct(date_seq,origin="1970-01-01")))
  lines(lowess(timings[,"zeroed"], time_to_response,f=.9),col="red",lwd=3.5,lty=lowess_lty)
  axis(2,at=0:6,labels=c(expression(1),expression(10^1),expression(10^2),expression(10^3),expression(10^4),expression(10^5),expression(10^6)))
  if(legend) legend("topright",legend=patient_names, col=dark_colors, pch=16,ncol=1,bg="white")
  
  
  
  plot(time_to_completion, pch=16,main="Time to Complete After First Response",
       col=person_dark_colors,xlim=c(0,nrow(timings)*ifelse(legend,1.3,1)),
       xlab="Unique Daily Measurement",ylab="Time to First Response (Seconds)",yaxt="n",ylim=c(0,6))
  axis(2,at=0:6,labels=c(expression(1),expression(10^1),expression(10^2),expression(10^3),expression(10^4),expression(10^5),expression(10^6)))
  if(legend) legend("topright",legend=patient_names, col=dark_colors, pch=16,ncol=1,bg="white")
  
  plot(timings[,"zeroed"], time_to_completion, pch=16,main="Time to Complete After First Response",
       col=person_lite_colors,xlim=x_range,#xaxt="n",
       xlab="Day",ylab="Time to First Response (Seconds)",yaxt="n",ylim=c(0,6))
  #axis(1, at=date_seq, label = as.Date(as.POSIXct(date_seq,origin="1970-01-01")))
  lines(lowess(timings[,"zeroed"], time_to_completion),col="red",lwd=3.5,lty=lowess_lty)
  axis(2,at=0:6,labels=c(expression(1),expression(10^1),expression(10^2),expression(10^3),expression(10^4),expression(10^5),expression(10^6)))
  if(legend) legend("topright",legend=patient_names, col=dark_colors, pch=16,ncol=1,bg="white")
  
  
  
  dev.off()
}