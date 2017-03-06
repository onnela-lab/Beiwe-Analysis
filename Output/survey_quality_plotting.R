survey_quality_plotting = function(output_filepath, curated_total){
  pdf(output_filepath,width=8,height=6)
  
  date_seq = seq(min(curated_total[,"Notified"]), max(curated_total[,"Notified"]), length.out=8)
  
  time_to_response   = log10(1+curated_total[,"Present"]-curated_total[,"Notified"])
  time_to_completion = log10(1+curated_total[,"Submitted"]-curated_total[,"Present"])
  plot(curated_total[,"Notified"], time_to_response, pch=16,
       col=colors[as.numeric(as.factor(curated_total[,"Person"]))],main="Time to First Response",
       xlab="Calendar Time",ylab="Time to First Response (Seconds)",yaxt="n",xaxt="n",ylim=c(0,6))
  axis(1, at=date_seq, label = as.Date(as.POSIXct(date_seq,origin="1970-01-01")))
  axis(2,at=0:6,labels=c(expression(1),expression(10^1),expression(10^2),expression(10^3),expression(10^4),expression(10^5),expression(10^6)))
  
  
  plot(curated_total[,"Present"], time_to_completion, pch=16,main="Time to Complete After First Response",
       col=colors[as.numeric(as.factor(curated_total[,"Person"]))],
       xlab="Calendar Time",ylab="Time to First Response (Seconds)",yaxt="n",xaxt="n",ylim=c(0,6))
  axis(1, at=date_seq, label = as.Date(as.POSIXct(date_seq,origin="1970-01-01")))
  axis(2,at=0:6,labels=c(expression(1),expression(10^1),expression(10^2),expression(10^3),expression(10^4),expression(10^5),expression(10^6)))
  
  dev.off()
}