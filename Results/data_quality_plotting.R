data_quality_plotting = function(output_filepath, counts, coverage, stream, frequency, pulse_duration, break_duration){
  par(mgp=c(2.5,1,0),cex.main=1.2,cex.lab=1.2)
  
  patients = counts %>% data.frame %>% dplyr::select(patient) %>% unique %>% unlist %>% as.character %>% sort
  colors = cols(length(patients),transparency=1-light_alpha)
  person_colors = colors[counts %>% data.frame %>% dplyr::select(patient) %>% unlist %>% as.numeric]
  names = counts %>% data.frame %>% dplyr::select(patient) %>% unique %>% unlist %>% as.character
  
  seconds_per_day = 60*60*24
  x_range = c(min(unlist(counts["zeroed"])), round(max(unlist(counts["zeroed"]))*1.3))

  pdf(output_filepath,width=8,height=6)
  
  plot(unlist(counts["zeroed"]),log10(1+unlist(counts["count"])),col=NA,yaxt = "n",ylim=c(0,5),xlim=x_range,
       xlab="Day",ylab="Number of Pulses",main=paste("Number of Pulses Per Day"," (",stream,")",sep=""))
  axis(2,at=0:5,labels=c(1, expression(10^1),expression(10^2),expression(10^3),expression(10^4),expression(10^5)))
  points(unlist(counts["zeroed"]),log10(1+unlist(counts["count"])),pch=16,col=person_colors)
  abline(h=log10(1+seconds_per_day/(pulse_duration+break_duration)),lwd=2,lty=1,col=line_col)
  legend("topright",legend=names, col=colors, pch=16,ncol=1,bg="white")
  
  
  plot(unlist(counts["zeroed"]),log10(1+unlist(counts["avg_pings"])),col=NA,ylim=c(0,5),xlim=x_range,
       yaxt = "n",xlab="Day",ylab="Average Pings per Pulse",main=paste("Average Pings per Pulse\nOver Time"," (",stream,")",sep=""))
  axis(2,at=0:5,labels=c(1, expression(10^1),expression(10^2),expression(10^3),expression(10^4),expression(10^5)))
  points(unlist(counts["zeroed"]),log10(1+unlist(counts["avg_pings"])),pch=16,col=person_colors)
  abline(h=log10(1+frequency*pulse_duration),lwd=2,lty=1,col=line_col)
  legend("topright",legend=names, col=colors, pch=16,ncol=1,bg="white")
  
  plot(unlist(counts["zeroed"]),log10(1+unlist(counts["avg_within_pulse_duration"])),col=NA,ylim=c(0,5),xlim=x_range,
       yaxt = "n",xlab="Day",ylab="Average Pings per Pulse",main=paste("Average Duration per Pulse\nOver Time"," (",stream,")",sep=""))
  axis(2,at=0:5,labels=c(1, expression(10^1),expression(10^2),expression(10^3),expression(10^4),expression(10^5)))
  points(unlist(counts["zeroed"]),log10(1+unlist(counts["avg_within_pulse_duration"])),pch=16,col=person_colors)
  abline(h=log10(pulse_duration),lwd=2,lty=1,col=line_col)
  legend("topright",legend=names, col=colors, pch=16,ncol=1,bg="white")
  
  plot(unlist(counts["zeroed"]),log10(unlist(counts["avg_between_pulse_duration"])-unlist(counts["avg_within_pulse_duration"])),
       ylim=c(0,5),col=NA,xlim=x_range,yaxt = "n",xlab="Day",ylab="Average Duration Between Pulses",
       main=paste("Average Duration Between Pulses\nOver Time"," (",stream,")",sep=""))
  axis(2,at=0:5,labels=c(1, expression(10^1),expression(10^2),expression(10^3),expression(10^4),expression(10^5)))
  points(unlist(counts["zeroed"]),log10(unlist(counts["avg_between_pulse_duration"])-unlist(counts["avg_within_pulse_duration"])),
         pch=16,col=person_colors)
  abline(h=log10(1+break_duration),lwd=2,lty=1,col=line_col)
  legend("topright",legend=names, col=colors, pch=16,ncol=1,bg="white")
  
  
  plot(unlist(coverage["zeroed"]), unlist(coverage["mean"]),col=NA,ylim=c(0,1),xlim=x_range,
       main=paste("Coverage Over Time"," (",stream,")",sep=""),xlab="Day",ylab="Coverage")
  points(unlist(coverage["zeroed"]), unlist(coverage["total"]),pch=16,col="gray")
  points(unlist(coverage["zeroed"]), unlist(coverage["mean"]),pch=16,col="black")
  legend("topright",pch=16,legend=c("Active Patients","All Patients"),col=c("black","gray"),bg="white")
  dev.off()
  dev.off()
}


# data_quality_plotting(output_filepath = "C:/Users/Patrick/Desktop/Accelerometer Data Quality.pdf",
#     accelerometer_counts, "accelerometer",
#     frequency = 10,
#     pulse_duration = 60,
#     break_duration = 60)
# 
# data_quality_plotting(output_filepath = "C:/Users/Patrick/Desktop/GPS Data Quality.pdf",
#     gps_counts, "gps",
#     frequency = 1,
#     pulse_duration = 60,
#     break_duration = 60*10)


