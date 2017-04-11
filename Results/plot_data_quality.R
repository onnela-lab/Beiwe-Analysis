plot_data_quality = function(stream, frequency, burst_duration, break_duration, legend=TRUE){
  
  plot_filename = paste(output_filepath, "/Results/Group/data_quality_",stream,".pdf",sep="")
  
  bursts   = readRDS(paste(output_filepath, "/Processed_Data/Group/", stream, "_bursts.rds", sep="")) %>% data.frame %>% subset(complete.cases(.))
  coverage = readRDS(paste(output_filepath, "/Processed_Data/Group/", stream, "_coverage.rds", sep="")) %>% data.frame
  
  patients = bursts %>% data.frame %>% dplyr::select(patient) %>% unique %>% unlist %>% as.character %>% sort
  dark_colors = cols(length(patients),transparency=1-light_alpha)
  lite_colors = cols(length(patients),dirtiness=0.8,darkness=.1)
  person_dark_colors = dark_colors[bursts %>% data.frame %>% dplyr::select(patient) %>% unlist %>% as.numeric]
  person_lite_colors = lite_colors[bursts %>% data.frame %>% dplyr::select(patient) %>% unlist %>% as.numeric]
  
  names = bursts %>% data.frame %>% dplyr::select(patient) %>% unique %>% unlist %>% as.character
  
  seconds_per_day = 60*60*24
  x_range = c(min(unlist(bursts["zeroed"])), round(max(unlist(bursts["zeroed"]))*ifelse(legend,1.3,1)))
  
  pdf(plot_filename,width=8,height=6)
  par(mgp=c(2.5,1,0),cex.main=1.5,cex.lab=1.5)
  
  plot(log10(1+bursts[,"num_bursts"]), col=NA,xlab="Unique Daily Measurements",xlim=c(1,nrow(bursts)*ifelse(legend,1.3,1)),
    main=paste("Number of Bursts Per Day"," (",stream,")",sep=""), ylab="Number of Bursts",yaxt="n",ylim=c(0,4))
  axis(2,at=log10(1+10^(0:4)),labels=c(1, expression(10^1),expression(10^2),expression(10^3),expression(10^4)))
  abline(h=log10(1+seconds_per_day/(burst_duration+break_duration)),lwd=2,col=dark_line_col)
  points(log10(.1+bursts[,"num_bursts"] %>% unlist), col=person_dark_colors,pch=16)
  if(legend) legend("topright",legend=names, col=dark_colors, pch=16,ncol=1,bg="white")
  box()
  
  plot(bursts[,"zeroed"],log10(1+bursts[,"num_bursts"]),col=NA,yaxt = "n",ylim=c(0,4),xlim=x_range,
       xlab="Day",ylab="Number of Bursts",main=paste("Number of Bursts Per Day"," (",stream,")",sep=""))
  axis(2,at=log10(1+10^(0:4)),labels=c(1, expression(10^1),expression(10^2),expression(10^3),expression(10^4)))
  points(unlist(bursts[,"zeroed"]),log10(1+unlist(bursts[,"num_bursts"])),pch=16,col=person_lite_colors)
  lines(lowess(bursts[,"zeroed"],log10(1+bursts[,"num_bursts"])),col="red",lwd=3.5,lty=lowess_lty)
  abline(h=log10(1+seconds_per_day/(burst_duration+break_duration)),lwd=2,lty=1,col=dark_line_col)
  if(legend) legend("topright",legend=names, col=dark_colors, pch=16,ncol=1,bg="white")
  box()

  plot(log10(.1+bursts[,"avg_within_burst_frequency"]), col=NA,xlab="Unique Daily Measurements",xlim=c(1,nrow(bursts)*ifelse(legend,1.3,1)),
    main=paste("Average Frequency Per Burst"," (",stream,")",sep=""), ylab="Average Frequency Per Burst",yaxt="n",ylim=c(-1,2))
  axis(2,at=log10(.1+c(0,10^(0:4))),labels=c(0, 1, expression(10^1),expression(10^2),expression(10^3),expression(10^4)))
  abline(h=log10(.1+frequency),lwd=2,col=dark_line_col)
  points(log10(.1+bursts[,"avg_within_burst_frequency"]), col=person_dark_colors,pch=16)
  if(legend) legend("topright",legend=names, col=dark_colors, pch=16,ncol=1,bg="white")
  box()
  
  plot(bursts[,"zeroed"],log10(.1+bursts[,"avg_within_burst_frequency"]),col=NA,ylim=c(-1,2),xlim=x_range,
       yaxt = "n",xlab="Day",ylab="Average Frequency Per Burst",main=paste("Average Frequency Per Burst"," (",stream,")",sep=""))
  axis(2,at=log10(.1+c(0,10^(0:4))),labels=c(0, 1, expression(10^1),expression(10^2),expression(10^3),expression(10^4)))
  points(unlist(bursts[,"zeroed"]),log10(.1+bursts[,"avg_within_burst_frequency"]),pch=16,col=person_lite_colors)
  lines(lowess(bursts[,"zeroed"],log10(.1+bursts[,"avg_within_burst_frequency"]),f=.3),col="red",lwd=3.5,lty=lowess_lty)
  abline(h=log10(.1+frequency),lwd=2,lty=1,col=dark_line_col)
  if(legend) legend("topright",legend=names, col=dark_colors, pch=16,ncol=1,bg="white")
  box()
  
  plot(log10(.1+bursts[,"avg_within_burst_duration"]), col=NA,xlab="Unique Daily Measurements",xlim=c(1,nrow(bursts)*ifelse(legend,1.3,1)),
    main=paste("Average Duration per Burst\nOver Time"," (",stream,")",sep=""), ylab="Average Duration per Burst",yaxt="n",ylim=c(0,4))
  axis(2,at=log10(1+10^(0:4)),labels=c(1, expression(10^1),expression(10^2),expression(10^3),expression(10^4)))
  abline(h=log10(.1+burst_duration),lwd=2,col=dark_line_col)
  points(log10(.1+bursts[,"avg_within_burst_duration"] %>% unlist), col=person_dark_colors,pch=16)
  if(legend) legend("topright",legend=names, col=dark_colors, pch=16,ncol=1,bg="white")
  box()
  
  plot(bursts[,"zeroed"],log10(1+bursts[,"avg_within_burst_duration"]),col=NA,ylim=c(0,4),xlim=x_range,
       yaxt = "n",xlab="Day",ylab="Average Duration Per Burst",main=paste("Average Duration per Burst\nOver Time"," (",stream,")",sep=""))
  axis(2,at=log10(1+10^(0:4)),labels=c(1, expression(10^1),expression(10^2),expression(10^3),expression(10^4)))
  points(bursts[,"zeroed"],log10(1+bursts[,"avg_within_burst_duration"]),pch=16,col=person_lite_colors)
  lines(lowess(bursts[,"zeroed"],log10(1+bursts[,"avg_within_burst_duration"])),col="red",lwd=3.5,lty=lowess_lty)
  abline(h=log10(burst_duration),lwd=2,lty=1,col=dark_line_col)
  if(legend) legend("topright",legend=names, col=dark_colors, pch=16,ncol=1,bg="white")
  box()
  
  plot(log10(1+bursts[,"avg_between_burst_duration"]-bursts[,"avg_within_burst_duration"]), col=NA,xlab="Unique Daily Measurements",xlim=c(1,nrow(bursts)*ifelse(legend,1.3,1)),
    main=paste("Average Duration Between Bursts\nOver Time"," (",stream,")",sep=""), ylab="Average Duration Between Bursts",yaxt="n",ylim=c(0,6))
  axis(2,at=log10(1+10^(0:6)),labels=c(1, expression(10^1),expression(10^2),expression(10^3),expression(10^4),expression(10^5),expression(10^6)))
  abline(h=log10(1+break_duration),lwd=2,col=dark_line_col)
  points(log10(1+bursts[,"avg_between_burst_duration"]-bursts[,"avg_within_burst_duration"]), col=person_dark_colors,pch=16)
  if(legend) legend("topright",legend=names, col=dark_colors, pch=16,ncol=1,bg="white")
  box()
  
  plot(bursts[,"zeroed"],log10(1+bursts[,"avg_between_burst_duration"]-bursts[,"avg_within_burst_duration"]),
       ylim=c(0,6),col=NA,xlim=x_range,yaxt = "n",xlab="Day",ylab="Average Duration Between Bursts",
       main=paste("Average Duration Between Bursts\nOver Time"," (",stream,")",sep=""))
  axis(2,at=log10(1+10^(0:6)),labels=c(1, expression(10^1),expression(10^2),expression(10^3),expression(10^4),expression(10^5),expression(10^6)))
  points(bursts[,"zeroed"],log10(1+bursts[,"avg_between_burst_duration"]-bursts[,"avg_within_burst_duration"]), pch=16,col=person_lite_colors)
  lines(lowess(bursts[,"zeroed"],log10(1+bursts[,"avg_between_burst_duration"]-bursts[,"avg_within_burst_duration"])),col="red",lwd=3.5,lty=lowess_lty)
  abline(h=log10(1+break_duration),lwd=2,lty=1,col=dark_line_col)
  if(legend) legend("topright",legend=names, col=dark_colors, pch=16,ncol=1,bg="white")
  box()


  plot(coverage[,"zeroed"], coverage[,"total_coverage_across_all_patients"],col=NA,ylim=c(0,1),
     main=paste("Overall Coverage Over Time"," (",stream,")",sep=""),xlab="Day",ylab="Coverage")
  points(coverage[,"zeroed"], coverage[,"total_coverage_across_all_patients"],pch=16,col="gray")
  points(coverage[,"zeroed"], coverage[,"total_coverage_across_active_patients"],pch=16,col="black")  
  if(legend) legend("bottomleft",ncol=2,pch=16,legend=c("Active Patients","All Patients"),col=c("black","gray"),bg="white")
  box()
  
  dev.off()
}






