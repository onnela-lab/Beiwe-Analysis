data_quality_plotting = function(stream, frequency, burst_duration, break_duration){
  
  plot_filename = paste(output_filepath, "/Results/Group/data_quality_",stream,".pdf",sep="")
  
  bursts   = readRDS(paste(output_filepath, "/Preprocessed_Data/Group/", stream, "_bursts.rds", sep="")) %>% data.frame
  coverage = readRDS(paste(output_filepath, "/Preprocessed_Data/Group/", stream, "_coverage.rds", sep="")) %>% data.frame
  
  par(mgp=c(2.5,1,0),cex.main=1.2,cex.lab=1.2)
  
  patients = bursts %>% data.frame %>% dplyr::select(patient) %>% unique %>% unlist %>% as.character %>% sort
  colors = cols(length(patients),transparency=1-light_alpha)
  person_colors = colors[bursts %>% data.frame %>% dplyr::select(patient) %>% unlist %>% as.numeric]
  names = bursts %>% data.frame %>% dplyr::select(patient) %>% unique %>% unlist %>% as.character
  
  seconds_per_day = 60*60*24
  x_range = c(min(unlist(bursts["zeroed"])), round(max(unlist(bursts["zeroed"]))*1.3))
  
  pdf(plot_filename,width=8,height=6)
  
  plot(bursts[,"zeroed"],log10(1+bursts[,"num_bursts"]),col=NA,yaxt = "n",ylim=c(0,4),xlim=x_range,
       xlab="Day",ylab="Number of Bursts",main=paste("Number of Bursts Per Day"," (",stream,")",sep=""))
  axis(2,at=log10(1+10^(0:4)),labels=c(1, expression(10^1),expression(10^2),expression(10^3),expression(10^4)))
  points(unlist(bursts[,"zeroed"]),log10(1+unlist(bursts[,"num_bursts"])),pch=16,col=person_colors)
  abline(h=log10(1+seconds_per_day/(burst_duration+break_duration)),lwd=2,lty=1,col=line_col)
  legend("topright",legend=names, col=colors, pch=16,ncol=1,bg="white")
  
  
  plot(bursts[,"zeroed"],log10(.1+bursts[,"avg_pings"]/burst_duration),col=NA,ylim=c(-1,2),xlim=x_range,
       yaxt = "n",xlab="Day",ylab="Average Pings per Second",main=paste("Average Pings per Second Within burst\nOver Time"," (",stream,")",sep=""))
  axis(2,at=log10(.1+c(0,10^(0:4))),labels=c(0, 1, expression(10^1),expression(10^2),expression(10^3),expression(10^4)))
  points(unlist(bursts[,"zeroed"]),log10(.1+bursts[,"avg_pings"]/burst_duration),pch=16,col=person_colors)
  abline(h=log10(.1+frequency),lwd=2,lty=1,col=line_col)
  legend("topright",legend=names, col=colors, pch=16,ncol=1,bg="white")
  
  plot(bursts[,"zeroed"],log10(1+bursts[,"avg_within_burst_duration"]),col=NA,ylim=c(0,4),xlim=x_range,
       yaxt = "n",xlab="Day",ylab="Average Pings per burst",main=paste("Average Duration per burst\nOver Time"," (",stream,")",sep=""))
  axis(2,at=log10(1+10^(0:4)),labels=c(1, expression(10^1),expression(10^2),expression(10^3),expression(10^4)))
  points(bursts[,"zeroed"],log10(1+bursts[,"avg_within_burst_duration"]),pch=16,col=person_colors)
  abline(h=log10(burst_duration),lwd=2,lty=1,col=line_col)
  legend("topright",legend=names, col=colors, pch=16,ncol=1,bg="white")
  
  plot(bursts[,"zeroed"],log10(bursts[,"avg_between_burst_duration"]-bursts[,"avg_within_burst_duration"]),
       ylim=c(0,6),col=NA,xlim=x_range,yaxt = "n",xlab="Day",ylab="Average Duration Between bursts",
       main=paste("Average Duration Between bursts\nOver Time"," (",stream,")",sep=""))
  axis(2,at=log10(1+10^(0:6)),labels=c(1, expression(10^1),expression(10^2),expression(10^3),expression(10^4),expression(10^5),expression(10^6)))
  points(unlist(bursts["zeroed"]),log10(unlist(bursts["avg_between_burst_duration"])-unlist(bursts["avg_within_burst_duration"])),
         pch=16,col=person_colors)
  abline(h=log10(1+break_duration),lwd=2,lty=1,col=line_col)
  legend("topright",legend=names, col=colors, pch=16,ncol=1,bg="white")

  plot(coverage[,"zeroed"], coverage[,"total_coverage_across_all_patients"],col=NA,ylim=c(0,1.15),
       main=paste("Coverage Over Time"," (",stream,")",sep=""),xlab="Day",ylab="Coverage")
  abline(h=1,lty=2,lwd=.75,col=rgb(.5,.5,.5))
  #points(coverage[,"zeroed"], coverage[,"total_coverage_across_all_patients"],pch=16,col="gray")
  points(coverage[,"zeroed"], coverage[,"num_bursts_coverage"],pch=16,col=cols(3,transparency = .8)[1])
  points(coverage[,"zeroed"], coverage[,"within_burst_length_coverage"],pch=16,col=cols(3,transparency = .8)[2])
  points(coverage[,"zeroed"], coverage[,"within_burst_frequency_coverage"],pch=16,col=cols(3,transparency = .8)[3])
  points(coverage[,"zeroed"], coverage[,"total_coverage_across_active_patients"],pch=16,col=rgb(.7,.7,.7))
  
  lines(lowess(coverage[,"zeroed"], coverage[,"num_bursts_coverage"],f=.25),lwd=3,col=cols(3)[1])
  lines(lowess(coverage[,"zeroed"], coverage[,"within_burst_length_coverage"],f=.25),lwd=3,col=cols(3)[2])
  lines(lowess(coverage[,"zeroed"], coverage[,"within_burst_frequency_coverage"],f=.25),lwd=3,col=cols(3)[3])
  lines(lowess(coverage[,"zeroed"], coverage[,"total_coverage_across_active_patients"],f=.25),lwd=3,col="black")
  
  legend("bottomleft",ncol=2,lwd=c(3,3,3,3),legend=c("Total Coverage","Burst Count", "Burst Length", "Frequency in Burst"),col=c("black",cols(3)),bg="white")
  box()
  dev.off()
  dev.off()
}