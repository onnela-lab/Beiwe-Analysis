plot_accelerometer = function(minutes, forward_shift = 8, fixed_days = NA, maximum_width = 1, use_patient_name = TRUE, ...){
  pdf(paste(output_filepath, "/Results/Group/slat_plot",".pdf",sep=""),width=9,height=5.3)
  par(cex.main=1.3, cex.lab=1.3)
  for(patient_name in patient_names){
    accelerometer_filename = paste(output_filepath,"/Preprocessed_Data/Individual/",patient_name,"/appended_sheared_file_acc_",minutes,".rds",sep="")
    if(file.exists(accelerometer_filename)){
      accmat = readRDS(accelerometer_filename)
      print(patient_name)
      accmat[,"obj"] = apply(accmat[,c("x","y","z")],1,function(x){log(.01+sqrt(mean(c(x[1]^2,x[2]^2,x[3]^2))))})
      accmat[,"obj"] = (accmat[,"obj"]-min(accmat[,"obj"],na.rm=T))/(max(accmat[,"obj"],na.rm=T)-min(accmat[,"obj"],na.rm=T))
    
      new_times        <- as.POSIXlt(accmat[,"timestamp"],origin="1970-01-01")
      new_times$hour   <- new_times$hour + forward_shift
      accmat[,"timestamp"] <- as.numeric(new_times)
      accmat[,c("hours","days")] = hours(accmat[,"timestamp"])
      
      
      days = accmat[,"days"] %>% unique %>% unlist
    
      D = ifelse(is.na(fixed_days), length(days), fixed_days)
	  patient_title = ifelse(use_patient_name, patient_name, which(patient_name == patient_names)[1]) 
      plot(0,0,col=NA,xlim=c(0+.87,24-.87), ylim=c(0.02,.98),bty="n",
         yaxt="n",ylab="Day",xaxt="n",xlab="Time of day",main=paste("Daily Accelerometer Data\nSubject ",patient_title,sep=""))
      axis(1,at=0:6*4,labels=0:6*4)
      axis(2,at=seq(0,1,length.out=5), label=round(seq(0,D,length.out=5)))
    
      for(day_ID in 1:D){
        day = days[day_ID]
        x=accmat %>% dplyr::filter(days==day) %>% dplyr::select(obj, hours) %>% data.frame
        #points(x[,"hours"], rep(day_ID/D,nrow(x)), pch=15, col=palette[round(100*x[,"obj"])])
        if(nrow(x)>1){
          for(row in 1:(nrow(x)-1)){
            x_min = x[row, "hours"]
            x_max = min(x[row+1, "hours"], x[row, "hours"]+maximum_width) # forces the plotting to only extend the curent estimate for a maximum of one hour.
            polygon(c(x_min, x_min, x_max, x_max), (day_ID-1+c(0,1,1,0))/D, col=palette[round(100*x[row,"obj"])],border=NA)
          }
        }
      }  
      box()
    } 
  }
  dev.off()
}   










anonymize = function(x){unlist(anonymized_names[x])}









