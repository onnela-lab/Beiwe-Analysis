patient_estimates = list()

data_filepath = "C:/Phoenix/School/Harvard/Research/Beiwe/Studies/Justin_Bipolar/Data/2016.10.20"



read_objective_filepath = "C:/Users/Patrick/Desktop/R_Code/read_objective_fixed_time.py"
TT=24
minutes = 10

samples_per_hour = 1# not exact right now.  It only filters by number of samples!  For Justin's data, 12 is about 2.25 minute samples.  Go for 15 minutes.
FORWARD_SHIFT = 8

patient_names = list.files(data_filepath)[-grep("\\.",list.files(data_filepath))]

for(patient in patient_names){#list.files()
  print(patient)
  appended_acc_file = paste(data_filepath,"/",patient,"/","appended_sheared_file_acc_",minutes,".txt",sep="")
  appended_pow_file = paste(data_filepath,"/",patient,"/","appended_sheared_file_pow_",minutes,".txt",sep="")
  if(!file.exists(appended_acc_file)){
    print(paste("working on ", patient))
    system(paste("python", read_objective_filepath, data_filepath, patient, minutes, sep=" "))
  }

  
  setwd(paste(data_filepath,patient,sep="/"))
  
  minute_mat = function(minute){
    appended_acc_file = paste(data_filepath,"/",patient,"/","appended_sheared_file_acc_",minute,".txt",sep="")
    appended_pow_file = paste(data_filepath,"/",patient,"/","appended_sheared_file_pow_",minute,".txt",sep="")
    accmat = read.table(appended_acc_file,header=T, stringsAsFactors = FALSE)
    if(file.size(appended_pow_file)==0) powmat = data.frame(timestamp=accmat[1,"timestamp"], UTC_time=accmat[1,"UTC_time"], segment=accmat[1,"segment"], screen_on_events=0)
    if(file.size(appended_pow_file) >0) powmat = read.table(appended_pow_file, header=T, stringsAsFactors = FALSE) # these three if pow is not empty.
    
    totmat = merge(x = accmat, y = powmat, by = "segment", all = TRUE)
    totmat$screen_on_events[is.na(totmat$screen_on_events)]=0
    
    
    colnames(totmat)[2] <- "timestamp"
    totmat[,"timestamp"] = totmat[,"timestamp"] / 1000
    movement = function(vec) sqrt(sum(vec**2))
    totmat[,"Movement"] = apply(totmat[,c("x","y","z")],1,movement)
    
    # SHIFT_FORWARD
    forward_shift        <- as.POSIXlt(totmat[,"timestamp"],origin="1970-01-01")
    forward_shift$hour   <- forward_shift$hour + FORWARD_SHIFT
    totmat[,"timestamp"] <- as.numeric(forward_shift)
    totmat = totmat[,c("segment","timestamp","UTC_time.x","screen_on_events","Movement")]
    totmat = totmat[complete.cases(totmat),]
    # END SHIFT FORWARD
    
    totmat[,c("Days", "Hours","Mins","Secs")] = totmat[,2] %>%
      as.POSIXct(origin="1970-01-01") %>%
      as.character %>%
      strsplit(" |:") %>%
      unlist %>%
      matrix(4) %>% t
    
    totmat["Hour_In_Day"] = as.numeric(data.matrix(totmat[,c("Hours","Mins","Secs")]) %*% c(1,1/60,1/3600))
    totmat = totmat[,c("segment", "timestamp", "Days", "Hours", "Mins", "Secs", "Hour_In_Day", "Movement", "screen_on_events")]
    totmat["Scaled_Movement"] = as.numeric(log10(.001+totmat[,"Movement"]))
    totmat["Scaled_Power_State"] = as.numeric(log10(1+totmat[,"screen_on_events"]))
    totmat["Objective"] = as.numeric(data.matrix(totmat[,"Scaled_Movement"]))
    #totmat["Objective"] = as.numeric(totmat["Scaled_Movement"]*1/2 + totmat["Scaled_Power_State"]*1/2)    
    #totmat = totmat[which(totmat["Objective"]> -3.5),]
    return(totmat)
  }

  daily_mat = minute_mat(minutes)
  selection = sort(sample(nrow(daily_mat),min(1500,nrow(daily_mat))))
  total_mat = daily_mat[selection,]
  
  
  daily_mat[,"Objective"] = (daily_mat[,"Objective"]-mean(daily_mat[,"Objective"],na.rm=T))/sd(daily_mat[,"Objective"],na.rm=T)
  total_mat[,"Objective"] = (total_mat[,"Objective"]-mean(total_mat[,"Objective"],na.rm=T))/sd(total_mat[,"Objective"],na.rm=T)
  
  
  daynames = unlist(unique(total_mat["Days"]))
  daily5 = list()
  daily60 = list()
  for(dayname in daynames){
    daily5[[dayname]] = daily_mat %>%
      filter(Days==dayname) %>%
      dplyr::select(Hour_In_Day, Objective) %>%
      #filter(Objective> -2) %>%
      mutate("diff_Hour_In_Day" = c(Hour_In_Day[1],diff(Hour_In_Day)))
    daily60[[dayname]] = total_mat %>%
      filter(Days==dayname) %>%
      dplyr::select(Hour_In_Day, Objective) %>%
      #filter(Objective> -2) %>%
      mutate("diff_Hour_In_Day" = c(Hour_In_Day[1],diff(Hour_In_Day)))
  }  
  
  time_sort = order(total_mat[,"Hour_In_Day"])
  plot(total_mat[time_sort,"Hour_In_Day"],total_mat[time_sort,"Objective"],col=rgb(0,0,0,.2))
  lines(lowess(total_mat[time_sort,"Hour_In_Day"],total_mat[time_sort,"Objective"],f=.125))
  #plot(total_mat[,"Hour_In_Day"], total_mat[,"Objective"],col=rgb(0,0,0,.2)) #!!!!! total_mat was designed to be more sparse and stable, and faster.  This is probably unnecessary --- the slow part is inverting, I believe.
  #lines(lowess(total_mat[,"Hour_In_Day"], total_mat[,"Objective"]))
  
  if(F){
  psi_guess = c(8.5,4,16,4,-1,.5,1,1) # perhaps generic
  daynames = names(daily5)
  start = Sys.time()
  output = Newton_Raphson_list(daily60, psi_guess, min_steps=50,max_steps=200,minimum_change_tolerated = .025, max_delta = .3, temperature = 0.075, record=TRUE)
  max_liks = list()
  for(dayname in daynames){
    print(dayname)
    max_liks[[dayname]] = max_lik_Psi_t_z(unlist(output$psi_hat), unlist(daily5[[dayname]]["Objective"]), unlist(daily5[[dayname]]["Hour_In_Day"]),mesh=0.25)
  };stopt = Sys.time()
  stopt-start
  
  psi = output$psi_hat
  pdf("Newton_Raphson.pdf",width=7,height=5.5)
  par(mgp=c(2.5,1,0))
  plot(1:8+.025, psi,col=NA,pch=15,xlim=c(.3,8),ylim=c(-2, 18),xlab=expression(Psi),ylab="Estimates",xaxt="n",main="Convergence of Psi",cex.lab=1.5,cex.main=1.5)
  axis(1,at=1:8,labels=Psi_labels)
  for(i in 1:8)
    lines(seq(i-.8,i-.1,length.out=output$num_steps),output$psi_hats[i,],col=rgb(.3,.3,.3),lwd=2)
  points(1:8-.1, output$psi_hat, col=cool_blue, pch=16)
  legend("topright",col=c(cool_blue),pch=c(16),legend=c("Estimate"),cex=1.2)
  dev.off()
  
  
  line = seq(-3,3,length.out=300)
  pdf("Total.pdf",width=7.5,height=6)
  par(cex.lab=1.5,cex.main=1.5,mgp=c(2.5,1,0))
  psi_hat = output$psi_hat
  ylims = range(total_mat[,"Objective"])
  plot(total_mat[time_sort,"Hour_In_Day"],total_mat[time_sort,"Objective"],xlab="Hour",ylab="Objective",cex=.75,col=rgb(.0,.0,.0,.15),pch=16)
  for(i in c(0,2)){
    curve = dnorm(line*psi_hat[2+i]+psi_hat[1+i],psi_hat[1+i],psi_hat[2+i])
    polygon(line*psi_hat[2+i]+psi_hat[1+i],curve*(ylims[2]-ylims[1])+ylims[1],col=cool_blue_alpha,border=NA)
    lines(rep(psi_hat[1+i],2),ylims[1]+c(0,.3*(ylims[2]-ylims[1])),col=cool_blue,lwd=2)  
  }
  for(i in c(0,2)){
    lines(TT+0:1/2+.25+i*.1,rep(psi_hat[5+i],2),col=cool_blue,lwd=2)  
    lines(TT+0:1/2+.25+i*.1,rep(psi_hat[5+i]+qnorm(.975)*psi_hat[6+i],2),col=cool_blue,lwd=1)
    lines(TT+0:1/2+.25+i*.1,rep(psi_hat[5+i]-qnorm(.975)*psi_hat[6+i],2),col=cool_blue,lwd=1)
    lines(rep(TT+.5+i*.1,2),psi_hat[5+i]-c(0,qnorm(.975)*psi_hat[6+i]),col=cool_blue,lwd=1)
    lines(rep(TT+.5+i*.1,2),psi_hat[5+i]+c(0,qnorm(.975)*psi_hat[6+i]),col=cool_blue,lwd=1)
  }
  dev.off()
  
  pdf("Daily.pdf",width=8,height=6)
  par(mfrow=c(3,4),mgp=c(2,1,0),mar=c(3,2,2,1))
  for(dayname in daynames){
    if(!is.null(daily5[[dayname]])){
      if(nrow(daily5[[dayname]])>0){
        plot(daily5[[dayname]][,"Hour_In_Day"],daily5[[dayname]][,"Objective"],xlim=c(0,24),ylim=range(daily5[[dayname]][,"Objective"]),#,
             xlab="Hour",ylab="Objective",main=dayname,cex=.75,col=rgb(.3,.3,.3),pch=16)
        abline(v=max_liks[[dayname]][2:3],col=cool_blue,lty=2,lwd=1.5)
      }
    }
  }
  dev.off()

  patient_estimates[[patient]][["psi_hat"]] = output$psi_hat
  patient_estimates[[patient]][["SD"]] = output$SD
  patient_estimates[[patient]][["Daily"]] = max_liks
  }
  pdf("slat_plot.pdf",width=8,height=6)
  par(cex=1.2,mgp=c(2.5,1,0))
  plot(0,0,col=NA,xlim=c(0+.7,24-.7),ylim=c(0,1),bty="n",
       yaxt="n",ylab="Day",xaxt="n",xlab="Time of day",main="All Daily Estimates")
  axis(1,at=0:6*4,labels=0:6*4)
  for(unique_day in daynames){
    daylist = daily5[[unique_day]]
    x=unlist(daylist["Hour_In_Day"])
    yloc=(as.numeric(as.POSIXlt(unique_day,origin="1970-01-01"))-min(as.numeric(as.POSIXlt(daynames,origin="1970-01-01"))))/(max(as.numeric(as.POSIXlt(daynames,origin="1970-01-01")))-min(as.numeric(as.POSIXlt(daynames,origin="1970-01-01"))))
    obj=unlist(daylist["Objective"])
    sizes=1-(obj-min(obj))/(max(obj)-min(obj))
    lines(c(0+.1,24-.1),rep(yloc,2),pch=15,col=rgb(.95,.91,.8),lwd=3.2)
    if(sum(is.na(sizes))==0)
      points(x,rep(yloc,nrow(daylist)),pch=15,cex=.4,col=rgb(sizes,sizes,sizes))
  }
  for(unique_day in daynames){
    daylist = daily5[[unique_day]]
    yloc=(as.numeric(as.POSIXlt(unique_day,origin="1970-01-01"))-min(as.numeric(as.POSIXlt(daynames,origin="1970-01-01"))))/(max(as.numeric(as.POSIXlt(daynames,origin="1970-01-01")))-min(as.numeric(as.POSIXlt(daynames,origin="1970-01-01"))))
    points(patient_estimates[[patient]][["Daily"]][[unique_day]][2:3],rep(yloc,2),col=rgb(.1,.4,.9),pch=18,cex=.75)
  }
  dev.off()
  
}   

saveRDS(patient_estimates,file=paste(data_filepath,"patient_estimates4.rds",sep="/"))
#pat = readRDS(paste(data_filepath,"patient_estimates.rds",sep="/"))







