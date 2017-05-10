# example
# homedir="C:/Users/Ian/Documents/Work/JP/Beiwe/Testing"
# ID="fdhgl5aj"
# ContinuousDataCollectionTracks(homedir,ID,tz="")
# ID="vw8whm3e"
# ContinuousDataCollectionTracks(homedir,ID,tz="")



# tol: length of time (minutes) between data points where we declare it to be a new interval (tolerance)
AccelerometerDataCollection = function(ACCvec,tol){
  outmat = matrix(NA,nrow=100,ncol=2)
  outmat[1,1]=ACCvec[1]
  counter=1
  for(i in 1:(length(ACCvec)-1)){
    if(ACCvec[i+1]-ACCvec[i]>tol*60*.5+tol*60*1){
      outmat[counter,2]=ACCvec[i]+tol*60
      if(nrow(outmat)<counter+1){
        outmat=rbind(outmat,matrix(NA,nrow=nrow(outmat),ncol=2))
      }
      outmat[counter+1,1]=ACCvec[i+1]
      counter=counter+1
    }
  }
  outmat=outmat[1:counter,]
  if(counter<2){
    return(NULL)
  }
  if(is.na(outmat[nrow(outmat),2])){
    outmat[nrow(outmat),2]=ACCvec[length(ACCvec)]+tol*60
  }
  return(outmat*1000) 
}



# # tol: length of time (seconds) between data points where we declare it to be a new interval (tolerance)
# AccelerometerDataCollectionOld = function(homedir,ID,tol=5){
#   cat("Scanning accelerometer data...\n")
#   accdir = paste(homedir,"Data",ID,"accelerometer",sep="/")
#   if(!file.exists(accdir)){return(NULL)}
#   filenames=list.files(path=accdir,full.names=T,pattern="\\.csv$")
#   outmat_ls = list()
#   for(i in 1:length(filenames)){
#     dat=read.csv(filenames[i])
#     outmat_ls[[i]]=dat[,1]
#   }
#   outmat = unlist(outmat_ls)
#   outmat2 = matrix(NA,nrow=100,ncol=2)
#   outmat2[1,1]=outmat[1]
#   counter=1
#   for(i in 1:(length(outmat)-1)){
#     if(outmat[i+1]-outmat[i]>tol*1000){
#       outmat2[counter,2]=outmat[i]
#       if(nrow(outmat2)<counter+1){
#         outmat2=rbind(outmat2,matrix(NA,nrow=nrow(outmat2),ncol=2))
#       }
#       outmat2[counter+1,1]=outmat[i+1]
#       counter=counter+1
#     }
#   }
#   outmat2=outmat2[1:counter,]
#   outmat2[nrow(outmat2),2]=dat[nrow(dat),1]
#   # plot(NA,xlim=c(outmat2[1,1],outmat2[nrow(outmat2),2]),ylim=c(0,2))
#   # for(i in 1:nrow(outmat2)){
#   #   lines(outmat2[i,],c(1,1))
#   # }
#   return(outmat2) 
# }



GPSDataCollection = function(GPSmat,tol=5){
  ID34=which(GPSmat[,1]>=3)
  if(length(ID34)>0){
    GPSmat = GPSmat[-ID34,]
  }
  outmat2 = matrix(NA,nrow=100,ncol=2)
  outmat2[1,1]=GPSmat[1,4]
  counter=1
  for(i in 1:(nrow(GPSmat)-1)){
    if(GPSmat[i+1,4]-GPSmat[i,7]>60*tol){
      outmat2[counter,2]=GPSmat[i,7]
      if(nrow(outmat2)<counter+1){
        outmat2=rbind(outmat2,matrix(NA,nrow=nrow(outmat2),ncol=2))
      }
      outmat2[counter+1,1]=GPSmat[i+1,4]
      counter=counter+1
    }
  }
  if(counter<2){
    return(NULL)
  }
  outmat2=outmat2[1:(counter-1),]
  if(is.na(outmat2[nrow(outmat2),2])){
    outmat2[nrow(outmat2),2]=GPSmat[nrow(GPSmat),7]
  }
  # plot(NA,xlim=c(outmat2[1,1],outmat2[nrow(outmat2),2]),ylim=c(0,2))
  # for(i in 1:nrow(outmat2)){
  #   lines(outmat2[i,],c(1,1))
  # }
  return(outmat2*1000)
}




# # tol: length of time (seconds) between data points where we declare it to be a new interval (tolerance)
# GPSDataCollectionOld = function(homedir,ID,tol=120){
#   cat("Scanning GPS data...\n")
#   gpsdir = paste(homedir,"Data",ID,"gps",sep="/")
#   if(!file.exists(gpsdir)){return(NULL)}
#   filenames=list.files(path=gpsdir,full.names=T,pattern="\\.csv$")
#   counter=0
#   for(i in 1:length(filenames)){
#     dat=read.csv(filenames[i])
#     if(i==1){
#       outmat=matrix(NA,nrow=nrow(dat),ncol=1)
#     }
#     if(counter + nrow(dat)>nrow(outmat)){
#       outmat = rbind(outmat,matrix(NA,nrow=max(nrow(dat),nrow(outmat)),ncol=1))
#     }
#     outmat[(counter+1):(counter+nrow(dat)),1]=dat[,1]
#     #outmat[(counter+1):(counter+nrow(dat)),2]=apply(dat,1,function(xx) sqrt(as.numeric(xx[4])^2+as.numeric(xx[5])^2+as.numeric(xx[6])^2))
#     counter=counter+nrow(dat)
#   }
#   outmat=as.matrix(outmat[1:counter,],nrow=counter,ncol=1)
#   outmat2 = matrix(NA,nrow=100,ncol=2)
#   outmat2[1,1]=outmat[1,1]
#   counter=1
#   for(i in 1:(nrow(outmat)-1)){
#     if(outmat[i+1,1]-outmat[i,1]>tol*1000){
#       outmat2[counter,2]=outmat[i,1]
#       if(nrow(outmat2)<counter+1){
#         outmat2=rbind(outmat2,matrix(NA,nrow=nrow(outmat2),ncol=2))
#       }
#       outmat2[counter+1,1]=outmat[i+1,1]
#       counter=counter+1
#     }
#   }
#   if(counter<2){
#     return(NULL)
#   }
#   outmat2=outmat2[1:(counter-1),]
#   outmat2[nrow(outmat2),2]=dat[nrow(dat),1]
#   # plot(NA,xlim=c(outmat2[1,1],outmat2[nrow(outmat2),2]),ylim=c(0,2))
#   # for(i in 1:nrow(outmat2)){
#   #   lines(outmat2[i,],c(1,1))
#   # }
#   return(outmat2)
# }

SVYDataCollection = function(homedir,ID){
  cat("Scanning survey data...\n")
  svydir = paste(homedir,ID,"survey_answers",sep="/")
  if(!file.exists(svydir)){return(NULL)}
  svynames=list.files(path=svydir,full.names=T)
  counter=0
  outvec = rep(NA,100)
  for(ii in 1:length(svynames)){
    filenames = list.files(path=svynames[ii],full.names=F,pattern="\\.csv$")
    filenames2= unlist(strsplit(filenames,".csv"))
    filenames3= gsub("_",":",filenames2)
    if(length(filenames3)+counter>length(outvec)){
      outvec = c(outvec,rep(NA,length(filenames3)))
    }
    outvec[(counter+1):(counter+length(filenames3))]=1000*as.numeric(as.POSIXct(filenames3,origin="1970-01-01",tz="GMT"))
    counter=counter+length(filenames3)
  }    
  outvec = outvec[1:counter]
  return(outvec)
}


# minval: minimum duration of truncation (in seconds) if no other data is present.
# This can be interpreted as an approximation for the amount of time it takes for a user to
# turn off their phone after unlocking it.
# bffr: Other data streams can begin collection before the screen is unlocked after a phone is
# turned on. As a result, truncating screen on/off won't work when phones are turned off for
# extensive periods of time unless these preliminary data collections from other streams is ignored
# bffr is the fraction of the time period from the start that truncation is performed over.
FindSCNOnOffTruncatedEndValue=function(t1,t2,ACCmat,GPSmat,bffr=.9,minval=1){
  ACCids=which(ACCmat[,2]<t2-bffr*(t2-t1))
  if(length(ACCids)>0){
    tACC=ACCmat[max(ACCids),2]
  }else{
    tACC=NULL
  }
  GPSids=which(GPSmat[,2]<t2-bffr*(t2-t1))
  if(length(GPSids)>0){
    tGPS=GPSmat[max(GPSids),2]
  }else{
    tGPS=NULL
  }
  if(is.null(tGPS) && is.null(tACC)){
    return(t2)
  }
  tTrunc = max(tACC,tGPS)
  if(tTrunc<t1){
    return(t1+minval*1000)
  }else{
    return(tTrunc)
  }
}


FindDZEOnOffTruncatedEndValue=function(t1,t2,ACCmat,GPSmat,SCNmat,minval=10){
  ACCids=which(ACCmat[,2]>t1+minval*1000)
  if(length(ACCids)>0){
    tACC=ACCmat[min(ACCids),2]
  }else{
    tACC=NULL
  }
  GPSids=which(GPSmat[,2]>t1+minval*1000)
  if(length(GPSids)>0){
    tGPS=GPSmat[min(GPSids),2]
  }else{
    tGPS=NULL
  }
  SCNids=which(SCNmat[,2]>t1+minval*1000)
  if(length(SCNids)>0){
    tSCN=SCNmat[min(SCNids),2]
  }else{
    tSCN=NULL
  }
  if(is.null(tGPS) && is.null(tACC) && is.null(tSCN)){
    return(t2)
  }
  tTrunc = min(tACC,tGPS,tSCN)
  return(tTrunc)
}


GetXTcks = function(itrvl,tz=""){
  ndays=(itrvl[2]-itrvl[1])/(1000*60*60*24)
  tcks=rep(NA,ndays)
  labels=rep(NA,ndays)
  for(i in 1:ndays){
    labels[i]=strsplit(as.character(as.POSIXct((itrvl[1]+i*(1000*60*60*24))/1000,origin="1970-01-01",tz=tz))," ")[[1]][1]
    tcks[i]=as.numeric(as.POSIXct(labels[i],origin="1970-01-01",tz=tz))*1000
  }
  return(list("labels"=labels,"tcks"=tcks))
}

PlotContinuousDataCollectionTracks=function(homedir,ID,ACCmat,GPSmat,SCNmat,DZEmat,SVYvec,tz=""){
  if(is.null(ACCmat) && is.null(GPSmat) && is.null(SCNmat)){
    return(NULL)
  }
  tmin=min(min(ACCmat),min(GPSmat),min(SCNmat))
  tmax=max(max(ACCmat),max(GPSmat),max(SCNmat))
  nweeks=ceiling((tmax-tmin)/(1000*60*60*24*7))
  # if(!file.exists(paste(homedir,"Output",sep="/"))){
  #   dir.create(paste(homedir,"Output",sep="/")) 
  # }
  # if(!file.exists(paste(homedir,"Output",ID,sep="/"))){
  #   dir.create(paste(homedir,"Output",ID,sep="/")) 
  # }
  ylabels=c()
  if(!is.null(ACCmat)){
    ylabels=c(ylabels,"Accelerometer")
  }
  if(!is.null(GPSmat)){
    ylabels=c(ylabels,"GPS")
  }
  if(!is.null(SCNmat)){
    ylabels=c(ylabels,"Screen on/off")
  }
  if(!is.null(DZEmat)){
    ylabels=c(ylabels,"Doze")
  }
  if(!is.null(SVYvec)){
    ylabels=c(ylabels,"Surveys")
  }
  pdf(paste(homedir,"Results","Individual",ID,"DataCollectionTracks.pdf",sep="/"),width=6,height=4)
  for(ii in 1:nweeks){
    itrvl=tmin+c(ii-1,ii)*1000*60*60*24*7
    # Get accelerometer submat for this week
    ACCID1=which(ACCmat[,1]>itrvl[1])
    NACCID1=which(ACCmat[,1]<=itrvl[1])
    ACCID2=which(ACCmat[,2]<itrvl[2])
    NACCID2=which(ACCmat[,2]>=itrvl[2])
    if(length(ACCID1)>0 && length(ACCID2)>0){
      ACCID1=min(ACCID1)
      ACCID2=max(ACCID2)
    }else{
      ACCID2=0
      ACCID1=0
    }
    if(ACCID2>ACCID1){
      subACCmat=ACCmat[ACCID1:ACCID2,]
      if(length(NACCID1)>0 && ACCmat[NACCID1[length(NACCID1)],2]>itrvl[1]){
        subACCmat = rbind(c(itrvl[1],ACCmat[NACCID1[length(NACCID1)],2]),subACCmat)
      }
      if(length(NACCID2)>0 && ACCmat[NACCID2[1],1]<itrvl[2]){
        subACCmat = rbind(subACCmat,c(ACCmat[NACCID2[1],1],itrvl[2]))
      }
    }else{
      subACCmat=NULL
    }
    if(length(NACCID1)>0 && length(NACCID2)>0 && NACCID1[length(NACCID1)]==NACCID2[1]){
      subACCmat=matrix(c(itrvl[1],itrvl[2]),nrow=1,ncol=2)
    }
    # Get gps submat for this week
    GPSID1=which(GPSmat[,1]>itrvl[1])
    GPSID2=which(GPSmat[,2]<itrvl[2])
    if(length(GPSID1)>0 && length(GPSID2)>0){
      GPSID1=min(GPSID1)
      GPSID2=max(GPSID2)
    }else{
      GPSID2=0
      GPSID1=0
    }
    if(GPSID2>GPSID1){
      subGPSmat=GPSmat[GPSID1:GPSID2,]
    }else{
      subGPSmat=NULL
    }
    # Get screen on/off submat for this week
    SCNID1=which(SCNmat[,1]>itrvl[1])
    SCNID2=which(SCNmat[,2]<itrvl[2])
    if(length(SCNID1)>0 && length(SCNID2)>0){
      SCNID1=min(SCNID1)
      SCNID2=max(SCNID2)
    }else{
      SCNID2=0
      SCNID1=0
    }
    if(SCNID2>SCNID1){
      subSCNmat=SCNmat[SCNID1:SCNID2,]
    }else{
      subSCNmat=NULL
    }
    # Get doze submat for this week
    DZEID1=which(DZEmat[,1]>itrvl[1])
    DZEID2=which(DZEmat[,2]<itrvl[2])
    if(length(DZEID1)>0 && length(DZEID2)>0){
      DZEID1=min(DZEID1)
      DZEID2=max(DZEID2)
    }else{
      DZEID2=0
      DZEID1=0
    }
    if(DZEID2>DZEID1){
      subDZEmat=DZEmat[DZEID1:DZEID2,]
    }else{
      subDZEmat=NULL
    }
    # Get surveys submat for this week
    SVYID1=which(SVYvec>itrvl[1])
    SVYID2=which(SVYvec<itrvl[2])
    if(length(SVYID1)>0 && length(SVYID2)>0){
      SVYID1=min(SVYID1)
      SVYID2=max(SVYID2)
    }else{
      SVYID2=0
      SVYID1=0
    }
    if(SVYID2>SVYID1){
      subSVYvec=SVYvec[SVYID1:SVYID2]
    }else{
      subSVYvec=NULL
    }
    # plot data for this week
    par(mai=c(1.2,1.3,.2,.2),mgp=c(0,.8,0))
    plot(NA,xlim=itrvl,ylim=c(0,length(ylabels)),xaxt="n",yaxt="n",xlab="",ylab="")
    xtcks=GetXTcks(itrvl,tz)
    axis(1,at=xtcks$tcks,labels=xtcks$labels,las=2)
    axis(2,at=seq(.5,length(ylabels)-.5,1),labels=ylabels,las=2)
    yc=0
    if(!is.null(ACCmat)){
      if(!is.null(subACCmat)){
        for(i in 1:nrow(subACCmat)){
          polygon(c(subACCmat[i,],rev(subACCmat[i,])),c(yc,yc,yc+1,yc+1),col=stream_colors[6],border=NA)
        }
      }
      yc=yc+1
    }
    if(!is.null(GPSmat)){
      if(!is.null(subGPSmat)){
        for(i in 1:nrow(subGPSmat)){
          polygon(c(subGPSmat[i,],rev(subGPSmat[i,])),c(yc,yc,yc+1,yc+1),col=stream_colors[5],border=NA)
        }
      }
      yc=yc+1
    }    
    if(!is.null(SCNmat)){
      if(!is.null(subSCNmat)){
        for(i in 1:nrow(subSCNmat)){
          polygon(c(subSCNmat[i,],rev(subSCNmat[i,])),c(yc,yc,yc+1,yc+1),col=stream_colors[4],border=NA)
        }
      }
      yc=yc+1
    }
    if(!is.null(DZEmat)){
      if(!is.null(subDZEmat)){
        for(i in 1:nrow(subDZEmat)){
          polygon(c(subDZEmat[i,],rev(subDZEmat[i,])),c(yc,yc,yc+1,yc+1),col=stream_colors[7],border=NA)
        }
      }
      yc=yc+1
    }
    if(!is.null(SVYvec)){
      if(!is.null(subSVYvec)){
        for(i in 1:length(subSVYvec)){
          polygon(subSVYvec[i]+(itrvl[2]-itrvl[1])*.001*c(-1,1,1,-1),c(yc,yc,yc+1,yc+1),col=stream_colors[1],border=NA)
		  #lines(rep(subSVYvec[i],2),yc+0:1,col=stream_colors[5],lwd=2)
        }
      }
      yc=yc+1
    }
  }
  dev.off()
}


DZEDataCollection = function(homedir,ID,ACCmat,GPSmat,SCNmat){
  cat("Scanning for doze...\n")
  scndir = paste(homedir,ID,"power_state",sep="/")
  if(!file.exists(scndir)){return(NULL)}
  filenames=list.files(path=scndir,full.names=T,pattern="\\.csv$")
  outmat=matrix(NA,nrow=100,ncol=2)
  counter=0
  for(i in 1:length(filenames)){
    dat=read.csv(filenames[i],stringsAsFactors=F)
    for(j in 1:nrow(dat)){
      if(length(grep("device in idle state",dat[j,3]))==1){
        counter=counter+1
        if(counter>nrow(outmat)){
          outmat=rbind(outmat,matrix(NA,nrow=nrow(outmat),ncol=2))
        }
        outmat[counter,1]=dat[j,1]
        if(counter>1 && is.na(outmat[counter-1,2])){
          outmat[counter-1,2]=dat[j,1]
        }
      }
      if(length(grep("device not in idle state",dat[j,3]))==1){
        outmat[counter,2]=dat[j,1]
      }
    }
  }
  if(counter<2){
    return(NULL)
  }
  outmat=outmat[1:(counter-1),]
  # check to see if other data streams are being collected
  for(i in 1:nrow(outmat)){
    outmat[i,2]=FindDZEOnOffTruncatedEndValue(outmat[i,1],outmat[i,2],ACCmat,GPSmat,SCNmat)
  }
  return(outmat)
}


# tol: minimum length of time (seconds) an interval can be where we then
# truncate the end of the interval if no other data streams are being collected.
SCNDataCollection = function(homedir,ID,ACCmat,GPSmat,tol=10*60){
  cat("Scanning power state data...\n")
  scndir = paste(homedir,ID,"power_state",sep="/")
  if(!file.exists(scndir)){return(NULL)}
  filenames=list.files(path=scndir,full.names=T,pattern="\\.csv$")
  outmat=matrix(NA,nrow=100,ncol=2)
  counter=0
  SCNON=FALSE
  for(i in 1:length(filenames)){
    dat=read.csv(filenames[i])
    for(j in 1:nrow(dat)){
      if(dat[j,3]=="Unlocked" || dat[j,3]=="Screen turned on"){
        SCNON=TRUE
        counter=counter+1
        if(counter>nrow(outmat)){
          outmat=rbind(outmat,matrix(NA,nrow=nrow(outmat),ncol=2))
        }
        outmat[counter,1]=dat[j,1]
        if(counter>1 && is.na(outmat[counter-1,2])){
          outmat[counter-1,2]=dat[j,1]
        }
      }
      if(SCNON && (dat[j,3]=="Locked" || dat[j,3]=="Screen turned off")){
        SCNON=FALSE
        outmat[counter,2]=dat[j,1]
      }
    }
  }
  if(counter<2){
    return(NULL)
  }
  outmat=outmat[1:(counter-1),]
  # check to see if the phone is off (truncate false positives)
  for(i in 1:nrow(outmat)){
    if(outmat[i,2]-outmat[i,1]>tol*1000){
      outmat[i,2]=FindSCNOnOffTruncatedEndValue(outmat[i,1],outmat[i,2],ACCmat,GPSmat)
    }
  }
  return(outmat)
}



ContinuousDataCollectionTracks = function(homedir,ID,tz=""){
  ACCmat = AccelerometerDataCollection(homedir,ID)
  GPSmat = GPSDataCollection(homedir,ID)
  SCNmat = SCNDataCollection(homedir,ID,ACCmat,GPSmat)
  SVYvec = SVYDataCollection(homedir,ID)
  DZEmat = DZEDataCollection(homedir,ID,ACCmat,GPSmat,SCNmat)
  PlotContinuousDataCollectionTracks(homedir,ID,ACCmat,GPSmat,SCNmat,DZEmat,SVYvec,tz)
}

ContinuousDataCollectionTracks = function(patient_name,ACCbinsize,tz="",...){
  # Accelerometer
  patient_data_filepath = paste(output_filepath, "/Preprocessed_Data/Individual/", patient_name, sep="")
  patient_data_filename_ACC = paste(patient_data_filepath, "/appended_sheared_file_acc_",ACCbinsize,".rds", sep="")
  if(file.exists(patient_data_filename_ACC)){
    ACCmat=AccelerometerDataCollection(readRDS(patient_data_filename_ACC)[,1],ACCbinsize)
  }else{
    ACCmat=NULL
  }
  # GPS
  patient_data_filepath = paste(output_filepath, "/Preprocessed_Data/Individual/", patient_name, sep="")
  patient_data_filename_GPS = paste(patient_data_filepath, "/gps_preprocessed.rds", sep="")
  if(file.exists(patient_data_filename_GPS)){
    inlist=readRDS(patient_data_filename_GPS)
    mobmatmiss=inlist[[2]]
    GPSmat = GPSDataCollection(mobmatmiss)
  }else{
    GPSmat=NULL
  }
  # Survey
  SVYvec = SVYDataCollection(data_filepath,patient_name)
  SCNmat = SCNDataCollection(data_filepath,patient_name,ACCmat,GPSmat)
  DZEmat = DZEDataCollection(data_filepath,patient_name,ACCmat,GPSmat,SCNmat)
  PlotContinuousDataCollectionTracks(output_filepath,patient_name,ACCmat,GPSmat,SCNmat,DZEmat,SVYvec,tz)
}
