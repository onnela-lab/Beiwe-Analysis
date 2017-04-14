
patient_trajectories_plots = function(pltname,y,X,dat,surveylab=NULL,MMH=1400,MML=1440*5/6){
  for(i in 1:nrow(dat)){
    ssp=strsplit(dat[i,2],"/")[[1]]
    if(length(ssp)>1){
      YEAR=ssp[3]
      MONTH=ssp[1]
      if(as.integer(MONTH)<10){
        MONTH=paste("0",MONTH,sep="")
      }
      DAY=ssp[2]
      if(as.integer(DAY)<10){
        DAY=paste("0",DAY,sep="")
      }
      dat[i,2]=paste(YEAR,MONTH,DAY,sep="-")
    }
  }
  MinsMissingCutHIGH=MMH
  MinsMissingCutLOW=MML
  if(is.null(surveylab)){
    surveylab="Outcome"
  }
  IIDs = unique(dat[,1])
  for(pnum in 1:6){
    if(pnum==1){
      x1=log(X$Hometime+1);x2=log(X$outgoing_texts+1);x3=log(X$outgoing_calllengths+1);labels=c("Time spent at home","# outgoing texts","total outgoing call length")    
    }else if(pnum==2){
      x1=log(X$DistTravelled+1);x2=X$text_outdegree;x3=X$call_outdegree;labels=c("Distance travelled","Text outdegree","Call outdegree")
    }else if(pnum==3){
      x1=log(X$MaxHomeDist+1);x2=log(X$incoming_texts+1);x3=log(X$incoming_calllengths+1);labels=c("Max distance from home","# incoming text","total incoming call length")
    }else if(pnum==4){
      x1=X$SigLocsVisited;x2=X$text_indegree;x3=X$call_indegree;labels=c("# Significant locations visited","Text indegree","Call indegree")
    }else if(pnum==5){
      x1=X$ProbPause;x2=log(X$text_reciprocity+1);x3=log(X$call_reciprocity+1);labels=c("Fraction of time spent not moving","Text reciprocity","Call reciprocity")    
    }else if(pnum==6){
      x1=X$CircdnRtn;x2=X$text_responsiveness;x3=X$call_responsiveness;labels=c("Circadian routine","Text responsiveness","Call responsiveness")    
    }
    mmiss = X$MinsMissing
    col_v=c("magenta2","lawngreen","lightslateblue")
    pch_v=c(2,1,4)
    minx1=min(x1,na.rm=T);maxx1=max(x1,na.rm=T)
    minx2=min(x2,na.rm=T);maxx2=max(x2,na.rm=T)
    minx3=min(x3,na.rm=T);maxx3=max(x3,na.rm=T)
    pdf(paste(pltname,"_PatientTrajectories_", gsub("/","-",max(dat[,2])),"_",pnum,".pdf",sep=""),width=6,height=3)
    plot(NA,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",bty="n",main="Patient trajectories")
    legend("center",c(surveylab,labels),pch=c(0,pch_v),col=c("darkorange",col_v),bty="n")
    for(i in 1:length(IIDs)){
      IDcur=which(dat[,1]==IIDs[i])
      mindate=min(dat[IDcur,2])
      mindatenum=as.POSIXct(mindate,origin="1970-01-01")
      xvals=round((as.POSIXct(dat[IDcur,2],origin="1970-01-01")-mindatenum)/86400)
      plot(NA,xlim=range(xvals,na.rm=T),ylim=range(y,na.rm=T),xlab="",ylab="",main=paste("Subject: ",IIDs[i],sep=""),xaxt="n",yaxt="n",cex.main=.8)
      axis(1,at=range(xvals,na.rm=T),labels=range(dat[IDcur,2]),las=2,cex.axis=.7)
      axis(2,at=range(y,na.rm=T),labels=c("Min","Max"),las=1,cex.axis=.7)
      for(j in 1:length(xvals)){
        points(xvals[j],min(y,na.rm=T)+(max(y,na.rm=T)-min(y,na.rm=T))*(x2[IDcur][j]-minx2)/(maxx2-minx2),pch=pch_v[2],col=col_v[2])
        points(xvals[j],min(y,na.rm=T)+(max(y,na.rm=T)-min(y,na.rm=T))*(x3[IDcur][j]-minx3)/(maxx3-minx3),pch=pch_v[3],col=col_v[3])
        if(!is.na(mmiss[IDcur][j]) && mmiss[IDcur][j]>MinsMissingCutLOW && mmiss[IDcur][j]<MinsMissingCutHIGH){
          points(xvals[j],min(y,na.rm=T)+(max(y,na.rm=T)-min(y,na.rm=T))*(x1[IDcur][j]-minx1)/(maxx1-minx1),pch=pch_v[1],col=col_v[1])      
        }
        points(xvals[j],y[IDcur][j],pch=0,col="darkorange")
      }
    }
    dev.off()  
  }  
}


# featurefile = "C:/Users/Ian/Documents/Work/JP/Schizophrenia/Processed_data/FeatureMatrixCLEAN.txt"
# outdir = "C:/Users/Ian/Documents/Work/JP/Schizophrenia/Output"
# PatientTrajectories(featurefile,outdir)
PatientTrajectories = function(featurefile,outdir,pltname=NULL,inds=NULL,labels=NULL){
  MinsMissingCutLOW=1200
  MinsMissingCutHIGH=1440
  if(!file.exists(outdir)){
    dir.create(outdir)
  }
  dat=read.csv(featurefile,sep="\t",stringsAsFactors=F)
  IIDs = unique(dat[,1])
  # the following for loop converts MM/DD/YYYY into YYYY-MM-DD format
  for(i in 1:nrow(dat)){
    ssp=strsplit(dat[i,2],"/")[[1]]
    if(length(ssp)>1){
      YEAR=ssp[3]
      MONTH=ssp[1]
      if(as.integer(MONTH)<10){
        MONTH=paste("0",MONTH,sep="")
      }
      DAY=ssp[2]
      if(as.integer(DAY)<10){
        DAY=paste("0",DAY,sep="")
      }
      dat[i,2]=paste(YEAR,MONTH,DAY,sep="-")
    }
  }
  if(is.null(inds)){
    inds[1]=which(names(dat)=="DistTravelled")
    inds[2]=which(names(dat)=="SigLocsVisited")
    inds[3]=which(names(dat)=="MinsMissing")
  }
  if(is.null(labels)){
    labels = names(dat)[inds]
  }
  x1 = as.numeric(dat[,inds[1]])
  x2 = as.numeric(dat[,inds[2]])
  x3 = as.numeric(dat[,inds[3]])
  mmiss = dat$MinsMissing
  col_v=c("magenta2","lawngreen","lightslateblue")
  pch_v=c(2,1,4)
  minx1=min(x1,na.rm=T);maxx1=max(x1,na.rm=T)
  minx2=min(x2,na.rm=T);maxx2=max(x2,na.rm=T)
  minx3=min(x3,na.rm=T);maxx3=max(x3,na.rm=T)
  if(is.null(pltname)){
    pltname = paste("PatientTrajectories_", gsub("/","-",max(dat[,2])),".pdf",sep="")
  }
  y=c(1,10)
  pdf(paste(outdir,pltname,sep="/"),width=6,height=3)
  plot(NA,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",bty="n",main="Patient trajectories")
  legend("center",c(labels),pch=pch_v,col=col_v,bty="n")
  for(i in 1:length(IIDs)){
    IDcur=which(dat[,1]==IIDs[i])
    mindate=min(dat[IDcur,2])
    mindatenum=as.POSIXct(mindate,origin="1970-01-01",tz="GMT")
    xvals=round((as.POSIXct(dat[IDcur,2],origin="1970-01-01",tz="GMT")-mindatenum)/86400)
    plot(NA,xlim=range(xvals,na.rm=T),ylim=range(y,na.rm=T),xlab="",ylab="",main=paste("Subject: ",IIDs[i],sep=""),xaxt="n",yaxt="n",cex.main=.8)
    axis(1,at=range(xvals,na.rm=T),labels=range(dat[IDcur,2]),las=2,cex.axis=.7)
    axis(2,at=range(y,na.rm=T),labels=c("Min","Max"),las=1,cex.axis=.7)
    for(j in 1:length(xvals)){
      points(xvals[j],min(y,na.rm=T)+(max(y,na.rm=T)-min(y,na.rm=T))*(x2[IDcur][j]-minx2)/(maxx2-minx2),pch=pch_v[2],col=col_v[2])
      points(xvals[j],min(y,na.rm=T)+(max(y,na.rm=T)-min(y,na.rm=T))*(x3[IDcur][j]-minx3)/(maxx3-minx3),pch=pch_v[3],col=col_v[3])
      if(!is.na(mmiss[IDcur][j]) && mmiss[IDcur][j]>MinsMissingCutLOW && mmiss[IDcur][j]<MinsMissingCutHIGH){
        points(xvals[j],min(y,na.rm=T)+(max(y,na.rm=T)-min(y,na.rm=T))*(x1[IDcur][j]-minx1)/(maxx1-minx1),pch=pch_v[1],col=col_v[1])      
      }
    }
  }
  dev.off()  
}