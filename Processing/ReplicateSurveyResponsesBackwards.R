

# fileloc = "C:/Users/Ian/Documents/Work/JP/Schizophrenia/Processed_data"
# filename = "FeatureMatrix.txt"
# filenameclean = paste(strsplit(filename,split='\\.')[[1]][1],"CLEAN.txt",sep="")
# surveycols = 5:40
# FillInNAS(fileloc,filename)
# daysback=2
# ReplicateSurveyResponsesBackwards(fileloc,filenameclean,surveycols,daysback)

FillInNAS = function(fileloc,filename){
  dat=read.csv(paste(fileloc,filename,sep="/"),stringsAsFactors=FALSE,header=TRUE,sep="\t")
  for(i in 1:nrow(dat)){
    for(j in 1:ncol(dat)){
      if(is.na(dat[i,j])){next}
      if(dat[i,j]==""){
        dat[i,j]=NA
      }
      if(dat[i,j]=="NOT_PRESENTED"){
        dat[i,j]=NA
      }
    }
  }
  write.table(dat,file=paste(fileloc,"/",strsplit(filename,split='\\.')[[1]][1],"CLEAN.txt",sep=""),sep="\t",quote=FALSE,row.names=FALSE)
}

ReplicateSurveyResponsesBackwards = function(fileloc,filename,surveycols,daysback=1){
  dat=read.csv(paste(fileloc,filename,sep="/"),stringsAsFactors=FALSE,header=TRUE,sep="\t")
  datenums=round(as.numeric(as.POSIXct(dat[,2]))/(24*3600))
  for(jj in 1:length(surveycols)){
    j=surveycols[jj]
    backvals = rep(NA,daysback)
    backvalsdates = datenums[nrow(dat):(nrow(dat)-daysback+1)]
    backvalsinds = dat[nrow(dat):(nrow(dat)-daysback+1),1]
    curID = dat[nrow(dat),1]
    for(i in nrow(dat):1){
      IDNOTNA = which(!is.na(backvals))
      IDSAMEIND = which(backvalsinds==dat[i,1])
      IDCLOSE = which(backvalsdates-datenums[i] <=daysback)
      IDPASS=intersect(intersect(IDNOTNA,IDSAMEIND),IDCLOSE)
      if(is.na(dat[i,j]) && length(IDPASS)>0){
        temp = dat[i,j]
        dat[i,j]=backvals[IDPASS[1]]
        backvals = c(temp,backvals[-daysback])
        backvalsdates = c(datenums[i],backvalsdates[-daysback])
        backvalsinds = c(dat[i,1],backvalsinds[-daysback])
      }else{
        backvals = c(dat[i,j],backvals[-daysback])
        backvalsdates = c(datenums[i],backvalsdates[-daysback])
        backvalsinds = c(dat[i,1],backvalsinds[-daysback])
      }
    }
  }
  write.table(dat,file=paste(fileloc,"/",strsplit(filename,split='\\.')[[1]][1],daysback,"REPSBACK.txt",sep=""),sep="\t",quote=FALSE,row.names=FALSE)
}
