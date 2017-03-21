

# fileloc = "C:/Users/Ian/Documents/Work/JP/Schizophrenia/Processed_data"
# filename = "FeatureMatrix.txt"
# filenameclean = paste(strsplit(filename,split='\\.')[[1]][1],"CLEAN.txt",sep="")
# surveycols = 5:40
# FillInNAS(fileloc,filename)
# daysback=2
# ReplicateSurveyResponsesBackwards(fileloc,filenameclean,surveycols,daysback)

fill_in_NAs = function(...){
  patient_input_filepath = paste(output_filepath, "/Processed_Data/Group",sep="")
  patient_input_filename = paste(patient_input_filepath, "/feature_matrix.rds",sep="")
  patient_output_filename = paste(patient_input_filepath, "/feature_matrix_clean.rds",sep="")
  if(!file.exists(patient_input_filename)){return(NULL)}
  dat=data.frame(readRDS(patient_input_filename)[[1]])
  for(i in 1:nrow(dat)){
    for(j in 1:ncol(dat)){
      if(is.na(dat[i,j])){next}
      if(is.nan(dat[i,j])){dat[i,j]=0}
      if(dat[i,j]=="NaN"){dat[i,j]="0"}
      if(dat[i,j]==""){
        dat[i,j]=NA
        next
      }
      if(dat[i,j]=="NOT_PRESENTED"){
        dat[i,j]=NA
      }
    }
  }
  saveRDS(list(dat),patient_output_filename)
  #write.table(dat,file=paste(fileloc,"/",strsplit(filename,split='\\.')[[1]][1],"CLEAN.txt",sep=""),sep="\t",quote=FALSE,row.names=FALSE)
}

replicate_survey_responses_backwards = function(surveycols,daysback=1,...){
  fileloc=paste(output_filepath,"/Processed_Data/Group",sep="")
  filename="feature_matrix_clean.rds"
  dat = readRDS(paste(fileloc,filename,sep="/"))[[1]]
  #dat=read.csv(paste(fileloc,filename,sep="/"),stringsAsFactors=FALSE,header=TRUE,sep="\t")
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
  saveRDS(list(dat),paste(fileloc,"/feature_matrix_clean_",daysback,"daycarry.rds",sep=""))
  #write.table(dat,file=paste(fileloc,"/",strsplit(filename,split='\\.')[[1]][1],daysback,"REPSBACK.txt",sep=""),sep="\t",quote=FALSE,row.names=FALSE)
}