# fileloc = "C:/Users/Ian/Documents/Work/JP/Schizophrenia/Processed_data"
# filename = "FeatureMatrixCLEAN2REPSBACK.txt"
# srvyinds=3:40
# labels = c("Anxiety","Depression","Meds","Sleep","Psychosis","WSS")
# groupings = list()
# groupings[[1]] = c(3,6,8,13,23,25)
# groupings[[2]] = c(5,7,10,11,14,21,24,26)
# groupings[[3]] = c(9)
# groupings[[4]] = c(5,11,12,22)
# groupings[[5]] = c(8,15,16,27)
# groupings[[6]] = c(3,4,5,7,10,11,12,14,21,22,23)

#CombineSurveyResponses(fileloc,filename,srvyinds,groupings,labels)
combine_survey_responses = function(srvyinds,groupings,labels){
  featurefile=paste(output_filepath,"/Processed_data","/Group",sep="")
  filename=paste("feature_matrix_clean_",daysback,"daycarry.rds",sep="")
  outfilename=paste("feature_matrix_clean_",daysback,"daycarry_combined.rds",sep="")
  outtxtname=paste("feature_matrix_clean_",daysback,"daycarry_combined.txt",sep="")
  infilename=paste(featurefile,filename,sep="/")
  if(!file.exists(infilename)){return(NULL)}
  dat=readRDS(infilename)[[1]]
  #dat=read.csv(paste(fileloc,filename,sep="/"),stringsAsFactors=FALSE,header=TRUE,sep="\t")
  outmat = cbind(dat[,1:2],matrix(NA,ncol=length(groupings),nrow=nrow(dat),dimnames=list(NULL,labels)),dat[,(max(srvyinds)+1):ncol(dat)])
  for(j in 1:length(groupings)){
    INDsPICK = groupings[[j]]
    for(i in 1:nrow(dat)){
      nAns=length(which(!is.na(dat[i,INDsPICK])))
      if(nAns>0){
        outmat[i,2+j]=sum(as.numeric(dat[i,INDsPICK]),na.rm=T)/(nAns)
      }
    }
  }
  saveRDS(list(outmat),paste(featurefile,outfilename,sep="/"))
  write.table(outmat,file=paste(featurefile,outtxtname,sep="/"),sep="\t",quote=FALSE,row.names=FALSE)
}


