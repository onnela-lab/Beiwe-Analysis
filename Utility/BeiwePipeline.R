source("C:/Users/Ian/Documents/Work/JP/Beiwe/Github/Beiwe-Analysis/Preprocessing/GPS_preprocessing.R")
data_directory="C:/Users/Ian/Documents/Work/JP/Schizophrenia/Data"
SIDs=unlist(lapply(strsplit(list.dirs(data_directory,recursive=FALSE),"/"),function(x) x[length(x)]))
simnum=1

cat("\nProcessing GPS data for",length(SIDs),"subjects:\n\n")
for(i in 1:length(SIDs)){
  cat(paste("Processing ID: ",SIDs[i]," (",i,"/",length(SIDs),")\n",sep=""))
  GPS_preprocessing(SIDs[i],data_directory)
  GPS_imputation(SIDs[i],data_directory,nreps=simnum)
}
