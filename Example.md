This .md file should be expanded to create a full example of the analysis pipeline.


Just source the GPS_preprocessing.R file located in the preprocessing folder and the following code shows how, give the data_directory and the patient_name, GPS will be proprocessed into flights and pauses, and then missingness will be replaced with imputed trajectories. The preprocessed GPS data is stored in the preprocessed_data folder in the patient's folder. The imputed files are stored in the processed_data folder.
```
source("GPS_preprocessing.R")
data_directory = "C:/Users/Ian/Documents/Work/JP/NeurosurgeryRecovery/Data"
SIDs=unlist(lapply(strsplit(list.dirs(data_directory,recursive=FALSE),"/"),function(x) x[length(x)]))
simnum=1
cat("\nProcessing GPS data for",length(SIDs),"subjects:\n\n")
for(i in 1:length(SIDs)){
  cat(paste("Processing ID: ",SIDs[i]," (",i,"/",length(SIDs),")\n",sep=""))
  GPS_preprocessing(SIDs[i],data_directory)
  GPS_imputation(SIDs[i],data_directory,nreps=simnum)
}
```
