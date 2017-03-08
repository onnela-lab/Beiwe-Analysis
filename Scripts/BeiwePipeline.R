

source("C:/Users/Ian/Documents/Work/JP/Beiwe/Github/Beiwe-Analysis/Preprocessing/GPS_preprocessing.R")
source("C:/Users/Ian/Documents/Work/JP/Beiwe/Github/Beiwe-Analysis/Processing/DailyFeatures.R")
source("C:/Users/Ian/Documents/Work/JP/Beiwe/Github/Beiwe-Analysis/Processing/ReplicateSurveyResponsesBackwards.R")
source("C:/Users/Ian/Documents/Work/JP/Beiwe/Github/Beiwe-Analysis/Processing/CombineSurveyResponses.R")
source("C:/Users/Ian/Documents/Work/JP/Beiwe/Github/Beiwe-Analysis/Output/AdherencePlots.R")
source("C:/Users/Ian/Documents/Work/JP/Beiwe/Github/Beiwe-Analysis/Output/PatientTrajectories/PatientTrajectories.R")
source("C:/Users/Ian/Documents/Work/JP/Beiwe/Github/Beiwe-Analysis/Output/TimeSeriesAnomaly/TSAnomalyDetection.R")
source("C:/Users/Ian/Documents/Work/JP/Beiwe/Github/Beiwe-Analysis/Output/ContinuousDataCollectionTracks.R")


#### USER SPECIFIED: points to location of data
homedir="C:/Users/Ian/Documents/Work/JP/Schizophrenia"
# select your time zone (tz="" defaults to the time zone of your computer)
tz=""
# number of imputations to do in the multiple imputation of GPS.
simnum=1

# Extract patient IDs
SIDs=unlist(lapply(strsplit(list.dirs(paste(homedir,"Data",sep="/"),recursive=FALSE),"/"),function(x) x[length(x)]))

### Remove non-patients
# file format for pinfo:
#   column 1: patient IDs
#   column 2: "yes" if a patient, "no" to be removed
#   column 3: important date (relapse, hospitalization, surgery, etc..,)
#   first row is assumed to be a header. The file should be comma delimited.
pinfo_filename="patientinfo.csv" 
pinfo = read.csv(paste(homedir,pinfo_filename,sep="/"),stringsAsFactors=F)
for(i in 1:length(SIDs)){
  rID=which(pinfo[,1]==SIDs[i])
  if(length(rID)>0){
    if(pinfo[rID[1],2]=="no"){
      unlink(paste(homedir,"Data",SIDs[i],sep="/"),recursive=TRUE)
    }
  }
}
# Update patient IDs after removing non-patients
SIDs=unlist(lapply(strsplit(list.dirs(paste(homedir,"Data",sep="/"),recursive=FALSE),"/"),function(x) x[length(x)]))

### Continuous data collection plots
cat("\nCreating data collection plots for ",length(SIDs),"subjects:\n\n")
for(i in 1:length(SIDs)){
  cat(paste("Processing ID for data collection plots: ",SIDs[i]," (",i,"/",length(SIDs),")\n",sep=""))
  ContinuousDataCollectionTracks(homedir,SIDs[i],tz)
}


### GPS preprocessing and processing
cat("\nProcessing GPS data for",length(SIDs),"subjects:\n\n")
for(i in 1:length(SIDs)){
  cat(paste("Processing ID for GPS mobility: ",SIDs[i]," (",i,"/",length(SIDs),")\n",sep=""))
  GPS_preprocessing(SIDs[i],homedir)
  GPS_imputation(SIDs[i],homedir,nreps=simnum)
  CreateMobilityFeatures(SIDs[i],homedir)
}

### Create feature matrix using mobility and sociability features
DailyFeatures(homedir) 
# location of feature matrix to be cleaned/modified
fileloc=paste(homedir,"Processed_data",sep="/") 
# cleans up feature matrix by replacing blanks and bad entries with NAs
FillInNAS(fileloc,filename="FeatureMatrix.txt") 

### Create data adherence plots
DataCollectionPlots(homedir,plotname="AdherencePlots.pdf")

### Create data trajectories plots
inds=c(41,51,64)
PatientTrajectories(featurefile=paste(homedir,"Processed_data","FeatureMatrixCLEAN.txt",sep="/"),outdir=paste(homedir,"Output",sep="/"),inds=inds)

# Columns of the feature matrix that correspond to survey questions (to be duplicated)
surveycols = 5:40
# Number of days prior to taking a survey that the survey responses are still pertinent
daysback = 2 
# Create a feature matrix that applies survey responses to the daysback days prior to taking each survey. Essentially adding duplicates.
ReplicateSurveyResponsesBackwards(fileloc,filenameclean="FeatureMatrixCLEAN.txt",surveycols,daysback)

#### Combine survey responses (via addition)
# location of input feature matrix to modify
fileloc = "C:/Users/Ian/Documents/Work/JP/Schizophrenia/Processed_data"
# output file name of feature matrix with combined surveys
filename = "FeatureMatrixCLEAN2REPSBACK.txt"
# Column indices of all the survey questions in the input matrix
srvyinds=3:40
# labels for the resulting groupings after combination. Must be the same length as the "groupings" lists.
labels = c("Anxiety","Depression","Meds","Sleep","Psychosis","WSS")
# The groupings list specifies which column indices of the input feature matrix should be combined together
groupings = list()
groupings[[1]] = c(3,6,8,13,23,25)
groupings[[2]] = c(5,7,10,11,14,21,24,26)
groupings[[3]] = c(9)
groupings[[4]] = c(5,11,12,22)
groupings[[5]] = c(8,15,16,27)
groupings[[6]] = c(3,4,5,7,10,11,12,14,21,22,23)
# The "CombineSurveyResponses" function performs the survey combination.
CombineSurveyResponses(fileloc,filename,srvyinds,groupings,labels)

### Time series anomaly detection
Nsurveys=6
cat("\nPerforming anomaly detection for ",length(SIDs),"subjects:\n\n")
for(i in 1:length(SIDs)){
  cat(paste("ID: ",SIDs[i]," (",i,"/",length(SIDs),")\n",sep=""))
  rID=which(pinfo[,1]==SIDs[i])
  vertmarks=c(NULL)
  if(length(rID)>0){
    vertmarks=c(pinfo[rID[1],3])
    if(is.na(vertmarks)){
      vertmarks=c(NULL)
    }
  }
  AnomalyDetectionPlot(homedir,filename="FeatureMatrixCLEAN2REPSBACKCOMBINED.txt",ID=SIDs[i],Nsurveys=Nsurveys,outfiletag="back2",NoSurvey=FALSE,vertmarks)
}

