

source("C:/Users/Ian/Documents/Work/JP/Beiwe/Github/Beiwe-Analysis/Preprocessing/GPS_preprocessing.R")
source("C:/Users/Ian/Documents/Work/JP/Beiwe/Github/Beiwe-Analysis/Processing/DailyFeatures.R")
source("C:/Users/Ian/Documents/Work/JP/Beiwe/Github/Beiwe-Analysis/Processing/ReplicateSurveyResponsesBackwards.R")
source("C:/Users/Ian/Documents/Work/JP/Beiwe/Github/Beiwe-Analysis/Output/AdherencePlots.R")
source("C:/Users/Ian/Documents/Work/JP/Beiwe/Github/Beiwe-Analysis/Output/PatientTrajectories/PatientTrajectories.R")

#### USER SPECIFIED: points to location of data
homedir="C:/Users/Ian/Documents/Work/JP/Schizophrenia"
####
SIDs=unlist(lapply(strsplit(list.dirs(paste(homedir,"Data",sep="/"),recursive=FALSE),"/"),function(x) x[length(x)]))
simnum=1

cat("\nProcessing GPS data for",length(SIDs),"subjects:\n\n")
for(i in 1:length(SIDs)){
  cat(paste("Processing ID: ",SIDs[i]," (",i,"/",length(SIDs),")\n",sep=""))
  GPS_preprocessing(SIDs[i],homedir)
  GPS_imputation(SIDs[i],homedir,nreps=simnum)
  CreateMobilityFeatures(SIDs[i],homedir)
}

# create feature matrix using mobility and sociability features
DailyFeatures(homedir) 
# location of feature matrix to be cleaned/modified
fileloc=paste(homedir,"Processed_data",sep="/") 
# cleans up feature matrix by replacing blanks and bad entries with NAs
FillInNAS(fileloc,filename="FeatureMatrix.txt") 

# Create data adherence plots
DataCollectionPlots(homedir,plotname="AdherencePlots.pdf")

# Create data trajectories plots
PatientTrajectories(featurefile=paste(homedir,"Processed_data","FeatureMatrixCLEAN.txt",sep="/"),outdir=paste(homedir,"Output",sep="/"))

# Columns of the feature matrix that correspond to survey questions (to be duplicated)
surveycols = 5:40
# Number of days prior to taking a survey that the survey responses are still pertinent
daysback = 2 
# Create a feature matrix that applies survey responses to the daysback days prior to taking each survey. Essentially adding duplicates.
ReplicateSurveyResponsesBackwards(fileloc,filenameclean="FeatureMatrixCLEAN.txt",surveycols,daysback)


