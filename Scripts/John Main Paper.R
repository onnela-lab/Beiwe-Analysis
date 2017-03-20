################################################################
################    Beiwe Master Pipeline    ###################
################################################################

source_filepath    = "C:/Users/Ian/Documents/Work/JP/Beiwe/Github/Beiwe-Analysis-master"
data_filepath      = "C:/Users/Ian/Documents/Work/JP/Schizophrenia/Data"
output_filepath    = "C:/Users/Ian/Documents/Work/JP/Schizophrenia/Output"

#source_filepath    = "C:/Users/Patrick/Desktop/Beiwe-Analysis"
#data_filepath      = "C:/Phoenix/School/Harvard/Research/Beiwe/Studies/John_Schizophrenia/Data/2017.01.09"
#output_filepath    = "C:/Phoenix/School/Harvard/Research/Beiwe/Studies/John_Schizophrenia/Output"

### Constants:
ACCbinsize=5 # width of bin in minutes used for combining accelerometer data


# Source all files

source_categories = c("Utility", "Preprocessing", "Processing", "Results")
for(source_category in source_categories){
  setwd(paste(source_filepath,source_category,sep="/"))
  file_sources = list.files(pattern="*.R")
  sapply(file_sources,source,.GlobalEnv)
}

###################################
### individual patient analysis ###
###################################


initialize_output_directory(data_filepath, output_filepath)

nonfolders=grep("\\.",list.files(data_filepath)) 
if(length(nonfolders)>0){  ### if you don't do this check, in if length(nonfolders)==0 will make patient_names empty
  patient_names = list.files(data_filepath)[-nonfolders]
}else{
  patient_names = list.files(data_filepath)
}

counter=0
for(patient_name in patient_names){
  print(patient_name)
  counter=counter+1
  if(counter<7){next}
  
  # Preprocess Data
  
  surveys_preprocessing(patient_name)
  #GPS_imputation_and_features(patient_name, automatic_preprocessing = TRUE)
  GPS_preprocessing(patient_name)
  text_preprocessing(patient_name)
  calls_preprocessing(patient_name)
  powerstate_preprocessing(patient_name)
  accelerometer_preprocessing(patient_name, minutes = ACCbinsize)
  
  
  
  
  # Process Data
  
  GPS_imputation(patient_name,nreps=1)
  CreateMobilityFeatures(patient_name)
  
  #warp_GPS(seed = 1)
  
  #textlocs  = text_locations (textmat,  mobmat)
  #calllocs  = call_locations (callmat,  mobmat)
  #statelocs = powerstate_locations(statemat, mobmat)
  
  
  # Output
  
  ContinuousDataCollectionTracks(patient_name,ACCbinsize)
  
  #location_plot(mobmat, textmat, callmat, statemat, accmat5, textlocs, calllocs, statelocs, daynames)
  
}


#####################################
#### combined patient processing ####
#####################################


daily_features()
fill_in_NAs()
daily_adherence_grid()
patient_trajectories_plots(inds=c(31,40,43))# inds: columns of the feature matrix that will be plotted in patient_trajectories(). Must pick 3.



###################################
#### combined patient analysis ####
###################################

# Columns of the feature matrix that correspond to survey questions (to be duplicated)
surveycols = 5:29
# Number of days prior to taking a survey that the survey responses are still pertinent
daysback = 2 
# Create a feature matrix that applies survey responses to the daysback days prior to taking each survey. Essentially adding duplicates.
replicate_survey_responses_backwards(surveycols,daysback)

#### Combine survey responses (via addition)
# labels for the resulting groupings after combination. Must be the same length as the "groupings" lists.
labels = c("Anxiety","Depression","Meds","Sleep","Psychosis","WSS")
# The groupings list specifies which column indices of the input feature matrix should be combined together
groupings = list()
groupings[[1]] = c(5,8,10,16,18,26)
groupings[[2]] = c(7,9,23,24,27,14,17,19)
groupings[[3]] = c(11)
groupings[[4]] = c(7,15,24,25)
groupings[[5]] = c(10,20,28,29)
groupings[[6]] = c(5,6,7,9,14,15,16,23,24,25,27)
# The "CombineSurveyResponses" function performs the survey combination.
combine_survey_responses(surveycols,groupings,labels)



#### Anomaly Detection
Nsurveys=6
cat("\nPerforming anomaly detection for ",length(patient_names),"subjects:\n\n")
for(i in 1:length(patient_names)){
  cat(paste("ID: ",patient_names[i]," (",i,"/",length(patient_names),")\n",sep=""))
  vertmarks=c(NULL)
  ########## The following commented out portion is used to read in hospitalization dates from a
  ########## csv file and mark the resulting plots with a red line on this dates.
  # rID=which(pinfo[,1]==patient_names[i])
  # if(length(rID)>0){
  #   vertmarks=c(pinfo[rID[1],3])
  #   if(is.na(vertmarks)){
  #     vertmarks=c(NULL)
  #   }
  # }
  anomaly_detection_plot(ID=patient_names[i],Nsurveys=Nsurveys,NoSurvey=FALSE,vertmarks=vertmarks,onesided=TRUE)
}



#################################
###### Patrick's stuff below this
#################################

survey_responsiveness()

data_quality(stream = "accelerometer",
             burst_duration = 60,
             break_duration = 60,
             frequency = 10,
             millisecond_divider = 10*1000)#30*1000

data_quality(stream = "gps",
             burst_duration = 60,
             break_duration = 60*10,
             frequency = 1,
             millisecond_divider = 30*1000)#120*1000

coverage_over_time("accelerometer")
coverage_over_time("gps")


survey_mat = survey_quality(data_filepath)




###################################
####          analysis         ####
###################################

#all_features = list(surveys, location_features, sleep_features, text_features, call_features)
#dataset = Reduce(full_join, all_features)
# call statistical analysis functions here.




###################################
####          plotting         ####
###################################

data_quality_plotting(stream = "accelerometer",
                      frequency = 10,
                      burst_duration = 60,
                      break_duration = 60)

data_quality_plotting(stream = "gps",
                      frequency = 1,
                      burst_duration = 60,
                      break_duration = 60*10)

survey_responsiveness_plotting()


bursts = readRDS("C:/Phoenix/School/Harvard/Research/Beiwe/Studies/John_Schizophrenia/Output/Preprocessed_Data/Group/gps_bursts.rds") %>% data.frame
coverage = readRDS("C:/Phoenix/School/Harvard/Research/Beiwe/Studies/John_Schizophrenia/Output/Preprocessed_Data/Group/gps_coverage.rds") %>% data.frame
curated_total = readRDS(paste(output_filepath, "/Processed_Data/Group/surveys_responsiveness.rds", sep=""))




