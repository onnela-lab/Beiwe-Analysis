################################################################
################    Beiwe Master Pipeline    ###################
################################################################


source_filepath    = "C:/Users/Ian/Documents/Work/JP/Beiwe/Github/Beiwe-Analysis"
data_filepath      = "C:/Users/Ian/Documents/Work/JP/PipelineDemo/Data"
output_filepath    = "C:/Users/Ian/Documents/Work/JP/PipelineDemo/Output"


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

for(patient_name in patient_names){
  print(patient_name)
  
  # Preprocess Data
  surveys_preprocessing(patient_name)
  GPS_preprocessing(patient_name)
  text_preprocessing(patient_name)
  calls_preprocessing(patient_name)
  powerstate_preprocessing(patient_name)
  accelerometer_preprocessing(patient_name, minutes = ACCbinsize)

  # Process Data
  GPS_imputation(patient_name,nreps=1)
  CreateMobilityFeatures(patient_name)

  # Output
  ContinuousDataCollectionTracks(patient_name,ACCbinsize)
}


DailyFeatures()
fill_in_NAs()
daily_adherence_grid()
