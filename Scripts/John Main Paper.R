################################################################
################    Beiwe Master Pipeline    ###################
################################################################


source_filepath    = "C:/Users/Patrick/Desktop/Beiwe-Analysis"
data_filepath      = "C:/Phoenix/School/Harvard/Research/Beiwe/Studies/John_Schizophrenia/Data/2017.01.09"
output_filepath    = "C:/Phoenix/School/Harvard/Research/Beiwe/Studies/John_Schizophrenia/Output"

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

patient_names = list.files(data_filepath)[-grep("\\.",list.files(data_filepath))]
for(patient_name in patient_names){
  print(patient_name)
  
  
  # Preprocess Data
  
  surveys_preprocessing(patient_name)
  GPS_imputation_and_features(patient_name, automatic_preprocessing = TRUE)
  text_preprocessing(patient_name)
  calls_preprocessing(patient_name)
  powerstate_preprocessing(patient_name)
  accelerometer_preprocessing(patient_name, minutes = 5)



  
  # Process Data
  
  #warp_GPS(seed = 1)
  
  #textlocs  = text_locations (textmat,  mobmat)
  #calllocs  = call_locations (callmat,  mobmat)
  #statelocs = powerstate_locations(statemat, mobmat)
  
  
  # Output
  
  #location_plot(mobmat, textmat, callmat, statemat, accmat5, textlocs, calllocs, statelocs, daynames)
  
}





###################################
#### combined patient analysis ####
###################################

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










