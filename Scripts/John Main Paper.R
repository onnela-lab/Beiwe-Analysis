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



###################################
####          plotting         ####
###################################

plot_data_quality(stream = "accelerometer",
                      frequency = 10,
                      burst_duration = 60,
                      break_duration = 60)

plot_data_quality(stream = "gps",
                      frequency = 1,
                      burst_duration = 60,
                      break_duration = 60*10)

plot_survey_timing()
plot_survey_completion()


###################################
####          analysis         ####
###################################

filters = list(
  All_Questions = get_questions(),
  WSS = c("Unable to cope with stress","Feeling tired","Feeling Confused or Puzzled","Feeling depressed or sad", "Difficulty falling asleep", "Difficulty staying asleep", "Waking up too early", "Don't feel rested after waking up", "Feeling nervous; scared; or anxious", "Little interest or pleasure in things", "Trouble concentrating"),
  #Missing_Medications = c("Missing doses of medications", "Missing Doses of Medication", "Missing Doses of Medications"),
  Tired = c("Feeling tired", "Difficulty staying asleep", "Waking up too early", "Difficulty falling asleep", "Don't feel rested after waking up", "Feeling Tired"),
  Anhedonia = c("Feeling Confused or Puzzled","Little interest or pleasure in things","Feeling bad or guilty about yourself","Feeling depressed or sad","Trouble concentrating","Difficulty thinking clearly","Withdrawing from social interaction"),
  Anxiety = c("Unable to cope with stress", "Feeling suspicious","Feeling nervous; scared; or anxious","Trouble relaxing","Poor appetite or overeating")
)

plot_data_quality_predictiveness(filters)






