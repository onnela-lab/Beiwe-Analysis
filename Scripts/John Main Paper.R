################################################################
################    Beiwe Master Pipeline    ###################
################################################################

#source_filepath    = "C:/Users/Ian/Documents/Work/JP/Beiwe/Github/Beiwe-Analysis-master"
#data_filepath      = "C:/Users/Ian/Documents/Work/JP/Schizophrenia/Data"
#output_filepath    = "C:/Users/Ian/Documents/Work/JP/Schizophrenia/Output"

source_filepath    = "C:/Users/Patrick/Desktop/Beiwe-Analysis"
data_filepath      = "C:/Phoenix/School/Harvard/Research/Beiwe/Studies/John_Schizophrenia/Data/2017.01.09"
output_filepath    = "C:/Phoenix/School/Harvard/Research/Beiwe/Studies/John_Schizophrenia/Output"

### Constants:

acc_binsize=5 # width of bin in minutes used for combining accelerometer data
acc_millisecond_divider = 30*1000
gps_millisecond_divider = 30*1000
acc_frequency = 10
gps_frequency = 1
acc_burst_duration = 60
gps_burst_duration = 60
acc_break_duration = 60
gps_break_duration = 60*10



# Source all files
source(paste(source_filepath, "Utility/Initialize.R",sep="/"))


###################################
### individual patient analysis ###
###################################

for(patient_name in patient_names){
  print(patient_name)
  
  # Preprocess Data
  
  surveys_preprocessing(patient_name)
  text_preprocessing(patient_name)
  calls_preprocessing(patient_name)
  powerstate_preprocessing(patient_name)
  accelerometer_preprocessing(patient_name, minutes = acc_binsize)
  GPS_preprocessing(patient_name)
  
  find_bursts(patient_name, stream = "accelerometer", acc_millisecond_divider)
  find_bursts(patient_name, stream = "GPS",           gps_millisecond_divider)
  # Process Data
  
  GPS_imputation(patient_name,nreps=1)
  #CreateMobilityFeatures(patient_name)
  
  #warp_GPS(seed = 1)
  #textlocs  = text_locations (textmat,  mobmat)
  #calllocs  = call_locations (callmat,  mobmat)
  #statelocs = powerstate_locations(statemat, mobmat)
  
  
  # Results
  
  ContinuousDataCollectionTracks(patient_name, acc_binsize)
  #location_plot(mobmat, textmat, callmat, statemat, accmat5, textlocs, calllocs, statelocs, daynames)
  
}



#####################################
#### combined patient processing ####
#####################################


daily_features()
fill_in_NAs()
daily_adherence_grid()
patient_trajectories_plots(inds=c(31,40,43))# inds: columns of the feature matrix that will be plotted in patient_trajectories(). Must pick 3.

find_questions()
survey_responsiveness()

summarize_data_quality(stream = "accelerometer", acc_burst_duration, acc_break_duration, acc_frequency)
summarize_data_quality(stream = "gps",           gps_burst_duration, gps_break_duration, gps_frequency)

coverage_over_time("accelerometer")
coverage_over_time("gps")


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


###################################
####          plotting         ####
###################################

plot_data_quality(stream = "accelerometer", acc_frequency, acc_burst_duration, acc_break_duration)
plot_data_quality(stream = "gps",           gps_frequency, gps_burst_duration, gps_break_duration)

plot_survey_responsiveness()
plot_survey_completion()
plot_accelerometer(minutes = 5, fixed_days = 8*7)

filters = list(
  All_Questions = get_questions(),
  WSS = c("Unable to cope with stress","Feeling tired","Feeling Confused or Puzzled","Feeling depressed or sad", "Difficulty falling asleep", "Difficulty staying asleep", "Waking up too early", "Don't feel rested after waking up", "Feeling nervous; scared; or anxious", "Little interest or pleasure in things", "Trouble concentrating"),
  #Missing_Medications = c("Missing doses of medications", "Missing Doses of Medication", "Missing Doses of Medications"),
  Tired = c("Feeling tired", "Difficulty staying asleep", "Waking up too early", "Difficulty falling asleep", "Don't feel rested after waking up", "Feeling Tired"),
  Anhedonia = c("Feeling Confused or Puzzled","Little interest or pleasure in things","Feeling bad or guilty about yourself","Feeling depressed or sad","Trouble concentrating","Difficulty thinking clearly","Withdrawing from social interaction"),
  Anxiety = c("Unable to cope with stress", "Feeling suspicious","Feeling nervous; scared; or anxious","Trouble relaxing","Poor appetite or overeating")
)


filters = list(
  Warning_Signs = c("Unable to cope with stress", "Feeling tired", "Feeling depressed or sad", "Feeling Confused or Puzzled", "Don't feel rested after waking up",  "Feeling nervous, scared, or anxious", "Little interest or pleasure in things", "Trouble concentrating"),
  Sleep = c("Difficulty falling asleep", "Don't feel rested after waking up", "Difficulty staying asleep", "Don't feel rested after waking up"),
  Mood_and_PHQ8 = c("Little interest or pleasure in things", "Poor appetite or overeating", "Feeling bad or guilty about yourself", "Trouble concentrating",  "Feeling tired", "Feeling depressed or sad", "Trouble relaxing", "Don't feel rested after waking up"),
  Anxiety_and_GAD7 = c("Unable to cope with stress", "Feeling nervous, scared, or anxious", "Worrying too much",  "Feeling bad or guilty about yourself",  "Feeling Confused or Puzzled", "Trouble relaxing",  "Easily annoyed or irritated"),
  Psychosis = c("Hearing voices or seeing things", "Feeling suspicious", "Difficulty thinking clearly", "Withdrawing from social interaction"),
  #Medications = c("Missing doses of medications", "Missing Doses of Medication", "Missing Doses of Medications"),
  Negative_and_Anhedonic = c("Withdrawing from social interaction", "Feeling depressed or sad", "Feeling bad or guilty about yourself", "Little interest or pleasure in things"),
  Cognitive = c ("Trouble concentrating", "Difficulty thinking clearly", "Feeling Confused or Puzzled"),
  Positive = c("Easily annoyed or irritated", "Hearing voices or seeing things", "Trouble relaxing", "Feeling nervous, scared, or anxious", "Feeling suspicious")
)


plot_data_quality_predictiveness(filters)


