################################################################
################    Beiwe Master Pipeline    ###################
################################################################

source_filepath    = "C:/Users/Patrick/Desktop/Beiwe-Analysis"
#data_filepath      = "C:/Phoenix/School/Harvard/Research/Beiwe/Studies/John_Schizophrenia/Data/2017.01.09"
#output_filepath    = "C:/Phoenix/School/Harvard/Research/Beiwe/Studies/John_Schizophrenia/Output"
data_filepath      = "C:/Phoenix/School/Harvard/Research/Beiwe/Studies/Janet_Eating/Data/2017.01.09"
output_filepath    = "C:/Phoenix/School/Harvard/Research/Beiwe/Studies/Janet_Eating/Output"

# Source all files

source(paste(source_filepath, "Utility/Initialize.R",sep="/"))


###################################
### individual patient analysis ###
###################################

for(patient_name in patient_names){
  print(patient_name)
  
  

  
  # Preprocess Data #
  
  find_bursts(patient_name, stream = "accelerometer", millisecond_divider = 30*1000)
  find_bursts(patient_name, stream = "GPS",           millisecond_divider = 30*1000)
  surveys_preprocessing(patient_name)
  GPS_imputation_and_features(patient_name, automatic_preprocessing = TRUE)
  text_preprocessing(patient_name)
  calls_preprocessing(patient_name)
  powerstate_preprocessing(patient_name)
  accelerometer_preprocessing(patient_name, minutes = 5)
}


  # Process Data
  
  #warp_GPS(seed = 1)
  
  #textlocs  = text_locations (textmat,  mobmat)
  #calllocs  = call_locations (callmat,  mobmat)
  #statelocs = powerstate_locations(statemat, mobmat)
  
  
  # Output
  
  #location_plot(mobmat, textmat, callmat, statemat, accmat5, textlocs, calllocs, statelocs, daynames)
  
#}





###################################
#### combined patient analysis ####
###################################

find_questions()
survey_responsiveness()

summarize_data_quality(stream = "accelerometer",
             burst_duration = 60,
             break_duration = 60,
             frequency = 10)#30*1000

summarize_data_quality(stream = "gps",
               burst_duration = 60,
               break_duration = 60*10,
               frequency = 1)#120*1000

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

plot_survey_responsiveness()
plot_survey_completion()
plot_accelerometer(minutes = 5, fixed_days = 8*7)



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




x=readRDS(paste(output_filepath, "/Processed_Data/Group/surveys_responsiveness.rds",sep=""))
y=(as.POSIXlt(x[,"Notified"],origin="1970-01-01") %>%
  sapply(function(x){as.numeric(strsplit(as.character(x)," "))}))[,2:3] %*% c(1,60)/60
pdf("C:/Users/Patrick/Desktop/Survey Notification Timings.pdf", width=9,height=5)
hist(y,breaks=96,col=rgb(.3,.3,.3),border=FALSE,xaxt="n",main="Histogram of Notification Timing (UTC)",xlab="Hour")
axis(1,0:24)
dev.off()










