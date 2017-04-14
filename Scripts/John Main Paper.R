################################################################
################    Beiwe Master Pipeline    ###################
################################################################

#source_filepath    = "C:/Users/Ian/Documents/Work/JP/Beiwe/Github/Beiwe-Analysis-master"
#data_filepath      = "C:/Users/Ian/Documents/Work/JP/Schizophrenia/Data"
#output_filepath    = "C:/Users/Ian/Documents/Work/JP/Schizophrenia/Output"

source_filepath    = "C:/Users/Ian/Documents/Work/JP/Beiwe/Github/Beiwe-Analysis"
data_filepath      = "C:/Users/Ian/Documents/Work/JP/MGH_Beiwe/Data"
output_filepath    = "C:/Users/Ian/Documents/Work/JP/MGH_Beiwe/Output"

# source_filepath    = "C:/Phoenix/School/Harvard/Research/Beiwe/Code/Beiwe-Analysis"
# data_filepath      = "C:/Phoenix/School/Harvard/Research/Beiwe/Studies/John_Schizophrenia/Data/2017.01.09"
# output_filepath    = "C:/Phoenix/School/Harvard/Research/Beiwe/Studies/John_Schizophrenia/Output"

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

stream = "accelerometer"
patient_data_filepath = paste(output_filepath, "/Preprocessed_Data/Individual/", patient_name, sep="")
patient_data_filename_TXT = paste(patient_data_filepath, "/", stream, "_bursts.txt",sep="")
code_filepath = paste(source_filepath, "/Preprocessing/find_bursts.py",sep="")
millisecond_divider = acc_millisecond_divider
system(paste("python", code_filepath, data_filepath, patient_data_filename_TXT, patient_name, stream, millisecond_divider))

system("C:/Python33/python.exe C:/Users/Patrick/Desktop/example.py hi")

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
  CreateMobilityFeatures(patient_name)
  
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
patient_trajectories_plots(inds=c(31,40,43))# inds: columns of the feature matrix that will be plotted in patient_trajectories(). Must pick 3.
patient_trajectories_plots(inds=c(3,54,57)) #MGH_Perlis

find_questions()
survey_responsiveness()

summarize_data_quality(stream = "accelerometer", acc_burst_duration, acc_break_duration, acc_frequency)
summarize_data_quality(stream = "gps",           gps_burst_duration, gps_break_duration, gps_frequency)

coverage_over_time("accelerometer")
coverage_over_time("gps")



surveycols = 5:29 # Columns of the feature matrix that correspond to survey questions (to be duplicated)
daysback = 2      # Number of days prior to taking a survey that the survey responses are still pertinent

replicate_survey_responses_backwards(surveycols,daysback) # Create a feature matrix that applies survey responses to the daysback days prior to taking each survey. Essentially adding duplicates.

labels = c("Anxiety","Depression","Meds","Sleep","Psychosis","WSS") # labels for the resulting groupings after combination. Must be the same length as the "groupings" lists. The groupings list specifies which column indices of the input feature matrix should be combined together
groupings = list()
groupings[[1]] = c(5,8,10,16,18,26)
groupings[[2]] = c(7,9,23,24,27,14,17,19)
groupings[[3]] = c(11)
groupings[[4]] = c(7,15,24,25)
groupings[[5]] = c(10,20,28,29)
groupings[[6]] = c(5,6,7,9,14,15,16,23,24,25,27)

combine_survey_responses(surveycols,groupings,labels)



#### Anomaly Detection ####
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

cols = function(n,dirtiness=.25, darkness=.2, transparency=0, ...){
  colors = rainbow(n, ...) 
  f = function(colors, val) strtoi(paste("0x",substr(colors,2*val, 2*val + 1),sep="")) 
  rgb = cbind(f(colors, 1),f(colors, 2),f(colors, 3))
  new_rgba = cbind(floor((rgb+(1-rgb/256)*dirtiness*256)*(1-darkness*dirtiness)),floor(255*(transparency)))
  if(transparency == 0){
    new_rgba = new_rgba[,1:3]
  }else{
    new_rgba[,4] = round(255*(1-transparency))
  }
  CHARS = format(as.hexmode(new_rgba),upper.case=TRUE,width=2)
  if(n==1){CHARMAT = t(as.matrix(CHARS))}else{CHARMAT = matrix(CHARS,ncol=ncol(new_rgba))}
  new_colors = apply(CHARMAT ,1,function(strings) paste("#",paste(strings,collapse=""),sep=""))
  return(new_colors)
}


###################################
####          plotting         ####
###################################

daily_adherence_grid()
plot_data_quality(stream = "accelerometer", acc_frequency, acc_burst_duration, acc_break_duration,legend=FALSE)
plot_data_quality(stream = "gps",           gps_frequency, gps_burst_duration, gps_break_duration,legend=FALSE)
plot_survey_responsiveness(legend=FALSE)
plot_survey_completion(legend=FALSE)
plot_legend("accelerometer")
plot_legend("gps")

plot_accelerometer(minutes = 5, fixed_days = 8*7, use_patient_name = FALSE)

filters = list(
  All_Questions = get_questions(),
  WSS = c("Unable to cope with stress","Feeling tired","Feeling Confused or Puzzled","Feeling depressed or sad", "Difficulty falling asleep", "Difficulty staying asleep", "Waking up too early", "Don't feel rested after waking up", "Feeling nervous; scared; or anxious", "Little interest or pleasure in things", "Trouble concentrating"),
  #Missing_Medications = c("Missing doses of medications", "Missing Doses of Medication", "Missing Doses of Medications"),
  Tired = c("Feeling tired", "Difficulty staying asleep", "Waking up too early", "Difficulty falling asleep", "Don't feel rested after waking up", "Feeling Tired"),
  Anhedonia = c("Feeling Confused or Puzzled","Little interest or pleasure in things","Feeling bad or guilty about yourself","Feeling depressed or sad","Trouble concentrating","Difficulty thinking clearly","Withdrawing from social interaction"),
  Anxiety = c("Unable to cope with stress", "Feeling suspicious","Feeling nervous; scared; or anxious","Trouble relaxing","Poor appetite or overeating")
)


filters = list(
  All_Questions = get_questions(),
  Warning_Signs = c("Unable to cope with stress", "Feeling tired", "Feeling depressed or sad", "Feeling Confused or Puzzled", "Don't feel rested after waking up",  "Feeling nervous, scared, or anxious", "Little interest or pleasure in things", "Trouble concentrating"),
  Mood_and_PHQ8 = c("Little interest or pleasure in things", "Poor appetite or overeating", "Feeling bad or guilty about yourself", "Trouble concentrating",  "Feeling tired", "Feeling depressed or sad", "Trouble relaxing", "Don't feel rested after waking up"),
  #Medications = c("Missing doses of medications", "Missing Doses of Medication", "Missing Doses of Medications"),
  #Negative_and_Anhedonic = c("Withdrawing from social interaction", "Feeling depressed or sad", "Feeling bad or guilty about yourself", "Little interest or pleasure in things"),
  Negative_and_Anhedonic = c("Feeling Confused or Puzzled","Little interest or pleasure in things","Feeling bad or guilty about yourself","Feeling depressed or sad","Trouble concentrating","Difficulty thinking clearly","Withdrawing from social interaction"),
  Sleep = c("Difficulty falling asleep", "Don't feel rested after waking up", "Difficulty staying asleep", "Don't feel rested after waking up"),
  Cognitive = c ("Trouble concentrating", "Difficulty thinking clearly", "Feeling Confused or Puzzled"),
  Psychosis = c("Hearing voices or seeing things", "Feeling suspicious", "Difficulty thinking clearly", "Withdrawing from social interaction"),
  #Positive = c("Easily annoyed or irritated", "Hearing voices or seeing things", "Trouble relaxing", "Feeling nervous, scared, or anxious", "Feeling suspicious"),
  Positive = c("Hearing voices or seeing things", "Feeling nervous; scared; or anxious","Feeling suspicious"),
  #Anxiety_and_GAD7 = c("Unable to cope with stress", "Feeling nervous, scared, or anxious", "Worrying too much",  "Feeling bad or guilty about yourself",  "Feeling Confused or Puzzled", "Trouble relaxing",  "Easily annoyed or irritated"),
  Anxiety_and_GAD7 = c("Unable to cope with stress", "Feeling nervous; scared; or anxious", "Worrying too much","Feeling bad or guilty about yourself", "Trouble relaxing","Poor appetite or overeating")
)
#filters = list(
#  Medications = c("Missing doses of medications", "Missing Doses of Medication", "Missing Doses of Medications")
#)

plot_data_quality_predictiveness(filters)

stream = "gps"
stream = "accelerometer"
coverage = readRDS(paste(output_filepath, "/Preprocessed_Data/Group/", stream, "_coverage.rds", sep="")) %>% data.frame
mean(coverage[1:30,"total_coverage_across_active_patients"])




pdf("C:/Users/Patrick/Desktop/plot.pdf",width=6,height=5)
par(mar = c(5, 4, 4, 2+4) + 0.1)
plot(0,0,xlim=c(1,10),ylim=c(1,10))
polygon(c(2,2,9,9),c(6,8,8,6),col=rgb(.8,.8,.8),border=FALSE)
points(1:10,pch=16)
legend("topright",col=cols(4),legend=1:4,pch=16,cex=1.5,box.col="white",inset=c(-0.2,0),xpd=TRUE)
dev.off()




stream = "accelerometer"
patient_data_filepath = paste(output_filepath, "/Preprocessed_Data/Individual/", patient_name, sep="")
patient_data_filename_TXT = paste(patient_data_filepath, "/", stream, "_bursts.txt",sep="")
system(paste("python", code_filepath, data_filepath, patient_data_filename_TXT, patient_name, stream, millisecond_divider))




