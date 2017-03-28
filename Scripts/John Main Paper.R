################################################################
################    Beiwe Master Pipeline    ###################
################################################################

#source_filepath    = "C:/Users/Ian/Documents/Work/JP/Beiwe/Github/Beiwe-Analysis-master"
#data_filepath      = "C:/Users/Ian/Documents/Work/JP/Schizophrenia/Data"
#output_filepath    = "C:/Users/Ian/Documents/Work/JP/Schizophrenia/Output"

source_filepath    = "C:/Phoenix/School/Harvard/Research/Beiwe/Code/Beiwe-Analysis"
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


###################################
####          plotting         ####
###################################

daily_adherence_grid()
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
  Positive = c("Hearing voices or seeing things","Feeling suspicious","Feeling nervous; scared; or anxious"),
  #Anxiety_and_GAD7 = c("Unable to cope with stress", "Feeling nervous, scared, or anxious", "Worrying too much",  "Feeling bad or guilty about yourself",  "Feeling Confused or Puzzled", "Trouble relaxing",  "Easily annoyed or irritated"),
  Anxiety_and_GAD7 = c("Unable to cope with stress", "Feeling nervous; scared; or anxious", "Worrying too much","Feeling bad or guilty about yourself", "Trouble relaxing","Poor appetite or overeating")
)
#filters = list(
#  Medications = c("Missing doses of medications", "Missing Doses of Medication", "Missing Doses of Medications")
#)

plot_data_quality_predictiveness(filters)



################# # # # # # #














































# current work
function(filters, ...){
  pdf(paste(output_filepath, "/Results/Group/Data_Quality_Predictiveness.pdf",sep=""),width=6,height=6.4)
  for(filter in 1:length(names(filters))){
    name_filter = gsub("_"," ",names(filters)[filter])
    print(name_filter)
    coefs=t(sapply(0:5,function(x){y=summary(model_data_quality_predictiveness(filters[[filter]], x))$coef[,3];sign(y)*-log10((1-pnorm(abs(y)))*2)}))
    colnames(coefs) = c("(int)", "Acc", "GPS", "View", "Sub", "Comp")
    
    plot(0,0,col=NA,yaxt="n",xaxt="n",xlab="Weeks Until Prediction",ylab="Data Quality Metric",xlim=c(1.04,7-.04),ylim=c(1.04,7-.04),axes=F,main=name_filter)
    axis(1,at=1:6+.5, labels = 6-1:6)
    axis(2,at=1:6+.5, labels = colnames(coefs))
    for(i in 1:6){
      for(j in 1:6){
        C=1-min(abs(coefs[i,j]),3)/3
        if(coefs[i,j] > 0)  polygon(7-i+c(0,0,1,1), j+c(0,1,1,0),col=rgb(C,C,1),pch=15,cex=2,border=FALSE)
        if(coefs[i,j] <= 0) polygon(7-i+c(0,0,1,1), j+c(0,1,1,0),col=rgb(1,C,C),pch=15,cex=2,border=FALSE)
        
        if(coefs[i,j] > 0)  polygon(7-i+c(0,0,1,1), j+c(0,1,1,0),col=cols(1,start=.6,transparency=C,dirtiness = .05+.15*(1-C)),pch=15,cex=2,border=FALSE)
        if(coefs[i,j] <= 0) polygon(7-i+c(0,0,1,1), j+c(0,1,1,0),col=cols(1,start=0, transparency=C,dirtiness = .05+.2*(1-C)),pch=15,cex=2,border=FALSE)
      }
    }
    for(i in 1:6){
      for(j in 1:6){
        if(abs(coefs[i,j]) > -log10(0.05)) polygon(7-i+c(0,0,1,1), j+c(0,1,1,0),col=NA,lwd=2)
      }
    }
  }
  dev.off()
}


function(questions_filter, SHIFT){
  patient_names = list.files(data_filepath)[-grep("\\.",list.files(data_filepath))]
  weekly_coverage = function(stream, shift, ...){
    data = readRDS(paste(output_filepath, "/Preprocessed_Data/Group/", stream, "_bursts.rds", sep="")) %>% data.frame %>% subset(complete.cases(.))
    data[,"zeroed_week"] = data[,"zeroed"] %/% 7 + shift
    return(
      data %>% group_by(patient, zeroed_week) %>% summarize(
        #mean_num_bursts_coverage = mean(num_bursts_coverage),
        #mean_within_burst_length_coverage = mean(within_burst_length_coverage),
        #mean_within_burst_frequency_coverage = mean(within_burst_frequency_coverage)
        mean_total_coverage = sum(total_coverage)/7
      )
    )
  }
  create_zeroed_columns = function(data, shift){
    data[,"zeroed"] = 0
    mins = data %>% group_by(patient) %>% summarise(min_date = min(numeric_date))
    for(pat in patient_names){
      sub = which(data[,"patient"]==pat)
      data[sub,"zeroed"] = data[sub,"numeric_date"]-unlist((mins %>% filter(patient == pat))[,"min_date"])
    }
    data[,"zeroed_week"] = data[,"zeroed"] %/% 7 + shift
    return(data)
  }
  
  
  weekly_accelerometer = weekly_coverage("accelerometer", shift = SHIFT)
  weekly_gps = weekly_coverage("gps", shift = SHIFT)
  
  timings = readRDS(paste(output_filepath, "/Processed_Data/Group/survey_timings.rds", sep="")) %>% data.frame %>% subset(complete.cases(.))
  timings[,"numeric_date"] =as.numeric(as.Date(as.POSIXct(timings[,"Notified"],origin="1970-01-01")))
  colnames(timings)[which(colnames(timings) == "Person")] = "patient"
  timings = timings %>% data.frame
  timings[,"time_to_present"] = log10(timings[,"Present"]-timings[,"Notified"])
  timings[,"time_to_submitted"] = log10(timings[,"Submitted"]-timings[,"Present"])
  timings = create_zeroed_columns(timings, shift = SHIFT)
  timings = timings[complete.cases(timings)&(!is.infinite(timings[,"time_to_present"]))&(!is.infinite(timings[,"time_to_submitted"])),]
  weekly_timings = timings %>% group_by(patient, zeroed_week) %>% summarize(
    mean_time_to_present = mean(time_to_present),
    mean_time_to_submitted = mean(time_to_submitted)
  ) %>% data.frame
  
  surveys = list()
  for(patient_name in patient_names){
    patient_survey_filename = paste(output_filepath, "/Preprocessed_Data/Individual/",patient_name, "/survey_data.rds",sep="")
    if(file.exists(patient_survey_filename))
      surveys[[patient_name]] = readRDS(patient_survey_filename) %>% dplyr::filter(question.text %in% questions_filter) %>% group_by(survey_id, timestamp) %>% summarize(count=n(), mean_score = mean(as.numeric(answer),na.rm=T), completion = sum(!is.na(as.numeric(answer)))) %>% data.frame %>% mutate(patient = patient_name)
  }
  surveys = do.call(rbind, surveys)
  surveys[,"date"] = as.factor(as.Date(as.POSIXct(surveys[,"timestamp"],origin="1970-01-01")))
  surveys[,"numeric_date"] = as.numeric(surveys[,"date"])
  surveys = create_zeroed_columns(surveys, shift = 0)
  weekly_surveys = surveys %>% group_by(patient, zeroed_week) %>% summarize(mean_score = mean(mean_score), completion = sum(completion))
  
  
  
  data=Reduce(function(...) merge(..., all = TRUE, by = c("patient", "zeroed_week")), 
              list(weekly_accelerometer, weekly_gps, weekly_timings, weekly_surveys))
  #data[which(is.infinite(data[,"mean_within_burst_frequency_coverage.y"])),"mean_within_burst_frequency_coverage.y"] = NA
  
  
  
  mod = lmer(mean_score~mean_total_coverage.x + mean_total_coverage.y + 
               mean_time_to_present + mean_time_to_submitted + completion + (1|patient), data=data)
  return(mod)
}














