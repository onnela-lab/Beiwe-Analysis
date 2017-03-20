survey_responsiveness = function(...){
  total = as.data.frame(matrix(NA,10000,5))
  colnames(total) = c("Person","Survey_ID","Notified","Present","Submitted")
  j=1
  for(patient_name in patient_names){
    print(patient_name)
    patient_name_surveys_filepath = paste(data_filepath, patient_name, "survey_timings",sep="/")
    surveys = list.files(patient_name_surveys_filepath)
    for(survey in surveys){
      patient_name_survey_answered_filepath = paste(patient_name_surveys_filepath,"/",survey,sep="")
      patient_name_surveys_answered = list.files(patient_name_survey_answered_filepath)
      for(patient_name_survey_answered in patient_name_surveys_answered){
        data=read.csv2(paste(patient_name_survey_answered_filepath, "/", patient_name_survey_answered,sep=""),sep=",")
        notified=NA;      present=NA;      submitted=NA
        if("event" %in% colnames(data)){ # double check these are the only cases where surveys are filled out!
          notified = data[which(data[,"event"]=="notified"),"timestamp"][1]
          present = data[which(data[,"event"]=="present"),"timestamp"][1]
          submitted = data[which(data[,"event"]=="submitted"),"timestamp"][1]
        }else{
          notified_id = which(data[,"question.id"] == "Survey first rendered and displayed to user")
          notified = data[notified_id, "timestamp"][1]
          if(!is.na(notified_id[1])){if(notified_id[1]+1<=nrow(data)){present = data[notified_id[1]+1, "timestamp"][1]}}
          submitted = data[which(data[,"question.id"] == "User hit submit"), "timestamp"][1]
        }
        total[j,] = c(patient_name, survey, notified, present, submitted)
        j = j+1  
      }
    }
  }
  total = total[apply(total,1,function(x){sum(is.na(x))<5}),]
  for(column in c("Notified","Present","Submitted"))
    total[,column] = as.character(as.numeric(as.character(total[,column]))%/%1000)
  
  curated_total = as.data.frame(matrix(NA,nrow(total),5))
  colnames(curated_total) = c("Person","Survey_ID","Notified","Present","Submitted")
  notified=NA;      present=NA;      submitted=NA
  j=1
  find_present = FALSE
  for(i in 1:nrow(total)){
    if(sum(is.na(total[i,c("Notified", "Present", "Submitted")]))==0){#if nothing's wrong, record it.
      curated_total[j,] = total[i,]
      j = j+1
    }
    
    if(!is.na(total[i,"Notified"]) & is.na(total[i,"Submitted"])){#if they are notified but did not submit,
      notified = total[i,"Notified"] # store the notification,
      find_present = TRUE            # and flag that you would like to find when they next show up.
    }
    
    if(find_present & !is.na(total[i,"Present"])){ # if you are looking for them to show up (because they were notified earlier) AND they are present,
      present = total[i,"Present"] # mark down their presence, and
      find_present = FALSE         # stop looking.
    }
    
    if(is.na(total[i,"Notified"]) & !is.na(total[i,"Submitted"])){ # If they submit but it's a different time than when they were notified,
      curated_total[j,] = c(total[i,c("Person","Survey_ID")],notified,present,total[i,"Submitted"]) # I assume I know when they were notified and present, so I record those, as well when they submit.
      notified=NA;      present=NA;      submitted=NA # now I can refresh these. This is not strictly necessary, but it will help with debugging later.
      j = j+1
    }
  }
  curated_total = curated_total[apply(curated_total,1,function(x){sum(is.na(x))==0}),]
  print(nrow(curated_total))
  
  for(column in c("Notified","Present","Submitted"))
    curated_total[,column] = as.numeric(as.character(curated_total[,column]))
  
  saveRDS(curated_total, paste(output_filepath, "/Processed_Data/Group/survey_responsiveness.rds", sep=""))
}