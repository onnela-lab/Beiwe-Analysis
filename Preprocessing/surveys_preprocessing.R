surveys_preprocessing = function(){
  survey_data = list()
  persons = list.files(data_filepath)[-grep("\\.",list.files(data_filepath))]
  for(person in persons){
    print(person)
    surveys_data_filepath = paste(data_filepath,person,"survey_answers",sep="/")
    if(file.exists(surveys_data_filepath)){
      survey_names = list.files(surveys_data_filepath)
      for(survey_name in survey_names){
        surveys_filepath = paste(surveys_data_filepath,survey_name,sep="/")
        specific_surveys = list.files(surveys_filepath)
        for(specific_survey in specific_surveys){
          specific_survey_filepath = paste(surveys_filepath,specific_survey,sep="/")
          specific_survey_data=read.csv2(specific_survey_filepath,header=T,sep=",")
          date = as.POSIXct(gsub("_",":",gsub(".csv","",specific_survey)),origin="1970-01-01")
          timestamp = as.numeric(date)
          specific_survey_data[,"survey_id"] = survey_name
          specific_survey_data["date"]       = date
          specific_survey_data[,"person"]   = person
          specific_survey_data[,"timestamp"] = timestamp
          specific_survey_data[,"date"]      = as.Date(date)
          survey_data[[specific_survey_filepath]] = specific_survey_data
        }
      }
    }
  }
  survey_data = do.call(rbind, survey_data)
  rownames(survey_data) = seq(nrow(survey_data))
  return(survey_data)
}
