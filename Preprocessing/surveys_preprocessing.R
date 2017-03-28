surveys_preprocessing = function(patient_name, ...){
  survey_data = list()
  surveys_data_filepath = paste(data_filepath, patient_name, "survey_answers",sep="/")
  if(file.exists(surveys_data_filepath)){
    survey_names = list.files(surveys_data_filepath)
    for(survey_name in survey_names){
      surveys_filepath = paste(surveys_data_filepath,survey_name,sep="/")
      specific_surveys = list.files(surveys_filepath)
      for(specific_survey in specific_surveys){
        specific_survey_filepath = paste(surveys_filepath,specific_survey,sep="/")
        specific_survey_data=read.csv2(specific_survey_filepath,header=T,sep=",")
        if(nrow(specific_survey_data)>0){
          date = as.POSIXct(gsub("_",":",gsub(".csv","",specific_survey)),origin="1970-01-01")
          timestamp = as.numeric(date)
          specific_survey_data[,"survey_id"]      = survey_name
          specific_survey_data["date"]            = date
          specific_survey_data[,"patient_name"]   = patient_name
          specific_survey_data[,"timestamp"]      = timestamp
          specific_survey_data[,"date"]           = as.Date(date)
          survey_data[[specific_survey_filepath]] = specific_survey_data
        }
      }
    }
  survey_data = do.call(rbind, survey_data)
  rownames(survey_data) = seq(nrow(survey_data))
  saveRDS(survey_data, paste(output_filepath, "/Preprocessed_Data/Individual/",patient_name, "/survey_data.rds",sep=""))
  }
}