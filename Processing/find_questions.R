find_questions = function(...){
  questions = list()
  for(patient_name in patient_names){
    patient_survey_filename = paste(output_filepath, "/Preprocessed_Data/Individual/",patient_name, "/survey_data.rds",sep="")
    if(file.exists(patient_survey_filename))
      questions[[patient_name]] = readRDS(patient_survey_filename) %>% dplyr::select(question.text) %>% unlist %>% as.character
  }
  questions = do.call(c, questions) %>% unique
  saveRDS(questions, paste(output_filepath,"/Processed_Data/Group/questions.rds",sep=""))
}

get_questions = function(...)
  readRDS(paste(output_filepath,"/Processed_Data/Group/questions.rds",sep=""))