source_categories = c("Utility", "Preprocessing", "Processing", "Results")
for(source_category in source_categories){
  source_category_filepath = paste(source_filepath,source_category,sep="/")
  file_sources = paste(source_category_filepath,list.files(source_category_filepath, pattern="*.R"),sep="/")
  file_sources = file_sources[which(paste(source_filepath, "Utility/Initialize.R",sep="/") != file_sources)]
  sapply(file_sources,source,.GlobalEnv)
}

non_patients = grep("\\.",list.files(data_filepath))
if(length(non_patients) > 0){
  patient_names = list.files(data_filepath)[-non_patients]
}else{
  patient_names = list.files(data_filepath)
}

initialize_output_directory()

