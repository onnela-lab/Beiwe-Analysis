initialize_output_directory = function(...){
  output_names = c("Preprocessed_Data", "Processed_Data", "Results")  
  for(output_name in output_names){
    group_filepath = paste(output_filepath,output_name,"Group",sep="/")
    if(!file.exists(group_filepath)) dir.create(group_filepath, recursive = TRUE)
    for(patient_name in patient_names){
      individual_filepath = paste(output_filepath,output_name,"Individual",patient_name,sep="/")
      if(!file.exists(individual_filepath)) dir.create(individual_filepath, recursive = TRUE)
    }
  }
  
  for(output_name in output_names){
    group_filepath = paste(output_filepath,output_name,"Group",sep="/")
    if(!file.exists(group_filepath)) dir.create(group_filepath, recursive = TRUE)
    for(patient_name in patient_names){
      individual_filepath = paste(output_filepath,output_name,"Individual",patient_name,sep="/")
      if(!file.exists(individual_filepath)) dir.create(individual_filepath, recursive = TRUE)
    }
  }
}