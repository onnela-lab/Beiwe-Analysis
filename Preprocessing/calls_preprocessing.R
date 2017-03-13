calls_preprocessing = function(patient_name, ...){
  callmat = NULL
  calls_filename = paste(data_filepath,patient_name,"calls",sep="/")
  if(file.exists(calls_filename)){
    call_files = list.files(calls_filename)
    if(length(call_files) > 0){
		callmat = c()
		for(call_file in call_files)
			callmat = rbind(callmat, data = read.csv(paste(calls_filename,"/",call_file,sep=""),header=T))
		callmat[,"timestamp"] = callmat[,"timestamp"] / 1000
		callmat = callmat[,-2]
		callmat[,c("hours","days")] = hours(callmat[,"timestamp"])
    }
  }
  saveRDS(callmat, paste(output_filepath, "/Preprocessed_Data/Individual/",patient_name, "/call_data.rds",sep=""))
}