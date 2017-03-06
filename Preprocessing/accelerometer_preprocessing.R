accelerometer_preprocessing = function(data_filepath, patient_name, source_filepath, minutes){
  print("Preprocessing accelerometer data")
  print(paste("Aggregating by",minutes,"minutes"))
  code_location = paste(source_filepath,"Preprocessing", "accelerometer_preprocessing.py",sep="/")
  system(paste("python", code_location, data_filepath, patient_name, minutes, sep=" "))
  accmat = read.table(paste(data_filepath, "/", patient_name, "/appended_sheared_file_acc_",minutes,".txt",sep=""), header = TRUE)
  accmat[,1] = accmat[,1] / 1000
  accmat = accmat[,-2]
  accmat[,c("hours","days")] = hours(accmat[,"timestamp"])
  return(accmat)
}