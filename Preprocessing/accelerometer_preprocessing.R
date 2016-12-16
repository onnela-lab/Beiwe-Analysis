accelerometer_preprocessing = function(minutes){
  print("Preprocessing accelerometer data")
  print(paste("Aggregating by",minutes,"minutes"))
  code_location = paste(source_directory,"Preprocessing", "accelerometer_preprocessing.py",sep="/")
  system(paste("python", code_location, data_directory, patient_name, minutes, sep=" "))
}