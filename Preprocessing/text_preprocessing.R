text_preprocessing = function(data_filepath, patient_name){
  # input: none
  # output: matrix consisting of the power state for all patients and UNIX time stamps in seconds. (this standard should probably be changed.)
  textmat = NULL
  texts_filename = paste(data_filepath,patient_name,"texts",sep="/")
  if(file.exists(texts_filename)){
    text_files = list.files(texts_filename)
    if(length(text_files) > 0){
		textmat = c()
		for(text_file in text_files)
			textmat = rbind(textmat, data = read.csv(paste(texts_filename,"/",text_file,sep=""),header=T))
		textmat[,"timestamp"] = textmat[,"timestamp"] / 1000
		textmat[,"time.sent"] = textmat[,"time.sent"] / 1000
		textmat = textmat[,-2]
		textmat[,c("hours","days")] = hours(textmat[,"timestamp"])
    }
  }
  return(textmat)
}