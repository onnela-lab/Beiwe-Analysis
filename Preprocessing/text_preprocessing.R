text_preprocessing = function(){
  # input: none
  # output: matrix consisting of the power state for all patients and UNIX time stamps in seconds. (this standard should probably be changed.)
  textmat = NULL
  if(file.exists("texts")){
    text_files = list.files("texts")
    if(length(text_files) > 0){
      textmat = c()
      for(text_file in text_files)
        textmat = rbind(textmat, data = read.csv(paste("texts/",text_file,sep=""),header=T))
      textmat[,1] = textmat[,1] / 1000
      textmat = textmat[,-2]
      textmat[,c("hours","days")] = hours(textmat[,"timestamp"])
    }
  }
  return(textmat)
}