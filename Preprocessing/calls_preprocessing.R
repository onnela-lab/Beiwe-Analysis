calls_preprocessing = function(){
  # input: none
  # output: matrix consisting of the calls for all patients and UNIX time stamps in seconds. (this standard should probably be changed.)
  if(file.exists("calls")){
    callmat = c()
    call_files = list.files("calls")
    for(call_file in call_files)
      callmat = rbind(callmat, data = read.csv(paste("calls/",call_file,sep=""),header=T))
    callmat[,1] = callmat[,1] / 1000
    callmat = callmat[,-2]
  }else{
    callmat = NULL
  }
  return(callmat)
}

  
