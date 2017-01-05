call_preprocessing = function(){
  # input: none
  # output: matrix consisting of the calls for all patients and UNIX time stamps in seconds. (this standard should probably be changed.)
  callmat = NULL
  if(file.exists("calls")){
    call_files = list.files("calls")
	if(length(call_files) > 0){
      callmat = c()
      for(call_file in call_files)
        callmat = rbind(callmat, data = read.csv(paste("calls/",call_file,sep=""),header=T))
      callmat[,1] = callmat[,1] / 1000
      callmat = callmat[,-2]
	  callmat[,c("hours","days")] = hours(callmat[,"timestamp"])
	}
  }
  return(callmat)
}

  
