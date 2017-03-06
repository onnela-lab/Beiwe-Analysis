powerstate_preprocessing = function(data_filepath, patient_name){
  # input: none
  # output: matrix consisting of the power state for all patients and UNIX time stamps in seconds. (this standard should probably be changed.)
  power_state_filename = paste(data_filepath,patient_name,"power_state",sep="/")
  if(file.exists(power_state_filename)){
  statemat = c()
  state_files = list.files(power_state_filename)
  for(state_file in state_files)
    statemat = rbind(statemat, data = read.csv(paste(power_state_filename,"/",state_file,sep=""),header=T))
  statemat[,1] = statemat[,1] / 1000
  statemat = statemat[,-2]
  statemat[,c("hours","days")] = hours(statemat[,"timestamp"])
    }else{
      statemat = NULL
    }
    return(statemat)
}
