powerstate_preprocessing = function(){
  # input: none
  # output: matrix consisting of the power state for all patients and UNIX time stamps in seconds. (this standard should probably be changed.)
  if(file.exists("power_state")){
  statemat = c()
  state_files = list.files("power_state")
  for(state_file in state_files)
    statemat = rbind(statemat, data = read.csv(paste("power_state/",state_file,sep=""),header=T))
  statemat[,1] = statemat[,1] / 1000
  statemat = statemat[,-2]
    }else{
      statemat = NULL
    }
    return(statemat)
}
