powerstate_preprocessing = function(patient_name, ...){
	power_state_filepath = paste(data_filepath, "/", patient_name, "/power_state",sep="")
	power_state_filename = paste(output_filepath, "/Preprocessed_Data/Individual/",patient_name, "/powerstate_data.rds",sep="")
	if(!file.exists(power_state_filename)){
	  statemat = list()
	  state_files = list.files(power_state_filepath)
	  if(length(state_files)>0){
		  for(state_file in state_files){
			mat = read.csv(paste(power_state_filepath, "/",state_file,sep=""),header=T)
			if(ncol(mat)==3) mat[,"level"] = NA
			if(nrow(mat)>0)
			  statemat[[state_file]] = mat
		  }
		  statemat = do.call(rbind, statemat)
		  statemat[,1] = statemat[,1] / 1000
		  statemat = statemat[,-2]
		  statemat[,c("hours","days")] = hours(statemat[,"timestamp"])
		  saveRDS(statemat, power_state_filename)
		}
	}
}

