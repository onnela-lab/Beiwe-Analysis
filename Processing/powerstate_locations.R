powerstate_locations = function(statemat, mobmat){
	statelocs = NULL
	statelocs = statemat[,1] %>%
		map(GPSlocation, mobility_matrix = mobmat) %>%
		do.call(rbind, .) %>%
		as.data.frame() %>%
		cbind(statemat[,c("event")])
	colnames(statelocs) = c("x","y", "event","timestamp","screen")
	statelocs[,c("hours","days")] = hours(unlist(statelocs[,"timestamp"]))
	statemat[,c("hours","days")]  = hours(statemat[,"timestamp"])
	return(statelocs)
}