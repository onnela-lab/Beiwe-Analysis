call_locations = function(callmat, mobmat){
	calllocs = NULL
	calllocs = callmat[,1] %>%
		map(GPSlocation, mobility_matrix = mobmat) %>%
		do.call(rbind, .) %>%
		as.data.frame() %>%
		cbind(callmat[,c("duration.in.seconds","call.type")])
	colnames(calllocs) = c("x","y", "code", "timestamp","length","call.type")
	calllocs[,c("hours","days")]  = hours(calllocs[,"timestamp"])
	return(calllocs)
}