text_locations = function(textmat, mobmat){
	textlocs = NULL
	textlocs = textmat[,1] %>%
		map(GPSlocation, mobility_matrix = mobmat) %>%
		do.call(rbind, .) %>%
		as.data.frame() %>%
		cbind(textmat[,c("message.length","sent.vs.received")])
	colnames(textlocs) = c("x","y", "code", "timestamp","length", "sent.vs.received")
	textlocs[,c("hours","days")]  = hours(textlocs[,"timestamp"])
	return(textlocs)
}