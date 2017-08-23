hours = function(timestamps){ # converts timestamps into hour of the day.
  days = timestamps %>% unlist %>% as.POSIXlt(origin = "1970-01-01") %>%
    as.character %>%
    map(function(x){if(is.na(x)){return(c("NA","NA"))}else{return(strsplit(x, " "))}}) %>%
    unlist %>%
    matrix(nrow=2) %>%
    t
  days = days[,1]
  hours = as.POSIXlt(timestamps, origin = "1970-01-01") %>%
    map(function(timestamp){timestamp %>%
        unclass() %>%
        unlist()}) %>%
    data.frame() %>% 
    cbind(timestamps) %>%
    dplyr::select(hour, min, sec) %>%
    apply(1, function(times) sum(times * c(1, 1/60, 1/3600)))
  output = as_data_frame(cbind(hours=hours, days=days))
  output["hours"] = lapply(output["hours"], as.numeric)
  return(output)
}