call_features = function(callmat){
  # input:  callmat, a processed array of calls
  # output: call_features, a processed array of calls features
  day = function(timestamp) strsplit(as.character(as.POSIXct(timestamp,tz="",origin="1970-01-01"))," ")[[1]][1]
  callmat_f = callmat # callmat for the features
  callmat_f[,"day"] = sapply(callmat[,"timestamp"], day)

  outgoing_calls = callmat_f %>%
    group_by(day) %>%
    filter(call.type == "Outgoing Call") %>%
    summarise(outgoing_calls = n())

  outgoing_calllengths = callmat_f %>%
    group_by(day) %>%
    filter(call.type == "Outgoing Call") %>%
    summarise(outgoing_calllengths = sum(duration.in.seconds))

  call_outdegree = callmat_f %>%
    group_by(day) %>%
    filter(call.type == "Outgoing Call") %>%
    select(day, hashed.phone.number) %>%
    distinct(hashed.phone.number) %>%
    summarise(call_outdegree = n())

  incoming_calls = callmat_f %>%
    group_by(day) %>%
    filter(call.type == "Incoming Call") %>%
    summarise(incoming_calls = n())

  incoming_calllengths = callmat_f %>%
    group_by(day) %>%
    filter(call.type == "Incoming Call") %>%
    summarise(incoming_calllengths = sum(duration.in.seconds))

  call_indegree = callmat_f %>%
    group_by(day) %>%
    filter(call.type == "Incoming Call") %>%
    select(day, hashed.phone.number) %>%
    distinct(hashed.phone.number) %>%
    summarise(call_indegree = n())

  send_receive_changes = function(x) sum(abs(diff(as.integer(x == "Outgoing Call"))))
  call_reciprocity = callmat_f %>%
    group_by(day, hashed.phone.number) %>%
    summarise(changes = send_receive_changes(call.type)) %>%
    summarize(reciprocity = sum(changes))

  response_time = function(x, y){# specifically, hours until responded to a call
    where = which((diff(as.integer(x == "Outgoing Call"))) > 0)
    output = mean(y[where+1]-y[where], na.rm=TRUE)
    round(output / 60 / 60, 2)
  }
  call_responsiveness = callmat_f %>% 
    group_by(day, hashed.phone.number) %>%
    summarize(responsiveness = response_time(call.type, timestamp)) %>%
    summarize(responsiveness = mean(responsiveness, na.rm=TRUE))

  call_feature_list = list(outgoing_calls, outgoing_calllengths, call_outdegree, 
                           incoming_calls, incoming_calllengths, call_indegree,
                           call_reciprocity, call_responsiveness)

  call_features = Reduce(full_join, call_feature_list) # reduces each feature to days for which data is available.
  # call_features[,"outgoing_calls"]/call_features[,"incoming_calls"]
  call_features = data.frame(call_features)
  return(call_features)
}
