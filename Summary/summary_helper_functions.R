day = function(timestamp) strsplit(as.character(as.POSIXct(timestamp,tz="",origin="1970-01-01"))," ")[[1]][1]

gps_summary = function(data_path, output_path, patient_id, timestamp_str, timezone) {
  # First, we look in the patient's "identifier" folder if it is available,
  # to store their device type. If that data isn't downloaded, we skip it.

  # Check if the patient has the "identifier" dataset downloaded
  identifier_data_path = paste0(data_path, "/", patient_id, "/identifiers/")
  if(dir.exists(identifier_data_path)) {
    patient_identifier_files <- list.files(path=identifier_data_path, pattern="\\.csv$")
    identifier_file <- patient_identifier_files[1] # In the case the patient has multiple identifier data files, take the first one.

    # Read in identifier dataset
    identifier_temp_data <- read.csv(paste0(identifier_data_path, identifier_file),
                                     fileEncoding="UTF-8",
                                     row.names=NULL,
                                     stringsAsFactors=F)

    # The 7th column should always be the device type.
    device_type <- identifier_temp_data[,7]
  } else {
    device_type = "Missing"
  }

  # Next, we calculate the GPS summaries as follows:
  gps_input = paste(data_path, patient_id, "gps", sep="/")
  #fildir=gps_input
  #filename=patient_ids[i]

  if(dir.exists(gps_input)) {
    # This function call generates the list with all GPS summary outputs!
    results = MobilityFeatures(patient_id, gps_input, tz=timezone)
  } else {
    results = list()
  }
  results[["patient_id"]] <- patient_id
  results[["device_type"]] <- device_type

  metric_summary_temp <- data.frame()
  if(!is.null(results[["featavg"]])) {
    temp_data <- as.data.frame(results[["featavg"]])
    temp_data$local_date <- rownames(results[["featavg"]])
    temp_time_dates <- as.POSIXct(temp_data$local_date, tz=timezone)
    attr(temp_time_dates, "tzone") <- "UTC"
    temp_data$utc_timecode <- as.numeric(temp_time_dates) * 1000 #stored in milliseconds
    temp_data$days_from_start <- ceiling((temp_data$utc_timecode - min(temp_data$utc_timecode))/1000/60/60/24)
    temp_data$patient_id <- as.factor(results[["patient_id"]])
    metric_summary_temp <- rbind(metric_summary_temp, temp_data)
  }

  # Next, reformat the data so that it is in more interpretable units
  metric_summary <- metric_summary_temp %>% mutate(Hometime_hrs = Hometime/60,
                                                   DistTravelled_km = DistTravelled/1000,
                                                   MaxDiam_km = MaxDiam/1000,
                                                   MaxHomeDist_km = MaxHomeDist/1000,
                                                   AvgFlightLen_km = AvgFlightLen/1000,
                                                   StdFlightLen_km = StdFlightLen/1000,
                                                   AvgFlightDur_min = AvgFlightDur/60,
                                                   StdFlightDur_min = StdFlightDur/60,
                                                   RoG_km = RoG / 1000)
  filtered_metric_summary = metric_summary %>% dplyr::select(patient_id,
                                                             Hometime_hrs,
                                                             DistTravelled_km,
                                                             MaxDiam_km,
                                                             MaxHomeDist_km,
                                                             AvgFlightLen_km,
                                                             StdFlightLen_km,
                                                             AvgFlightDur_min,
                                                             StdFlightDur_min,
                                                             RoG_km)
  write.csv(filtered_metric_summary, file=paste0(output_path, patient_id, "_gps_summaries_", timestamp_str, ".csv"))
}


text_summary = function(data_path, output_path, patient_id, timestamp_str, timezone) {
  text_data_path = paste0(data_path, "/", patient_id, "/texts/")
  if(dir.exists(text_data_path)) {
    textmat = c()
    text_files = list.files(path=text_data_path, pattern="\\.csv$")
    for(text_file in text_files)
      textmat = rbind(textmat, data=read.csv(paste0(text_data_path, text_file), header=T))
    textmat[,1] = textmat[,1] / 1000
    textmat = textmat[,-2]
    textmat_f = textmat # textmat for the features
    textmat_f[,"day"] = sapply(textmat[,"timestamp"], day)

    outgoing_texts = textmat_f %>%
      group_by(day) %>%
      filter(sent.vs.received == "sent SMS") %>%
      summarise(outgoing_texts = n())

    outgoing_textlengths = textmat_f %>%
      group_by(day) %>%
      filter(sent.vs.received == "sent SMS") %>%
      summarise(outgoing_textlengths = sum(as.numeric(message.length)))

    text_outdegree = textmat_f %>%
      group_by(day) %>%
      filter(sent.vs.received == "sent SMS") %>%
      dplyr::select(day, hashed.phone.number) %>%
      distinct(hashed.phone.number) %>%
      summarise(text_outdegree = n())

    incoming_texts = textmat_f %>%
      group_by(day) %>%
      filter(sent.vs.received == "received SMS") %>%
      summarise(incoming_texts = n())

    incoming_textlengths = textmat_f %>%
      group_by(day) %>%
      filter(sent.vs.received == "received SMS") %>%
      summarise(incoming_textlengths = sum(as.numeric(message.length)))

    text_indegree = textmat_f %>%
      group_by(day) %>%
      filter(sent.vs.received == "received SMS") %>%
      dplyr::select(day, hashed.phone.number) %>%
      distinct(hashed.phone.number) %>%
      summarise(text_indegree = n())

    send_receive_changes = function(x) sum(abs(diff(as.integer(x == "sent SMS"))))

    text_reciprocity = textmat_f %>%
      group_by(day, hashed.phone.number) %>%
      summarise(changes = send_receive_changes(sent.vs.received)) %>%
      summarize(reciprocity = sum(changes))

    response_time = function(x, y) { # specifically, hours until responded to a text
      where = which((diff(as.integer(x == "sent SMS"))) > 0)
      output = mean(y[where+1]-y[where], na.rm=TRUE)
      round(output / 60 / 60, 2)
    }
    text_responsiveness = textmat_f %>%
      group_by(day, hashed.phone.number) %>%
      summarize(responsiveness = response_time(sent.vs.received, timestamp)) %>%
      summarize(responsiveness = mean(responsiveness, na.rm=TRUE))

    text_feature_list = list(outgoing_texts, outgoing_textlengths, text_outdegree,
                             incoming_texts, incoming_textlengths, text_indegree,
                             text_reciprocity, text_responsiveness)

    text_features = Reduce(inner_join, text_feature_list) # reduces each feature to days for which data is available.
    text_features = data.frame(text_features, stringsAsFactors=F)
    write.csv(text_features, file=paste0(output_path, patient_id, "_text_summary_", timestamp_str, ".csv"))
  }
}


call_summary = function(data_path, output_path, patient_id, timestamp_str, timezone) {
  call_data_path = paste0(data_path, "/", patient_id, "/calls/")
  if(dir.exists(call_data_path)) {
    callmat = c()
    call_files = list.files(path=call_data_path, pattern="\\.csv$")
    for(call_file in call_files)
      callmat = rbind(callmat, data = read.csv(paste0(call_data_path, call_file), header=T))
    callmat[,1] = callmat[,1] / 1000
    callmat = callmat[,-2]
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
      dplyr::select(day, hashed.phone.number) %>%
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
      dplyr::select(day, hashed.phone.number) %>%
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

    call_features = Reduce(inner_join, call_feature_list) # reduces each feature to days for which data is available.

    call_features = data.frame(call_features,stringsAsFactors=F)
    write.csv(call_features, file=paste0(output_path, patient_id, "_call_summary_", timestamp_str, ".csv"))
  }
}


powerstate_summary = function(data_path, output_path, patient_id, timestamp_str, timezone) {
  powerstate_data_path = paste0(data_path, "/", patient_id, "/power_state/")
  if(dir.exists(powerstate_data_path)) {
    statemat = c()
    state_files = list.files(path=powerstate_data_path, pattern="\\.csv$")
    for(state_file in state_files){
      statemat = rbind(statemat, data = read.csv(paste0(powerstate_data_path, state_file), header=T))
    }
    statemat[,1] = statemat[,1] / 1000
    statemat = statemat[,-2]
    statemat_f = statemat
    statemat_f[,"day"] = sapply(statemat[,"timestamp"], day)

    total_screen_events = statemat_f %>%
      group_by(day) %>%
      filter(event == "Screen turned off" | event == "Screen turned on") %>%
      dplyr::select(day, event) %>%
      summarise(total_screen_events = n())

    # For iPhone only
    #total_unlock_events = statemat_f %>%
      #group_by(day) %>%
      #filter(event == "Unlocked") %>%
      #dplyr::select(day, event) %>%
      #summarise(total_unlock_events = n())

    total_power_events = statemat_f %>%
      group_by(day) %>%
      filter(event == "Power connected" | event == "Power disconnected") %>%
      dplyr::select(day, event) %>%
      summarize(total_power_events = n())

    #power_state_list = list(total_screen_events, total_unlock_events, total_power_events)
    #power_state_features = Reduce(inner_join, power_state_list) # reduces each feature to days for which data is available.
    power_state_features = merge(total_screen_events, total_power_events, by="day", all=T)
    write.csv(power_state_features, file=paste0(output_path, patient_id, "_powerstate_summary_", timestamp_str, ".csv"))
  }
}


accelerometer_summary = function(data_path, output_path, patient_id, timestamp_str, timezone) {
  accelerometer_data_path = paste0(data_path, "/", patient_id, "/accelerometer/")
  if(dir.exists(accelerometer_data_path)) {
    accmat = c()
    acc_files = list.files(path=accelerometer_data_path, pattern="\\.csv$")
    for(acc_file in acc_files){
      accmat = rbind(accmat, data = read.csv(paste0(accelerometer_data_path, acc_file), header=T))
    }
    accmat[,1] = accmat[,1] / 1000
    accmat = accmat[,-2]
    accmat_f = accmat
    accmat_f[,"day"] = sapply(accmat[,"timestamp"], day)

    write.csv(accmat_f, file=paste0(output_path, patient_id, "_accelerometer_summary_", timestamp_str, ".csv"))
  }
}
