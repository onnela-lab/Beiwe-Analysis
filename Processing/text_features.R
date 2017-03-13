text_features = function(textmat){
  # input:  textmat, a processed array of texts
  # output: text_features, a processed array with text features
  day = function(timestamp) strsplit(as.character(as.POSIXct(timestamp,tz="",origin="1970-01-01"))," ")[[1]][1]
  textmat_f = textmat # textmat for the features
  textmat_f[,"day"] = sapply(textmat[,"timestamp"], day)

  outgoing_texts = textmat_f %>%
    group_by(day) %>%
    filter(sent.vs.received == "sent SMS") %>%
    summarise(outgoing_texts = n())

  outgoing_textlengths = textmat_f %>%
    group_by(day) %>%
    filter(sent.vs.received == "sent SMS") %>%
    summarise(outgoing_textlengths = sum(message.length))

  text_outdegree = textmat_f %>%
    group_by(day) %>%
    filter(sent.vs.received == "sent SMS") %>%
    select(day, hashed.phone.number) %>%
    distinct(hashed.phone.number) %>%
    summarise(text_outdegree = n())

  incoming_texts = textmat_f %>%
    group_by(day) %>%
    filter(sent.vs.received == "received SMS") %>%
    summarise(incoming_texts = n())

  incoming_textlengths = textmat_f %>%
    group_by(day) %>%
    filter(sent.vs.received == "received SMS") %>%
    summarise(incoming_textlengths = sum(message.length))

  text_indegree = textmat_f %>%
    group_by(day) %>%
    filter(sent.vs.received == "received SMS") %>%
    select(day, hashed.phone.number) %>%
    distinct(hashed.phone.number) %>%
    summarise(text_indegree = n())

  send_receive_changes = function(x) sum(abs(diff(as.integer(x == "sent SMS"))))
  text_reciprocity = textmat_f %>%
    group_by(day, hashed.phone.number) %>%
    summarise(changes = send_receive_changes(sent.vs.received)) %>%
    summarize(reciprocity = sum(changes))

  response_time = function(x, y){# specifically, hours until responded to a text
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

  text_features = Reduce(full_join, text_feature_list) # reduces each feature to days for which data is available.
  text_features = data.frame(text_features)
  # text_features[,"outgoing_texts"]/text_features[,"incoming_texts"]
  return(text_features)
}


