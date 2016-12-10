library(GPSmobility)
library(stringr)
library(plyr)
library(pryr)
library(dplyr)
library(tidyr)
library(purrr)
library(xtable)
detach("package:plyr", unload=TRUE)

#setwd("C:/Users/Patrick/Desktop/schizophrenia_patients/x64sum6q/gps")
#load("sz_patient.Rdata")
setwd("C:/Users/Patrick/Desktop/schizophrenia_patients/x64sum6q") # change for each patient!  Of course, this can be standardized.



# Texts
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

text_features = Reduce(inner_join, text_feature_list) # reduces each feature to days for which data is available.
xtable(data.frame(text_features))



# Calls
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

call_features = Reduce(inner_join, call_feature_list) # reduces each feature to days for which data is available.

# Both --- You need to think about the length of these lists --- they get reduced!!!
# text_features[,"outgoing_texts"]/text_features[,"incoming_texts"]
# call_features[,"outgoing_calls"]/call_features[,"incoming_calls"]
# head(call_features)
xtable(data.frame(call_features))




