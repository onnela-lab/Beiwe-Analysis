library(MCMCpack)
library(stringr)
library(pryr)
library(dplyr)
library(purrr)
library(tibble)

sleep_col = rgb(.2,.2,.8)
wake_col = rgb(.8,.2,.2)

filename = "C:/Users/Patrick/Desktop/schizophrenia_patients"
code_location = "C:/Users/Patrick/Desktop/read_accelerometer.py"
setwd(filename)
samples_per_hour = 12

patient_names = c("ws535wyt","x64sum6q","v5k3vk1b","upgskgun","euvxbf3w","dske5c2t") #just restricted to patients with full data for now
#patient_names = "v5k3vk1b"#"upgskgun"#"euvxbf3w"#"x64sum6q"#
for(patient in patient_names){#list.files()
  print(patient)
  acc_file = paste(filename,patient,"accelerometer",sep="/")
  if(file.exists(acc_file)){
    sheared_file = paste(acc_file, "appended_sheared_file.txt",sep="/")
    if(!file.exists(sheared_file)){
      print(paste("working on ", patient))
      system(paste("python", code_location, acc_file, sep=" "))
    }
  }


setwd(paste(filename,patient,sep="/"))
accmat = read.csv(sheared_file,header=T)
accmat[,1] = accmat[,1] / 1000

movement = function(vec) sqrt(sum(vec**2))
accmat[,"Movement"] = apply(accmat[,c("x","y","z")],1,movement)

# SHIFT_FORWARD
forward_shift      <- as.POSIXlt(accmat[,"timestamp"],origin="1970-01-01")
forward_shift$hour <- forward_shift$hour + 12
accmat[,"timestamp"]         <- as.numeric(forward_shift)
# END SHIFT FORWARD

accmat[,c("Days", "Hours","Mins","Secs")] = accmat[,1] %>%
  as.POSIXct(origin="1970-01-01") %>%
  as.character %>%
  strsplit(" |:") %>%
  unlist %>%
  matrix(4) %>% t

file.exists("power_state")
statemat = c()
state_files = list.files("power_state")
for(state_file in state_files)
  statemat = rbind(statemat, data = read.csv(paste("power_state/",state_file,sep=""),header=T))
statemat[,1] = statemat[,1] / 1000
statemat = statemat[,-2]

# SHIFT_FORWARD
forward_shift          <- as.POSIXlt(statemat[,"timestamp"],origin="1970-01-01")
forward_shift$hour     <- forward_shift$hour + 12
statemat[,"timestamp"] <- as.numeric(forward_shift)
# END SHIFT FORWARD


powerstate_indeces = function(tt){
  timing = max(which(tt >= statemat[,"timestamp"]))
  ifelse(timing == -Inf,"Unknown", ifelse(statemat[timing, "event"]=="Screen turned off","Off","On"))
}

accmat[,"Power_State"] = sapply(accmat[,"timestamp"], powerstate_indeces)

hours = function(timestamps){
  days = as.POSIXlt(timestamps, origin = "1970-01-01") %>%
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
    select(hour, min, sec) %>%
    apply(1, function(times) sum(times * c(1, 1/60, 1/3600)))
  output = as_data_frame(cbind(hours=hours, days=days))
  output["hours"] = lapply(output["hours"], as.numeric)
  return(output)
}

statemat[,c("hours","days")] = hours(statemat[,"timestamp"])
daynames = unlist(unique(statemat["days"]))

if(file.exists("survey_answers")){
  survey_responses = NA
  for(survey in list.files("survey_answers")){
    survey_days = list.files(paste("survey_answers", survey, sep="/"))
    for(survey_day in survey_days){
      data = read.csv(paste("survey_answers", survey,survey_day,sep="/"))
      data[,c("Days")] = unlist(strsplit(survey_day, " "))[[1]]
      if(!sum(is.na(survey_responses))==0){survey_responses = data}
      else{survey_responses = rbind(survey_responses,data)}
    }
  }
  sleep_questions = c("Difficulty staying asleep","Difficulty falling asleep","Waking up too early","Don't feel rested after waking up")
  sleep_responses = survey_responses[which(survey_responses[,"question.text"] %in% sleep_questions),]
}


sleepmat = accmat %>% select(Movement, Power_State,Days, Hours, Mins, Secs)
sleepmat[,"Movement"] = sleepmat[,"Movement"]/sd(sleepmat[,"Movement"])
sleepmat[,"Power_State"] = as.numeric(sleepmat[,"Power_State"] == "On")
sleepmat[,"Power_State"] = sleepmat[,"Power_State"]/sd(sleepmat[,"Power_State"])
sleepmat[,"Time"] = as.numeric(sleepmat[,"Hours"]) + as.numeric(sleepmat[,"Mins"])/60 + as.numeric(sleepmat[,"Secs"])/60/60
sleepmat = sleepmat %>% select(Movement, Power_State, Days, Time, Hours)

daynames = unique(sleepmat[,"Days"])
T_est = matrix(NA, length(daynames),2)
T_est_wald = rep(NA,length(daynames))
for(i in seq_along(daynames)){
  sleep_subset = sleepmat %>%
    filter(Days == daynames[i]) %>% group_by(Hours) %>%
    summarise(Move_Mean=mean(Movement), Power_Mean=mean(Power_State))
  sleep_subset[,"Hours"] = as.numeric(unlist(sleep_subset[,"Hours"]))
  obj = matrix(NA,24,24)
  for(j in 2:12){
  for(k in (j+2):24){
  obj[j,k] = log(sleep_subset %>% filter(j>Hours | Hours>=k) %>% dplyr::select(Move_Mean, Power_Mean) %>% unlist %>% mean) - 
    log(sleep_subset %>% filter(j<=Hours, Hours<k) %>% dplyr::select(Move_Mean, Power_Mean) %>% unlist %>% mean)
  }}
  
  maxes = which(obj==max(obj,na.rm=T),arr.ind=T)
  if(dim(maxes)[1]==0){T_est[i,] = c(NA, NA)}
  if(dim(maxes)[1]==1){T_est[i,] = maxes}
  if(dim(maxes)[1]>1){T_est[i,] = maxes[1,]}
  T_est_wald[i] = max(obj,na.rm=T) / sd(obj, na.rm=TRUE)
}

alphas_est = rep(NA,3)
tS_bar = mean(T_est[,1],na.rm=TRUE)/24
tA_bar = 1-mean(T_est[,2],na.rm=TRUE)/24
tS_sd  = sd(T_est[,1],na.rm=TRUE)/24
a0 = tS_bar*(1-tS_bar)/tS_sd - 1# MOM estimator (see your sheet!)
alphas_est[1] = tS_bar * a0
alphas_est[2] = tA_bar * a0
alphas_est[3] = a0 - alphas_est[1] - alphas_est[2]
if(min(alphas_est)<2.5) alphas_est = alphas_est/min(alphas_est)*2.5
T_shrunk = matrix(NA, length(daynames),2)
for(i in seq_along(daynames)){
  sleep_subset = sleepmat %>%
    filter(Days == daynames[i]) %>% group_by(Hours) %>%
    summarise(Move_Mean=mean(Movement), Power_Mean=mean(Power_State))
  sleep_subset[,"Hours"] = as.numeric(unlist(sleep_subset[,"Hours"]))
  obj = matrix(NA,24,24)
  for(j in 2:12){
    for(k in (j+2):23){
      obj[j,k] = (
          log(sleep_subset %>% filter(j>Hours | Hours>=k) %>% select(Move_Mean, Power_Mean) %>% unlist %>% mean) - 
        log(sleep_subset %>% filter(j<=Hours, Hours<k) %>% select(Move_Mean, Power_Mean) %>% unlist %>% mean)
      ) * ddirichlet(c(j/24, 1-k/24, k/24-j/24),alpha=alphas_est)
    }}
  maxes = which(obj==max(obj,na.rm=T),arr.ind=T)
  if(dim(maxes)[1]==0){T_shrunk[i,] = c(NA, NA)}
  if(dim(maxes)[1]==1){T_shrunk[i,] = maxes}
  if(dim(maxes)[1]>1){T_shrunk[i,] = maxes[1,]}
}


pdf("Initial_and_Adjusted.pdf",width=8,height=6)
par(cex.lab=1.5,cex.main=1.5)
plot(0,0,col=NA,xlim=c(0,24),ylim=c(1,nrow(T_est)),axes=FALSE,ylab="Day",xlab="",main="Initial and Centered Estimates")
axis(1,at=c(0 ,12, 24),labels=c("Noon","Midnight","Noon"))
points(T_est[,1],1:nrow(T_est),col=sleep_col,pch=1,cex=1.2)
points(T_est[,2],1:nrow(T_est),col=wake_col,pch=1,cex=1.2)

points(T_shrunk[,1],1:nrow(T_shrunk),col=sleep_col,pch=16,cex=1.2)
points(T_shrunk[,2],1:nrow(T_shrunk),col=wake_col,pch=16,cex=1.2)
box()
legend("bottomleft",legend=c("Initial Sleep","Initial Wake","Centered Sleep","Centered Wake"),
       ncol=1,pch=c(1,1,16,16),col=c(sleep_col,wake_col,sleep_col,wake_col),bg="white")
dev.off()



### Plotting
daynames = unique(accmat[,"Days"])
pdf("sleep.pdf",width=10,height=6)
par(mfrow=c(3,4),mar=c(2,1.5,1,0)+1)
for(i in 1:length(daynames)){#+30, or whatever
  data = accmat[which(accmat[,"Days"]==daynames[i]),c("Hours","Mins","Movement","Power_State")]
  movement = data[,"Movement"]
  movement = movement/max(movement)
  timing = as.numeric(data[,"Hours"])+as.numeric(data[,"Mins"])/60
  plot(0,0, col=F, xlim=c(0,24),ylim=c(-.1,1), type="l", xlab="Time",ylab="Movement",xaxt="n",main=daynames[i])
  axis(1,at=c(0, 12, 24),label=c("Noon", "Midnight", "Noon"))
  for(j in seq_along(timing))
    lines(rep(timing[j],2),c(0,-0.1),col=ifelse(data[j,"Power_State"]=="On",rgb(0,0,1), rgb(1,0,0,.05)),lwd=.2)
  if(length(timing)>1){
    for(j in 1:(length(timing)-1)){
      if(timing[j+1]-timing[j] < .5){ # tests is time between intervals of accelerometer are less than 30 minutes.
        lines(timing[j+0:1], movement[j+0:1])
      }else{lines(timing[j+0:1], movement[j+0:1],col=rgb(.8,.8,.8))}
    }
  }
  surveys_to_print = which(unlist(sleep_responses["Days"]) == daynames[i])
  if(length(surveys_to_print)>0){
    for(j in seq_along(surveys_to_print)){
      text(0+j*5,.8,sleep_responses[surveys_to_print[j],"answer"])
    }
  }
  
  lines(T_shrunk[i,],rep(-.03,2),col=rgb(.2,.6,.8),lwd=3)
  lines(T_est[i,]   ,rep(-.08,2),col=rgb(.8,.2,.6),lwd=3)
}
dev.off()
}

# plot(T_est_wald)








