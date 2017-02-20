##usage:
## Rscript CreateFeatureMatrix.R datadir simnum

library(stringr)
library(plyr)
library(pryr)
library(dplyr)
library(tidyr)
library(purrr)
detach("package:plyr", unload=TRUE)




### Helper functions:

GetSurveyRow = function(aIDs,qIDs){
  outvec = rep(NA,length(names(qIDs)))
  for(i in 1:length(names(qIDs))){
    if(length(which(names(aIDs)==names(qIDs)[i]))>0){
      outvec[i]=aIDs[[names(qIDs)[i]]]
    }
  }
  return(outvec)
}

# dictionary: key is date, element is dictionary of qIDs


f1 = function(survey_id,SID,aIDs,qIDs,datadir){
  fildir=paste(datadir,SID,"survey_answers",survey_id,sep="/")
  #setwd(fildir)
  filelist <- list.files(fildir,pattern = "\\.csv$")
  if(length(filelist)==0){return(NULL)}
  date_v=substr(filelist,1,10)
  ## keep only the last survey of each day
  IDkeep=length(date_v)
  curdate = date_v[length(date_v)]
  for(i in length(date_v):1){
    nexdate=date_v[i]
    if(curdate!=nexdate){
      IDkeep=c(IDkeep,i)
      curdate=nexdate
    }
  }
  IDkeep=rev(IDkeep)
  
  outmat=c()
  for(i in IDkeep){
    x=read.csv(paste(fildir,filelist[i],sep="/"),fileEncoding="UTF-8")
    if(length(x$question.id)>0){
      if(!exists(date_v[i],where=aIDs)){
        aIDs[[date_v[i]]]=list()
      }
      for(k in 1:length(x$question.id)){
        if(!exists(as.character(x$question.id[k]),where=qIDs)){
          qIDs[[as.character(x$question.id[k])]]=as.character(x$question.text[k])
        }
        if(!exists(as.character(x$question.id[k]),where=aIDs[[date_v[i]]])){
          aIDs[[date_v[i]]][[as.character(x$question.id[k])]]=as.character(x$answer[k])
        }
      }      
    }
  }
  return(list('qIDs'=qIDs,'aIDs'=aIDs))
}

CreateMobilityFeatures = function(patient_id,homedir){
  pcsfilename =paste(homedir,"Processed_data",patient_id,paste("gps_imputed_",patient_id,".Rdata",sep=""),sep="/")
  if(!file.exists(pcsfilename)){
    cat("No imputed data.\n")
    return(NULL)
  }  
  load(pcsfilename)
  prepcsfilename = paste(homedir,"Preprocessed_data",patient_id,paste("gps_preprocessed_",patient_id,".Rdata",sep=""),sep="/")
  if(!file.exists(prepcsfilename)){
    cat("No preprocessed data.\n")
    return(NULL)
  }  
  load(prepcsfilename)
  nreps = length(mobmatsims)
  lsmf = list()
  lssigloc = list()
  for(repnum in 1:nreps){
    out=mobmatsims[[repnum]]
    IDundef=which(out[,1]==3)
    if(length(IDundef)>0){
      out=out[-IDundef,]      
    }
    obj2=objsims[[repnum]]
    out_GMFM=GetMobilityFeaturesMat(out,obj2,mobmatmiss,tz,CENTERRAD,ITRVL)
    lsmf[[repnum]]=out_GMFM[[1]]
    lssigloc[[repnum]]=out_GMFM[[2]]
  }
  cat("\n\n")
  if(length(lsmf)!=0){
    featavg = lsmf[[1]]
    if(nreps>1){
      for(i in 2:nreps){
        featavg=featavg+lsmf[[i]]
      }    
      featavg=featavg/nreps
    }    
    outmat = cbind(rownames(featavg),featavg)
    colnames(outmat)=c("Date",colnames(featavg))
    #write.table(outmat,paste("MobFeatMat_",filename,".txt",sep=""),quote=F,col.names=T,row.names=F,sep="\t")
  }else{
    featavg=NULL
  }
  outfilename = file.path(paste(homedir,"Processed_data",patient_id,sep="/"),paste("MobFeatures_",patient_id,".Rdata",sep=""))
  save(file=outfilename,outmat,lsmf,lssigloc,featavg)  
}




DailyFeatures = function(homedir){
  datadir = paste(homedir,"Data",sep="/")
  SIDs=lapply(strsplit(list.dirs(paste(homedir,"Data",sep="/"),recursive=FALSE),"/"),function(x) x[length(x)])
  qIDs = list()
  aIDs_full = list()
  mob_full = list()
  for(i in 1:length(SIDs)){
    patient_id=SIDs[[i]]
    # GPS
    mobmat=NULL
    aIDs=list()
    mobfilename = file.path(paste(homedir,"Processed_data",patient_id,sep="/"),paste("MobFeatures_",patient_id,".Rdata",sep=""))
    if(file.exists(mobfilename)  ){
      load(mobfilename)
      mobmat = data.frame(outmat,stringsAsFactors=FALSE)
    }
    mob_full[[patient_id]]=mobmat
    # Survey Answers
    if(file.exists(paste(homedir,"Data",patient_id,"survey_answers",sep="/"))){
      SurIDs=lapply(strsplit(list.dirs(paste(homedir,"Data",patient_id,"survey_answers",sep="/"),recursive=FALSE),"/"),function(x) x[length(x)])
      if(length(SurIDs)>0){
        for(j in 1:length(SurIDs)){
          survey_id = SurIDs[[j]]
          svout = f1(survey_id,patient_id,aIDs,qIDs,datadir)
          qIDs=svout$qIDs
          aIDs=svout$aIDs
        }
      }
    }
    aIDs_full[[patient_id]] = aIDs
  }

  outmat=c()
  for(i in 1:length(SIDs)){
    srvydates=names(aIDs_full[[SIDs[[i]]]])
    mobdates=mob_full[[SIDs[[i]]]]$Date
    if(length(mobdates)==0 &&length(srvydates)==0){
      next
    }
    dates=sort(unique(c(srvydates,mobdates)))
    #  indmat=matrix(NA,nrow=length(dates),ncol=length(qIDs)+ncol(mob_full[[SIDs[[i]]]])+1)  
    mobmat=mob_full[[SIDs[[i]]]]
    if(is.null(mobmat)){
      mobmat=data.frame(matrix(rep(NA,16),ncol=16))
      colnames(mobmat)=c("Date",rep(NA,15))
    }
    indmat=matrix(NA,nrow=length(dates),ncol=length(qIDs)+ncol(mobmat)+1)    
    for(j in 1:length(dates)){
      IDmob=which(mobmat$Date==dates[j])
      if(length(IDmob)==1){
        indmat[j,]=c(SIDs[[i]],dates[j],GetSurveyRow(aIDs_full[[SIDs[[i]]]][[dates[j]]],qIDs),as.numeric(mobmat[IDmob,2:ncol(mobmat)]))      
      }else{
        emptymobvec = rep(NA,ncol(mobmat)-1)
        emptymobvec[which(colnames(mobmat)=="MinsMissing")-1]="1440"
        indmat[j,]=c(SIDs[[i]],dates[j],GetSurveyRow(aIDs_full[[SIDs[[i]]]][[dates[j]]],qIDs),emptymobvec)      
      }
    }
    outmat=rbind(outmat,indmat)
  }
  outmat = cbind(outmat,matrix(0,nrow=nrow(outmat),ncol=16))
  textlabels=c("outgoing_texts", "outgoing_textlengths", "text_outdegree","incoming_texts", "incoming_textlengths", "text_indegree","text_reciprocity", "text_responsiveness")
  calllabels=c("outgoing_calls", "outgoing_calllengths", "call_outdegree", "incoming_calls", "incoming_calllengths", "call_indegree","call_reciprocity", "call_responsiveness")
  colnames(outmat)=c("IID","Date",unlist(lapply(names(qIDs),function(x) qIDs[[x]])),colnames(mobmat)[2:(length(colnames(mobmat)))],textlabels,calllabels)
  outmat=data.frame(outmat,stringsAsFactors=F)
  
  
  
  ## create text and call features using Patrick's code and append them to outmat
  day = function(timestamp) strsplit(as.character(as.POSIXct(timestamp,tz="",origin="1970-01-01"))," ")[[1]][1]
  for(i in 1:length(SIDs)){
    setwd(paste(datadir,SIDs[[i]],sep="/")) # change for each patient!  Of course, this can be standardized.
    if(file.exists("texts")){
      textmat = c()
      text_files = list.files("texts")
      for(text_file in text_files)
        textmat = rbind(textmat, data = read.csv(paste("texts/",text_file,sep=""),header=T))
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
      text_features = data.frame(text_features,stringsAsFactors=F)
      IDindmatch=which(outmat[,1]==SIDs[[i]])
      IDoutmatstart=which(colnames(outmat)==textlabels[1])
      for(j in 1:nrow(text_features)){
        IDdatematch=which(outmat[,2]==text_features[j,1])
        IDmatch=intersect(IDindmatch,IDdatematch)
        if(length(IDmatch)==1){
          for(k in 1:length(textlabels)){
            outmat[IDmatch,IDoutmatstart+k-1]=text_features[j,k+1] 
          }
        }
      }
    }
    
    if(file.exists("calls")){
      callmat = c()
      call_files = list.files("calls")
      for(call_file in call_files)
        callmat = rbind(callmat, data = read.csv(paste("calls/",call_file,sep=""),header=T))
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
      
      call_features = data.frame(call_features,stringsAsFactors=F)
      IDindmatch=which(outmat[,1]==SIDs[[i]])
      IDoutmatstart=which(colnames(outmat)==calllabels[1])
      for(j in 1:nrow(call_features)){
        IDdatematch=which(outmat[,2]==call_features[j,1])
        IDmatch=intersect(IDindmatch,IDdatematch)
        if(length(IDmatch)==1){
          for(k in 1:length(calllabels)){
            outmat[IDmatch,IDoutmatstart+k-1]=call_features[j,k+1] 
          }
        }
      }    
    }
  }
  write.table(outmat,file=paste(homedir,"Processed_data","FeatureMatrix.txt",sep="/"),sep="\t",row.names=FALSE,col.names=TRUE)
}
