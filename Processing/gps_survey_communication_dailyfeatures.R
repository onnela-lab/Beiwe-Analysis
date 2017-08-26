##usage:
## Rscript CreateFeatureMatrix.R datadir simnum

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

CreateMobilityFeatures = function(patient_name, ...){
  # input processed imputed GPS data
  patient_input_filepath = paste(output_filepath, "/Processed_Data/Individual/", patient_name, sep="")
  patient_input_filename = paste(patient_input_filepath, "/gps_imputed_mobmat.rds",sep="")
  if(!file.exists(patient_input_filename)){
    cat("No imputed data.\n")
    return(NULL)
  }  
  inlist=readRDS(patient_input_filename)
  mobmatsims=inlist[[1]];objsims=inlist[[2]]
  
  #input preprocessed GPS data
  patient_input_filepath = paste(output_filepath, "/Preprocessed_Data/Individual/", patient_name, sep="")
  patient_input_filename = paste(patient_input_filepath, "/gps_preprocessed.rds",sep="")
  if(!file.exists(patient_input_filename)){
    cat("No preprocessed data.\n")
    return(NULL)
  }  
  inlist=readRDS(patient_input_filename)
  mobmat=inlist[[1]];mobmatmiss=inlist[[2]];obj=inlist[[3]];tz=inlist[[4]];CENTERRAD=inlist[[5]];ITRVL=inlist[[6]]
  
  #output feature file
  patient_output_filepath = paste(output_filepath, "/Processed_Data/Individual/", patient_name, sep="")
  patient_output_filename = paste(patient_output_filepath,"/MobFeatures.rds",sep="")
  
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
  saveRDS(list(outmat,lsmf,lssigloc,featavg),patient_output_filename)  
}


# svout = f1(survey_id,patient_id,aIDs,qIDs,data_filepath)
# SID=patient_id
# datadir=data_filepath

daily_features = function(...){
  patient_output_filepath = paste(output_filepath, "/Processed_Data/Group",sep="")
  patient_output_filename = paste(patient_output_filepath, "/feature_matrix.rds",sep="")
  SIDs=patient_names
  qIDs = list()
  aIDs_full = list()
  mob_full = list()
  for(i in 1:length(SIDs)){
    patient_id=SIDs[[i]]
    # GPS
    mobmat=NULL
    aIDs=list()
    patient_input_filepath = paste(output_filepath, "/Processed_Data/Individual/", patient_id, sep="")
    patient_input_filename = paste(patient_input_filepath,"/MobFeatures.rds",sep="")
    if(file.exists(patient_input_filename)){
      inlist=readRDS(patient_input_filename)
      #list(outmat,lsmf,lssigloc,featavg)
      mobmat = data.frame(inlist[[1]],stringsAsFactors=FALSE)
    }
    mob_full[[patient_id]]=mobmat
    # Survey Answers
    patient_survey_filepath = paste(data_filepath,patient_id,"survey_answers",sep="/")
    if(file.exists(patient_survey_filepath)){
      SurIDs=lapply(strsplit(list.dirs(patient_survey_filepath,recursive=FALSE),"/"),function(x) x[length(x)])
      if(length(SurIDs)>0){
        for(j in 1:length(SurIDs)){
          survey_id = SurIDs[[j]]
          svout = f1(survey_id,patient_id,aIDs,qIDs,data_filepath)
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
  mobmatcolnames=c("Hometime","DistTravelled","RoG","MaxDiam","MaxHomeDist","SigLocsVisited","AvgFlightLen","StdFlightLen","AvgFlightDur","StdFlightDur","ProbPause","SigLocEntropy","MinsMissing","CircdnRtn","WkEndDayRtn")
  colnames(outmat)=c("IID","Date",unlist(lapply(names(qIDs),function(x) qIDs[[x]])),mobmatcolnames,textlabels,calllabels)
  outmat=data.frame(outmat,stringsAsFactors=F)
  
  
  
  ## create text and call features using Patrick's code and append them to outmat
  day = function(timestamp) strsplit(as.character(as.POSIXct(timestamp,tz="",origin="1970-01-01"))," ")[[1]][1]
  for(i in 1:length(SIDs)){
    setwd(paste(data_filepath,SIDs[[i]],sep="/")) # change for each patient!  Of course, this can be standardized.
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
        summarise(outgoing_textlengths = sum(as.numeric(message.length)))
      
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
        summarise(incoming_textlengths = sum(as.numeric(message.length)))
      
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
  saveRDS(list(outmat),patient_output_filename)
  write.table(outmat,file=paste(patient_output_filepath,"/feature_matrix.txt",sep="/"),sep="\t",row.names=FALSE,col.names=TRUE)
}





ExtractTimePeriod = function(tstart,tend,out){
  tstart = as.POSIXct(tstart)
  tend = as.POSIXct(tend)
  INDs=intersect(which(apply(out[,c(4,7)],1,function(x) max(x,na.rm=T))>=tstart),which(out[,4]<=tend))
  suboutmat=out[INDs,]  
  return(suboutmat)
}

SimulateMobilityGaps = function(suboutmat,obj,wtype="TL",spread_pars=c(1,10)){
  ind11=obj$ind11;ind12=obj$ind12;fd=obj$fd;ft=obj$ft;fts=obj$fts;fa=obj$fa;pt=obj$pt;pts=obj$pts;allts=obj$allts;phatall=obj$phatall;fxs=obj$fxs;fys=obj$fys;pxs=obj$pxs;pys=obj$pys;allxs=obj$allxs;allys=obj$allys
  if(nrow(suboutmat)==0){
    return(suboutmat)
  }
  foutmat_ls = list()
  counter=1
  foutmat=c()
  for(i in 1:nrow(suboutmat)){
    if(suboutmat[i,1]==1){
      curx=suboutmat[i,5]
      cury=suboutmat[i,6]
      #foutmat=rbind(foutmat,suboutmat[i,])
      foutmat_ls[[counter]]=suboutmat[i,]
      counter=counter+1
    }else  if(suboutmat[i,1]<=3){
      curx=suboutmat[i,2] 
      cury=suboutmat[i,3]
      #foutmat=rbind(foutmat,suboutmat[i,])
      foutmat_ls[[counter]]=suboutmat[i,]
      counter=counter+1
    }
    if(suboutmat[i,1]==4 && i>1 && i<nrow(suboutmat)){
      varmult=1
      while(TRUE){
        fw=dnorm((fts-mean(c(suboutmat[i,4],suboutmat[i,7])))/(varmult*(suboutmat[i,7]-suboutmat[i,4])))
        pw=dnorm((pts-mean(c(suboutmat[i,4],suboutmat[i,7])))/(varmult*(suboutmat[i,7]-suboutmat[i,4])))
        allw=dnorm((allts-mean(c(suboutmat[i,4],suboutmat[i,7])))/(varmult*(suboutmat[i,7]-suboutmat[i,4])))
        if(length(pts)>0 && length(fts)>0 && sum(fw)>0 && sum(pw)>0){break}
        if(length(pts)==0 && length(fts)>0 && sum(fw)>0){break}
        if(length(fts)==0 && length(pts)>0 && sum(pw)>0){break}
        varmult=varmult*2
      }
      s11=sum(allw[ind11],na.rm=T)
      s12=sum(allw[ind12],na.rm=T)
      if(s11+s12==0){phatcur=phatall}else{phatcur=s12/(s11+s12)}
      if(wtype=="LI"){
        #foutmat=rbind(foutmat,c(1,curx,cury,suboutmat[i,4],suboutmat[i+1,2],suboutmat[i+1,3],suboutmat[i,7]))
        foutmat_ls[[counter]]=c(1,curx,cury,suboutmat[i,4],suboutmat[i+1,2],suboutmat[i+1,3],suboutmat[i,7])
        counter=counter+1
      }else{
        rbout=matrix(RandomBridge(x0=curx,y0=cury,x1=suboutmat[i+1,2],y1=suboutmat[i+1,3],t0=suboutmat[i,4],t1=suboutmat[i,7],fd=fd,ft=ft,fts=fts,fa=fa,fw=fw,probp=phatcur,pt=pt,pts=pts,pw=pw,allts=allts,allw=allw,ind11=ind11,ind12=ind12,i_ind=i,pxs=pxs,pys=pys,fxs=fxs,fys=fys,allxs=allxs,allys=allys,wtype=wtype,canpause=suboutmat[i-1,1]==1,niter=100,spread_pars=spread_pars),ncol=7)
        #foutmat=rbind(foutmat,rbout)        
        foutmat_ls[[counter]]=rbout
        counter=counter+1
      }
    }
  }  
  foutmat=do.call(rbind,foutmat_ls)
  return(foutmat)
}






MaxDistBetweenTrajectories = function(mat1,mat2,t_gap=1){
  mat1=matrix(mat1,ncol=7);mat2=matrix(mat2,ncol=7)
  t0=mat1[1,4];t1=mat2[nrow(mat2),7]
  t_mesh = seq(t0,t1,t_gap)
  d_v = rep(0,length(t_mesh))
  for(i in 1:length(t_mesh)){
    ID1=intersect(which(mat1[,4]<=t_mesh[i]),which(mat1[,7]>=t_mesh[i]))[1]
    if(mat1[ID1,1]==2){
      x1=mat1[ID1,2]
      y1=mat1[ID1,3]
    }else{
      w1=(t_mesh[i]-mat1[ID1,4])/(mat1[ID1,7]-mat1[ID1,4])
      x1=mat1[ID1,2]*(1-w1)+mat1[ID1,5]*w1
      y1=mat1[ID1,3]*(1-w1)+mat1[ID1,6]*w1
    }
    ID2=intersect(which(mat2[,4]<=t_mesh[i]),which(mat2[,7]>=t_mesh[i]))[1]
    if(mat2[ID2,1]==2){
      x2=mat2[ID2,2]
      y2=mat2[ID2,3]
    }else{
      w2=(t_mesh[i]-mat2[ID2,4])/(mat2[ID2,7]-mat2[ID2,4])
      x2=mat2[ID2,2]*(1-w2)+mat2[ID2,5]*w2
      y2=mat2[ID2,3]*(1-w2)+mat2[ID2,6]*w2
    }
    d_v[i] = sqrt((x1-x2)^2+(y1-y2)^2)
  }
  return(max(d_v))
}


SigLocs = function(mobmat,obj,CENTERRAD=125,MINPAUSETIME=600,tz=""){
  if(length(obj$ID2)==0){
    warning("No pauses in mobmat within function SigLocs!")
    return(NULL)
  }else if(length(obj$ID2)==1){
    outmat=data.frame('x'=mobmat[obj$ID2[1],2],'y'=mobmat[obj$ID2[1],3],'timepresent'=c(0),'home'=c(1))
    nrowfc=1
  }else{  
    ptred=floor(obj$pt/MINPAUSETIME)
    if(length(which(ptred>0))<2){
      warning("No pauses long enough in mobmat within function SigLocs!")
      return(NULL)
    }
    pmat_ls = list()
    counter=1
    pmat=c()
    for(i in 1:length(obj$ID2)){
      if(ptred[i]>0){
        pmat=rbind(pmat,matrix(rep(mobmat[obj$ID2[i],2:3],ptred[i]),ncol=2,byrow=T))
        pmat_ls[[counter]]=matrix(rep(mobmat[obj$ID2[i],2:3],ptred[i]),ncol=2,byrow=T)
        counter=counter+1
      }
    }
    pmat = do.call(rbind,pmat_ls)
    kmeansk_v=2:length(which(ptred>0))
    lsfit = list()
    for(i in 1:length(kmeansk_v)){
      kmeansk = kmeansk_v[i]
      fit = kmeans(pmat,centers=kmeansk)
      lsfit[[i]]=fit
      if(min(dist(fit$centers))<CENTERRAD*2 || i==length(kmeansk_v)){
        if(i>1){
          kmeansk=kmeansk_v[i-1]
          fit = lsfit[[i-1]]      
        }
        break
      }
    }
    nrowfc = nrow(fit$centers)
    outmat=data.frame('x'=fit$centers[,1],'y'=fit$centers[,2],'timepresent'=rep(0,nrow(fit$centers)),'home'=rep(0,nrow(fit$centers)))
  }
  #Determine time spent at these significant locations
  for(i in 1:length(obj$ID2)){
    for(j in 1:nrowfc){
      if(sqrt((mobmat[obj$ID2[i],2]-outmat$x[j])^2+(mobmat[obj$ID2[i],3]-outmat$y[j])^2)<CENTERRAD){
        outmat[j,3]=outmat[j,3]+obj$pt[i]
      }
    }
  }
  #Determine which is home (where is the night spent)
  for(i in 1:length(obj$ID2)){
    xx=as.POSIXct((mobmat[obj$ID2[i],7]+mobmat[obj$ID2[i],4])/2,tz=tz,origin="1970-01-01")
    hourofday = as.numeric(strsplit(strsplit(as.character(xx),":")[[1]][1]," ")[[1]][2])
    if(hourofday>=21 || hourofday<6){
      for(j in 1:nrowfc){
        if(sqrt((mobmat[obj$ID2[i],2]-outmat$x[j])^2+(mobmat[obj$ID2[i],3]-outmat$y[j])^2)<CENTERRAD){
          outmat[j,4]=outmat[j,4]+obj$pt[i]
        }
      }      
    }
  }
  IDmax =order(outmat[,4],decreasing=T)[1] 
  outmat[,4]=rep(0,nrow(outmat))
  outmat[IDmax,4]=1
  outmat=outmat[order(outmat[,3],decreasing=T),]
  IDrm=which(outmat[,3]==0)
  if(length(IDrm)>0){
    outmat=outmat[-IDrm,]
  }
  rownames(outmat)=NULL
  return(outmat)
}

#filename=SIDs[[i]]
#fildir=paste(datadir,SIDs[[i]],"gps",sep="/")


## Input: CSV files
## output: mobility features
MobilityFeatures = function(filename,
                            fildir,
                            ACCURACY_LIM=51, ### meters GPS accuracy
                            ITRVL=10, ### seconds (data concatenation)
                            nreps=1, ### simulate missing data numer of times
                            tz="", ### time zone of data, defaults to current time zone
                            CENTERRAD=200, ### meters radius from significant locations considered
                            wtype="GLR",
                            spread_pars=c(10,1),
                            minpausedur=300,
                            minpausedist=60,
                            rad_fp=NULL,
                            wid_fp=NULL
){
  try1=try(setwd(fildir),silent=TRUE)
  if(class(try1) == "try-error"){
    warning(paste(filedir,"does not exist."))
    return(NA)
  }
  if(file.exists(paste(filename,".Rdata",sep=""))){
    load(paste(filename,".Rdata",sep=""),envir=.GlobalEnv)
  }else{
    filelist <- list.files(pattern = "\\.csv$")
    mobmatmiss=GPS2MobMat(filelist,itrvl=ITRVL,accuracylim=ACCURACY_LIM,r=rad_fp,w=wid_fp)
    mobmat = GuessPause(mobmatmiss,mindur=minpausedur,r=minpausedist)
    obj=InitializeParams(mobmat)
    save(file=paste(filename,".Rdata",sep=""),mobmat,mobmatmiss,obj)
  }
  qOKmsg=MobmatQualityOK(mobmat,obj)
  if(qOKmsg!=""){
    cat(qOKmsg,"\n")
    return(NULL)
  } 
  #try(DailyMobilityPlots(mobmat,obj,tz,filename),silent=TRUE)
  lsmf = list()
  lssigloc = list()
  for(repnum in 1:nreps){
    if(repnum==1){
      cat("Sim #: 1")
    }else if(repnum<=nreps-1){
      cat(paste(" ",repnum,sep=""))
    }else{
      cat(paste(" ",nreps,"\n",sep=""))
    }
    out3=SimulateMobilityGaps(mobmat,obj,wtype,spread_pars)
    IDundef=which(out3[,1]==3)
    if(length(IDundef)>0){
      out3=out3[-IDundef,]      
    }
    obj3=InitializeParams(out3)
    out_GMFM=GetMobilityFeaturesMat(out3,obj3,mobmatmiss,tz,CENTERRAD,ITRVL)
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
    write.table(outmat,paste("MobFeatMat_",filename,".txt",sep=""),quote=F,col.names=T,row.names=F,sep="\t")
  }else{
    featavg=NULL
  }
  return(list('mobmat'=mobmat,'mobmatmiss'=mobmatmiss,'featsims'=lsmf,'siglocsims'=lssigloc,'featavg'=featavg))
}


DailyMobilityPlots = function(mobmat,obj,tz,filename){
  curdate=strsplit(as.character(as.POSIXct(mobmat[1,4],tz=tz,origin="1970-01-01"))," ")[[1]][1]
  curtime=strsplit(strsplit(as.character(as.POSIXct(mobmat[1,4],tz=tz,origin="1970-01-01"))," ")[[1]][2],":")
  subsetinds_v = list()
  daystr_v = c(curdate)
  dayind=1
  subsetinds = c(1)
  for(i in 2:nrow(mobmat)){
    nexdate=strsplit(as.character(as.POSIXct(mobmat[i,4],tz=tz,origin="1970-01-01"))," ")[[1]][1]
    if(nexdate == curdate && i<nrow(mobmat)){
      subsetinds=c(subsetinds,i)
    }else{
      subsetinds_v[[dayind]]=subsetinds
      dayind=dayind+1
      if(mobmat[i-1,1]==2 && (mobmat[i-1,7]-mobmat[i-1,4])/(60*60*24)>1){
        ii=1
        middate = strsplit(as.character(as.POSIXct(mobmat[i-1,4]+(60*60*24)*ii,tz=tz,origin="1970-01-01"))," ")[[1]][1]
        while(middate!=nexdate){
          subsetinds_v[[dayind]]=i-1
          dayind=dayind+1
          if(middate!=daystr_v[length(daystr_v)]){
            daystr_v=c(daystr_v,middate)        
          }          
          ii=ii+1
          middate = strsplit(as.character(as.POSIXct(mobmat[i-1,4]+(60*60*24)*ii,tz=tz,origin="1970-01-01"))," ")[[1]][1]
        }
      }
      curdate=nexdate
      subsetinds=c(i-1,i)
      if(curdate!=daystr_v[length(daystr_v)] && length(daystr_v)<length(subsetinds_v)){
        daystr_v=c(daystr_v,curdate)        
      }
    }
  }  
  plot.flights(mobmat,diminch=4,outfile=paste("FlightsPlot_full_",filename,".pdf",sep=""),xrang=plotlimits(mobmat)$xrang,yrang=plotlimits(mobmat)$yrang)
  for(i in 1:length(daystr_v)){
    submat=matrix(mobmat[subsetinds_v[[i]],],ncol=7)
    plot.flights(submat,diminch=4,outfile=paste("FlightsPlot_",daystr_v[i],"_ZOOMOUT_",filename,".pdf",sep=""),xrang=plotlimits(mobmat)$xrang,yrang=plotlimits(mobmat)$yrang)      
    plot.flights(submat,diminch=4,outfile=paste("FlightsPlot_",daystr_v[i],"_ZOOMIN_",filename,".pdf",sep=""))      
  }
}



GetMobilityFeaturesMat = function(mobmat,obj,mobmatmiss,tz,CENTERRAD,ITRVL){
  #### Get significant locations
  slout=SigLocs(mobmat,obj,CENTERRAD,tz=tz)
  IDhome=which(slout[,4]==1)
  if(length(IDhome)==0){IDhome=1}
  homex=slout[IDhome,1];homey=slout[IDhome,2]
  #### Partion mobmat data into daily subsets: create subsetinds_v and daystr_v.
  curdate=strsplit(as.character(as.POSIXct(mobmat[1,4],tz=tz,origin="1970-01-01"))," ")[[1]][1]
  curtime=strsplit(strsplit(as.character(as.POSIXct(mobmat[1,4],tz=tz,origin="1970-01-01"))," ")[[1]][2],":")
  subsetinds_v = list()
  subsetdayofweek_v = c()
  subsetstarttime_v = c()
  daystr_v = c(curdate)
  dayind=1
  subsetinds = c(1)
  if(nrow(mobmat)<2){
    outmat=NULL
    return(list(outmat,slout))
  }
  for(i in 2:nrow(mobmat)){
    nexdate=strsplit(as.character(as.POSIXct(mobmat[i,4],tz=tz,origin="1970-01-01"))," ")[[1]][1]
    if(nexdate == curdate && i<nrow(mobmat)){
      subsetinds=c(subsetinds,i)
    }else{
      subsetinds_v[[dayind]]=subsetinds
      subsetdayofweek_v = c(subsetdayofweek_v,weekdays(as.Date(curdate)))
      subsetstarttime_v = c(subsetstarttime_v,as.numeric(as.POSIXct(paste(curdate," 00:00:00",sep=""),tz=tz,origin="1970-01-01")))
      dayind=dayind+1          
      if(mobmat[i-1,1]==2 && (mobmat[i-1,7]-mobmat[i-1,4])/(60*60*24)>1){
        ii=1
        middate = strsplit(as.character(as.POSIXct(mobmat[i-1,4]+(60*60*24)*ii,tz=tz,origin="1970-01-01"))," ")[[1]][1]
        while(middate!=nexdate){
          subsetdayofweek_v = c(subsetdayofweek_v,weekdays(as.Date(middate)))
          subsetstarttime_v = c(subsetstarttime_v,as.numeric(as.POSIXct(paste(middate," 00:00:00",sep=""),tz=tz,origin="1970-01-01")))
          subsetinds_v[[dayind]]=i-1
          dayind=dayind+1
          if(middate!=daystr_v[length(daystr_v)]){
            daystr_v=c(daystr_v,middate)        
          }          
          ii=ii+1
          middate = strsplit(as.character(as.POSIXct(mobmat[i-1,4]+(60*60*24)*ii,tz=tz,origin="1970-01-01"))," ")[[1]][1]
        }
      }
      curdate=nexdate
      subsetinds=c(i-1,i)
      if(curdate!=daystr_v[length(daystr_v)]&& !(length(daystr_v)==length(subsetinds_v) && nrow(mobmat)==i)){
        daystr_v=c(daystr_v,curdate)        
      }
    }
  }
  #### Partion mobmatmiss data into daily subsets: create subsetinds_v and daystr_v.
  curdate=strsplit(as.character(as.POSIXct(mobmatmiss[1,4],tz=tz,origin="1970-01-01"))," ")[[1]][1]
  curtime=strsplit(strsplit(as.character(as.POSIXct(mobmatmiss[1,4],tz=tz,origin="1970-01-01"))," ")[[1]][2],":")
  subsetindsmiss_v = list()
  daystrmiss_v=c(curdate)
  dayind=1
  subsetinds = c(1)
  for(i in 2:nrow(mobmatmiss)){
    nexdate=strsplit(as.character(as.POSIXct(mobmatmiss[i,4],tz=tz,origin="1970-01-01"))," ")[[1]][1]
    if(nexdate == curdate && i<nrow(mobmatmiss)){
      subsetinds=c(subsetinds,i)
    }else{
      curdate=nexdate
      subsetindsmiss_v[[dayind]]=subsetinds
      if(curdate!=daystrmiss_v[length(daystrmiss_v)] && !(length(daystrmiss_v)==length(subsetindsmiss_v) && nrow(mobmatmiss)==i)){
        daystrmiss_v=c(daystrmiss_v,curdate)        
      }
      subsetinds=c(i-1,i)
      dayind=dayind+1
      if(mobmatmiss[i-1,1]==2 && (mobmatmiss[i-1,7]-mobmatmiss[i-1,4])/(60*60*24)>1){
        ii=1
        middate = strsplit(as.character(as.POSIXct(mobmatmiss[i-1,4]+(60*60*24)*ii,tz=tz,origin="1970-01-01"))," ")[[1]][1]
        while(middate!=nexdate){
          subsetindsmiss_v[[dayind]]=i-1
          dayind=dayind+1
          if(middate!=daystrmiss_v[length(daystrmiss_v)]){
            daystrmiss_v=c(daystrmiss_v,middate)        
          }          
          ii=ii+1
          middate = strsplit(as.character(as.POSIXct(mobmatmiss[i-1,4]+(60*60*24)*ii,tz=tz,origin="1970-01-01"))," ")[[1]][1]
        }
      }
    }
  }
  ##### intersect mobmat and mobmatmiss to ignore missing data
  IDkeep=c()
  IDkeepmiss=c()
  for(i in 1:length(daystr_v)){
    for(j in 1:length(daystrmiss_v)){
      if(daystr_v[i]==daystrmiss_v[j]){
        IDkeep=c(IDkeep,i)
        IDkeepmiss=c(IDkeepmiss,j)
        break
      }
    }
  }
  if(length(IDkeep)==0){
    outmat=NULL
    return(list(outmat,slout))
  }
  daystr_v=daystr_v[IDkeep]
  subsetinds_v=subsetinds_v[IDkeep]
  subsetdayofweek_v=subsetdayofweek_v[IDkeep]
  subsetstarttime_v=subsetstarttime_v[IDkeep]
  daystrmiss_v=daystrmiss_v[IDkeepmiss]
  subsetindsmiss_v=subsetindsmiss_v[IDkeepmiss]
  ##### Compute mobility features for each day
  Nfeatures=15
  outmat=matrix(NA,nrow=length(daystr_v),ncol=Nfeatures)
  colnames(outmat)=c("Hometime","DistTravelled","RoG","MaxDiam","MaxHomeDist","SigLocsVisited","AvgFlightLen","StdFlightLen","AvgFlightDur","StdFlightDur","ProbPause","SigLocEntropy","MinsMissing","CircdnRtn","WkEndDayRtn")
  rownames(outmat)=daystr_v
  for(i in 1:length(daystr_v)){
    if(length(subsetinds_v[[i]])==0){next}
    submat=matrix(mobmat[subsetinds_v[[i]],],ncol=7)
    if(submat[1,1]==2 && submat[1,4]<subsetstarttime_v[[i]]){
      submat[1,4]=subsetstarttime_v[[i]]
    }
    if(submat[nrow(submat),1]==2 && submat[nrow(submat),7]>subsetstarttime_v[[i]]+60*60*24){
      submat[nrow(submat),7]=subsetstarttime_v[[i]]+60*60*24
    }
    submatmiss=matrix(mobmatmiss[subsetindsmiss_v[[i]],],ncol=7)
    if(submatmiss[1,1]==4 && submatmiss[1,4]<subsetstarttime_v[[i]]){
      submatmiss[1,4]=subsetstarttime_v[[i]]
    }
    if(submatmiss[nrow(submatmiss),1]==4 && submatmiss[nrow(submatmiss),7]>subsetstarttime_v[[i]]+60*60*24){
      submatmiss[nrow(submatmiss),7]=subsetstarttime_v[[i]]+60*60*24
    }
    if(nrow(submat)==0 || length(which(slout$home==1))==0){
      outmat[i,]=c(rep(NA,12),1440,rep(NA,2))
    }else{
      outmat[i,1]=Hometime(submat,slout,CENTERRAD=200)    
      outmat[i,2]=DistanceTravelled(submat)
      outmat[i,3]=RadiusOfGyration(submat,ITRVL)
      outmat[i,4]=MaxDiam(submat)
      outmat[i,5]=MaxHomeDist(submat,homex,homey)
      outmat[i,6]=SigLocsVisited(submat,slout,CENTERRAD)
      outmat[i,7]=AvgFlightLen(submat)
      outmat[i,8]=StdFlightLen(submat)
      outmat[i,9]=AvgFlightDur(submat)
      outmat[i,10]=StdFlightDur(submat)
      outmat[i,11]=ProbPause(submat)
      outmat[i,12]=SigLocEntropy(submat,slout,CENTERRAD)
      outmat[i,13]=MinsMissing(submatmiss)
      DRIout=DailyRoutineIndex(i,mobmat,subsetinds_v,subsetdayofweek_v,subsetstarttime_v,tz,CENTERRAD)
      outmat[i,14]=DRIout$cscore
      outmat[i,15]=DRIout$wscore      
    }
  }
  return(list(outmat,slout))
}


ProgressBar=function (maxn, ind){
  if(maxn<51){
    if (ind == 1) {
      cat("|1%--------------------50%--------------------100%|\n")
      cat("|")
      return()
    }else{
      numprint=floor(50*ind/maxn)-floor(50*(ind-1)/maxn)
      if(numprint>0){
        for(i in 1:numprint){
          cat("|")
        }
      }
    }
  }else{
    if (ind == 1) {
      cat("|1%--------------------50%--------------------100%|\n")
      cat("|")
      return()
    }
    if (maxn == ind) {
      cat("|\n")
      return()
    }
    if (floor(50 * ind/maxn) != floor(50 * (ind - 1)/maxn)) {
      cat("|")
    }    
  }
}



###### Mobility Measures
# Time spent at home (within CENTERRAD meters) in minutes
Hometime = function(mat,slout,CENTERRAD){
  IDhome=which(slout$home==1)
  xcenter=slout[IDhome,1]
  ycenter=slout[IDhome,2]
  tottime=0
  for(i in 1:nrow(mat)){
    if(mat[i,1]==2 && sqrt((mat[i,2]-xcenter)^2+(mat[i,3]-ycenter)^2)<CENTERRAD){ ### error missing value where true/false needed
      tottime=tottime+mat[i,7]-mat[i,4]
    }
  }
  return(tottime/60)
}



### ITRVL is the number of seconds that is collapsing interval width
RadiusOfGyration=function(mat,ITRVL){
  mat=matrix(mat,ncol=7)
  IDskip=which(mat[,1]==4)
  if(length(IDskip)>0){
    mat=matrix(mat[-IDskip,],ncol=7)
  }
  N=nrow(mat)
  w_v=rep(0,N)
  x_v=rep(0,N)
  y_v=rep(0,N)
  for(i in 1:N){
    if(mat[i,1]==4){
      next
    } 
    if(mat[i,1]==3){
      x_v[i]=mat[i,2]
      y_v[i]=mat[i,3]
      w_v[i]=ITRVL
    }
    if(mat[i,1]==1){
      x_v[i]=mean(mat[i,c(2,5)])
      y_v[i]=mean(mat[i,c(3,6)])
      w_v[i]=mat[i,7]-mat[i,4]
    }
    if(mat[i,1]==2){
      x_v[i]=mat[i,2]
      y_v[i]=mat[i,3]
      w_v[i]=mat[i,7]-mat[i,4]
    }
  }
  sumw_v=sum(w_v)
  xavg=sum(w_v*x_v)/sumw_v
  yavg=sum(w_v*y_v)/sumw_v
  return(sqrt(sum(((x_v-xavg)^2+(y_v-yavg)^2)*w_v)/sumw_v))
}

DistanceTravelled = function(mat){
  dt=0
  for(i in 1:nrow(mat)){
    if(mat[i,1]==1){
      dt=dt+sqrt((mat[i,5]-mat[i,2])^2+(mat[i,6]-mat[i,3])^2)
    }
  }
  return(dt)
}

MaxDiam = function(mat){
  IDmv=which(mat[,1]<=2)
  if(length(IDmv)<2){return(0)}
  return(max(dist(mat[IDmv,2:3])))
}

MaxHomeDist = function(mat,homex,homey){
  IDmv=which(mat[,1]<=2)
  if(length(IDmv)==0){return(NA)}
  dfhome=rep(NA,length(IDmv))
  for(i in 1:length(IDmv)){
    dfhome[i]=sqrt((mat[IDmv[i],2]-homex)^2+(mat[IDmv[i],3]-homey)^2)
  }
  return(max(dfhome))
}

SigLocsVisited = function(mat,slout,CENTERRAD){
  places_visited = rep(0,nrow(slout))
  for(i in 1:nrow(mat)){
    if(mat[i,1]<=3){
      for(j in 1:nrow(slout)){
        if(sqrt((slout[j,1]-mat[i,2])^2 + (slout[j,2]-mat[i,3])^2)<CENTERRAD){
          places_visited[j]=1
        }
      }
    }
  }
  return(sum(places_visited))
}

AvgFlightLen = function(mat){
  num=0
  tot=0
  for(i in 1:nrow(mat)){
    if(mat[i,1]==1){
      tot=tot+sqrt((mat[i,5]-mat[i,2])^2+(mat[i,6]-mat[i,3])^2)
      num=num+1
    }
  }
  if(num==0){return(0)}
  return(tot/num)
}

StdFlightLen = function(mat){
  ID1=which(mat[,1]==1)
  if(length(ID1)<=1){return(0)}
  try1=try(sd(as.numeric(sqrt((mat[ID1,6]-mat[ID1,3])^2+(mat[ID1,5]-mat[ID1,2])^2))),silent=TRUE)
  if(class(try1) == "try-error" || is.na(try1)){
    return(0)
  }else{
    return(try1)
  }
}

AvgFlightDur = function(mat){
  num=0
  tot=0
  for(i in 1:nrow(mat)){
    if(mat[i,1]==1){
      tot=tot+mat[i,7]-mat[i,4]
      num=num+1
    }
  }
  if(num==0){return(0)}
  return(tot/num)
}


StdFlightDur = function(mat){
  ID1=which(mat[,1]==1)
  if(length(ID1)==0){return(0)}
  try1=try(sd(as.numeric(mat[ID1,7]-mat[ID1,4])),silent=TRUE)
  if(class(try1) == "try-error" || is.na(try1)){
    return(0)
  }else{
    return(try1)
  }
}

ProbPause = function(mat){
  tpause = 0 
  tflight = 0
  for(i in 1:nrow(mat)){
    if(mat[i,1]==1){
      tflight = tflight + mat[i,7]-mat[i,4]
    }
    if(mat[i,1]==2){
      tpause = tpause + mat[i,7]-mat[i,4]
    }
  }
  return(tpause/(tpause+tflight))
}

SigLocEntropy = function(mat,slout,CENTERRAD){
  tp = rep(0,nrow(slout))
  for(i in 1:nrow(mat)){
    if(mat[i,1]==2){
      for(j in 1:nrow(slout)){
        if(sqrt((slout[j,1]-mat[i,2])^2+(slout[j,2]-mat[i,3])^2)<CENTERRAD){
          tp[j] = tp[j] + mat[i,7]-mat[i,4]
        }
      } 
    }
  }
  tot=0
  if(sum(tp)==0){return(0)}
  for(i in 1:nrow(slout)){
    p=tp[i]/sum(tp)
    if(p>0){
      tot=tot-p*log(p)
    }
  }
  return(tot)
}

MinsMissing = function(mat){
  tot=0
  for(i in 1:nrow(mat)){
    if(mat[i,1]==4){
      tot = tot+mat[i,7]-mat[i,4]
    }
  }
  return(tot/60)
}


# Probability that, on another day in mobmat at around the same time,
# the person is within CENTERRAD meters of where they are on the day indday.
# The 'cscore' corresponds to circadian routine probability, i.e. compared to all other
# days equally.
# The  'wscore' corresponds to the weekend/weekday stratified probabilities.

DailyRoutineIndex = function(indday,mobmat,subsetinds_v,subsetdayofweek_v,subsetstarttime_v,tz,CENTERRAD){
  submat=matrix(mobmat[subsetinds_v[[indday]],],ncol=7)
  daydist_v = rep(NA,length(subsetinds_v))
  for(i in 1:length(subsetinds_v)){
    if(i == indday){next}
    daydist_v[i]=DayDist(indday,i,mobmat,subsetinds_v,subsetstarttime_v,tz,CENTERRAD)
  }
  if(length(which(!is.na(daydist_v)))==0){
    circscore=NA
  }else{
    circscore = mean(daydist_v,na.rm=T)    
  }
  if(subsetdayofweek_v[indday] == "Saturday" || subsetdayofweek_v[indday] == "Sunday"){
    IDcompare = c(which(subsetdayofweek_v=="Saturday"),which(subsetdayofweek_v=="Sunday"))
  }else{
    IDcompare = c(which(subsetdayofweek_v=="Monday"),which(subsetdayofweek_v=="Tuesday"),which(subsetdayofweek_v=="Wednesday"),which(subsetdayofweek_v=="Thursday"),which(subsetdayofweek_v=="Friday"))
  }
  if(length(IDcompare)==0 || length(which(!is.na(daydist_v[IDcompare])))==0){
    wkscore=NA
  }else{
    wkscore = mean(daydist_v[IDcompare],na.rm=T)    
  }
  return(list('cscore'=circscore,'wscore'=wkscore))
}

DayDist = function(i1,i2,mobmat,subsetinds_v,subsetstarttime_v,tz,CENTERRAD){
  chkpts=subsetstarttime_v[i1]+seq(from=30*60,by=60*60,length.out=24)
  chkpts2=subsetstarttime_v[i2]+seq(from=30*60,by=60*60,length.out=24)
  mat1=matrix(mobmat[subsetinds_v[[i1]],],ncol=7)
  mat2=matrix(mobmat[subsetinds_v[[i2]],],ncol=7)
  SamePlace = rep(NA,24)
  for(i in 1:24){
    loc1=LocationAt(mat1,chkpts[i])
    if(is.null(loc1)){
      next
    }
    IDs=c(which(abs(mat2[,4]-chkpts[i])%%(60*60*24)<30*60),which(abs(mat2[,4]-chkpts[i])%%(60*60*24)>(60*60*24)-30*60))
    if(length(IDs)>0){
      CanBe0=FALSE
      for(j in 1:length(IDs)){
        if(mat2[IDs[j],1]<=3){
          CanBe0 = TRUE
          if(sqrt((mat2[IDs[j],2]-loc1$x)^2+(mat2[IDs[j],3]-loc1$y)^2)<CENTERRAD){
            SamePlace[i]=1
          }
        }
      }
      if(CanBe0 && is.na(SamePlace[i])){
        SamePlace[i]=0
      }
    }else{
      for(j in 1:nrow(mat2)){
        if(!is.na(mat2[j,4])&&!is.na(mat2[j,7])&& mat2[j,4]<chkpts2[i] && mat2[j,7]>chkpts2[i]){
          if(mat2[j,1]!=4 && sqrt((mat2[j,2]-loc1$x)^2+(mat2[j,3]-loc1$y)^2)<CENTERRAD){
            SamePlace[i]=1
          }else{
            SamePlace[i]=0
          }
          break
        }
      }
    }
  }
  if(length(which(!is.na(SamePlace)))==0){return(NA)}
  return(mean(SamePlace,na.rm=T))
}

LocationAt = function(mat,tt){
  for(i in 1:nrow(mat)){
    if(mat[i,1]<=2){
      if(mat[i,4]<=tt && mat[i,7]>=tt){
        return(list('x'=mat[i,2],'y'=mat[i,3]))
      }
    }else if(mat[i,1]==3){
      if(mat[i,4]==tt){
        return(list('x'=mat[i,2],'y'=mat[i,3]))
      }      
    }
  }
  return(NULL)
}


WriteSurveyAnswers2File = function(fildir,survey_id,SID){
  try1=try(setwd(fildir),silent=TRUE)
  if(class(try1) == "try-error"){
    warning(paste(fildir,"does not exist."))
    return(NULL)
  }
  filelist <- list.files(pattern = "\\.csv$")
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
  outmat_ls=list()
  counter=1
  outmat=c()
  for(i in IDkeep){
    x=read.csv(filelist[i],fileEncoding="UTF-8")
    #outmat=rbind(outmat,c(date_v[i],x$answer))
    outmat_ls[[counter]]=c(date_v[i],x$answer)
    counter=counter+1
  }
  outmat=do.call(rbind,outmat_ls)
  try1=try(colnames(outmat)<-c("Date",paste(as.character(x$question.text)," (",as.character(x$question.answer.options),")",sep="")),silent=TRUE)
  if(class(try1) == "try-error"){
    warning(paste("Survey non-constant over time for ID:",SID))
    return(NULL)
  }  
  write.table(outmat,paste("SurveyAnswers_",survey_id,"_",SID,".txt",sep=""),quote=F,col.names=T,row.names=F,sep="\t")
  return("success")
}