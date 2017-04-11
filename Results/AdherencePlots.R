daily_adherence_grid = function(...){
  plotname="daily_adherence_grid.pdf"
  SIDs=patient_names
  survey_IDs_v=c()
  seconds_per_day=60*60*24
  tmin_glob=10^10
  tmax_glob=0
  maxdur = 0
  IDvals = list()
  lenvals = list()
  for(i in 1:length(SIDs)){
    print(SIDs[i])
    calldur_ls=NULL
    callmis_ls=NULL
    texts_ls=NULL
    gpsvals=NULL
    surveydates_ls=NULL
    ## read in GPS missing data dates and amounts
    #    filname=paste(homedir,"Preprocessed_data",SIDs[[i]],"gps",paste("MobFeatMat_",SIDs[[i]],".txt",sep=""),sep="/")
    #    filname=paste(homedir,"Processed_data",SIDs[[i]],paste("MobFeatures_",SIDs[i],".Rdata",sep=""),sep="/")    
    patient_output_filepath = paste(output_filepath,"/Processed_Data","/Individual/",SIDs[i],"/MobFeatures.rds",sep="")
    if(file.exists(patient_output_filepath)){
      gpsvals=list()
      #mobmat=read.table(filname,header=T)
      #load(filname)
      inlist=readRDS(patient_output_filepath)
      featavg=inlist[[4]]
      mobmat = data.frame(featavg)
      gpsvals[['gpsmis']]=mobmat$MinsMissing
      gpsvals[['gpsht']]=mobmat$Hometime
      gpsvals[['gpsdt']]=mobmat$DistTravelled
      gpsvals[['gpsmhd']]=mobmat$MaxHomeDist
      gpsvals[['gpsslv']]=mobmat$SigLocsVisited
      gpsvals[['gpsrtn']]=mobmat$CircdnRtn
      gpsdates=as.character(row.names(mobmat))
      tmin=min(as.numeric(as.POSIXct(gpsdates,origin="1970-01-01")))
      tmax=max(as.numeric(as.POSIXct(gpsdates,origin="1970-01-01")))
    }else{
      gpsvals=NULL
      gpsdates=NULL
      tmin=NULL
      tmax=NULL
    }
    ## Read in dates where surveys are taken 
    dirname=paste(data_filepath,SIDs[i],"survey_answers",sep="/")
    if(file.exists(dirname)){
      surveydates_ls=list()
      survey_IDs=lapply(strsplit(list.dirs(dirname,recursive=FALSE),"/"),function(x) x[length(x)])
      if(length(survey_IDs)>0){
        for(j in 1:length(survey_IDs)){
          if(length(which(survey_IDs_v==survey_IDs[[j]]))==0){
            survey_IDs_v=c(survey_IDs_v,survey_IDs[[j]])
          }
          temp=c()
          surveydirname=paste(dirname,survey_IDs[[j]],sep="/")
          filelist <- list.files(path=surveydirname,pattern = "\\.csv$")
          for(file in filelist){
            temp=c(temp,strsplit(file," ")[[1]][1])
          }
          surveydates_ls[[survey_IDs[[j]]]]=unique(temp)
          if(is.null(tmin) || tmin > min(as.numeric(as.POSIXct(temp,origin="1970-01-01")))){
            tmin=min(as.numeric(as.POSIXct(temp,origin="1970-01-01")))
          }
          if(is.null(tmax) || tmax < max(as.numeric(as.POSIXct(temp,origin="1970-01-01")))){
            tmax=max(as.numeric(as.POSIXct(temp,origin="1970-01-01")))
          }
        }
      }
    }else{
      surveydates_ls=NULL
    } 
    ### read in call data
    calldirname=paste(data_filepath,SIDs[i],"calls",sep="/")
    if(file.exists(calldirname)){
      calldur_ls=list()
      callmis_ls=list()
      texts_ls=list()
      # call duration
      # calls missed    
      filelist <- list.files(path=calldirname,pattern = "\\.csv$")
      for(j in seq_along(filelist)){
        x=read.csv(paste(calldirname,filelist[j],sep="/"),fileEncoding="UTF-8")
        for(k in 1:nrow(x)){
          curdate=as.character(as.POSIXct(x[k,]$UTC.time,tz="",origin='1970-01-01'))
          if(!is.numeric(calldur_ls[curdate])){
            calldur_ls[curdate]=0
          }
          if(!is.numeric(callmis_ls[curdate])){
            callmis_ls[curdate]=0
          }
          if(as.character(x$call.type[k])=="Missed Call"){
            callmis_ls[curdate]=as.numeric(callmis_ls[curdate])+1
          }else{
            calldur_ls[curdate]=as.numeric(calldur_ls[curdate])+x$duration.in.seconds[k]          
          }          
          if(is.null(tmin) || tmin>as.numeric(as.POSIXct(x[k,]$UTC.time,tz="",origin='1970-01-01'))){
            tmin=as.numeric(as.POSIXct(x[k,]$UTC.time,tz="",origin='1970-01-01'))
          }
          if(is.null(tmax) || tmax<as.numeric(as.POSIXct(x[k,]$UTC.time,tz="",origin='1970-01-01'))){
            tmax=as.numeric(as.POSIXct(x[k,]$UTC.time,tz="",origin='1970-01-01'))
          }        
        }
      }
      ### read in text data
      textdirname=paste(data_filepath,SIDs[i],"texts",sep="/")
      if(file.exists(textdirname)){
        # # sent, # received, length sent, length received
        filelist <- list.files(path=textdirname,pattern = "\\.csv$")
        for(j in seq_along(filelist)){
          x=read.csv(paste(textdirname,filelist[j],sep="/"),fileEncoding="UTF-8")
          for(k in 1:nrow(x)){
            curdate=as.character(as.POSIXct(x[k,]$UTC.time,tz="",origin='1970-01-01'))
            if(!is.numeric(texts_ls[[curdate]])){
              texts_ls[[curdate]]=c(0,0,0,0)
            }
            if(as.character(x$sent.vs.received[k])=="sent SMS"){
              texts_ls[[curdate]][1]=as.numeric(texts_ls[[curdate]][1])+1
              texts_ls[[curdate]][3]=as.numeric(texts_ls[[curdate]][3])+x$message.length[k]
            }else{
              texts_ls[[curdate]][2]=as.numeric(texts_ls[[curdate]][1])+1
              texts_ls[[curdate]][4]=as.numeric(texts_ls[[curdate]][3])+x$message.length[k]
            }          
            if(is.null(tmin) || tmin>as.numeric(as.POSIXct(x[k,]$UTC.time,tz="",origin='1970-01-01'))){
              tmin=as.numeric(as.POSIXct(x[k,]$UTC.time,tz="",origin='1970-01-01'))
            }
            if(is.null(tmax) || tmax<as.numeric(as.POSIXct(x[k,]$UTC.time,tz="",origin='1970-01-01'))){
              tmax=as.numeric(as.POSIXct(x[k,]$UTC.time,tz="",origin='1970-01-01'))
            }        
          }
        }
      }
      if(!(is.null(tmax) || is.null(tmin))){
        if(maxdur<tmax-tmin){
          maxdur=tmax-tmin
        }
        if(tmin_glob>tmin){
          tmin_glob=tmin
        }
        if(tmax_glob<tmax){
          tmax_glob=tmax
        }  
      }
    }else{
      calldur_ls=NULL
    }
    ## add all data to IDvals master list
    IDvals[[SIDs[i]]]=list('gpsvals'=gpsvals,'gpsdates'=gpsdates,'surveydates_ls'=surveydates_ls,'calldur_ls'=calldur_ls,'callmis_ls'=callmis_ls,'texts_ls'=texts_ls,'tmin'=tmin,'tmax'=tmax)
  }
  rowlabels=c("GPS amount recorded","Home time", "Distance travelled","Max distance from home","# Significant locations visited","Circadian routine","# texts sent","Total length of texts sent","# texts received","Total length of texts received","Call duration","# missed calls",paste("SurveyID:",survey_IDs_v))
  plotw=8
  ploth=4
  bufferlines=3
  nvals=length(survey_IDs_v)+1+1+6+4+bufferlines
  
  outdirplot=paste(output_filepath,"Results","Group",sep="/")
  pdf(paste(outdirplot,plotname,sep="/"),width=plotw,height=ploth)
  
  for(k in 1:length(SIDs)){
    par(mai=c(0,0,0,0))
    surveydates_ls = IDvals[[SIDs[k]]]$surveydates_ls
    gpsdates = IDvals[[SIDs[k]]]$gpsdates
    gpsvals = IDvals[[SIDs[k]]]$gpsvals
    calldur_ls=IDvals[[SIDs[k]]]$calldur_ls
    callmis_ls=IDvals[[SIDs[k]]]$callmis_ls
    texts_ls=IDvals[[SIDs[k]]]$texts_ls
    tmin =IDvals[[SIDs[k]]]$tmin 
    tmax=IDvals[[SIDs[k]]]$tmax
    if(is.null(tmin) || is.null(tmax)){
      next
    }
    xlimvals = c(-.2*(tmax-tmin) / seconds_per_day,(tmax-tmin) / seconds_per_day)
    plot(NA,ylim=c(-1,nvals),xlim=xlimvals,bty="n",xaxt="n",yaxt="n",xlab="",ylab="",main="",asp=1)
    text(mean(xlimvals),nvals,paste("Subject ID:",SIDs[k]))
    for(j in 1:length(rowlabels)){
      text(0,j,rowlabels[j],pos=2,cex=.4)
    }
    for(kk in 0:xlimvals[2]){
      if(kk%%7==0){
        text(kk,-(.5+xlimvals[2]/45),paste("Week",as.integer(kk/7)+1),srt=270,cex=.4)        
      }
    }
    ## survey answers
    for(i in seq_along(survey_IDs_v)){
      for(j in 0:((tmax-tmin) / seconds_per_day)){
        if(length(which(names(surveydates_ls)==survey_IDs_v[i]))>0){
          IDsurvey=which(round((as.numeric(as.POSIXct(surveydates_ls[[survey_IDs_v[i]]],origin="1970-01-01"))-tmin) / seconds_per_day)==j)
          yval=i+12 #############################################################################################
          if(length(IDsurvey)>0){
            xval=j
            eps=.5
            
            polygon(c(xval-eps,xval-eps,xval+eps,xval+eps),c(yval-eps,yval+eps,yval+eps,yval-eps),col=light_color(rgb2col(stream_colors[1]),ink_depth=1),border=light_color(c(0,0,0),ink_depth=.2),lwd=.1) 
          }else{
            xval=j
            eps=.5
            polygon(c(xval-eps,xval-eps,xval+eps,xval+eps),c(yval-eps,yval+eps,yval+eps,yval-eps),col="white",border=light_color(c(0,0,0),ink_depth=.2),lwd=.1) 
          }        
        }else{
          xval=j
          eps=.5
          yval=i
          polygon(c(xval-eps,xval-eps,xval+eps,xval+eps),c(yval-eps,yval+eps,yval+eps,yval-eps),col="white",border=light_color(c(0,0,0),ink_depth=.2),lwd=.1) 
        }
      }
    }
    ## add GPS data
    if(!is.null(gpsdates)){
      gpsxvals=round((as.numeric(as.POSIXct(gpsdates,origin="1970-01-01"))-tmin) / seconds_per_day)
    }else{
      gpsxvals = NULL
    }
    for(j in 0:((tmax-tmin) / seconds_per_day)){
      xval=j
      eps=.5
      yval=1 #############################################################################################
      if(!is.null(gpsxvals) && length(which(gpsxvals==j))>0){
        IDgps=which(gpsxvals==j)
        if(is.numeric(gpsvals[['gpsmis']][IDgps])){
          #polygon(c(xval-eps,xval-eps,xval+eps,xval+eps),c(yval-eps,yval+eps,yval+eps,yval-eps),col=light_color(c(1,0,1),ink_depth=(1-gpsvals[['gpsmis']][IDgps]/1440)^.2),border=light_color(c(0,0,0),ink_depth=.2),lwd=.1)        
          polygon(c(xval-eps,xval-eps,xval+eps,xval+eps),c(yval-eps,yval+eps,yval+eps,yval-eps),col=light_color(rgb2col(stream_colors[5]),ink_depth=(1-gpsvals[['gpsmis']][IDgps]/1440)^.2),border=light_color(c(0,0,0),ink_depth=.2),lwd=.1)        
          
        }else{
          polygon(c(xval-eps,xval-eps,xval+eps,xval+eps),c(yval-eps,yval+eps,yval+eps,yval-eps),col="white",border=light_color(c(0,0,0),ink_depth=.2),lwd=.1)        
        }
        if(is.numeric(gpsvals[['gpsht']][IDgps])){
          polygon(c(xval-eps,xval-eps,xval+eps,xval+eps),c(yval-eps+1,yval+eps+1,yval+eps+1,yval-eps+1),col=light_color(rgb2col(stream_colors[5]),ink_depth=(gpsvals[['gpsht']][IDgps]/1440)^1),border=light_color(c(0,0,0),ink_depth=.2),lwd=.1)
        }else{
          polygon(c(xval-eps,xval-eps,xval+eps,xval+eps),c(yval-eps+1,yval+eps+1,yval+eps+1,yval-eps+1),col="white",border=light_color(c(0,0,0),ink_depth=.2),lwd=.1)
        }
        if(is.numeric(gpsvals[['gpsdt']][IDgps])){
          polygon(c(xval-eps,xval-eps,xval+eps,xval+eps),c(yval-eps+2,yval+eps+2,yval+eps+2,yval-eps+2),col=light_color(rgb2col(stream_colors[5]),ink_depth=min((gpsvals[['gpsdt']][IDgps]/200000)^.5,1)),border=light_color(c(0,0,0),ink_depth=.2),lwd=.1)
        }else{
          polygon(c(xval-eps,xval-eps,xval+eps,xval+eps),c(yval-eps+2,yval+eps+2,yval+eps+2,yval-eps+2),col="white",border=light_color(c(0,0,0),ink_depth=.2),lwd=.1)
        }
        if(is.numeric(gpsvals[['gpsmhd']][IDgps])){
          polygon(c(xval-eps,xval-eps,xval+eps,xval+eps),c(yval-eps+3,yval+eps+3,yval+eps+3,yval-eps+3),col=light_color(rgb2col(stream_colors[5]),ink_depth=min((gpsvals[['gpsmhd']][IDgps]/100000)^.5,1)),border=light_color(c(0,0,0),ink_depth=.2),lwd=.1)        
        }else{
          polygon(c(xval-eps,xval-eps,xval+eps,xval+eps),c(yval-eps+3,yval+eps+3,yval+eps+3,yval-eps+3),col="white",border=light_color(c(0,0,0),ink_depth=.2),lwd=.1)        
        }
        if(is.numeric(gpsvals[['gpsslv']][IDgps])){
          polygon(c(xval-eps,xval-eps,xval+eps,xval+eps),c(yval-eps+4,yval+eps+4,yval+eps+4,yval-eps+4),col=light_color(rgb2col(stream_colors[5]),ink_depth=min((gpsvals[['gpsslv']][IDgps]/6),1)),border=light_color(c(0,0,0),ink_depth=.2),lwd=.1)        
        }else{
          polygon(c(xval-eps,xval-eps,xval+eps,xval+eps),c(yval-eps+4,yval+eps+4,yval+eps+4,yval-eps+4),col="white",border=light_color(c(0,0,0),ink_depth=.2),lwd=.1)
        }
        if(is.numeric(gpsvals[['gpsrtn']][IDgps])){
          polygon(c(xval-eps,xval-eps,xval+eps,xval+eps),c(yval-eps+5,yval+eps+5,yval+eps+5,yval-eps+5),col=light_color(rgb2col(stream_colors[5]),ink_depth=gpsvals[['gpsrtn']][IDgps]),border=light_color(c(0,0,0),ink_depth=.2),lwd=.1)      
        }else{
          polygon(c(xval-eps,xval-eps,xval+eps,xval+eps),c(yval-eps+5,yval+eps+5,yval+eps+5,yval-eps+5),col="white",border=light_color(c(0,0,0),ink_depth=.2),lwd=.1)
        }
      }else{
        polygon(c(xval-eps,xval-eps,xval+eps,xval+eps),c(yval-eps,yval+eps,yval+eps,yval-eps),col="white",border=light_color(c(0,0,0),ink_depth=.2),lwd=.1)
        polygon(c(xval-eps,xval-eps,xval+eps,xval+eps),c(yval-eps+1,yval+eps+1,yval+eps+1,yval-eps+1),col="white",border=light_color(c(0,0,0),ink_depth=.2),lwd=.1)
        polygon(c(xval-eps,xval-eps,xval+eps,xval+eps),c(yval-eps+2,yval+eps+2,yval+eps+2,yval-eps+2),col="white",border=light_color(c(0,0,0),ink_depth=.2),lwd=.1)
        polygon(c(xval-eps,xval-eps,xval+eps,xval+eps),c(yval-eps+3,yval+eps+3,yval+eps+3,yval-eps+3),col="white",border=light_color(c(0,0,0),ink_depth=.2),lwd=.1)
        polygon(c(xval-eps,xval-eps,xval+eps,xval+eps),c(yval-eps+4,yval+eps+4,yval+eps+4,yval-eps+4),col="white",border=light_color(c(0,0,0),ink_depth=.2),lwd=.1)
        polygon(c(xval-eps,xval-eps,xval+eps,xval+eps),c(yval-eps+5,yval+eps+5,yval+eps+5,yval-eps+5),col="white",border=light_color(c(0,0,0),ink_depth=.2),lwd=.1)
      }
    }
    ## call duration
    for(j in 0:((tmax-tmin) / seconds_per_day)){
      xval=j
      eps=.5
      yval=11 #############################################################################################
      if(!is.null(calldur_ls)){
        calldurxvals=round((as.numeric(as.POSIXct(names(calldur_ls),origin="1970-01-01"))-tmin) / seconds_per_day)
      }
      if(!is.null(calldur_ls) && length(which(calldurxvals==j))>0){
        secdur=calldur_ls[[names(calldur_ls)[which(calldurxvals==j)]]]
        # categorize by 1 hour, 30 mins, 10 mins, 5 mins, 1 min, 0
        if(secdur==0){
          catdur=0
        }else if(secdur<60){
          catdur=1
        }else if(secdur<300){
          catdur=2
        }else if(secdur<600){
          catdur=3
        }else if(secdur<1800){
          catdur=4
        }else if(secdur<3600){
          catdur=5
        }else{
          catdur=6
        }
        polygon(c(xval-eps,xval-eps,xval+eps,xval+eps),c(yval-eps,yval+eps,yval+eps,yval-eps),col=light_color(rgb2col(stream_colors[2]),ink_depth=catdur/6),border=light_color(c(0,0,0),ink_depth=.2),lwd=.1)
      }else{
        polygon(c(xval-eps,xval-eps,xval+eps,xval+eps),c(yval-eps,yval+eps,yval+eps,yval-eps),col="white",border=light_color(c(0,0,0),ink_depth=.2),lwd=.1)
      }
    }
    ## missed calls
    for(j in 0:((tmax-tmin) / seconds_per_day)){
      xval=j
      eps=.5
      yval= 12 #############################################################################################
      if(!is.null(callmis_ls)){
        callmisxvals=round((as.numeric(as.POSIXct(names(callmis_ls),origin="1970-01-01"))-tmin) / seconds_per_day)
      }
      if(!is.null(callmis_ls) && length(which(callmisxvals==j))>0){
        nummis=callmis_ls[[names(callmis_ls)[which(callmisxvals==j)]]]
        # categorize by 1 hour, 30 mins, 10 mins, 5 mins, 1 min, 0
        if(nummis==0){
          catmis=0
        }else if(nummis==1){
          catmis=1
        }else if(nummis==2){
          catmis=2
        }else if(nummis==3){
          catmis=3
        }else if(nummis==4){
          catmis=4
        }else if(nummis==5){
          catmis=5
        }else{
          catmis=6
        }
        polygon(c(xval-eps,xval-eps,xval+eps,xval+eps),c(yval-eps,yval+eps,yval+eps,yval-eps),col=light_color(rgb2col(stream_colors[2]),ink_depth=catmis/6),border=light_color(c(0,0,0),ink_depth=.2),lwd=.1)
      }else{
        polygon(c(xval-eps,xval-eps,xval+eps,xval+eps),c(yval-eps,yval+eps,yval+eps,yval-eps),col="white",border=light_color(c(0,0,0),ink_depth=.2),lwd=.1)
      }
    }
    ## number texts sent
    for(j in 0:((tmax-tmin) / seconds_per_day)){
      xval=j
      eps=.5
      yval=7 #############################################################################################

      if(!is.null(texts_ls) && !is.null(names(texts_ls))){
        textxvals=round((as.numeric(as.POSIXct(names(texts_ls),origin="1970-01-01"))-tmin) / seconds_per_day)

        if(length(which(textxvals==j))>0){
          numsnt = texts_ls[[names(texts_ls)[which(textxvals==j)]]][1]
          if(numsnt==0){
            catsnt=0
          }else if(numsnt<6){
            catsnt=1
          }else if(numsnt<10){
            catsnt=2
          }else if(numsnt<15){
            catsnt=3
          }else if(numsnt<20){
            catsnt=4
          }else if(numsnt<25){
            catsnt=5
          }else{
            catsnt=6
          }
          polygon(c(xval-eps,xval-eps,xval+eps,xval+eps),c(yval-eps,yval+eps,yval+eps,yval-eps),col=light_color(rgb2col(stream_colors[3]),ink_depth=catsnt/6),border=light_color(c(0,0,0),ink_depth=.2),lwd=.1)
        }
      }
    }
    ## length texts sent
    for(j in 0:((tmax-tmin) / seconds_per_day)){
      xval=j
      eps=.5
      yval=8 #############################################################################################
      if(!is.null(texts_ls) && !is.null(names(texts_ls))){
        textxvals=round((as.numeric(as.POSIXct(names(texts_ls),origin="1970-01-01"))-tmin) / seconds_per_day)
        if(length(which(textxvals==j))>0){
          dursnt = texts_ls[[names(texts_ls)[which(textxvals==j)]]][3]
          if(dursnt==0){
            catdur=0
          }else if(dursnt<76){
            catdur=1
          }else if(dursnt<151){
            catdur=2
          }else if(dursnt<226){
            catdur=3
          }else if(dursnt<301){
            catdur=4
          }else if(dursnt<376){
            catdur=5
          }else{
            catdur=6
          }
          polygon(c(xval-eps,xval-eps,xval+eps,xval+eps),c(yval-eps,yval+eps,yval+eps,yval-eps),col=light_color(rgb2col(stream_colors[3]),ink_depth=catdur/6),border=light_color(c(0,0,0),ink_depth=.2),lwd=.1)
        }
      }
    }
    ## number texts received
    for(j in 0:((tmax-tmin) / seconds_per_day)){
      xval=j
      eps=.5
      yval= 9 #############################################################################################
      if(!is.null(texts_ls) && !is.null(names(texts_ls))){
        textxvals=round((as.numeric(as.POSIXct(names(texts_ls),origin="1970-01-01"))-tmin) / seconds_per_day)

        if(length(which(textxvals==j))>0){
          numsnt = texts_ls[[names(texts_ls)[which(textxvals==j)]]][2]
          if(numsnt==0){
            catsnt=0
          }else if(numsnt<6){
            catsnt=1
          }else if(numsnt<10){
            catsnt=2
          }else if(numsnt<15){
            catsnt=3
          }else if(numsnt<20){
            catsnt=4
          }else if(numsnt<25){
            catsnt=5
          }else{
            catsnt=6
          }
          polygon(c(xval-eps,xval-eps,xval+eps,xval+eps),c(yval-eps,yval+eps,yval+eps,yval-eps),col=light_color(rgb2col(stream_colors[3]),ink_depth=catsnt/6),border=light_color(c(0,0,0),ink_depth=.2),lwd=.1)
        }
      }
    }
    ## length texts received
    for(j in 0:((tmax-tmin) / seconds_per_day)){
      xval=j
      eps=.5
      yval = 10 #############################################################################################
      if(!is.null(texts_ls) && !is.null(names(texts_ls))){
        textxvals=round((as.numeric(as.POSIXct(names(texts_ls),origin="1970-01-01"))-tmin) / seconds_per_day)
        if(length(which(textxvals==j))>0){
          dursnt = texts_ls[[names(texts_ls)[which(textxvals==j)]]][4]
          if(is.na(dursnt) || dursnt==0){
            catdur=0
          }else if(dursnt<76){
            catdur=1
          }else if(dursnt<151){
            catdur=2
          }else if(dursnt<226){
            catdur=3
          }else if(dursnt<301){
            catdur=4
          }else if(dursnt<376){
            catdur=5
          }else{
            catdur=6
          }
          polygon(c(xval-eps,xval-eps,xval+eps,xval+eps),c(yval-eps,yval+eps,yval+eps,yval-eps),col=light_color(rgb2col(stream_colors[3]),ink_depth=catdur/6),border=light_color(c(0,0,0),ink_depth=.2),lwd=.1)
        }
      }
    }
    for(j in 0:((tmax-tmin) / seconds_per_day)){
      tt=as.POSIXct(tmin+j*seconds_per_day,origin="1970-01-01")
      hr=strsplit(as.character(tt)," ")[[1]] 
      if(length(hr)>1 && hr[2]=="23:00:00"){
        tmin=tmin+3600
      }else if(length(hr)>1 && hr[2]=="01:00:00"){
        tmin=tmin-3600
      }
      if(weekdays(as.POSIXct(tmin+j*seconds_per_day,origin="1970-01-01"))=="Sunday"){
        eps=.5
        lines(c(j+eps,j+eps),c(0+eps,nvals-bufferlines+eps),lwd=1.5,col="Black")
      }
    }
  }
  #legend("bottomleft",title="Data collected",c("GPS (darker for more data collected)",paste("Survey ID:",survey_IDs_v)),pch=c(15,0,0,0,0,0),col=c("black",col_survey_v),cex=.4,lty=c(NA,NA,NA,NA,NA,NA),lwd=c(1,.2,.2,.2,.2,.2),bty="n")
  dev.off() 
}