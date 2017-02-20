
homedir="C:/Users/Ian/Documents/Work/JP/NeurosurgeryRecovery"
surveyID="56d60b801206f7036f8919ee"

outls=SurveyAdherence(homedir,surveyID)

eps=.1
telaps = 2
delta=1
xvals=seq(telaps+eps,60,eps)
yvals=rep(0,length(xvals))
nvals=rep(0,length(xvals))
for(i in 1:length(xvals)){
  for(j in 1:length(outls)){
    v = outls[[j]]
    if(is.null(v)){next}
    if(max(v)>xvals[i]){
      nvals[i]=nvals[i]+1
      #yvals[i]=yvals[i]+(length(which(v<xvals[i])))/xvals[i]
      yvals[i]=yvals[i]+(length(intersect(which(v<=xvals[i]),which(v>xvals[i]-delta))))/delta
    }
  }
  yvals[i]=yvals[i]/nvals[i]
}
yvals=100*yvals

pdf(paste(homedir,"PainSurveyAdherence.pdf",sep="/"),width=8,height=6)
plot(xvals,yvals,ylim=c(0,100),ylab="Fraction of patients taking the daily survey (%)",xlab="Days since study start",col=rgb(0,0,0,.2))
lines(lowess(xvals,yvals))
dev.off()
SurveyAdherence = function(homedir,surveyID){
  SIDs=unlist(lapply(strsplit(list.dirs(paste(homedir,"Data",sep="/"),recursive=FALSE),"/"),function(x) x[length(x)]))
  x=list()
  for(i in 1:length(SIDs)){
    surveydir=paste(homedir,"Data",SIDs[i],"survey_answers",surveyID,sep="/")
    if(!file.exists(surveydir)){
      x[[i]]=NULL
      next
    }
    filelist = list.files(path=surveydir,pattern = "\\.csv$",full.names=F)
    if(length(filelist)<=1){
      x[[i]]=NULL
      next
    }
    outvec=rep(0,length(filelist))
    for(j in 1:length(filelist)){
      ts = as.numeric(as.POSIXct(gsub("_",":",strsplit(filelist[j],".csv")[[1]][1]),tz="GMT"))
      if(j==1){
        t0=ts
        next
      }
      outvec[j]=(ts-t0)/(3600*24)
    }
    x[[i]]=outvec
  }
  return(x)
}