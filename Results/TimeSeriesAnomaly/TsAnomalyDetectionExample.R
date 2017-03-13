library(Rcpp)
library(mvtnorm)
library(Matrix)

Rcpp::sourceCpp('C:/Users/Ian/Documents/Work/JP/Beiwe/Github/Beiwe-Analysis/Output/TimeSeriesAnomaly/TSDecompositionMissing.cpp')

DecomposeErrors = function(y,times=NULL,dft=2,tscale=10,mintotvals=7,minvaleach=2,onesided=FALSE,buffer=.2){
  if(is.null(times)){
    times = 1:length(y)
  }
  rt=range(times,na.rm=T)
  tscale = (rt[2]-rt[1])/tscale
  mu_v=DecomposeErrorsMu(y,times,dft,tscale,onesided,buffer)
  s_v=DecomposeErrorsWeeklySeasonality(y,times,mintotvals,minvaleach)
  eps_v=y-(mu_v+s_v)
  return(list(y=y,mu=mu_v,s=s_v,eps=eps_v))
}


AnomalyDetectionTS = function(mat,B=1000,onesided=F,nonparam=T,ALPHA=0.05){
  epsmat = matrix(NA,nrow=nrow(mat),ncol=ncol(mat))
  for(i in 1:nrow(epsmat)){
    epsmat[i,]=DecomposeErrors(mat[i,],onesided=onesided)$eps
  }
  minp_v = MinpDistribution(epsmat,B,nonparam)
  x=HotellingsTS(epsmat,TRUE)
  cutoff=quantile(minp_v,ALPHA)
  IDanomaly=which(x[3,]<cutoff)
  return(list('IDanomaly'=IDanomaly,'cutoff'=cutoff,'epsmat'=epsmat,'output'=x,'minp_v'=minp_v))
}


### Example:


homedir = "C:/Users/Ian/Documents/Work/JP/Schizophrenia"
ID = "vh7ab23d"; IDhosp = 42
ID = "ety2hu13"; IDhosp = 38


filename="FeatureMatrixCLEAN2REPSBACKCOMBINED.txt"
ID = "vh7ab23d"
ID = "euvxbf3w"
homedir="C:/Users/Ian/Documents/Work/JP/Schizophrenia"
vertmarks = c("2017-01-26")
AnomalyDetectionPlot(homedir,filename,ID,vertmarks)
AnomalyDetectionPlot = function(homedir,filename,ID,vertmarks=NULL){
  dat = read.table(paste(homedir,"Processed_data",filename,sep="/"),header=T,stringsAsFactors=F)
  rIDs=which(dat$IID==ID)
  y = dat[rIDs,]
  outdir=paste(homedir,"Output",ID,sep="/")
  if(!file.exists(outdir)){
    dir.create(outdir)
  }  
  for(i in 1:3){
    if(i==1){
      out=AnomalyDetectionTS(t(data.matrix(y[,3:8])),B=1000); DTYPE="Surveys" #surveys only
    }else if(i==2){
      out=AnomalyDetectionTS(t(data.matrix(y[,9:23])),B=1000); DTYPE="Mobility" #mobility only
    }else{
      out=AnomalyDetectionTS(t(data.matrix(y[,24:ncol(y)])),B=1000); DTYPE="Sociability" #sociability only
    }
    png(paste(homedir,"Output",ID,paste("AnomalyDetection",DTYPE,"-",ID,".png",sep=""),sep="/"),width=6,height=5,units="in",res=300)
    par(mai=c(1,.6,.4,.1))
    par(mgp=c(1.7,.6,0))
    plot(as.numeric(as.POSIXct(y[,2])),-log10(out$output[3,]),xaxt="n",xlab="",ylab=expression(-log[10](p)),pch=16,main=paste("Anomaly Detection: ",DTYPE,sep=""))
    LocTck=seq(min(as.numeric(as.POSIXct(y[,2]))),max(as.numeric(as.POSIXct(y[,2]))),3600*24*7)
    axis(1,at=LocTck,labels=unlist(lapply(strsplit(as.character(as.POSIXct(LocTck,origin="1970-01-01")),split=" "),function(xx) xx[1])),las=2)
    #lines(rep(as.numeric(as.POSIXct(y[IDhosp,2])),2),c(-10,10),col="Red")
    if(!is.null(vertmarks)){
      for(j in 1:length(vertmarks)){
        lines(rep(as.numeric(as.POSIXct(vertmarks[j],origin="1970-01-01")),2),c(-10,10),col="Red")
      }
    }
    lines(range(as.numeric(as.POSIXct(y[,2])))+c(-1000000,1000000),rep(-log10(out$cutoff),2),lty=2)
    dev.off()
  }
}




testmat = out$output
colnames(testmat)=y[,2]



INDplot=8
plot(as.numeric(as.POSIXct(y[,2])),y[,INDplot],ylim=c(0,3),xaxt="n",xlab="",ylab=names(y)[INDplot])
axis(1,at=as.numeric(as.POSIXct(y[,2])),labels=y[,2],las=2)
lines(rep(as.numeric(as.POSIXct(y[42,2])),2),c(0,3),col="Red")
# create time series with missingness
# p=3 # number of data streams
# m = 200 # number of consecutive time points
# pmis = 0 # proportion of values missing, selected at random
# times = 1:m
# anomalyloc = 50
# ymat=matrix(NA,nrow=p,ncol=m)
# for(i in 1:p){
#   y = 2*sin(2*pi*(1:m)/(2*m))+1*sin(2*pi*(1:m)/7) + rnorm(m)
# #  y = rnorm(m)
#   y[anomalyloc] = y[anomalyloc]+7
#   y[sample(1:m,floor(pmis*m))]=NA  
#   ymat[i,]=y
# }
# out=AnomalyDetectionTS(ymat,B=1000)
# out$output[3,anomalyloc]  ## p-value for anomaly at anomalyloc
# plot(-log10(out$output[3,]))
# lines(c(0,m),rep(-log10(out$cutoff),2))
# out$cutoff ## p-value cutoff based on 0.05 FWER
# 
# 
# 
# 
# out=DecomposeErrors(ymat[3,],onesided=F)
# 
# plot(times,y,type="l")
# (out$mu+out$s)[50]
# lines(times,out$mu,col="Red")
# lines(times,out$mu+out$s,col="Blue")
# lines(times,out$mu+out$s+out$eps,col="orange")