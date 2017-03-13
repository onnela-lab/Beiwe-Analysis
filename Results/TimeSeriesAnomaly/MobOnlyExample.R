


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


dat=read.table("C:/Users/Ian/Documents/Work/JP/NeurosurgeryRecovery/Data/2b5y7oc1/gps/MobFeatMat_2b5y7oc1.txt",header=T)
dat=read.table("C:/Users/Ian/Documents/Work/JP/NeurosurgeryRecovery/Data/5y5ovrjn/gps/MobFeatMat_5v5ovrjn.txt",header=T)
dat=read.table("C:/Users/Ian/Documents/Work/JP/NeurosurgeryRecovery/Data/9mgp3m9r/gps/MobFeatMat_9mgp3m9r.txt",header=T)
dat=read.table("C:/Users/Ian/Documents/Work/JP/NeurosurgeryRecovery/Data/fjgvhbaa/gps/MobFeatMat_fjgvhbaa.txt",header=T)


> datadir = "C:/Users/Ian/Documents/Work/JP/NeurosurgeryRecovery/Data"
> mobfeat_subset = c("Hometime","DistTravelled","SigLocsVisited",
                     "ProbPause","SigLocEntropy","CircdnRtn")
> out = AnomalyDetectionTS(datadir,subjID="fjgvhbaa",featID = mobfeat_subset)
> names(out)
[1] "IDanomaly" "cutoff"    "epsmat"    "output"    "minp_v"  


> out$IDanomaly
[1] 33 40
> out$output[,30:36]
      04-13      04-14      04-15      04-16        04-17      04-18      04-19
CHI2  3.2594478  5.8609379  6.1806439  21.14424167  9.4333958  7.7520365  5.2360796
DF    7.0000000  7.0000000  7.0000000  7.000000000  7.0000000  7.0000000  7.0000000
PVAL  0.8600091  0.5560777  0.5188221  0.003562429  0.2230187  0.3549662  0.6311778
> out$cutoff
5% 
0.01027132 

# plot of p-values
plot(-log10(out$output[3,]),ylab=expression(-log[10](PVAL)),pch=16,col=rgb(.3,.3,.3,.8),main="Anomaly detection\nSubject: fjgvhbaa")
lines(c(-m,2*m),rep(-log10(out$cutoff),2),lty=2,lwd=2)

# plot of seasonality breakdown



IDkeep=intersect(which(dat$MinsMissing < 1400),which(dat$MinsMissing>5*1440/6))
ymat = t(dat[IDkeep,-1])
colnames(ymat)=dat[IDkeep,1]

IDvars = c(1,2,6,7,11,12,13,14)
#IDvars = c(1,2)
ymat = log(1+ymat[IDvars,])


out=AnomalyDetectionTS(ymat,B=1000)
plot(-log10(out$output[3,]))
lines(c(0,m),rep(-log10(out$cutoff),2))
out$cutoff ## p-value cutoff based on 0.05 FWER
-log10(out$cutoff)

output = out$output
colnames(output)=colnames(ymat)
