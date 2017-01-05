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


# create time series with missingness
p=3 # number of data streams
m = 200 # number of consecutive time points
pmis = 0 # proportion of values missing, selected at random
times = 1:m
anomalyloc = 50
ymat=matrix(NA,nrow=p,ncol=m)
for(i in 1:p){
  y = 2*sin(2*pi*(1:m)/(2*m))+1*sin(2*pi*(1:m)/7) + rnorm(m)
  y[anomalyloc] = y[anomalyloc]+7
  y[sample(1:m,floor(pmis*m))]=NA  
  ymat[i,]=y
}
out=AnomalyDetectionTS(ymat,B=1000)
out$output[3,anomalyloc]  ## p-value for anomaly at anomalyloc
out$cutoff ## p-value cutoff based on 0.05 FWER


