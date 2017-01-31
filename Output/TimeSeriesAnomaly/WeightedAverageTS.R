library(Rcpp)
library(mvtnorm)
library(Matrix)
setwd("C:/Users/Ian/Documents/Work/JP/Beiwe/Github/Beiwe-Analysis/Output/TimeSeriesAnomaly")


# First, do weighted averages EXCLUDING the day of interest. Do a fat tailed distribution
# Second, take out weekly effects. Must have 2 per category.


Rcpp::sourceCpp('TSDecompositionMissing.cpp')
missprob=0.5
mvndat = t(rmvnorm(100,mean=c(0,0),sigma=rbind(c(1,.2),c(.2,1))))
mvndat[sample(1:length(mvndat),floor(length(mvndat)*missprob))]=NA
x=HotellingsTS(mvndat,TRUE)
hist(x[1,which(x[2,]==2)],breaks=40,prob=T)
lines(seq(0,10,.1),dchisq(seq(0,10,.1),df=2))
hist(x[1,which(x[2,]==1)],breaks=80,prob=T)
lines(seq(0,10,.1),dchisq(seq(0,10,.1),df=1))

epsmat = matrix(NA,nrow=nrow(mvndat),ncol=ncol(mvndat))
for(i in 1:nrow(epsmat)){
  epsmat[i,]=DecomposeErrors(mvndat[i,],onesided=FALSE)$eps
}
minp_v = MinpDistribution(epsmat,1000,TRUE)
hist(minp_v,breaks=40)


#### df for the t distribution.
#### The kernel for the weighted average comes from the t distribution
#         with df degrees of freedom evaluated at the time difference divided by 
#         (range(y)[2]-range(y)[1])/tscale

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


# create time series with missingness
p=10 # number of data streams
m = 100 # number of consecutive time points
pmis = 0 # proportion of values missing, selected at random
times = 1:m
ymat=matrix(NA,nrow=p,ncol=m)
for(i in 1:p){
  y = 4*sin(2*pi*(1:m)/(2*m))+2*sin(2*pi*(1:m)/7) + rnorm(m)
  y[sample(1:m,floor(pmis*m))]=NA  
  ymat[i,]=y
}

 

out=DecomposeErrors(y,onesided=F)

plot(times,y,type="l")
lines(times,out$mu,col="Red")
lines(times,out$mu+out$s,col="Blue")
lines(times,out$mu+out$s+out$eps,col="orange")

plot(times,y)
points(times,out$mu,col="Red")
points(times,out$mu+out$s,col="Blue")

plot(out$eps)
max(abs(out$eps),na.rm=T)


Rcpp::sourceCpp('TSDecompositionMissing.cpp')


#### Input is a p by m matrix for a single subject, y. p = number of features, m = number of days.
#### Decompose y into trend, seasonality (if possible/applicable), and then the remaining error
out=DecomposeErrors(y,onesided=F)
plot(out$eps)
#### Find cutoff for test of global null at 0.05 level.
#### This is done by permuting the error for the original data and repeating analysis.
epsmat = matrix(NA,nrow=nrow(ymat),ncol=ncol(ymat))
for(i in 1:nrow(epsmat)){
  epsmat[i,]=DecomposeErrors(ymat[i,],onesided=F)$eps
}
B=1000
epsmatperm=epsmat
IDkeepls = list()
for(i in 1:nrow(epsmat)){
  IDkeepls[[i]]=which(!is.na(epsmat[i,]))
}
minp = rep(NA,B)
for(b in 1:B){
  cat(b)
  epsmatperm = matrix(NA,nrow=nrow(epsmat),ncol=ncol(epsmat))
  for(i in 1:nrow(epsmat)){
    epsmatperm[i,IDkeepls[[i]]]=sample(epsmat[i,IDkeepls[[i]]])
  }
  x=HotellingsTS(epsmatperm,TRUE)
  minp[b]=min(x[3,])
}
x=HotellingsTS(epsmat,TRUE)


AnomalyDetectionTS = function(mat,B=1000,onesided=F,nonparam=T,ALPHA=0.05){
  epsmat = matrix(NA,nrow=nrow(mat),ncol=ncol(mat))
  for(i in 1:nrow(epsmat)){
    epsmat[i,]=DecomposeErrors(mat[i,],onesided)$eps
  }
  minp_v = MinpDistribution(epsmat,B,nonparam)
  x=HotellingsTS(epsmat,TRUE)
  cutoff=quantile(minp_v,ALPHA)
  IDanomaly=which(x[3,]<ALPHA)
  return(list('IDanomaly'=IDanomaly,'cutoff'=cutoff,'epsmat'=epsmat,'output'=x))
}

hist(x[3,])
vv=c(-0.1348,-0.5106,-0.6125,-0.4128,0.4128,-2.0496,0.0448)
sinv = matrix(c(-0.1307,  -2.3026,  -0.1888,   0.9183,   0.6463,   2.3355,   0.5490,
         -2.3026,  -3.9523,  -0.9683,   1.0261,   1.3859,   4.1839,  1.1696,
         -0.1888,  -0.9683,   1.1858,   0.1744,   0.2257,   0.7275,  -0.3208,
         0.9183,   1.0261,   0.1744,   1.1864,  -0.5954,  -0.7998,   0.3114,
         0.6463,   1.3859,   0.2257,  -0.5954,   0.8209,  -1.1176,  -0.9724,
         2.3355,   4.1839,   0.7275,  -0.7998,  -1.1176,  -3.1446,  -1.3937,
         0.5490,   1.1696,  -0.3208,   0.3114,  -0.9724,  -1.3937,   1.4342),nrow=7,byrow=T)

evals=eigen(solve(sinv))$values
evals[which(evals<0)]=0
eigen(solve(sinv))$vectors%*%diag(evals)%*%t(eigen(solve(sinv))$vectors)
nearPDsym(solve(sinv))

