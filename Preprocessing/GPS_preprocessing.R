GPS_preprocessing = function(patient_name,
                             data_directory,
                             ACCURACY_LIM=51, ### meters GPS accuracy
                             ITRVL=10, ### seconds (data concatenation)
                             tz="", ### time zone of data, defaults to current time zone
                             CENTERRAD=200, ### meters radius from significant locations considered
                             minpausedur=300,
                             minpausedist=60,
                             rad_fp=NULL,
                             wid_fp=NULL
){
  outdir = file.path(paste(data_directory,patient_name,sep="/"),"preprocessed_data")
  if(!file.exists(outdir)){
    dir.create(outdir)
  }  
  outfilename =paste(outdir,paste("gps_preprocessed_",patient_name,".Rdata",sep=""),sep="/")
  if(file.exists(outfilename)){
    cat("GPS already preprocessed.\n")
    return(NULL)
  }
  filelist <- list.files(path=paste(data_directory,patient_name,"gps",sep="/"),pattern = "\\.csv$",full.names=T)
  if(length(filelist)==0){return(NULL)}
  mobmatmiss=GPS2MobMat(filelist,itrvl=ITRVL,accuracylim=ACCURACY_LIM,r=rad_fp,w=wid_fp)
  mobmat = GuessPause(mobmatmiss,mindur=minpausedur,r=minpausedist)
  obj=InitializeParams(mobmat)
  qOKmsg=MobmatQualityOK(mobmat,obj)
  if(qOKmsg!=""){
    cat(qOKmsg,"\n")
    return(NULL)
  }
  save(file=outfilename,mobmat,mobmatmiss,obj)  
}

GPS_imputation = function(patient_name,
                          data_directory,
                          wtype="GLR",
                          spread_pars=c(10,1),
                          nreps=1 ### simulate missing data numer of times
){
  # Check to see if GPS has been processed
  # IF so, load mobmat, and obj
  predir = file.path(paste(data_directory,patient_name,sep="/"),"preprocessed_data",paste("gps_preprocessed_",patient_name,".Rdata",sep=""))
  if(!file.exists(predir)){
    cat("No preprocessed data.\n")
    return(NULL)
  }  
  load(predir)
  outdir = paste(data_directory,patient_name,"processed_data",sep="/")
  if(!file.exists(outdir)){
    dir.create(outdir)
  }  
  outfilename =paste(data_directory,patient_name,"processed_data",paste("gps_imputed_",patient_name,".Rdata",sep=""),sep="/")
  mobmatsims = list()
  objsims = list()
  for(repnum in 1:nreps){
    if(repnum==1){
      cat("Sim #: 1")
    }else if(repnum<=nreps-1){
      cat(paste(" ",repnum,sep=""))
    }else{
      cat(paste(" ",nreps,"\n",sep=""))
    }
    mobmat2=SimulateMobilityGaps(mobmat,obj,wtype,spread_pars)
    IDundef=which(mobmat2[,1]==3)
    if(length(IDundef)>0){
      mobmat2=mobmat2[-IDundef,]      
    }
    obj2=InitializeParams(mobmat2)
    mobmatsims[[repnum]]=mobmat2
    objsims[[repnum]]=obj2
  }  
  save(file=outfilename,mobmatsims,objsims)  
}



# Function: GPS2MobMat
# #### Arguments
# filelist:  vector of cvs files containing GPS trace
# itrvl:     Interval width (in seconds) that observations are averaged over
# r:         Minimum distance between consecutive locations needed to be
#            covered to be considered movement (instead of a pause)
# w:         Maximum allowed distance from the flight path for intermediate points
# #### Value
# column 1: codes.
#           1=flight
#           2=pause
#           3=unclassified
#           4=missing data

GPS2MobMat = function(filelist,itrvl=10,accuracylim=51,r=NULL,w=NULL,tint_m=NULL,tint_k=NULL){
  if(is.null(r)){
    r=sqrt(itrvl)
  }
  count=0
  dims_v = rep(0,length(filelist))
  filels = list()
  cat("Read in all GPS csv files...\n")
  for(i in 1:length(filelist)){
    file = filelist[i]
    count=count+1
    #ProgressBar(length(filelist),count)
    dat=read.csv(file,fileEncoding="UTF-8")
    dims_v[i]=nrow(dat)
    filels[[i]]=dat[,c(1,3,4,5,6)] # ignore UTC.time
  }
  mat=matrix(NA,nrow=sum(dims_v),ncol=5)
  startind=1
  for(i in 1:length(filelist)){
    mat[startind:(startind+dims_v[i]-1),]=as.matrix(filels[[i]])
    startind=startind+dims_v[i]
  }
  colnames(mat)=c("timestamp","latitude","longitude","altitude","accuracy")
  mat=data.frame(mat)
  mat = mat[order(mat[,1]),]
  mat=mat[which(mat$accuracy<accuracylim),]
  if(!is.null(tint_k) && !is.null(tint_m)){
    t0 = mat$timestamp[1]/1000;
    mat=mat[which((mat$timestamp/1000-t0)%%(tint_k+tint_m)<tint_k),]    
  }
  if(is.null(w)){
    w=mean(mat$accuracy)+itrvl
  }
  tstart=mat[1,1]/1000
  tend=mat[nrow(mat),1]/1000
  avgmat = matrix(NA,nrow=ceiling((tend-tstart)/itrvl)+2,ncol=4)
  IDam = 1
  count = 0
  nextline=c(1,tstart+itrvl/2,mat[1,2],mat[1,3]) 
  numitrvl=1
  cat("Collapse data within",itrvl,"second intervals...\n")
  for(i in 2:nrow(mat)){
    #ProgressBar(nrow(mat)-1,i-1)
    if(mat[i,1]/1000<tstart+itrvl){
      nextline[3]=nextline[3]+mat[i,2]
      nextline[4]=nextline[4]+mat[i,3]
      numitrvl=numitrvl+1
    }else{
      nextline[3]=nextline[3]/numitrvl
      nextline[4]=nextline[4]/numitrvl
      #avgmat=rbind(avgmat,nextline)
      avgmat[IDam,]=nextline
      count=count+1
      IDam=IDam+1
      nummiss=floor((mat[i,1]/1000-(tstart+itrvl))/itrvl)
      if(nummiss>0){
        #avgmat = rbind(avgmat,c(4,tstart+itrvl/2,tstart+itrvl*(nummiss+1)+itrvl/2,NA))
        avgmat[IDam,] = c(4,tstart+itrvl/2,tstart+itrvl*(nummiss+1)+itrvl/2,NA)
        count=count+1
        IDam=IDam+1
      }
      tstart=tstart+itrvl*(nummiss+1)
      nextline[1]=1
      nextline[2]=tstart+itrvl/2
      nextline[3]=mat[i,2]
      nextline[4]=mat[i,3]
      numitrvl=1
    }
  }
  avgmat = avgmat[1:count,]
  avgmat=cbind(avgmat[,1:4],NA,NA)
  ID1 = which(avgmat[,1]==1)
  cat("Convert from Lat/Lon to X/Y...\n")
  obj=LatLong2XY(avgmat[ID1,3],avgmat[ID1,4])
  avgmat[ID1,5:6]=cbind(obj$x_v,obj$y_v)
  outmat=c()
  curind=1
  cat("Convert from X/Y to flights/pauses...\n")
  for(i in 1:nrow(avgmat)){
    #ProgressBar(nrow(avgmat),i)
    if(avgmat[i,1]==4){
      outmat=rbind(outmat,ExtractFlights(avgmat[curind:(i-1),c(5,6,2)],r,w),
                   c(avgmat[i,1],NA,NA,avgmat[i,2],NA,NA,avgmat[i,3]))
      curind=i+1
    }
  }  
  if(curind<=nrow(avgmat)){  
    outmat=rbind(outmat,ExtractFlights(avgmat[curind:nrow(avgmat),c(4,3,2)],r,w))
  }
  rownames(outmat)=NULL
  colnames(outmat)=c("Code","x0","y0","t0","x1","y1","t1")
  return(outmat)
}

MaxRadius = function(mat){
  cent=colMeans(mat,na.rm=TRUE)
  return(max(apply(mat,1,function(x) sqrt((x[1]-cent[1])^2+(x[2]-cent[2])^2)),na.rm=TRUE))
}

GuessPause = function(mat,mindur=300,r=75){
  cat("Inferring pauses...\n")
  flatmat=c()
  collapse=FALSE
  inds=1
  incr=1
  tcur=mat[inds,4]
  while(TRUE){
    if(!is.na(mat[inds+incr,7])){
      tnex=mat[inds+incr,7]
    }else{
      tnex=mat[inds+incr,4]
    }
    if(tnex-tcur>=mindur){
      if(mat[inds+incr,1]==1){
        maxr=MaxRadius(rbind(mat[inds:(inds+incr),2:3],mat[inds:(inds+incr),5:6]))
      }else{
        maxr=MaxRadius(mat[inds:(inds+incr),2:3])        
      }
      if(maxr>r && !collapse){
        inds=inds+1
        tcur=mat[inds,1]
        incr=0
      }
      if(maxr>r && collapse){
        if(mat[inds+incr-1,1]==4){
          if(inds<inds+incr-2){
            flatmat=rbind(flatmat,c(inds,inds+incr-2))                                
          }
        }else{
          flatmat=rbind(flatmat,c(inds,inds+incr-1))          
        }
        inds=inds+incr
        tcur=mat[inds,1]
        incr=0
        collapse=FALSE
      }
      if(maxr<=r){
        collapse=TRUE
      }
    }
    incr=incr+1
    if(inds+incr>nrow(mat)){
      if(maxr<=r && tnex-tcur>=mindur){
        flatmat=rbind(flatmat,c(inds,inds+incr-1))
      }
      break
    }
  }
  if(nrow(flatmat)==0){
    return(mat)
  }else{
    outmat=c()
    if(flatmat[1,1]>1){
      outmat=mat[1:(flatmat[1,1]-1),]
    }
    for(i in 1:nrow(flatmat)){
      #ProgressBar(nrow(flatmat),i)
      outmat=rbind(outmat,Collapse2Pause(mat[flatmat[i,1]:flatmat[i,2],]))
      if(i<nrow(flatmat) && flatmat[i,2]<flatmat[i+1,1]-1){
        outmat=rbind(outmat,mat[(flatmat[i,2]+1):(flatmat[i+1,1]-1),])
      }
    }    
    if(flatmat[nrow(flatmat),2]<nrow(mat)){
      outmat=rbind(outmat,mat[(flatmat[nrow(flatmat),2]+1):nrow(mat),])
    }
  }
  if(nrow(outmat)==1){
    rownames(outmat)=NULL
    colnames(outmat)=c("Code","x0","y0","t0","x1","y1","t1")
    return(outmat)
  }
  # Group together adjacent pauses, averaging their location
  flatmat2=c()
  outmat2=c()
  collapse=FALSE
  for(i in 2:nrow(outmat)){
    if(outmat[i,1]!=2 && !collapse){
      next
    }else if(outmat[i,1]!=2 && collapse){
      collapse=FALSE
      flatmat2=rbind(flatmat2,c(cstart,i-1))
    }else  if(outmat[i,1]==2 && outmat[i-1,1]==2 && !collapse){
      cstart=i-1
      collapse=TRUE
    }else if(outmat[i,1]==2 && collapse){
      next
    }
  }
  if(collapse && outmat[nrow(outmat),1]==2){
    flatmat2=rbind(flatmat2,c(cstart,nrow(outmat)))
  }
  if(is.null(flatmat2)){
    outmat2=outmat
  }else{
    flatmat2=matrix(flatmat2,ncol=2)
    outmat2=c()
    if(flatmat2[1,1]>1){
      outmat2=outmat[1:(flatmat2[1,1]-1),]
    }
    for(i in 1:nrow(flatmat2)){
      outmat2=rbind(outmat2,Collapse2Pause(outmat[flatmat2[i,1]:flatmat2[i,2],]))
      if(i<nrow(flatmat2) && flatmat2[i,2] < flatmat2[i+1,1]-1){
        outmat2=rbind(outmat2,outmat[(flatmat2[i,2]+1):(flatmat2[i+1,1]-1),])
      }
    }
    if(flatmat2[nrow(flatmat2),2]<nrow(outmat)){
      outmat2=rbind(outmat2,outmat[(flatmat2[nrow(flatmat2),2]+1):nrow(outmat),])
    }
  }
  # Set flight endpoints equal to pause endpoints
  if(outmat2[1,1]==1 && outmat2[2,1]==2){
    outmat2[1,5]=outmat2[2,2]
    outmat2[1,6]=outmat2[2,3]
  }
  for(i in 2:(nrow(outmat2)-1)){
    if(outmat2[i,1]==1 && outmat2[i-1,1]==2){
      outmat2[i,2]=outmat2[i-1,2]
      outmat2[i,3]=outmat2[i-1,3]
    }
    if(outmat2[i,1]==1 && outmat2[i+1,1]==2){
      outmat2[i,5]=outmat2[i+1,2]
      outmat2[i,6]=outmat2[i+1,3]
    }
  }
  if(outmat2[nrow(outmat2)-1,1]==2 && outmat2[nrow(outmat2),1]==1){
    outmat2[nrow(outmat2),2]=outmat2[nrow(outmat2)-1,2]
    outmat2[nrow(outmat2),3]=outmat2[nrow(outmat2)-1,3]
  }
  rownames(outmat2)=NULL
  colnames(outmat2)=c("Code","x0","y0","t0","x1","y1","t1")
  return(outmat2)
}

Collapse2Pause = function(mat){
  cent=colMeans(mat[,2:3],na.rm=TRUE)
  if(!is.na(mat[nrow(mat),7])){
    return(c(2,cent[1],cent[2],mat[1,4],NA,NA,mat[nrow(mat),7]))
  }else{
    return(c(2,cent[1],cent[2],mat[1,4],NA,NA,mat[nrow(mat),4]))    
  }
}

LatLong2XY = function(lat_v,lon_v,R=6.371*10^6){
  th0 = min(lon_v)
  th1 = max(lon_v)
  ph0 = min(lat_v)
  ph1 = max(lat_v)
  d1 = 2*pi*R*((ph1-ph0)*2*pi/360)/(2*pi)
  d2 = 2*pi*(R*sin(pi/2-ph1*2*pi/360))*((th1-th0)*2*pi/360)/(2*pi)
  d3 = 2*pi*(R*sin(pi/2-ph0*2*pi/360))*((th1-th0)*2*pi/360)/(2*pi)
  x_v=rep(0,length(lon_v))
  y_v=rep(0,length(lat_v))
  for(i in 1:length(lat_v)){
    w1=(lat_v[i]-ph0)/(ph1-ph0)
    w2=(lon_v[i]-th0)/(th1-th0)
    x_v[i]=w1*abs(d3-d2)/2+w2*(d3*(1-w1)+d2*w1)
    y_v[i]=w1*d1*sin(acos(abs((d3-d2)/(2*d1))))
  }
  return(list("x_v"=x_v,"y_v"=y_v))
}

# parameters:
# mat: matrix of coordinate positions. It has 3 columns (x,y,time), ordered by time.
# r: Maximum distance between two locations, adjacent in time, such that within that radius is considered a pause in movement.
# w: max distance from flight path allowed for travel points in that flight.

ExtractFlights = function(mat,r,w){
  out = c()  
  if(!is.matrix(mat)){
    out=matrix(c(3,mat[1],mat[2],mat[3],NA,NA,NA),nrow=1)
    colnames(out)=c("code","lon1","lat1","t1","lon2","lat2","t2")
    return(out)
  }
  nextline = rep(NA,6)
  nextline[1:3]=mat[1,]
  curind = 1
  while(TRUE){
    nexind = curind+1
    if(nexind==nrow(mat)){
      nextline[4:6]=mat[nexind,]
      out=rbind(out,nextline)
      break
    }
    while(TRUE){
      if(!IsFlight(mat[curind:nexind,],r,w)){
        break
      }
      nexind=nexind+1
      if(nexind>nrow(mat)){
        break
      }
    }
    if(nexind==curind+1 && curind!=nrow(mat)){
      nextline[3]=mat[nexind,3]
      mat=mat[-nexind,]
    }else{
      nextline[4:6]=mat[nexind-1,]
      out=rbind(out,nextline)
      nextline=rep(NA,6)
      curind=nexind-1
      nextline[1:3]=mat[curind,]
    }
    if(nexind>nrow(mat)){break}
  }
  outp=c()
  if(out[1,3]>min(mat[,3])){
    outp=rbind(outp,c(2,out[1,1],out[1,2],min(mat[,3]),NA,NA,out[1,3]))
    if(nrow(out)==1){
      outp[1,7]=out[1,6]
      return(outp)
    }
  }
  if(nrow(out)==1){
    outp=rbind(outp,c(1,out))
    return(outp)
  }
  for(i in 1:(nrow(out)-1)){
    outp=rbind(outp,c(1,out[i,]))
    if(out[i,6]<out[i+1,3]){
      outp=rbind(outp,c(2,out[i,4],out[i,5],out[i,6],NA,NA,out[i+1,3]))
    }
  }
  # convert flights with distance 0 into pauses
  IDf=which(outp[,1]==1)
  IDp=which((outp[IDf,2]-outp[IDf,5])^2+(outp[IDf,3]-outp[IDf,6])^2==0)
  if(length(IDp)>0){
    for(i in 1:length(IDp)){
      outp[IDf[IDp[i]],1]=2
      outp[IDf[IDp[i]],5]=NA
      outp[IDf[IDp[i]],6]=NA
    }
  }
  outp=rbind(outp,c(1,out[nrow(out),]))
  colnames(outp)=c("code","lon1","lat1","t1","lon2","lat2","t2")
  return(outp)
}


IsFlight = function(mat,r,w){
  num=nrow(mat)
  if(sqrt((mat[1,1]-mat[num,1])^2+(mat[1,2]-mat[num,2])^2)<r){
    return(FALSE)
  }
  if(min(sqrt((mat[2:num,1]-mat[1:(num-1),1])^2+(mat[2:num,2]-mat[1:(num-1),2])^2))<r){
    return(FALSE)
  }
  if(num==2){
    return(TRUE)
  }
  if(mat[1,1]==mat[num,1]){
    if(max(abs(mat[2:(num-1),1]))>w){
      return(FALSE)
    }else{
      return(TRUE)
    }
  }
  if(mat[1,1]>mat[num,1]){
    mat=mat[num:1,]
  }
  mat[,1]=mat[,1]-mat[1,1]
  mat[,2]=mat[,2]-mat[1,2]
  theta=-atan(mat[num,2]/mat[num,1])
  A=matrix(c(cos(theta),-sin(theta),sin(theta),cos(theta)),nrow=2,byrow=TRUE)
  rotpts=A%*%t(matrix(mat[2:(num-1),1:2],ncol=2))
  if(max(abs(rotpts[2,]))>w){
    return(FALSE)
  }else{
    return(TRUE)
  }
}


# (x0,y0) is the starting position in meters
# (x1,y1) is the ending position in meters
# (t0,t1) is the starting and ending time in seconds
# fd is the distribution of flight distances in meters
# ft is the distribution of flight times in seconds
# fa is the distribution of flight angles in radians (0,2pi)
# fp is the distribution of pause times in seconds
# fw is the weighting function for flights
# pt is the distribution of pause times
# probp is the probability of a pause
# pw is the weighting function for pauses


RandomBridge = function(x0,y0,x1,y1,t0,t1,fd,ft,fts,fa,fw,probp,pt,pts,pw,allts,allw,ind11,ind12,i_ind,pxs,pys,fxs,fys,allxs,allys,wtype,canpause,niter=100,spread_pars){
  success=FALSE
  for(i in 1:niter){
    outmat=c()
    curx=x0
    cury=y0
    curt=t0
    tarrive=t0
    while(TRUE){
      ## new
      varmult=1
      while(TRUE){
        if(wtype=="TL"){          
          fw=dt(spread_pars[1]*(fts-curt)/(varmult*(t1-t0)),df=spread_pars[2])
          pw=dt(spread_pars[1]*(pts-curt)/(varmult*(t1-t0)),df=spread_pars[2])
          allw=dt(spread_pars[1]*(allts-curt)/(varmult*(t1-t0)),df=spread_pars[2])          
        }else if(wtype=="GL"){
          fw=dt(spread_pars[1]*sqrt((fxs-curx)^2+(fys-cury)^2)/(50*varmult),df=spread_pars[2])
          pw=dt(spread_pars[1]*sqrt((pxs-curx)^2+(pys-cury)^2)/(50*varmult),df=spread_pars[2])
          allw=dt(spread_pars[1]*sqrt((allxs-curx)^2+(allys-cury)^2)/(50*varmult),df=spread_pars[2])
        }else if(wtype=="GLR"){ 
          fw=dt(spread_pars[1]*sqrt((fxs-curx)^2+(fys-cury)^2)/(50*varmult),df=spread_pars[2])*dt(spread_pars[1]*apply(cbind(abs((fts-curt))%%(60*60*24),(60*60*24)-abs((fts-curt))%%(60*60*24)),1,min)/(varmult*(t1-t0)),df=spread_pars[2])
          pw=dt(spread_pars[1]*sqrt((pxs-curx)^2+(pys-cury)^2)/(50*varmult),df=spread_pars[2])*dt(spread_pars[1]*apply(cbind(abs((pts-curt))%%(60*60*24),(60*60*24)-abs((pts-curt))%%(60*60*24)),1,min)/(varmult*(t1-t0)),df=spread_pars[2])
          allw=dt(spread_pars[1]*sqrt((allxs-curx)^2+(allys-cury)^2)/(50*varmult),df=spread_pars[2])*dt(spread_pars[1]*apply(cbind(abs((allts-curt))%%(60*60*24),(60*60*24)-abs((allts-curt))%%(60*60*24)),1,min)/(varmult*(t1-t0)),df=spread_pars[2])
        }
        if(length(pts)>0 && length(fts)>0 && sum(fw)>0 && sum(pw)>0){break}
        if(length(pts)==0 && length(fts)>0 && sum(fw)>0){break}
        if(length(fts)==0 && length(pts)>0 && sum(pw)>0){break}
        varmult=varmult*2
      }
      s11=sum(allw[ind11],na.rm=T)
      s12=sum(allw[ind12],na.rm=T)
      if(s11+s12==0){phatcur=probp}else{phatcur=s12/(s11+s12)}
      probp=phatcur      
      ## end
      
      if(canpause && runif(1)<probp){ #pause happens
        canpause=FALSE
        p_samp=sample(pt,1,prob=pw)
        if(curt+p_samp<t1){
          nextline = c(2,curx,cury,curt,NA,NA,curt+p_samp)
          curt=curt+p_samp
          outmat=rbind(outmat,nextline)
        }else{
          break
        }
      }else{ #flight happens
        canpause=TRUE
        IDsamp = sample(1:length(fa),1,prob=fw)
        a_samp = fa[IDsamp]
        d_samp = fd[IDsamp]
        t_samp = ft[IDsamp]
        if(curt+t_samp<t1){
          nexx=curx+cos(a_samp)*d_samp
          nexy=cury+sin(a_samp)*d_samp
          if(is.nan(nexx)){break}
          nextline = c(1,curx,cury,curt,nexx,nexy,curt+t_samp)      
          curt=curt+t_samp
          curx=nexx
          cury=nexy
          outmat=rbind(outmat,nextline)
          tarrive=curt
        }else{
          break
        }
      }
    }    
    if(tarrive>t0){
      success=TRUE
      break
    }
  }
  if(!success){
    return(c(1,x0,y0,t0,x1,y1,t1))
    return(NULL)
  }else{
    curx=x0
    cury=y0
    for(i in 1:nrow(outmat)){
      if(outmat[i,1]==1){
        outmat[i,2]=curx
        outmat[i,3]=cury
        curt=outmat[i,7]
        w=(tarrive-curt)/(tarrive-t0)
        outmat[i,5]=outmat[i,5]*w+x1*(1-w)
        outmat[i,6]=outmat[i,6]*w+y1*(1-w)
        curx=outmat[i,5]
        cury=outmat[i,6]
      }else{
        curt=outmat[i,4]
        outmat[i,2]=curx
        outmat[i,3]=cury
      }
    }
    outmat[nrow(outmat),7]=t1
    rownames(outmat)=NULL
    colnames(outmat)=c("Code","x0","y0","t0","x1","y1","t1")
    return(outmat)
  }
}


InitializeParams = function(out){
  ID1=which(out[,1]==1)
  ID2=which(out[,1]==2)
  ID3=which(out[,1]==3)
  ID4=which(out[,1]==4)
  
  # probability of a pause after a flight
  ID1p1=ID1+1    
  if(length(ID1)>0 && ID1[length(ID1)]==nrow(out)){  
    ID1p1=ID1p1[-length(ID1p1)]
  }
  allts=apply(out,1,function(xx) mean(xx[c(4,7)]))
  allxs=out[,2]
  allys=out[,3]
  ind11=ID1p1[which(out[ID1p1,1]==1)]
  ind12=ID1p1[which(out[ID1p1,1]==2)]
  l1=length(ind11)
  l2=length(ind12)
  if(l1+l2>0){
    phatall=l2/(l1+l2)    
  }
  if(l1+l2==0){phatall=length(ID2)/(length(ID1)+length(ID2))}
  #flight distances
  fd=apply(out[ID1,],1,function(xx) sqrt((xx[2]-xx[5])^2+(xx[3]-xx[6])^2))
  
  # flight times: ft
  ft=apply(out[ID1,],1,function(xx) (xx[7]-xx[4]))
  fxs=out[ID1,2]
  fys=out[ID1,3]
  # flight angles range [0,2pi]: fa
  #fa=apply(out[ID1,],1,function(xx) atan((xx[6]-xx[3])/(xx[5]-xx[2]))-((sign(xx[6]-xx[3])-1)/2)*pi)
  fa=rep(0,length(ID1))
  yvals=out[ID1,6]-out[ID1,3]
  xvals=out[ID1,5]-out[ID1,2]
  IDyg0=which(yvals>=0)
  IDxg0=which(xvals>=0)
  IDyl0=which(yvals<0)
  IDxl0=which(xvals<0)  
  IDgg=intersect(IDyg0,IDxg0)
  IDlg=intersect(IDyg0,IDxl0)
  IDgl=intersect(IDyl0,IDxg0)
  IDll=intersect(IDyl0,IDxl0)
  fa[IDgg]=atan(yvals[IDgg]/xvals[IDgg])
  fa[IDgl]=atan(yvals[IDgl]/xvals[IDgl])+2*pi
  fa[IDlg]=atan(yvals[IDlg]/xvals[IDlg])+pi
  fa[IDll]=atan(yvals[IDll]/xvals[IDll])+pi
  # flight time stamps: fts
  fts=out[ID1,4]
  
  # pause times
  pt=apply(matrix(out[ID2,],ncol=7),1,function(xx) xx[7]-xx[4])
  pxs=out[ID2,2]
  pys=out[ID2,3]
  #pause time stamp: pts
  pts=out[ID2,4]  
  return(list(ID1=ID1,ID2=ID2,ID3=ID3,ID4=ID4,ID1p1=ID1p1,allts=allts,ind11=ind11,ind12=ind12,phatall=phatall,fd=fd,ft=ft,fa=fa,fts=fts,pt=pt,pts=pts,fxs=fxs,fys=fys,pxs=pxs,pys=pys,allxs=allxs,allys=allys))  
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
  foutmat=c()
  for(i in 1:nrow(suboutmat)){
    if(suboutmat[i,1]==1){
      curx=suboutmat[i,5]
      cury=suboutmat[i,6]
      foutmat=rbind(foutmat,suboutmat[i,])
    }else  if(suboutmat[i,1]<=3){
      curx=suboutmat[i,2] 
      cury=suboutmat[i,3]
      foutmat=rbind(foutmat,suboutmat[i,])
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
        foutmat=rbind(foutmat,c(1,curx,cury,suboutmat[i,4],suboutmat[i+1,2],suboutmat[i+1,3],suboutmat[i,7]))
      }else{
        rbout=matrix(RandomBridge(x0=curx,y0=cury,x1=suboutmat[i+1,2],y1=suboutmat[i+1,3],t0=suboutmat[i,4],t1=suboutmat[i,7],fd=fd,ft=ft,fts=fts,fa=fa,fw=fw,probp=phatcur,pt=pt,pts=pts,pw=pw,allts=allts,allw=allw,ind11=ind11,ind12=ind12,i_ind=i,pxs=pxs,pys=pys,fxs=fxs,fys=fys,allxs=allxs,allys=allys,wtype=wtype,canpause=suboutmat[i-1,1]==1,niter=100,spread_pars=spread_pars),ncol=7)
        foutmat=rbind(foutmat,rbout)        
      }
    }
  }  
  return(foutmat)
}


  
plotlimits = function(mat,defaultdist=100){
  xrang=range(c(mat[which(mat[,1]<=2),2],mat[which(mat[,1]<=1),5]))
  yrang=range(c(mat[which(mat[,1]<=2),3],mat[which(mat[,1]<=1),6]))
  if(xrang[2]==xrang[1]){
    xrang[2]=xrang[2]+defaultdist/2
    xrang[1]=xrang[1]-defaultdist/2
  }
  if(yrang[2]==yrang[1]){
    yrang[2]=yrang[2]+defaultdist/2
    yrang[1]=yrang[1]-defaultdist/2
  }
  return(list(xrang=xrang,yrang=yrang))
}

# This function may be outdated. We will want to replace it with the plots that Patrick has created incorporating other data streams.
plot.flights=function(mat,xrang=NULL,yrang=NULL,diminch=6,add2plot=FALSE,addlegend=TRUE,outfile=NULL,title=NULL){
  #col24hour_v=c("#253494","#2c7fb8","#41b6c4","#7fcdbb","#c7e9b4","#ffffcc","#fed976","#feb24c","#fd8d3c","#fc4e2a","#e31a1c","#b10026","#7a0177","#ae017e","#dd3497","#f768a1","#fa9fb5","#fcc5c0","#edf8fb","#ccece6","#99d8c9","#66c2a4","#2ca25f","#006d2c")
  col24hour_v=c(c("#08306b","#08519c","#2171b5","#4292c6","#6baed6","#9ecae1","#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#99000d"),rev(c("#08306b","#08519c","#2171b5","#4292c6","#6baed6","#9ecae1","#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#99000d")))  
  if(nrow(mat)==0){
    return(NULL)
  }
  if(add2plot){outfile=NULL}
  write2file=FALSE
  if(!is.null(outfile)){
    write2file=TRUE
  }
  if(write2file){
    pdf(outfile,width=diminch*11/10,height=diminch)
  }
  if(is.null(xrang)){
    xrang=plotlimits(mat)$xrang
  }
  if(is.null(yrang)){
    yrang=plotlimits(mat)$yrang
  }
  if(xrang[2]-xrang[1]>yrang[2]-yrang[1]){
    dif=((xrang[2]-xrang[1])-(yrang[2]-yrang[1]))
    yrang[2] = yrang[2]+dif/2
    yrang[1] = yrang[1]-dif/2
  }else{
    dif=((yrang[2]-yrang[1])-(xrang[2]-xrang[1]))
    xrang[2] = xrang[2]+dif/2
    xrang[1] = xrang[1]-dif/2    
  }
  xrang[1]=xrang[1]-(xrang[2]-xrang[1])/10
  if(!add2plot){
    if(!is.null(title)){
      par(mai=c(0,0,.4,0))      
      plot(NA,xlim=xrang
           ,ylim=yrang
           ,xaxt="n"
           ,yaxt="n"
           ,xlab=""
           ,ylab=""
           ,bty="n"
           ,main=title)  
    }else{
      par(mai=c(0,0,0,0))      
      plot(NA,xlim=xrang
           ,ylim=yrang
           ,xaxt="n"
           ,yaxt="n"
           ,xlab=""
           ,ylab=""
           ,bty="n"
           ,main="")        
    }
    xleg1=xrang[1]      
    xleg2=xleg1+(xrang[2]-xrang[1])/30
    yincr=(yrang[2]-yrang[1])/40
    legtext=c(" 6AM"," 12PM"," 6PM"," 12AM")
    if(addlegend){
      for(i in 1:24){
        polygon(c(xleg1,xleg1,xleg2,xleg2),c(yrang[1]+(i-1)*yincr,yrang[1]+i*yincr,yrang[1]+i*yincr,yrang[1]+(i-1)*yincr),col=col24hour_v[i])
        if(i%%6==0){
          text(xleg2,yrang[1]+i*yincr,legtext[floor(i/6)],adj=0,cex=.5)        
        }
      }
      points(xleg1,yrang[1]+26*yincr,cex=2,pch=16)
      text(xleg2,yrang[1]+26*yincr,">4 hrs",adj=0,cex=.5)
      points(xleg1,yrang[1]+28*yincr,cex=1,pch=16)
      text(xleg2,yrang[1]+28*yincr,"1 hr",adj=0,cex=.5)
      points(xleg1,yrang[1]+30*yincr,cex=.5,pch=16)
      text(xleg2,yrang[1]+30*yincr,"<30 mins",adj=0,cex=.5)
      #text(xleg1,yrang[1]+32.3*yincr,"Pause\nDuration",adj=0,cex=.5)
      legdist=10^floor(log10((yrang[2]-yrang[1])/5))
      lines(c(xrang[1],xrang[1]),c(yrang[2],yrang[2]-legdist))
      lines(c(xrang[1]-(xrang[2]-xrang[1])/120,xrang[1]+(xrang[2]-xrang[1])/120),c(yrang[2],yrang[2]))
      lines(c(xrang[1]-(xrang[2]-xrang[1])/120,xrang[1]+(xrang[2]-xrang[1])/120),c(yrang[2]-legdist,yrang[2]-legdist))
      if(log10(legdist)<3){
        text(xrang[1]+(xrang[2]-xrang[1])/50,yrang[2]-legdist/2,paste(as.character(legdist),"m"),cex=.5,srt=270,adj=c(.5,0))      
      }else{
        text(xrang[1]+(xrang[2]-xrang[1])/50,yrang[2]-legdist/2,paste(as.character(legdist/1000),"km"),cex=.5,srt=270,adj=c(.5,0))      
      }      
    }
  }
  for(i in 1:nrow(mat)){
    hour=as.numeric(strsplit(strsplit(as.character(as.POSIXct(mean(c(mat[i,4],mat[i,7]),na.rm=T),origin='1970-01-01'))," ")[[1]][2],":")[[1]][1])
    if(mat[i,1]==1){
      lines(c(mat[i,2],mat[i,5]),c(mat[i,3],mat[i,6]),col=col24hour_v[hour+1])
    }
    if(mat[i,1]==2){
      pwidth=max(.5,min(sqrt(2*(mat[i,7]-mat[i,4])/7200),2))
      points(mat[i,2],mat[i,3],pch=19,col=paste(col24hour_v[hour+1],"CC",sep=""),cex=pwidth)
    }
  }  
  if(write2file){
    dev.off()
  }
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
    pmat=c()
    for(i in 1:length(obj$ID2)){
      if(ptred[i]>0){
        pmat=rbind(pmat,matrix(rep(mobmat[obj$ID2[i],2:3],ptred[i]),ncol=2,byrow=T))
      }
    }  
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

MobmatQualityOK = function(mobmat,obj){
  msg=""
  if(!is.matrix(mobmat)){
    msg = "Mobmat not a matrix. Removing individual from analysis.\n"
  }
  if(length(obj$ID1)==0){
    msg= "No flights in mobmat. Removing individual from analysis.\n"
  }
  if(length(obj$ID2)==0){
    msg= "No pauses in mobmat. Removing individual from analysis.\n"
  }
  return(msg)
}

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
<<<<<<< HEAD
  } 
=======
  } #=
>>>>>>> origin/master
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
  outmat=matrix(,nrow=length(daystr_v),ncol=Nfeatures)
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
  
  outmat=c()
  for(i in IDkeep){
    x=read.csv(filelist[i],fileEncoding="UTF-8")
    outmat=rbind(outmat,c(date_v[i],x$answer))
  }
  try1=try(colnames(outmat)<-c("Date",paste(as.character(x$question.text)," (",as.character(x$question.answer.options),")",sep="")),silent=TRUE)
  if(class(try1) == "try-error"){
    warning(paste("Survey non-constant over time for ID:",SID))
    return(NULL)
  }  
  write.table(outmat,paste("SurveyAnswers_",survey_id,"_",SID,".txt",sep=""),quote=F,col.names=T,row.names=F,sep="\t")
  return("success")
}

