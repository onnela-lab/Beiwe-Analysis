GPS_preprocessing = function(patient_name,
                             ACCURACY_LIM=51, ### meters GPS accuracy
                             ITRVL=10, ### seconds (data concatenation)
                             tz="", ### time zone of data, defaults to current time zone
                             CENTERRAD=200, ### meters radius from significant locations considered
                             minpausedur=300,
                             minpausedist=60,
                             rad_fp=NULL,
                             wid_fp=NULL,
                             verbose=TRUE,
                             ...
){
  patient_output_filepath = paste(output_filepath, "/Preprocessed_Data/Individual/", patient_name, sep="")
  patient_output_filename = paste(patient_output_filepath, "/gps_preprocessed.rds",sep="")
  if(file.exists(patient_output_filename)){
    if(verbose) cat("GPS already preprocessed.\n")
  }else{
    filelist <- list.files(path=paste(data_filepath,patient_name,"gps",sep="/"),pattern = "\\.csv$",full.names=T)
    if(length(filelist)==0){return(NULL)}
    mobmatmiss=GPS2MobMat(filelist,itrvl=ITRVL,accuracylim=ACCURACY_LIM,r=rad_fp,w=wid_fp)
    mobmat = GuessPause(mobmatmiss,mindur=minpausedur,r=minpausedist)
    if(!is.matrix(mobmat)){
      cat("Insufficient GPS data.\n")
      return(NULL)
    }
    obj=InitializeParams(mobmat)
    qOKmsg=MobmatQualityOK(mobmat,obj)
    if(qOKmsg!=""){
      if(verbose){cat(qOKmsg,"\n")}
      return(NULL)
    }else{
      saveRDS(list(mobmat,mobmatmiss,obj,tz,CENTERRAD,ITRVL),patient_output_filename)  
    }
  }
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
  outmat_ls=list()
  counter=1
  for(i in 1:nrow(avgmat)){
    #ProgressBar(nrow(avgmat),i)
    if(avgmat[i,1]==4){
      outmat_ls[[counter]]=ExtractFlights(avgmat[curind:(i-1),c(5,6,2)],r,w)
      outmat_ls[[counter+1]]=c(avgmat[i,1],NA,NA,avgmat[i,2],NA,NA,avgmat[i,3])
      counter=counter+2
      #      outmat=rbind(outmat,ExtractFlights(avgmat[curind:(i-1),c(5,6,2)],r,w),
      #                   c(avgmat[i,1],NA,NA,avgmat[i,2],NA,NA,avgmat[i,3]))
      curind=i+1
    }
  }
  outmat = do.call(rbind,outmat_ls)
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
  flatmat_ls = list()
  counter=1
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
            #flatmat=rbind(flatmat,c(inds,inds+incr-2))
            flatmat_ls[[counter]]=c(inds,inds+incr-2)
            counter=counter+1
          }
        }else{
          #flatmat=rbind(flatmat,c(inds,inds+incr-1))          
          flatmat_ls[[counter]]=c(inds,inds+incr-1)
          counter=counter+1
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
        #flatmat=rbind(flatmat,c(inds,inds+incr-1))
        flatmat_ls[[counter]]=c(inds,inds+incr-1)
        counter=counter+1
      }
      break
    }
  }
  flatmat = do.call(rbind,flatmat_ls)
  if(!is.matrix(flatmat) || nrow(flatmat)==0){
    return(mat)
  }else{
    outmat=c()
    outmat_ls=list()
    counter=1
    if(flatmat[1,1]>1){
      outmat=mat[1:(flatmat[1,1]-1),]
      outmat_ls[[counter]]=mat[1:(flatmat[1,1]-1),]
      counter=counter+1
    }
    for(i in 1:nrow(flatmat)){
      #ProgressBar(nrow(flatmat),i)
      #outmat=rbind(outmat,Collapse2Pause(mat[flatmat[i,1]:flatmat[i,2],]))
      outmat_ls[[counter]]=Collapse2Pause(mat[flatmat[i,1]:flatmat[i,2],])
      counter=counter+1
      if(i<nrow(flatmat) && flatmat[i,2]<flatmat[i+1,1]-1){
        #outmat=rbind(outmat,mat[(flatmat[i,2]+1):(flatmat[i+1,1]-1),])
        outmat_ls[[counter]]=mat[(flatmat[i,2]+1):(flatmat[i+1,1]-1),]
        counter=counter+1
      }
    }    
    if(flatmat[nrow(flatmat),2]<nrow(mat)){
      #outmat=rbind(outmat,mat[(flatmat[nrow(flatmat),2]+1):nrow(mat),])
      outmat_ls[[counter]]=mat[(flatmat[nrow(flatmat),2]+1):nrow(mat),]
      counter=counter+1
    }
  }
  outmat=do.call(rbind,outmat_ls)
  if(nrow(outmat)==1){
    rownames(outmat)=NULL
    colnames(outmat)=c("Code","x0","y0","t0","x1","y1","t1")
    return(outmat)
  }
  # Group together adjacent pauses, averaging their location
  flatmat2=c()
  flatmat2_ls = list()
  counterf = 1
  outmat2=c()
  outmat2_ls = list()
  countero = 1
  collapse=FALSE
  for(i in 2:nrow(outmat)){
    if(outmat[i,1]!=2 && !collapse){
      next
    }else if(outmat[i,1]!=2 && collapse){
      collapse=FALSE
      #flatmat2=rbind(flatmat2,c(cstart,i-1))
      flatmat2_ls[[counterf]]=c(cstart,i-1)
      counterf=counterf+1
    }else  if(outmat[i,1]==2 && outmat[i-1,1]==2 && !collapse){
      cstart=i-1
      collapse=TRUE
    }else if(outmat[i,1]==2 && collapse){
      next
    }
  }
  if(collapse && outmat[nrow(outmat),1]==2){
    #flatmat2=rbind(flatmat2,c(cstart,nrow(outmat)))
    flatmat2_ls[[counterf]]=c(cstart,nrow(outmat))
    counterf=counterf+1
  }
  flatmat2 = do.call(rbind,flatmat2_ls)
  if(is.null(flatmat2)){
    #outmat2=outmat
    outmat2_ls[[countero]]=outmat
    countero=countero+1
  }else{
    flatmat2=matrix(flatmat2,ncol=2)
    #outmat2=c()
    if(flatmat2[1,1]>1){
      #outmat2=outmat[1:(flatmat2[1,1]-1),]
      outmat2_ls[[countero]]=outmat[1:(flatmat2[1,1]-1),]
      countero=countero+1
    }
    for(i in 1:nrow(flatmat2)){
      #outmat2=rbind(outmat2,Collapse2Pause(outmat[flatmat2[i,1]:flatmat2[i,2],]))
      outmat2_ls[[countero]]=Collapse2Pause(outmat[flatmat2[i,1]:flatmat2[i,2],])
      countero=countero+1
      if(i<nrow(flatmat2) && flatmat2[i,2] < flatmat2[i+1,1]-1){
        #outmat2=rbind(outmat2,outmat[(flatmat2[i,2]+1):(flatmat2[i+1,1]-1),])
        outmat2_ls[[countero]]=outmat[(flatmat2[i,2]+1):(flatmat2[i+1,1]-1),]
        countero=countero+1
      }
    }
    if(flatmat2[nrow(flatmat2),2]<nrow(outmat)){
      #outmat2=rbind(outmat2,outmat[(flatmat2[nrow(flatmat2),2]+1):nrow(outmat),])
      outmat2_ls[[countero]]=outmat[(flatmat2[nrow(flatmat2),2]+1):nrow(outmat),]
      countero=countero+1
    }
  }
  outmat2 = do.call(rbind,outmat2_ls)
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
  counter=1
  out_ls = list()
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
      #out=rbind(out,nextline)
      out_ls[[counter]]=nextline
      counter=counter+1
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
      #out=rbind(out,nextline)
      out_ls[[counter]]=nextline
      counter=counter+1
      nextline=rep(NA,6)
      curind=nexind-1
      nextline[1:3]=mat[curind,]
    }
    if(nexind>nrow(mat)){break}
  }
  out = do.call(rbind,out_ls)
  outp_ls=list()
  counter=1
  #outp=c()
  if(out[1,3]>min(mat[,3])){
    #outp=rbind(outp,c(2,out[1,1],out[1,2],min(mat[,3]),NA,NA,out[1,3]))
    outp_ls[[counter]]=c(2,out[1,1],out[1,2],min(mat[,3]),NA,NA,out[1,3])
    counter=counter+1
    if(nrow(out)==1){
      outp=matrix(outp_ls[[1]],nrow=1)
      outp[1,7]=out[1,6]
      return(outp)
    }
  }
  if(nrow(out)==1){
    if(counter==2){
      outp=matrix(outp_ls[[1]],nrow=1)
    }else{
      outp=c()
    }
    outp=rbind(outp,c(1,out))
    return(outp)
  }
  for(i in 1:(nrow(out)-1)){
    #outp=rbind(outp,c(1,out[i,]))
    outp_ls[[counter]]=c(1,out[i,])
    counter=counter+1
    if(out[i,6]<out[i+1,3]){
      #outp=rbind(outp,c(2,out[i,4],out[i,5],out[i,6],NA,NA,out[i+1,3]))
      outp_ls[[counter]]=c(2,out[i,4],out[i,5],out[i,6],NA,NA,out[i+1,3])
      counter=counter+1
    }
  }
  outp=do.call(rbind,outp_ls)
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