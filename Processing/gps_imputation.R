<<<<<<< HEAD
GPS_imputation = function(patient_name,
                          wtype="GLR",
                          spread_pars=c(10,1),
                          nreps=1, ### simulate missing data numer of times
                          ...
){
  patient_input_filepath = paste(output_filepath, "/Preprocessed_Data/Individual/", patient_name, sep="")
  patient_input_filename = paste(patient_input_filepath, "/gps_preprocessed.rds",sep="")
  patient_output_filepath = paste(output_filepath, "/Processed_Data/Individual/", patient_name, sep="")
  patient_output_filename = paste(patient_output_filepath, "/gps_imputed_mobmat.rds",sep="")
  # Check to see if GPS has been processed
  # IF so, load mobmat, and obj
  if(!file.exists(patient_input_filename)){
    cat("No preprocessed data.\n")
    return(NULL)
  }  
  inlist=readRDS(patient_input_filename)
  mobmat=inlist[[1]];mobmatmiss=inlist[[2]];obj=inlist[[3]];tz=inlist[[4]];CENTERRAD=inlist[[5]];ITRVL=inlist[[6]]
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
  cat("\n")
  saveRDS(list(mobmatsims,objsims,nreps),patient_output_filename)  
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
    outmat_ls=list()
    counter=1
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
          #outmat=rbind(outmat,nextline)
          outmat_ls[[counter]]=nextline
          counter=counter+1
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
          #outmat=rbind(outmat,nextline)
          outmat_ls[[counter]]=nextline
          counter=counter+1
          tarrive=curt
        }else{
          break
        }
      }
    }
    outmat = do.call(rbind,outmat_ls)
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



=======
GPS_imputation = function(patient_name,
                          wtype="GLR",
                          spread_pars=c(10,1),
                          nreps=1, ### simulate missing data numer of times
                          ...
){
  patient_input_filepath = paste(output_filepath, "/Preprocessed_Data/Individual/", patient_name, sep="")
  patient_input_filename = paste(patient_input_filepath, "/gps_preprocessed.rds",sep="")
  patient_output_filepath = paste(output_filepath, "/Processed_Data/Individual/", patient_name, sep="")
  patient_output_filename = paste(patient_output_filepath, "/gps_imputed_mobmat.rds",sep="")
  # Check to see if GPS has been processed
  # IF so, load mobmat, and obj
  if(!file.exists(patient_input_filename)){
    cat("No preprocessed data.\n")
    return(NULL)
  }  
  inlist=readRDS(patient_input_filename)
  mobmat=inlist[[1]];mobmatmiss=inlist[[2]];obj=inlist[[3]];tz=inlist[[4]];CENTERRAD=inlist[[5]];ITRVL=inlist[[6]]
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
  cat("\n")
  saveRDS(list(mobmatsims,objsims,nreps),patient_output_filename)  
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
    outmat_ls=list()
    counter=1
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
          #outmat=rbind(outmat,nextline)
          outmat_ls[[counter]]=nextline
          counter=counter+1
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
          #outmat=rbind(outmat,nextline)
          outmat_ls[[counter]]=nextline
          counter=counter+1
          tarrive=curt
        }else{
          break
        }
      }
    }
    outmat = do.call(rbind,outmat_ls)
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



>>>>>>> origin/master
