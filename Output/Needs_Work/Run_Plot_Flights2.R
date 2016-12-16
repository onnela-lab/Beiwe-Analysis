for(patient_name in patient_names){
  print(patient_name)
  setwd(paste(data_directory,patient_name,"gps",sep="/"))
  load(paste(patient_name,".Rdata",sep=""))
  setwd(paste(data_directory,patient_name,sep="/"))
  
  # Warp the GPS data.
  num_warps  = 10
  warp_matrix = matrix(c(runif(num_warps,min(mobmat[,c("x0","x1")],na.rm=T), max(mobmat[,c("x0","x1")],na.rm=T)),
                         runif(num_warps,min(mobmat[,c("y0","y1")],na.rm=T), max(mobmat[,c("y0","y1")],na.rm=T))),num_warps,2)
  pushes = rep(c(1,0),length.out=num_warps)
  sds = 2*apply(mobmat[,c("x0","y0")], 2, sd, na.rm=TRUE)
  mobmat[,c("x0","y0")] = warps(mobmat[,c("x0","y0")], warp_matrix, pushes, sd1=sds[1],sd2=sds[2])
  mobmat[,c("x1","y1")] = warps(mobmat[,c("x1","y1")], warp_matrix, pushes, sd1=sds[1],sd2=sds[2])
  

  accmat=c()
  textlocs=c();calllocs=c()


  if(file.exists("accelerometer")){
    accmat = read.csv("accelerometer/appended_sheared_file.txt",header=T)
    accmat[,1] = accmat[,1] / 1000
    accmat[,c("hours","days")]    = hours(accmat[,"timestamp"])
  }
  if(file.exists("texts")){
    textlocs = textmat[,1] %>%
      map(GPSlocation, mobility_matrix = mobmat) %>%
      do.call(rbind, .) %>%
      as.data.frame() %>%
      cbind(textmat[,c("message.length","sent.vs.received")])
    colnames(textlocs) = c("x","y", "code", "timestamp","length", "sent.vs.received")
    textlocs[,c("hours","days")]  = hours(textlocs[,"timestamp"])
  }
  if(file.exists("calls")){
    calllocs = callmat[,1] %>%
      map(GPSlocation, mobility_matrix = mobmat) %>%
      do.call(rbind, .) %>%
      as.data.frame() %>%
      cbind(callmat[,c("duration.in.seconds","call.type")])
    colnames(calllocs) = c("x","y", "code", "timestamp","length","call.type")
    calllocs[,c("hours","days")]  = hours(calllocs[,"timestamp"])
  }
  if(file.exists("power_state")){
    statelocs = statemat[,1] %>%
      map(GPSlocation, mobility_matrix = mobmat) %>%
      do.call(rbind, .) %>%
      as.data.frame() %>%
      cbind(statemat[,c("event")])
    colnames(statelocs) = c("x","y", "event","timestamp","screen")
    statelocs[,c("hours","days")] = hours(unlist(statelocs[,"timestamp"]))
    statemat[,c("hours","days")]  = hours(statemat[,"timestamp"])
  }
  
  mobmat = as_data_frame(mobmat)
  mobmat[,c("hours","days")] = hours(unlist(mobmat[,"t0"]))
  mob_subsets = daily_subsets(mobmat, "t0", "t1")
  
  if(file.exists("texts")){
    #textlocs_counts  = ddply(textlocs,.(x,y,code,length,sent.vs.received,hours),nrow)
    text_subsets = daily_subsets(textmat, "timestamp")
    max_text_length = max(textlocs[,"length"])
  }
  if(file.exists("calls")){
    #calllocs_counts  = ddply(calllocs,.(x,y,code,length,call.type,hours),nrow)
    call_subsets = daily_subsets(callmat, "timestamp")
    max_call_length = max(calllocs[,"length"])
    call_brightness = calllocs[,"length"]
  }
  if(file.exists("power_state")){
    #statelocs_counts = ddply(statelocs,.(x,y,event,hours),nrow)
    state_subsets = daily_subsets(statemat, "timestamp")
  }
  
  daynames = list(
    unique(textlocs[,"days"]),
    unique(calllocs[,"days"]),
    unique(statelocs[,"days"])) %>%
    Reduce(union, .) %>%
    unique %>% sort %>% setdiff("NA")
  
  rang_mat = matrix(NA,length(daynames),4)
  for(day_ind in seq_along(daynames)){
    mob_layer = mobmat %>% subset(days==daynames[day_ind])
    lims = plotlimits(mob_layer)
    rang_mat[day_ind,] = c(lims$xrang, lims$yrang)[c(1,3,2,4)]
  }
  S = c(5e8, 1e10, 1e11,1e13)
  lims = Fuzzy.Window(rang_mat,S)
  lims$days = daynames
  
  inner_box_top = 0.9
  pdf("Daily_Plots.pdf", width=9,height=4.25)
  par(cex=1.8, mfrow=c(1,2))
  for(day_ind in seq_along(daynames)){
    textmat_layer = NA; textlocs_layer = NA; callmat_layer = NA; calllocs_layer = NA; state_layer = NA; acc_layer = NA
    mob_layer = mobmat[mob_subsets[[daynames[day_ind]]],]
    if(is.null(nrow(mob_layer))) mob_layer = t(as.data.frame(mob_layer))
    if(daynames[day_ind] %in% unique(textlocs[,"days"])){
      textmat_layer = textmat %>% subset(days==daynames[day_ind])
      textlocs_layer = textlocs %>% subset(days==daynames[day_ind])
    }
    if(daynames[day_ind] %in% unique(calllocs[,"days"])){
      callmat_layer = callmat %>% subset(days==daynames[day_ind])
      calllocs_layer = calllocs %>% subset(days==daynames[day_ind])
    }
    if(daynames[day_ind] %in% unique(statelocs[,"days"])){
      state_layer = statelocs %>% subset(days==daynames[day_ind])
    }
    if(daynames[day_ind] %in% unique(accmat[ ,"days"])){
      acc_layer = accmat %>% subset(days==daynames[day_ind])
    }
    if(daynames[day_ind] %in% unique(statemat[,"days"])){
      statemat_layer = statemat %>% subset(days == daynames[day_ind])
    }
    
    xrang=plotlimits(mobmat)$xrang
    yrang=plotlimits(mobmat)$yrang
    if(xrang[2]-xrang[1]>yrang[2]-yrang[1]){ # inserted from plot.flights2.
      dif=((xrang[2]-xrang[1])-(yrang[2]-yrang[1]))
      yrang[2] = yrang[2]+dif/2
      yrang[1] = yrang[1]-dif/2
    }else{
      dif=((yrang[2]-yrang[1])-(xrang[2]-xrang[1]))
      xrang[2] = xrang[2]+dif/2
      xrang[1] = xrang[1]-dif/2    
    }#end insertion.
    global_length = max(xrang[2]-xrang[1], yrang[2]-yrang[1])
    global_relative_length = 0.05
    total_plot(mob_layer, textmat_layer, textlocs_layer, callmat_layer, calllocs_layer,
               state_layer, xrang = xrang, yrang = yrang,
               rectangle = lims$W[day_ind,], rect_lwd=.75, rect_col="gray", rect_lty=2,
               addlegend=TRUE, main_rectangle = "",title="Static Global View")
    
    par(xpd=NA)
    #statemat_layer = as_data_frame(rbind(statemat_layer[1,],statemat_layer,statemat_layer[nrow(statemat_layer),]))
    #statemat_layer[1,c("event","hours")] = c(ifelse(statemat_layer[1,"event"]=="Screen turned on","Screen turned off","Screen turned on"),0)
    #statemat_layer[nrow(statemat_layer),c("event","hours")] = c(ifelse(statemat_layer[nrow(statemat_layer),"event"]=="Screen turned on","Screen turned off","Screen turned on"),24)
    #statemat_layer[,"hours"] = sapply(statemat_layer[,"hours"], as.numeric) # these four lines fill in states, where you should actually actually look to previous days' data.
    
    for(i in 1:(nrow(statemat_layer)-1)){
      polygon(xrang[2]+(xrang[2]-xrang[1])*midcolumn_x[c(1,1,2,2)],
              yrang[1]+unlist(statemat_layer[i+c(0,1,1,0),"hours"])/24*(yrang[2]-yrang[1])*inner_box_top,#yrang[c(1,2,2,1)],
              border=FALSE,col=ifelse(statemat_layer[i,"event"]=="Screen turned on",light_color(limegreen,1), rgb(.95,.95,.95)))#
    }
    
    if(sum(!is.na(acc_layer))>0){
      timing=as.numeric(acc_layer[,"hours"])
      movement=apply(acc_layer[,c("x","y","z")],1,function(x) sqrt(sum(x**2)))
      movement = movement/max(movement)
      if(length(timing)>1){
        for(j in 1:(length(timing)-1)){
          if(timing[j+1]-timing[j] < .5){ # tests is time between intervals of accelerometer are less than 30 minutes.
            lines(xrang[2]+(xrang[2]-xrang[1])*(midcolumn_x[2]+movement[j+0:1]*(midcolumn_x[2]-midcolumn_x[1])), yrang[1]+(yrang[2]-yrang[1])*timing[j+0:1]/24*inner_box_top)
          }else{lines(xrang[2]+(xrang[2]-xrang[1])*(midcolumn_x[2]+movement[j+0:1]*(midcolumn_x[2]-midcolumn_x[1])), yrang[1]+(yrang[2]-yrang[1])*timing[j+0:1]/24*inner_box_top,col=rgb(.8,.8,.8))}
        }
      }
    }
    polygon(xrang[2]+(xrang[2]-xrang[1])*midcolumn_x[c(1,1,2,2)], yrang[1]+(yrang[c(1,2,2,1)]-yrang[1])*inner_box_top)
    text(rep(xrang[2]*1.1,2),yrang[1]-(yrang[2]-yrang[1])*.02, daynames[day_ind])
    par(xpd=FALSE)
    xrang=lims$W[day_ind,c(1,3)]
    yrang=lims$W[day_ind,c(2,4)]
    if(xrang[2]-xrang[1]>yrang[2]-yrang[1]){ # inserted from plot.flights2.
      dif=((xrang[2]-xrang[1])-(yrang[2]-yrang[1]))
      yrang[2] = yrang[2]+dif/2
      yrang[1] = yrang[1]-dif/2
    }else{
      dif=((yrang[2]-yrang[1])-(xrang[2]-xrang[1]))
      xrang[2] = xrang[2]+dif/2
      xrang[1] = xrang[1]-dif/2    
    }
    global_length = max(xrang[2]-xrang[1], yrang[2]-yrang[1])
    global_relative_length = 0.05
    total_plot(mob_layer=mob_layer, textmat_layer=NA, textlocs_layer=textlocs_layer, callmat_layer=NA, calllocs_layer=calllocs_layer,
               state_layer, xrang = xrang, yrang = yrang,
               rectangle = lims$W[day_ind,], rect_lwd=.75, rect_col="gray", rect_lty=2,
               addlegend=FALSE, title="Dynamic Local View")
  }
  dev.off()
  
}
code_location = paste(source_directory,"Preprocessing", "accelerometer_preprocessing.py",sep="/")
system(paste("python", code_location, data_directory, patient_name, 60, sep=" "))

system("python accelerometer_preprocessing.py")
