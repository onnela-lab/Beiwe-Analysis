midcolumn_x = c(.07, .11)

GPSlocation = function(time_of_interest, mobility_matrix){
  row = try(min(which((mobility_matrix[,"t0"] < time_of_interest) & (time_of_interest < mobility_matrix[,"t1"]))))
  if(row != Inf){# this function 
    code = mobility_matrix[row,"Code"]
    timestamp = mobility_matrix[row,"t0"]
    if(code==1){
      row_time = mobility_matrix[row,c("t0","t1")]
      time_within_row = (row_time[2] - time_of_interest)/(row_time[2]-row_time[1])
      GPS_location = apply(rbind(mobility_matrix[row,c("x0","y0")], mobility_matrix[row,c("x1","y1")]),2,
                           weighted.mean, w = c(1-time_within_row,time_within_row))
      return(c(GPS_location,code,timestamp))
    }
    if(mobility_matrix[row,"Code"] %in% c(2,3))
      return(c(mobility_matrix[row,c("x0","y0")],code, timestamp))
    if(code== 4)
      return(c(NA,NA,code, timestamp))
  }
  else(return(NA))
}

GPSarrow = function(x, y, length = .1,
                    angle = 0,
                    arrowhead_at_end = TRUE,
                    solid_head = TRUE,
                    arrowcolor = rgb(.5,.5,.5),
                    arrow_thickness = .9,
                    arrowhead_length = .1,
                    solid_arrowhead_length = .2,
                    arrow_width = .6){
  arrowhead_length = arrowhead_length*length
  solid_arrowhead_length = solid_arrowhead_length*length
  arrow_narrowness = 2.3 + (3-2.3)*(1-arrow_width)
  x1 = x+cos(angle)*length
  y1 = y+sin(angle)*length
  if(!arrowhead_at_end){
    x2 = x
    y2 = y
    x  = x1
    y  = y1
    x1 = x2
    y1 = y2
    angle = angle+pi
  }
  lines(c(x,x1),c(y,y1),lwd=arrow_thickness,col=arrowcolor)
  if(solid_head){
    polygon(
      c(x1,x1+cos(angle+pi*arrow_narrowness/3*c(-1,1))*solid_arrowhead_length),
      c(y1,y1+sin(angle+pi*arrow_narrowness/3*c(-1,1))*solid_arrowhead_length),col=arrowcolor,
      border=arrowcolor,lwd=arrow_thickness)
  }
  if(!solid_head){
    lines(c(x1,x1+cos(angle+pi*arrow_narrowness/-3)*solid_arrowhead_length),c(y1,y1+sin(angle+pi*2.55/-3)*solid_arrowhead_length),lwd=arrow_thickness,col=arrowcolor)
    lines(c(x1,x1+cos(angle+pi*arrow_narrowness/3 )*solid_arrowhead_length),c(y1,y1+sin(angle+pi*2.55/3 )*solid_arrowhead_length),lwd=arrow_thickness,col=arrowcolor)
  }
}

plot.flights2 <-
  function(mat,xrang=NULL,yrang=NULL,diminch=6,add2plot=FALSE,addscale = TRUE, addlegend=TRUE,
           outfile=NULL,title=NULL, main_rectangle = NA){

    #col24hour_v=c(c("#08306b","#08519c","#2171b5","#4292c6","#6baed6","#9ecae1","#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#99000d"),rev(c("#08306b","#08519c","#2171b5","#4292c6","#6baed6","#9ecae1","#fcbba1","#fc9272","#fb6a4a","#ef3b2c","#cb181d","#99000d")))
    col24hour_v=gray(seq(.85,.15,length.out=24))
    
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
      
      if(!is.na(main_rectangle)){
        par(xpd=NA)
        left_rect = c(xrang+c(-1,1)*.0*(xrang[2]-xrang[1]), yrang+c(-1,1)*.0*(yrang[2]-yrang[1]))
        polygon(c(left_rect[1],left_rect[1],left_rect[2],left_rect[2]),
                c(left_rect[3],left_rect[4],left_rect[4],left_rect[3]),
                border="gray", lwd=.75, lty = 1)
        par(xpd=FALSE)
      }
      
      
      
      xleg1=xrang[1] + (xrang[2]-xrang[1])*.02  
      xleg2=xleg1+(xrang[2]-xrang[1])*.038
      xleg_mid = xleg1 + (xrang[2]-xrang[1])*.015
      yincr=(yrang[2]-yrang[1]) * 0.022
      legtext=c(" 6AM"," 12PM"," 6PM"," 12AM")
      if(addlegend){
        for(i in 1:24){
          polygon(c(xleg1,xleg1,xleg2,xleg2),c(yrang[1]+(i-1)*yincr,yrang[1]+i*yincr,yrang[1]+i*yincr,yrang[1]+(i-1)*yincr),col=col24hour_v[i])
          if(i%%6==0){
            text(xleg2,yrang[1]+i*yincr,legtext[floor(i/6)],adj=0,cex=.5)        
          }
        }
        lines(c(xleg1,xleg1*.2+xleg2*.8), rep(yrang[1]+27*yincr, 2), lty=2)
        text(xleg2,yrang[1]+27*yincr,"Flight",adj=0,cex=.5)
        points(xleg_mid,yrang[1]+29*yincr,cex=2,pch=16)
        text(xleg2,yrang[1]+29*yincr,">4 Hr Pause",adj=0,cex=.5)
        points(xleg_mid,yrang[1]+31*yincr,cex=1,pch=16)
        text(xleg2,yrang[1]+31*yincr,"1 Hr Pause",adj=0,cex=.5)
        points(xleg_mid,yrang[1]+33*yincr,cex=.5,pch=16)
        text(xleg2,yrang[1]+33*yincr,"<30 Min Pause",adj=0,cex=.5)
        text(xleg2,yrang[1]+35*yincr,"Calls",adj=0,cex=.5)
        text(xleg2,yrang[1]+37*yincr,"Texts",adj=0,cex=.5)
        GPSarrow(xleg1,yrang[1]+35*yincr, arrowcolor = light_color(cerulean, ink_depth = .5),  length=(xrang[2]-xrang[1])*.025, arrow_thickness = 1.2, arrow_width = 1, solid_arrowhead_length = .35)
        GPSarrow(xleg1,yrang[1]+37*yincr, arrowcolor = light_color(vermillion, ink_depth = .5),length=(xrang[2]-xrang[1])*.025, arrow_thickness = 1.2, arrow_width = 1, solid_arrowhead_length = .35)
        points(mean(xleg_mid,xleg2),yrang[1]+39*yincr,cex=.5,pch=16, col = light_color(limegreen, 1))
        text(xleg2,yrang[1]+39*yincr,"Screen On/Off",adj=0,cex=.5)
        #text(xleg1,yrang[1]+32.3*yincr,"Pause\nDuration",adj=0,cex=.5)
      }
      if(addscale){
        y_leg = yrang[2]-(yrang[2]-yrang[1])*.01
        legdist=10^floor(log10((yrang[2]-yrang[1])*.2))
        lines(c(xleg_mid,xleg_mid),c(y_leg,y_leg-legdist))
        lines(xleg_mid+c(-1,1)*(xrang[2]-xrang[1])/120,c(y_leg,y_leg))
        lines(xleg_mid+c(-1,1)*(xrang[2]-xrang[1])/120,c(y_leg-legdist,y_leg-legdist))
        if(log10(legdist)<3){
          text(xleg_mid+(xrang[2]-xrang[1])/50,y_leg-legdist/2,paste(as.character(legdist),"m"),cex=.5,srt=270,adj=c(.5,0))      
        }else{
          text(xleg_mid+(xrang[2]-xrang[1])/50,y_leg-legdist/2,paste(as.character(legdist/1000),"km"),cex=.5,srt=270,adj=c(.5,0))      
        } 
      }
    }
    for(i in 1:nrow(mat)){
      hour=unlist(floor(mat[i,"hours"]))
      if(mat[i,1]==1){
        lines(c(mat[i,2],mat[i,5]),c(mat[i,3],mat[i,6]),col=col24hour_v[hour+1])
      }
      if(mat[i,1]==2){
        pwidth=max(.5,min(sqrt(2*(mat[i,7]-mat[i,4])/7200),2))
        points(mat[i,2],mat[i,3],pch=1,col=paste(col24hour_v[hour+1],"CC",sep=""),cex=pwidth)
      }
    }  
    if(write2file){
      dev.off()
    }
  }

daily_subsets = function(mat, time_string = "t0", end_time_string = NA, tz=""){
  curdate=strsplit(as.character(as.POSIXct(as.numeric(mat[1,time_string]),tz=tz,origin="1970-01-01"))," ")[[1]][1]
  curtime=strsplit(strsplit(as.character(as.POSIXct(as.numeric(mat[1,time_string]),tz=tz,origin="1970-01-01"))," ")[[1]][2],":")
  subsetinds_v = list()
  daystr_v = c(curdate)
  dayind=1
  subsetinds = c(1)
  for(i in 2:nrow(mat)){
    nexdate=strsplit(as.character(as.POSIXct(as.numeric(mat[i,time_string]),tz=tz,origin="1970-01-01"))," ")[[1]][1]
    if(nexdate == curdate && i<nrow(mat)){
      subsetinds=c(subsetinds,i)
    }else{
      subsetinds_v[[curdate]]=subsetinds
      dayind=dayind+1
      if(mat[i-1,1]==2 && (mat[i-1,end_time_string]-mat[i-1,time_string])/(60*60*24)>1){
        ii=1
        middate = strsplit(as.character(as.POSIXct(as.numeric(mat[i-1,time_string])+(60*60*24)*ii,tz=tz,origin="1970-01-01"))," ")[[1]][1]
        while(middate!=nexdate){
          subsetinds_v[[dayind]]=i-1
          dayind=dayind+1
          if(middate!=daystr_v[length(daystr_v)]){
            daystr_v=c(daystr_v,middate)        
          }          
          ii=ii+1
          middate = strsplit(as.character(as.POSIXct(as.numeric(mat[i-1,time_string])+(60*60*24)*ii,tz=tz,origin="1970-01-01"))," ")[[1]][1]
        }
      }
      curdate=nexdate
      subsetinds=c(i-1,i)
      if(curdate!=daystr_v[length(daystr_v)] && length(daystr_v)!=length(subsetinds_v)){
        daystr_v=c(daystr_v,curdate)        
      }
    }
  }  
  return(subsetinds_v)
}

hours = function(timestamps){ # converts timestamps into hour of the day.
  days = timestamps %>% unlist %>% as.POSIXlt(origin = "1970-01-01") %>%
    as.character %>%
    map(function(x){if(is.na(x)){return(c("NA","NA"))}else{return(strsplit(x, " "))}}) %>%
    unlist %>%
    matrix(nrow=2) %>%
    t
  days = days[,1]
  hours = as.POSIXlt(timestamps, origin = "1970-01-01") %>%
    map(function(timestamp){timestamp %>%
        unclass() %>%
        unlist()}) %>%
    data.frame() %>% 
    cbind(timestamps) %>%
    select(hour, min, sec) %>%
    apply(1, function(times) sum(times * c(1, 1/60, 1/3600)))
  output = as_data_frame(cbind(hours=hours, days=days))
  output["hours"] = lapply(output["hours"], as.numeric)
  return(output)
}

#patient_names = c("ws535wyt","x64sum6q","v5k3vk1b","upgskgun","euvxbf3w","dske5c2t") #just restricted to patients with full data for now
#setwd("C:/Users/Patrick/Desktop/schizophrenia_patients")

total_plot = function(mob_layer, textmat_layer=NA, textlocs_layer=NA,callmat_layer=NA, calllocs_layer=NA,
                      state_layer=NA, acc_layer = NA,rectangle = NA, rect_col = "black",
                      rect_lwd = 1.5, rect_lty = 1, second_arrow = TRUE,arrowhead_at_end = TRUE, solid_head=TRUE,
                      text_brightness=.5, call_brightness=.5,...){
  plot.flights2(mob_layer, ...)
  color = vermillion
  if(length(unlist(textlocs_layer))>0){if(sum(!is.na(textlocs_layer))>0){
    for(i in 1:nrow(textlocs_layer)){
      text_brightness = textlocs_layer[i,"length"]/max_text_length
      angle = i/nrow(textlocs_layer)*pi*.9 # ensures full spread across quadrants 1 and 2
      #angle = pi/2-text_layer[i,"hours"]/24*2*pi # 24-hour clock.
      if(!is.na(angle)) if(angle<=0) angle = angle + 2*pi

      arrowhead_at_end = textlocs_layer[i,"sent.vs.received"] == "sent SMS"
      GPSarrow(textlocs_layer[i,1], textlocs_layer[i,2],
               length = global_relative_length * global_length,
               angle=angle, arrowhead_at_end = arrowhead_at_end,
               arrowcolor=light_color(color, text_brightness))
    }}}
    if(length(unlist(textmat_layer))>0){if(sum(!is.na(textmat_layer))>0){
      for(i in 1:nrow(textmat_layer)){
        par(xpd=NA)
        GPSarrow(xrang[2]+(xrang[2]-xrang[1])*midcolumn_x[1],yrang[1]+(yrang[2]-yrang[1])*textmat_layer[i,"hours"]/24*inner_box_top, # pretty clunky.  But it get both arrows on!
               length = global_relative_length * global_length,
               angle=pi, arrowhead_at_end = arrowhead_at_end,solid_head = TRUE,
               arrowcolor=light_color(color, text_brightness))
        par(xpd=FALSE)
    }}}
  color = cerulean
  if(length(unlist(calllocs_layer))>0){if(sum(!is.na(calllocs_layer))>0){
    for(i in 1:nrow(calllocs_layer)){
      call_brightness = calllocs_layer[i,"length"]/max_call_length
      angle = pi-i/nrow(calllocs_layer)*pi*.9 # ensures full spread across quadrants 3 and 4
      #angle = pi/2-call_layer[i,"hours"]/24*2*pi # 24-hour clock.
      if(!is.na(angle)) if(angle<=0) angle = angle + 2*pi
      solid_head = calllocs_layer[i,"call.type"] != "Missed Call"
      if(sum(!is.na(calllocs_layer))>0) arrowhead_at_end = calllocs_layer[i,"call.type"] == "Outgoing Call"
      
      if(sum(!is.na(calllocs_layer))>0) GPSarrow(calllocs_layer[i,"x"], calllocs_layer[i,"y"],
               length = global_relative_length * global_length,
               angle=angle, arrowhead_at_end = arrowhead_at_end,
               solid_head = solid_head,
               arrowcolor=light_color(color, call_brightness))
    }}}      
    if(length(unlist(callmat_layer))>0){if(sum(!is.na(callmat_layer))>0){
      for(i in 1:nrow(callmat_layer)){
      par(xpd=NA)
      GPSarrow(xrang[2]+(xrang[2]-xrang[1])*midcolumn_x[1],yrang[1]+(yrang[2]-yrang[1])*callmat_layer[i,"hours"]/24*inner_box_top,
               length = global_relative_length * global_length,
               angle=pi, arrowhead_at_end = arrowhead_at_end,
               solid_head = solid_head,
               arrowcolor=light_color(color, call_brightness))
      par(xpd=FALSE)
      }}}
  if(length(unlist(state_layer))>0){if(sum(!is.na(state_layer))>0){
    color = light_color(limegreen,1)
    for(i in 1:nrow(state_layer)){
      points(state_layer[i,1], state_layer[i,2], col=color,pch=16,cex=.4)
    }
  }}
  if(sum(!is.na(rectangle))>0){
    polygon(c(rectangle[1],rectangle[1],rectangle[3],rectangle[3]),
            c(rectangle[2],rectangle[4],rectangle[4],rectangle[2]),
            border=rect_col, lwd=rect_lwd, lty = rect_lty)
  }
}











