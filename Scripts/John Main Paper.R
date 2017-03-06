start = Sys.time()

# Source all files

source_filepath  = "C:/Users/Patrick/Desktop/Beiwe-Analysis-master"
data_filepath    = "C:/Phoenix/School/Harvard/Research/Beiwe/Studies/John_Schizophrenia/Data/2017.01.09"

source_categories = c("Utility", "Preprocessing", "Processing", "Output")
for(source_category in source_categories){
  setwd(paste(source_filepath,source_category,sep="/"))
  file_sources = list.files(pattern="*.R")
  sapply(file_sources,source,.GlobalEnv)
}
###################################
### individual patient analysis ###
###################################

patient_names = list.files(data_filepath)[-grep("\\.",list.files(data_filepath))]
for(patient_name in patient_names){
  
  # Preprocess Data
  
  print(patient_name)
  GPS_analysis = MobilityFeatures(patient_name, paste(data_filepath, patient_name,"gps",sep="/"))
  textmat  = text_preprocessing(data_filepath, patient_name)
  callmat  = calls_preprocessing(data_filepath, patient_name)
  statemat = powerstate_preprocessing(data_filepath, patient_name)
  accmat60 = accelerometer_preprocessing(data_filepath, patient_name, source_filepath, minutes = 60)
  surveymat = surveys_preprocessing(data_filepath, patient_name)
  
  
  # Process Data
  
  mobmat = warp_GPS(GPS_analysis$mobmat, seed = 1)
  textlocs  = text_locations (textmat,  mobmat)
  calllocs  = call_locations (callmat,  mobmat)
  statelocs = powerstate_locations(statemat, mobmat)
  daynames = get_days(textlocs, calllocs, statelocs)
  
  
  # Output
  
  location_plot(mobmat, textmat, callmat, statemat, accmat60, textlocs, calllocs, statelocs, daynames)
  
}




###################################
#### combined patient analysis ####
###################################

accelerometer_counts = data_quality(data_filepath, code_filepath = "C:/Phoenix/School/Harvard/Research/Beiwe/Code/find_pulses.py",
  stream = "accelerometer",
  pulse_duration = 60,
  break_duration = 60,
  frequency = 10,
  millisecond_divider = 60*1000)
accelerometer_coverage = coverage_over_time(accelerometer_counts)


gps_counts = data_quality(data_filepath, code_filepath = "C:/Phoenix/School/Harvard/Research/Beiwe/Code/find_pulses.py",
  stream = "gps",
  pulse_duration = 60,
  break_duration = 60*10,
  frequency = 1,
  millisecond_divider = 60*1000)
gps_coverage = coverage_over_time(gps_counts)

survey_mat = survey_quality(data_filepath)





###################################
####          plotting         ####
###################################


data_quality_plotting(output_filepath = "C:/Users/Patrick/Desktop/Accelerometer Data Quality.pdf",
                      counts = accelerometer_counts, coverage = accelerometer_coverage, stream = "accelerometer", frequency = 10, pulse_duration = 60, break_duration = 60)

data_quality_plotting(output_filepath = "C:/Users/Patrick/Desktop/GPS Data Quality.pdf",
                      counts = gps_counts, coverage = gps_coverage, stream = "gps", frequency = 1, pulse_duration = 60, break_duration = 60*10)

survey_quality_plotting("C:/Users/Patrick/Desktop/Survey Data Quality.pdf", curated_total)
















# # look at location plots later.
# 
# max_text_length = max(textlocs[,"length"])
# max_call_length = max(calllocs[,"length"])
# call_brightness = calllocs[,"length"]
# 
# mob_subsets = daily_subsets(mobmat, "t0", "t1")
# if(exists("texts")) text_subsets = daily_subsets(textmat, "timestamp")
# if(exists("calls")) call_subsets = daily_subsets(callmat, "timestamp")
# if(exists("power_state")) state_subsets = daily_subsets(statemat, "timestamp")
# 
# mobmat = as_data_frame(mobmat)
# mobmat[,c("hours","days")] = hours(unlist(mobmat[,"t0"]))
# 
# rang_mat = matrix(NA,length(daynames),4)
# for(day_ind in seq_along(daynames)){
#   mob_layer = mobmat %>% subset(days==daynames[day_ind])
#   lims = plotlimits(mob_layer)
#   rang_mat[day_ind,] = c(lims$xrang, lims$yrang)[c(1,3,2,4)]
# }
# S = c(5e8, 1e10, 1e11,1e13)
# lims = Fuzzy.Window(rang_mat,S)
# lims$days = daynames
# 
# pdf("Daily_Plots.pdf", width=9,height=4.25)
# par(cex=1.8, mfrow=c(1,2))
# for(day_ind in seq_along(daynames)){
#   textmat_layer = NA; textlocs_layer = NA; callmat_layer = NA; calllocs_layer = NA; state_layer = NA; acc_layer = NA
#   mob_layer = mobmat[mob_subsets[[daynames[day_ind]]],]
#   if(is.null(nrow(mob_layer))) mob_layer = t(as.data.frame(mob_layer))
#   
#   if(daynames[day_ind] %in% unique(textlocs[,"days"])){
#     textmat_layer = textmat %>% subset(days==daynames[day_ind])
#     textlocs_layer = textlocs %>% subset(days==daynames[day_ind])
#   }
#   if(daynames[day_ind] %in% unique(calllocs[,"days"])){
#     callmat_layer = callmat %>% subset(days==daynames[day_ind])
#     calllocs_layer = calllocs %>% subset(days==daynames[day_ind])
#   }
#   if(daynames[day_ind] %in% unique(statelocs[,"days"]))
#     state_layer = statelocs %>% subset(days==daynames[day_ind])
#   
#   if(daynames[day_ind] %in% unique(accmat60[ ,"days"]))
#     acc_layer = accmat60 %>% subset(days==daynames[day_ind])
#   
#   if(daynames[day_ind] %in% unique(statemat[,"days"]))
#     statemat_layer = statemat %>% subset(days == daynames[day_ind])
#   
#   
#   xrang=plotlimits(mobmat)$xrang
#   yrang=plotlimits(mobmat)$yrang
#   if(xrang[2]-xrang[1]>yrang[2]-yrang[1]){ # inserted from plot.flights2.
#     dif=((xrang[2]-xrang[1])-(yrang[2]-yrang[1]))
#     yrang[2] = yrang[2]+dif/2
#     yrang[1] = yrang[1]-dif/2
#   }else{
#     dif=((yrang[2]-yrang[1])-(xrang[2]-xrang[1]))
#     xrang[2] = xrang[2]+dif/2
#     xrang[1] = xrang[1]-dif/2    
#   }#end insertion.
#   global_length = max(xrang[2]-xrang[1], yrang[2]-yrang[1])
#   global_relative_length = 0.05
#   total_plot(mob_layer, textmat_layer, textlocs_layer, callmat_layer, calllocs_layer,
#              state_layer, xrang = xrang, yrang = yrang,
#              global_length=global_length, global_relative_length=global_relative_length, 
#              max_text_length=max_text_length, max_call_length=max_call_length,call_brightness=call_brightness,
#              rectangle = lims$W[day_ind,], rect_lwd=.75, rect_col="gray", rect_lty=2,
#              addlegend=TRUE, main_rectangle = "",title="Static Global View")
#   par(xpd=NA)
#   #statemat_layer = as_data_frame(rbind(statemat_layer[1,],statemat_layer,statemat_layer[nrow(statemat_layer),]))
#   #statemat_layer[1,c("event","hours")] = c(ifelse(statemat_layer[1,"event"]=="Screen turned on","Screen turned off","Screen turned on"),0)
#   #statemat_layer[nrow(statemat_layer),c("event","hours")] = c(ifelse(statemat_layer[nrow(statemat_layer),"event"]=="Screen turned on","Screen turned off","Screen turned on"),24)
#   #statemat_layer[,"hours"] = sapply(statemat_layer[,"hours"], as.numeric) # these four lines fill in states, where you should actually actually look to previous days' data.
#   
#   for(i in 1:(nrow(statemat_layer)-1)){
#     polygon(xrang[2]+(xrang[2]-xrang[1])*midcolumn_x[c(1,1,2,2)],
#             yrang[1]+unlist(statemat_layer[i+c(0,1,1,0),"hours"])/24*(yrang[2]-yrang[1])*inner_box_top,#yrang[c(1,2,2,1)],
#             border=FALSE,col=ifelse(statemat_layer[i,"event"]=="Screen turned on",light_color(limegreen,1), rgb(.95,.95,.95)))#
#   }
#   
#   if(sum(!is.na(acc_layer))>0){
#     timing=as.numeric(acc_layer[,"hours"])
#     movement=apply(acc_layer[,c("x","y","z")],1,function(x) sqrt(sum(x**2)))
#     movement = movement/max(movement)
#     if(length(timing)>1){
#       for(j in 1:(length(timing)-1)){
#         if(timing[j+1]-timing[j] < .5){ # tests is time between intervals of accelerometer are less than 30 minutes.
#           lines(xrang[2]+(xrang[2]-xrang[1])*(midcolumn_x[2]+movement[j+0:1]*(midcolumn_x[2]-midcolumn_x[1])), yrang[1]+(yrang[2]-yrang[1])*timing[j+0:1]/24*inner_box_top)
#         }else{lines(xrang[2]+(xrang[2]-xrang[1])*(midcolumn_x[2]+movement[j+0:1]*(midcolumn_x[2]-midcolumn_x[1])), yrang[1]+(yrang[2]-yrang[1])*timing[j+0:1]/24*inner_box_top,col=rgb(.8,.8,.8))}
#       }
#     }
#   }
#   polygon(xrang[2]+(xrang[2]-xrang[1])*midcolumn_x[c(1,1,2,2)], yrang[1]+(yrang[c(1,2,2,1)]-yrang[1])*inner_box_top)
#   text(rep(xrang[2]*1.1,2),yrang[1]-(yrang[2]-yrang[1])*.02, daynames[day_ind])
#   par(xpd=FALSE)
#   xrang=lims$W[day_ind,c(1,3)]
#   yrang=lims$W[day_ind,c(2,4)]
#   if(xrang[2]-xrang[1]>yrang[2]-yrang[1]){ # inserted from plot.flights2.
#     dif=((xrang[2]-xrang[1])-(yrang[2]-yrang[1]))
#     yrang[2] = yrang[2]+dif/2
#     yrang[1] = yrang[1]-dif/2
#   }else{
#     dif=((yrang[2]-yrang[1])-(xrang[2]-xrang[1]))
#     xrang[2] = xrang[2]+dif/2
#     xrang[1] = xrang[1]-dif/2    
#   }
#   global_length = max(xrang[2]-xrang[1], yrang[2]-yrang[1])
#   total_plot(mob_layer=mob_layer, textmat_layer=NA, textlocs_layer=textlocs_layer, callmat_layer=NA, 
#              calllocs_layer=calllocs_layer,
#              state_layer, xrang = xrang, yrang = yrang,
#              global_length = global_length, global_relative_length = global_relative_length, 
#              max_text_length=max_text_length, max_call_length=max_call_length,call_brightness=call_brightness,
#              rectangle = lims$W[day_ind,], rect_lwd=.75, rect_col="gray", rect_lty=2,
#              addlegend=FALSE, title="Dynamic Local View")
# }
















