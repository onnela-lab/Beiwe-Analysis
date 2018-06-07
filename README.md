# Introduction
`Beiwe-Analysis` is our GitHub code repository for analyzing Beiwe and collaborator data.  The idea is to help make study analysis as systematic and error-free as the data itself.  Some of this code references **`Python`** or **`C`**-related code for speed, but most of this code is in **`R`**, and is readily converted into an **`R`** package.  In the near future, we will provide a notebook example showing how to use these functions.

So far this has been an effort of Ian and Patrick, but we intend to include the projects of the whole lab, with the goal of making our work public in the Fall.  These efforts are closely related to Matt's `beiwedata`, and we plan to merge our efforts in the future.

# Table of Contents
- [Overview](#Overview)
    - [Preprocessing](#Preprocessing)
    - [Processing](#Processing)
    - [Outputs](#Outputs)
    - [Utility](#Utility)
    

## Overview
![beiwe_analysis_overview](https://github.com/onnela-lab/Beiwe-Analysis/blob/master/Figures/beiwe_analysis_overview.png)
## Preprocessing
Beiwe data comes in [standard formats](https://github.com/onnela-lab/beiwedata#data-overview), but requires processing before generating any insights.  This includes the most basic tasks, such as determining how much data you have, or its quality.  Also, our collaborators usually have separate data they would like to combine with Beiwe data.  It is not obvious how all of this should be done.

We say data is ***preprocessed*** if it takes the form of a two-dimensional array (*i.e.* matrix, data frame) with at least two columns:
* **`Datetime`**, a string containing a universal format for date and time.  For now, it's a **`POSIXct`** object, but this may change.
* **`Person`**, a string containing an ID for the subject at hand.

The goal of preprocessing with raw Beiwe data or collaborator is to convert it to processed data.  For now, all preprocessed data is small enough to be loaded into working memory.

## Processing
Even after preprocessing, most analyses require basic steps before creating plots or performing statistical analysis.  For example, plotting may require preprocessed data using several different datasteams, or statistical analysis may require comparing one time with a fixed time lag in the past.  To avoid errors in these tasks, we list how we do them here.  Data is called ***processed*** if it is preprocessed, and is the product of the functions specified here.  The goal is to do as much processing as possible using only these functions.

## Outputs
Once the data is processed, we arrive at the juicy part: plots and statistical analysis.  These functions take processed data in a specified format as inputs, and return highly specific ***outputs*** such as plots, models, or tests.  These functions should not contain any processing steps.

## Utility
A great deal of functions call into the above categories, but some do not.  All other helper functions belong in ***utility***.  Examples include **`R`** package dependencies, and global constants such as plotting colors.


# Running the Beiwe-Analysis code locally

This documentation will demonstrate how to run Beiwe data analysis code locally and will also provide a brief introduction for each function.

## Table of Contents
- [Environment Setup](#environment-setup) 
- [Introduction to the functions involved in Beiwe-Analysis](#introduction-to-the-functions-involved-in-beiwe-analysis)
    - [Preprocessing](#preprocessing)
    - [Processing](#processing)
    - [Results](#results)
    - [Utility](#utility) 

## Environment Setup

Some of the Beiwe-Analysis code references **`Python`** or **`C`**-related code for speed, but most of this code is in **`R`**, so we suggest running the code in the **`Rstudio`**. If you already have **`Rstudio`** installed, we suggest updating it to the latest version.

The following steps instruct how to install necessary packages including **Rcpp** that facilitate interfacing **`C`** code in R Packages. 

- **Rcpp requires gcc and gfortran installed**
    - **For macOS users**:
        - Open the terminal console, and check if you already installed Xcode Command Line Tools by typing:
            ```
            $ xcode-select -p
            ```
         	If you see the following returns then the full Xcode package is already installed
	     	```
	      	/Applications/Xcode.app/Contents/Developer
	     	```
			If not, enter the following to install
			```
			$xcode-select --install
			```
			The following can be used to verify **`gcc`** is installed:
			```
			$gcc --version
			```
		- The next step is to install **`gfortran`**. Follow the steps in [How to install gfortran on Mac OS X](http://skipperkongen.dk/2012/04/27/how-to-install-gfortran-on-mac-os-x/).
    - **For windows users**, ...

- **Install/Load Required R Packages and Set Directories in Rstudio**


    - Open **Rstudio** and run the following code to install/load all the R packages needed for the Beiwe-Analysis:
        ```
        list.of.packages = c(
          "Rcpp",
          "RcppArmadillo",
          "mvtnorm",
          "Matrix",
          "MCMCpack",
          "VGAM",
          "stringr",
          "plyr",
          "pryr",
          "dplyr",
          "tidyr",
          "purrr",
          "tibble",
          "lme4",
          'lmerTest',
          'glmmLasso'
        )

        new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
        if(length(new.packages)) install.packages(new.packages)
        lapply(list.of.packages, require, character.only = TRUE)


        detach("package:plyr", unload=TRUE)

        ```
    - Set filepaths for code source, patient data and analysis output in Rstudio. Below is an example to set these filepaths:
    	
	   ```
	   source_filepath    = "/Users/OnnelaLab/Beiwe-Analysis"
	   data_filepath      = "/Users/OnnelaLab/Sample-Data"
	   output_filepath    = "/Users/OnnelaLab/output"
	   root_filepath      = "/Users/OnnelaLab/"
	   ```
## Introduction to the functions involved in Beiwe-Analysis
### Preprocessing

#### surveys_preprocessing
```
function(patient_name, ...){
  ...
          specific_survey_data[,"survey_id"]      = survey_name
          specific_survey_data["date"]            = date
          specific_survey_data[,"patient_name"]   = patient_name
          specific_survey_data[,"timestamp"]      = timestamp
          specific_survey_data[,"date"]           = as.Date(date)
          survey_data[[specific_survey_filepath]] = specific_survey_data
        }
  ...
```

survey preprocessing:
in this preprocessing, we call each patient name by loop and read survey data from “specific survey filepath”. If there exists information in the survey file, we read the information by such order that: survey name, date, patient name, timestamp, etc. We store this information into several lists and that save then as RDS file.


#### text_preprocessing
```
function(patient_name, ...){
  ...
		textmat[,"timestamp"] = textmat[,"timestamp"] / 1000
		textmat = textmat[,-2]
		textmat[,c("hours","days")] = hours(textmat[,"timestamp"])
    }
  }
  saveRDS(textmat, paste(output_filepath, "/Preprocessed_Data/Individual/",patient_name, "/text_data.rds",sep=""))
}
```

calling the text preprocessing function, we create textmat by adding information from texts file. To be specific, we first convert textmat by dividing it by 1000. Then we creat the variables "hours","days" by splitting the variable  "timestamp”. The reason to do this is since timestamp combines the information of hours and days and we want to extract them from this variable. 


#### calls_preprocessing
```
function(patient_name, ...){
 ...
			callmat = rbind(callmat, data = read.csv(paste(calls_filename,"/",call_file,sep=""),header=T))
		callmat[,"timestamp"] = callmat[,"timestamp"] / 1000
		callmat = callmat[,-2]
		callmat[,c("hours","days")] = hours(callmat[,"timestamp"])
    }
  }
  saveRDS(callmat, paste(output_filepath, "/Preprocessed_Data/Individual/",patient_name, "/call_data.rds",sep=""))
}
```
To call the calls preprocessing, we read files from our working directory and basically did the same thing as the text preprocessing. We also combine information from each file and split the variable "timestamp” into “hours” and “days” and combine their information together and store them in the format of call.
 

#### powerstate_preprocessing
```
function(patient_name, ...){
	...
		  statemat = do.call(rbind, statemat)
		  statemat[,1] = statemat[,1] / 1000
		  statemat = statemat[,-2]
		  statemat[,c("hours","days")] = hours(statemat[,"timestamp"])
		  saveRDS(statemat, power_state_filename)
		}
	}
}
```

to call the powerstate preprocessing, we read files from our working directory and basically do the same thing as the text preprocessing. We also combine information from each file and split the variable "timestamp” into “hours” and “days” and combine their information together and store them in the format of statemat and store them in RDS file.


#### accelerometer_preprocessing
```
function(patient_name, minutes, verbose = TRUE, ...){
...
		accmat[,1] = accmat[,1] / 1000
		accmat = accmat[,-2]
		accmat[,c("hours","days")] = hours(accmat[,"timestamp"])
		saveRDS(accmat, patient_data_filename_RDS)
		file.remove(patient_data_filename_TXT)
	  }
  }
}
```

to call the accelerometer_preprocessing, we read files from our working directory and basically do the same thing as the text preprocessing. We also combine information from each file and split the variable "timestamp” into “hours” and “days” and combine their information together and store them in the format of accmat and store them in RDS file.

}}


#### find_bursts
```
find_bursts = function(patient_name,
						stream,
						millisecond_divider,
						code_filepath = paste(source_filepath, "/Preprocessing/find_bursts.py",sep=""),
						verbose = TRUE,
						...)
{						  
...
  
  if(file.exists(patient_data_filename_RDS)){
  if(verbose) cat(paste(stream, "bursts file already exists.\n"))
  }else{
    system(paste("python", code_filepath, data_filepath, patient_data_filename_TXT, patient_name, stream, millisecond_divider))
	data = read.csv2(patient_data_filename_TXT, sep=",", header=T)
...

```

For this function, we input the names of patients and the filepath of code to find bursts. If the file already exists, then we report the information as “bursts file already exists”, otherwise we retrieve the information by calling “system” function and combine the filepath from code, data, patient_data_filename_TXT, patient_name, etc. and save it as RDS.



#### gps_preprocessing
```
...
mobmatmiss=GPS2MobMat(filelist,itrvl=ITRVL,accuracylim=ACCURACY_LIM,r=rad_fp,w=wid_fp)
    mobmat = GuessPause(mobmatmiss,mindur=minpausedur,r=minpausedist)
    if(!is.matrix(mobmat)){
      cat("Insufficient GPS data.\n")
      return(NULL)
    }
...
```
Function GPS_preprocessing is a main function unifies the functions that follow it. The file basically takes raw GPS data, converts it to "x and y" coordinates (some planar projection of the portion of the Earth), determines the times during which a subject is paused (and where they are), and periods where they are moving (a "flight"), and where they started and ended. To be specific, all the flights and pauses put together for a person is the final output of this module.





### Processing
#### Call_features

```
  call_feature_list = list(outgoing_calls, outgoing_calllengths, call_outdegree, 
                           incoming_calls, incoming_calllengths, call_indegree,
                           call_reciprocity, call_responsiveness)

  call_features = Reduce(full_join, call_feature_list) # reduces each feature to days for which data is available.
  # call_features[,"outgoing_calls"]/call_features[,"incoming_calls"]
  call_features = data.frame(call_features)
  return(call_features)
}
```

Call_features is a long function, which is used to retrieve features from data.
Basically, this function inputs the information of callmat, a processed array of calls, and outputs a processed array of calls features
To be specific, We have the variable of “day”, which is used for grouping the information by days. Then we collect information of outgoing/incoming calls, the length of outgoing/incoming calls, the degree of outgoing/incoming calls, changes, responsive time, etc., and obtain the information by days in numeric unit. In the end, we combine these information and join them into one data frame.


#### Call_locations
```
call_locations = function(callmat, mobmat){
	calllocs = NULL
	calllocs = callmat[,1] %>%
		map(GPSlocation, mobility_matrix = mobmat) %>%
		do.call(rbind, .) %>%
		as.data.frame() %>%
		cbind(callmat[,c("duration.in.seconds","call.type")])
	colnames(calllocs) = c("x","y", "code", "timestamp","length","call.type")
	calllocs[,c("hours","days")]  = hours(calllocs[,"timestamp"])
	return(calllocs)
}
```
this function inputs the callmat and mobmat, and then uses the first column of callmat matrix to map into GPS location. After obtain the timestamp, we combine this information with the duration in second and types of the call. In the end, this function outputs the calllocs, the matrix containing the information of the call, including the days, hours, type and length.


#### CombineSurveyResponses
```
combine_survey_responses = function(srvyinds,groupings,labels){

      nAns=length(which(!is.na(dat[i,INDsPICK])))
      if(nAns>0){
        outmat[i,2+j]=sum(as.numeric(dat[i,INDsPICK]),na.rm=T)/(nAns)
      }
    }
  }
  saveRDS(list(outmat),paste(featurefile,outfilename,sep="/"))
  write.table(outmat,file=paste(featurefile,outtxtname,sep="/"),sep="\t",quote=FALSE,row.names=FALSE)
}
```
the function is used for combing survey response. Based on the response of survey, we remove NA and files which do not exist. By running loops, we conclude the information of average of grouping in each entry. By this aggregation and operation step, we combine the information from survey.


#### coverage_over_time
```
coverage_over_time = function(stream,
                              verbose = TRUE,
                              ...)
...
      group_by(zeroed) %>%
      summarize(
        total_coverage_across_all_patients = sum(total_coverage,na.rm=T)/n_patients,
        total_coverage_across_active_patients = mean(total_coverage,na.rm=T),
        num_bursts_coverage = mean(num_bursts_coverage,na.rm=T),
        within_burst_length_coverage    = mean(within_burst_length_coverage,na.rm=T),
        within_burst_frequency_coverage = mean(within_burst_frequency_coverage,na.rm=T)

```
this function is used for calculating the coverage of patients. We first obtain the total number of patience, and then aggregate information of total coverage across all patients, total coverage across all active patients, number of bursts coverage. After this aggregation, we convert them into data frame of coverage over time. If the information in coverage over time is infinite, we define this case as one.


#### find_questions
```
find_questions = function(...){
...

    if(file.exists(patient_survey_filename))
      questions[[patient_name]] = readRDS(patient_survey_filename) %>% dplyr::select(question.text) %>% unlist %>% as.character
  }
  questions = do.call(c, questions) %>% unique
...
```
this function is used for find question by calling each patient name and finding the corresponding filename of survey for each patient. 


#### get_days
```
get_days = function(...){
	list(...) %>%
    lapply(function(x) x[,'days']) %>%
    Reduce(union, .) %>%
    unique %>%
	sort %>%
	setdiff("NA")
}
```
we input any number of dataframe objects with a "days" column and output a vector of all the unique days.


#### hours
```
hours = function(timestamps
  ...
    apply(1, function(times) sum(times * c(1, 1/60, 1/3600)))
  output = as_data_frame(cbind(hours=hours, days=days))
  output["hours"] = lapply(output["hours"], as.numeric)
  return(output)
}
```
this function is used for converting timestamps into hour of the day. We first remove the data as “NA” and use as.POSIXlt to extract the information of hour. We convert them into units of minutes and seconds. Finally, this function return the column of “hour”.


#### model_data_quality_predictiveness
```
...
      data %>% group_by(patient, zeroed_week) %>% summarize(
        #mean_num_bursts_coverage = mean(num_bursts_coverage),
        #mean_within_burst_length_coverage = mean(within_burst_length_coverage),
        #mean_within_burst_frequency_coverage = mean(within_burst_frequency_coverage)
        mean_total_coverage = sum(total_coverage)/7
      )
    )
  }
...
```
This function first groups the data by patient and aggregate information of coverage. Then creates zeroed column and zeroed week. We then define the variable of weekly_accelerometer, weekly_gps by calling “weekly_coverage” function. After removing the “NA”, and including time variables such as “time_to_present” and “time_to_submitted”, we have the new data of weekly_timeing. In the end, we convert the date into “hours”, and finalizing our data by combing these information together (weekly_accelerometer, weekly_gps, weekly_timings, etc.)


#### powerstate_locations
```
powerstate_locations = function(statemat, mobmat){
	statelocs = NULL
	statelocs = statemat[,1] %>%
		map(GPSlocation, mobility_matrix = mobmat) %>%
		do.call(rbind, .) %>%
		as.data.frame() %>%
		cbind(statemat[,c("event")])
	colnames(statelocs) = c("x","y", "event","timestamp","screen")
	statelocs[,c("hours","days")] = hours(unlist(statelocs[,"timestamp"]))
	statemat[,c("hours","days")]  = hours(statemat[,"timestamp"])
	return(statelocs)
}
```
this function inputs the statemat and mobmat, and then uses the first column of statemat matrix to map into GPS location. After obtain the timestamp, we combine this information with the duration in second and types of the call. In the end, this function outputs the statelocs, the matrix contains the information of the call, including the days, hours, type and length.


#### ReplicateSurveyResponsesBackwards
```
fill_in_NAs = function(...){
 ...
 
  for(i in 1:nrow(dat)){
    for(j in 1:ncol(dat)){
      if(is.na(dat[i,j])){next}
      if(is.nan(dat[i,j])){dat[i,j]=0}
      if(dat[i,j]=="NaN"){dat[i,j]="0"}
      if(dat[i,j]==""){
        dat[i,j]=NA
        next
...
replicate_survey_responses_backwards = function(surveycols,daysback=1,...){
  fileloc=paste(output_filepath,"/Processed_Data/Group",sep="")
  filename="feature_matrix_clean.rds"
  dat = readRDS(paste(fileloc,filename,sep="/"))[[1]]
...

```
this function contains two parts: fill in NAs and replicate survey responses. This function runs two loops based on dimension of the dataset to fill in all NAs, and retrieve the information of back values by running loop and scan the information of data as well. 


#### text_locations
```
text_locations = function(textmat, mobmat){
...
		cbind(textmat[,c("message.length","sent.vs.received")])
	colnames(textlocs) = c("x","y", "code", "timestamp","length", "sent.vs.received")
...

}
```
this function inputs the textmat and mobmat, and then uses the first column of textmat matrix to map into GPS location. After obtain the timestamp, we combine this information with the duration in second and types of the call. In the end, this function outputs the textlocs, the matrix contains the information of the call, including the days, hours, type and length.


#### gps_imputation
```
...
RandomBridge = function(x0,y0,x1,y1,t0,t1,fd,ft,fts,fa,fw,probp,pt,pts,pw,allts,allw,ind11,ind12,i_ind,pxs,pys,fxs,fys,allxs,allys,wtype,canpause,niter=100,spread_pars){
  success=FALSE
...

```
the function need to be filled in the gaps in time via imputation since people are in fact continuous in time. In this function, we calculate the random process where the two endpoints are known by random bridge.



#### gps_survey_communication_dailyfeatures
```
...
GetSurveyRow = function(aIDs,qIDs){
  outvec = rep(NA,length(names(qIDs)))
  for(i in 1:length(names(qIDs))){
    if(length(which(names(aIDs)==names(qIDs)[i]))>0){
      outvec[i]=aIDs[[names(qIDs)[i]]]
    }
...

```
With an imputed dataset of flights and pauses, we calculate a number of daily-level features by this function. In this function, we defines things like time spent at home, total distance from home, fancier things like significant location entropy, etc.


#### FuzzyWindow
```
...
Find.Group.Memberships = function(Data, S){
  D = nrow(Data)
  areas = apply(Data,1,area)
  R = S[sapply(areas, minimum_relevant_window, vec = S)]

...

```
Given a set of daily minimum and maximum x and y coordinates, this function finds a set of windows that cover all data points and jointly minimize the number of windows used AND the area of each window. 


#### ReplicateSurveyResponsesBackwards
```
...
      IDPASS=intersect(intersect(IDNOTNA,IDSAMEIND),IDCLOSE)
      if(is.na(dat[i,j]) && length(IDPASS)>0){
        temp = dat[i,j]
        dat[i,j]=backvals[IDPASS[1]]
        backvals = c(temp,backvals[-daysback])
        backvalsdates = c(datenums[i],backvalsdates[-daysback])
        backvalsinds = c(dat[i,1],backvalsinds[-daysback])
...

```
This function is used for filling surveys for days during which subjects did not take a survey. Lack of information in survey is common in digital phenotyping. To be specific, if someone did not take the survey, we simply replace this information by "0".


#### summarize_data_quality
```
...
  bursts = bursts %>% group_by(patient, date) %>%
    mutate(total_coverage = pmin(1,sum_pings/((burst_duration+break_duration)*burst_duration*frequency)),
        num_bursts_coverage = num_bursts/(24*60*60/(burst_duration+break_duration)),
        within_burst_length_coverage = avg_within_burst_duration/(burst_duration),
        within_burst_frequency_coverage = avg_within_burst_frequency/frequency

...

```
A function input the duration with 60 and frequent with 10 and integrate information group by each patient. Here, we add burst frequent, burst length coverage to patients, and then summarize the date which “burst == pat ” for each patient.


#### survey_responsiveness
```
...
  total = total[apply(total,1,function(x){sum(is.na(x))<5}),]
  for(column in c("Notified","Present","Submitted"))
    total[,column] = as.character(as.numeric(as.character(total[,column]))%/%1000)
...

```
Create a frame with information of "Person","Survey_ID","Notified","Present", and "Submitted”. For the event variable, it has three levels of ”notified”, “present” and “submitted”, we run loops and add the information to corresponding ID if nothing is going wrong. To be specific, if it is “notified”, we store the information and flag that we would like to find when they next. However, if it is “present”, we mark down their presence, and stop looking. In the end, we also calculate the total of each category.


#### text_features
```
...
  response_time = function(x, y){# specifically, hours until responded to a text
    where = which((diff(as.integer(x == "sent SMS"))) > 0)
    output = mean(y[where+1]-y[where], na.rm=TRUE)
    round(output / 60 / 60, 2)
...

```
This function inputs textmat, a processed array of texts and outputs text_features, a processed array with text features. It calculates the numeric records of outgoing texs and ingoing texts groups by day. In the end, it integrates the text_responsiveness and response time where we define response time is the hours until responded to a text. All the numeric information listed in the end and save as text features.


#### warp_GPS
```
...
max(mobmat[,c("y0","y1")],na.rm=T))),num_warps,2)
  pushes = rep(c(1,0),length.out=num_warps)
  sds = 2*apply(mobmat[,c("x0","y0")], 2, sd, na.rm=TRUE)
...

```
wrapper function is using warps on data and the inputs is mobmat, a dataframe with at least "x0", "x1", "y0", and "y1", by finding out the minimum, maximum, and length of the input, we fill out the matrix by coordinates and output warped GPS coordinates.


#### warps
```
...
warp_matrix = matrix(runif(num_warps*2,-2,2),num_warps,2)
pushes = rep(c(1,0),length.out=num_warps)
warped = warps(points, warp_matrix, pushes, sd1=2,sd2=2)
...

```
Inputs of this function are points with 2xN matrix of two-dimension. We starts with 2xK matrix of two-dimensional locations that warp `points`, called warp_matrix, and fill a vector of integers with 1 if and only if warp_matrix row value pushes points. In the end, the output is a 2xN matrix of warped two-dimensional points.




### Results

#### Individual Results
The functions below generate plots for individual patient analysis. All plots of analyses for an individual patient are saved in a folder named as the patient’s id in `output/Results/Individual/`.

-`ContinuousDataCollectionTracks()` 

Plots daily data collection status for **Surveys**, **Screen on/off**, **GPS**, and **Accelerometer**. This function requires the RDS file MobFeatures.rds created by the `CreateMobilityFeatures()` function.

<p align="center"> 
<img src="./example_plots/DataCollectionTracks.png" width="500">
</p>

#### Group Results

The functions below generate plots for group analysis results and save pdf files in `output/Results/Group`.

-`daily_adherence_grid()` 

Plots grid graphs demonstrating the daily adherence status of all patients. The columns of each grid graph are days, and the rows are survey responses, # of missed calls, call duration, total length of texts received, # of texts received, total length of texts sent, circadian routine, # of significant locations visited, max distance from home, distance travelling, time at home, and GPS amount recorded. A blank cell will be displayed on a given day if the data are 0 or were not collected for that category on that day. If the data are available, the greater the value is, the deeper color the cell will show. An example plot for one subject is shown below. 

<p align="center"> 
<img src="./example_plots/ExampleDataCollectionPlot.png">
</p>

-` plot_data_quality()` 

Reads processed RDS data files accelerometer_bursts.rds, gps_bursts.rds, accelerometer_coverage.rds, and gps_coverage.rds (all created by `find_burst()`), and generates 9 individual scatter plots to show daily quality for accelerometer or GPS data for all subjects in the study. When plotting accelerometer data quality, run ` plot_data_quality(stream = "accelerometer", acc_frequency, acc_burst_duration, acc_break_duration,legend=FALSE)`. When plotting GPS data quality, run ` plot_data_quality(stream = "gps",  gps_frequency, gps_burst_duration, gps_break_duration,legend=FALSE)`. 


Among the 9 plots, 4 are plotting **Number of Busts Per Day**, **Average Frequency Per Burst**, **Average Duration per Burst Over Time**, and **Average Duration Between Bursts Over Time** over **Unique Daily Measurements**. Points of each subject’s records are shown in a unique color. One example of Number of Busts Per Day over Unique Daily Measurements with 3 patients’ GPS data is shown below: 

<p align="center"> 
<img src="./example_plots/quality_unique_day_measurement.png" width="500">
</p>



For the other 5 plots, the x-axes are **Day**, and the y-axes are **Number of Bursts**, **Average Frequency Per Burst**, **Average Duration per Burst**, **Average Duration per Burst**, and **Overall Coverage**. One example of Number of Number of Busts Per Day over Day with 2 patients’ GPS data is shown below:

<p align="center"> 
<img src="./example_plots/quality_day.png" width="500">
</p>

-`plot_survey_responsiveness()` 

Generates 4 individual scatter plots to show daily survey responsiveness (measured by **Time to First Response** and **Time to Complete After First Response**) for subjects in the study. **Time to First Response** and **Time to Complete After First Response** are plotted over **Unique Daily Measurements** and over **Day**. This function needs the RDS file survey_responsiveness.rds created by the ``survey_responsiveness()`` function.


-`plot_survey_completion()` 

Generates 2 scatter plots to visualize weekly survey completion. For both plots, the y-axis is **Number of Survyes Taken**. The x-axis for the first plot is unique weekly measurements, and the x-axis for the second plot is **week** with range as the entire study period. 

-`plot_accelerometer()` 

Takes minutes = acc_binsize (width of bin in minutes used for combining accelerometer data) as inputs, and generates individual plot of **Daily Accelerometer Data** for each patient. The x-axis for each plot is **Time of day** (in 24 hours format), and the y-axis is **Day** over the entire study period. 

<p align="center"> 
<img src="./example_plots/slat_plot.png" width="500">
</p>


### Utility

-`initialize.R`

Imports all functions needed from the local Beiwe Analysis folder, reads all patients' data into R, and initializes output directories for preprocessed data, processed data, and result plots. 

-`moving_average(x, n_neighbors = 1)`

Takes a vector/list **x** and number of neighbors **n_neighbors** with default value as 1. The function returns the moving averages of **x**.
