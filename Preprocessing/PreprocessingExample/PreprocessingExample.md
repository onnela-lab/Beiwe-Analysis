
# Step-by-step example of Beiwe data preprocessing
## GPS preprocessing for a single patient

First, find the directory containing your Beiwe data.

![alt text](https://github.com/onnela-lab/Beiwe-Analysis/blob/master/Preprocessing/PreprocessingExample/screencapfinddirectory.png "Logo Title Text 1")

Store this directory name in the `fildir` variable. Pick a name for that will be used to label your output files and store it in the `filename` variable. Also, load the `BeiwePackageNameHere` package.

```
## library(BeiwePackageNameHere) \\ This will ultimately replace the source call that comes on th
source("C:/Users/Ian/Documents/Work/JP/Beiwe/Github/Beiwe-Analysis/Preprocessing/GPS_preprocessing.R")
fildir="C:/Users/Ian/Documents/Work/JP/Beiwe/Github/Beiwe-Analysis/Preprocessing/PreprocessingExample"
filename="Example"
```

Now we will run the `MobilityFeatures` function. This will:

1. Process the raw GPS data by removing low-accuracy pings.
2. Denoising the remaining pings by projecting to a 2D plane and then converting them into a sequence of flights and pauses. The result is referred to as a *mobility trace*.
3. The mobility trace is stored in **Example.Rdata** and a set of meaningful mobility features, calculated for every day in the mobility trace, is also produced.

```
mout=MobilityFeatures(filename,fildir)
```

## GPS preprocessing for all patients in a study

Store the directory name where all data in your study is located in the `fildir` variable. This folder should contain one subfolder for each patient, labelled by patient ID. Also, load the `BeiwePackageNameHere` package.

```
## library(BeiwePackageNameHere) \\ This will ultimately replace the source call that comes on th
source("C:/Users/Ian/Documents/Work/JP/Beiwe/Github/Beiwe-Analysis/Preprocessing/GPS_preprocessing.R")
fildir="C:/Users/Ian/Documents/Work/JP/Schizophrenia/Data"
```

The patient IDs are then taken from the subfolder names and stored in the `SIDs` variable.
```
SIDs=unlist(lapply(strsplit(list.dirs(fildir,recursive=FALSE),"/"),function(x) x[length(x)]))
```

The `MobilityFeatures` function is then executed on each patient's data separately. 
```
cat("\nProcessing GPS data for",length(SIDs),"subjects:\n\n")
for(i in 1:length(SIDs)){
  cat(paste("Processing ID: ",SIDs[i]," (",i,"/",length(SIDs),")\n",sep=""))
  fildir=paste(datadir,SIDs[i],"gps",sep="/")
  if(file.exists(fildir)){
    out=MobilityFeatures(SIDs[[i]],fildir,nreps=simnum)    
  }else{
    cat("No GPS data found.\n")
  }
}
```
The `.rdata` files containing the processed data are stored in each the patient subfolders.

