
# Step-by-step example of Beiwe data preprocessing
## GPS preprocessing

First, find the directory containing your Beiwe data.

![alt text](https://github.com/onnela-lab/Beiwe-Analysis/blob/master/Preprocessing/PreprocessingExample/screencapfinddirectory.png "Logo Title Text 1")

Store this directory name in the `fildir` variable. Also, load the `BeiwePackageNameHere` package.

```
## library(BeiwePackageNameHere) \\ This will ultimately replace the source call that comes on th
source("C:/Users/Ian/Documents/Work/JP/Beiwe/Github/Beiwe-Analysis/Preprocessing/GPS_preprocessing.R")
fildir="C:/Users/Ian/Documents/Work/JP/Beiwe/Github/Beiwe-Analysis/Preprocessing/PreprocessingExample"
```

Now we will run the `MobilityFeatures` function. This will
1. Process the raw GPS data by removing low-accuracy pings.
2. Denoising the remaining pings by projecting to a 2D plane and then converting them into a sequence of flights and pauses. The result is referred to as a *mobility trace*.
2. The mobility trace is stored in **Example.Rdata** and a set of meaningful mobility features, calculated for every day in the mobility trace, is also produced.



