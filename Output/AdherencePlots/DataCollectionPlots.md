# Demonstration producing study-wide data collection plots

Before creating data collection/adherence plots for your study, the [GPS needs to be processed](https://github.com/onnela-lab/Beiwe-Analysis/blob/master/Preprocessing/PreprocessingExample/PreprocessingExample.md).

The following demonstration will show how to create data collection plots for your study in R.

The source location should point to the location of the [`AdherencePlots.R`](https://github.com/onnela-lab/Beiwe-Analysis/blob/master/Output/AdherencePlots/AdherencePlots.R) file on your local machine.
```
source("C:/Users/Ian/Documents/Work/JP/Beiwe/Github/Beiwe-Analysis/Output/AdherencePlots/AdherencePlots.R")
```

The `fildir` variable should point to the location of the Beiwe data for your study.
```
fildir="C:/Users/Ian/Documents/Work/JP/Schizophrenia/Data"
```

The  `plotname` variable is the output file name that will contain the data collection plots for your study. It needs to have the pdf extension.
```
plotname="DataCollectionPlot-12-12-16.pdf"
```

The `fildir` and `plotname` variables are the only input required to create the data collection plots, which will be stored in `fildir`:
```
DataCollectionPlots(fildir,plotname)
```
This will produce a plot for every patient in your study. An example such plot is shown below:

![alt text](https://github.com/onnela-lab/Beiwe-Analysis/blob/master/Output/AdherencePlots/ExampleDataCollectionPlot.png "Logo Title Text 1")
