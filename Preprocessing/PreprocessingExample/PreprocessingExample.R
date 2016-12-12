source("C:/Users/Ian/Documents/Work/JP/Beiwe/Github/Beiwe-Analysis/Preprocessing/GPS_preprocessing.R")

fildir="C:/Users/Ian/Documents/Work/JP/Beiwe/Github/Beiwe-Analysis/Preprocessing/PreprocessingExample/gps"
### filename: All of the output files (figures/features/Rdata files) will contain
###           filename in their file names.
filename="Example"
### The following function call will produce the following:
###    -Mobility plots in .pdf format: "FlightsPlot_Date_filename.pdf". One
###     for each separate day, along with a plot for the full data.
###    -Feature matrix in .txt format: "MobFeatMat_filename.txt". This file
###     is tab delimited, containing one row for each day in the person's data
###     set and one column for each mobility feature. The first and last days
###     may contain incomplete data so it may be advisable to ignore them.
mout=MobilityFeatures(filename,fildir)
