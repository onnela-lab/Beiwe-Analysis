import os, sys, math, time
import pandas as pd, numpy as np

data_filepath = sys.argv[1]#"C:/Phoenix/School/Harvard/Research/Beiwe/Studies/John_Schizophrenia/Data/2017.01.09"
output_filepath = sys.argv[2]
stream = sys.argv[3]
milliseconds = int(sys.argv[4]) # 30000

patients = [patient for patient in os.listdir(data_filepath) if "." not in patient]

def find_changes(G, patient, timestamps, UTCs, change_val):
    change = np.where(np.diff(timestamps)>change_val)[0] + 1 # indeces of the times
                # that the difference in timestamps is greater than one minute.
    if len(change) > 0: # If there are changes, turn them into a sequence of
                        # index increments, and as a list.
        change = [change[0]] + list(np.diff(change))
    while len(change) > 0:
        addition  = change[0] # The total number of indexes for the current change.
        beginning = timestamps[0] # the beginning timestamp of a new change.
        end       = timestamps[addition-1] # the end timestamp of a new change.
        day       = UTCs[addition].split("T")[0] # Which day this will count as.
        burst = [patient, day, beginning, end, addition] # define the burst.
        # bursts.append(burst) # add the burst.
        G.write(",".join([str(i) for i in burst])+"\n")
        timestamps = timestamps[addition:] # delete the considered files.
        UTCs       = UTCs[addition:] # same.
        change = change[1:] # delete the current consideration.
    return timestamps, UTCs

def record_bursts(stream, change_val):
    total_start = time.time()
    stream_output_file = output_filepath + "/" + stream + "_bursts.txt"
    with open(stream_output_file, "w+") as G:
        G.write("patient,date,start,end,pings\n")
        for patient in patients:
            start = time.time()
            bursts, timestamps, UTCs = [], [], [] # bursts is unnecessary, delete soon.
            filename = data_filepath + "/" + patient + "/" + stream
            if os.path.exists(filename):
                files = [file for file in os.listdir(filename) if ".csv" in file]
                for file in files:
                    F = pd.read_csv(data_filepath + "/" + patient + "/" + stream + "/" + file)
                    timestamps = timestamps + list(F["timestamp"])
                    UTCs = UTCs + list(F["UTC time"])
                    timestamps, UTCs = find_changes(G, patient, timestamps, UTCs, change_val)
            stop = time.time()
            print(patient + ": " + str(stop - start))
    total_stop = time.time()
    print(total_stop - total_start)

record_bursts(stream, milliseconds)


