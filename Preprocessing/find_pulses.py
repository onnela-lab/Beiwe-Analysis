import os, sys, math, time
import pandas as pd, numpy as np

data_filepath = sys.argv[1]#"C:/Phoenix/School/Harvard/Research/Beiwe/Studies/John_Schizophrenia/Data/2017.01.09"
output_filepath = data_filepath + "/output"
stream = sys.argv[2]
milliseconds = int(sys.argv[3]) # 30000

patients = [patient for patient in os.listdir(data_filepath) if "." not in patient]

if not os.path.exists(output_filepath):
    os.mkdir(output_filepath)

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
        pulse = [patient, day, beginning, end, addition] # define the pulse.
        # pulses.append(pulse) # add the pulse.
        G.write(",".join([str(i) for i in pulse])+"\n")
        timestamps = timestamps[addition:] # delete the considered files.
        UTCs       = UTCs[addition:] # same.
        change = change[1:] # delete the current consideration.
    return timestamps, UTCs

def record_pulses(stream, change_val):
    total_start = time.time()
    stream_output_file = output_filepath + "/" + stream + "_pulses.txt"
    with open(stream_output_file, "w+") as G:
        G.write("patient,date,start,end,pings\n")
        for patient in patients:
            start = time.time()
            pulses, timestamps, UTCs = [], [], [] # pulses is unnecessary, delete soon.
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

record_pulses(stream, milliseconds)


