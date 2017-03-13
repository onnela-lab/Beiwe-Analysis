import os, sys, math

data_filepath   = sys.argv[1] # main folder, containing patient ID subfolders.  example: "C:/Users/Patrick/Desktop/2016.10.20_justin_data"
output_filepath = sys.argv[2] # Output folder.
patient_name = sys.argv[3]    # patient ID, as a subdirectory.  example: "jr7j7cmd"
minutes = int(sys.argv[4])    # number of minutes to bin by.  example: 5

acc_filename = output_filepath + "/Preprocessed_Data/Individual/" + patient_name + "/appended_sheared_file_acc_"+str(minutes)+".txt"

if not os.path.isfile(acc_filename):
    open(acc_filename, "a").close()

with open(acc_filename, "w") as F:
    F.write('timestamp UTC_time segment x y z\n')
    filenames = os.listdir(data_filepath+"/"+patient_name+"/accelerometer")
    for filename in filenames:
        with open(data_filepath+"/"+patient_name+"/accelerometer"+"/"+filename) as G:
            G.readline()
            current_segment_date = []
            data = []
            for line in G:
                line = line.strip().split(",")
                date_and_time = line[1].split("T")
                time = date_and_time[1].split(":")[:2]
                time[1] = str("%02d"%(minutes*(int(time[1])//minutes)))
                date_and_time = [date_and_time[0]] + time
                if current_segment_date == date_and_time:
                    data.append([float(i) for i in line[3:]])
                else:
                    current_segment_date = date_and_time[:]
                    if len(data) > 1:
                        to_write = " ".join(line[:2])
                        to_write = to_write + " " + "-".join(date_and_time)
                        data = list(zip(*data))
                        sd_xyz = [math.sqrt(max(0, sum([i**2 for i in j])/len(j) - (sum(j)/len(j))**2)) for j in data]
                        sd_line = " ".join(to_write.split(",")[:3]+[str(round(i,3)) for i in sd_xyz])
                        F.write(sd_line + '\n')
                        data = []
            to_write = " ".join(line[:2])
            to_write = to_write + " " + "-".join(date_and_time)
            data = list(zip(*data))
            sd_xyz = [math.sqrt(max(0, sum([i**2 for i in j])/len(j) - (sum(j)/len(j))**2)) for j in data]
            sd_line = " ".join(to_write.split(",")[:3]+[str(round(i,3)) for i in sd_xyz])
            F.write(sd_line + '\n')

with open("buffer.txt", "w") as F:
    with open(acc_filename, "r") as G:
        for line in G.readlines():
            if len(line.split(" ")) == 6:
                F.write(line)

os.remove(acc_filename)
os.rename("buffer.txt", acc_filename)

