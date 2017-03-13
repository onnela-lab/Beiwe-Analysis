import os, sys, math

data_filepath = sys.argv[1] # main folder, containing patient ID subfolders.  example: "C:/Users/Patrick/Desktop/2016.10.20_justin_data"
patient_name = sys.argv[2]       # patient ID, as a subdirectory.  example: "jr7j7cmd"
minutes = int(sys.argv[3])  # number of minutes to bin by.  example: 5

pow_filename = output_filepath + "/Individual/" + patient_name + "/appended_sheared_file_pow_"+str(minutes)+".txt"

if not os.path.isfile(pow_filename):
    open(pow_filename, "a").close()
			
with open(pow_filename, "w") as F:
    F.write('timestamp UTC_time segment screen_on_events\n')
    filenames = os.listdir(data_filepath+"/"+patient_name+"/power_state")
    for filename in filenames:
        with open(data_filepath+"/"+patient_name+"/power_state"+"/"+filename) as G:
            G.readline()
            current_segment_date = []
            count=0
            for line in G:
                line = line.strip().split(",")
                date_and_time = line[1].split("T")
                time = date_and_time[1].split(":")[:2]
                time[1] = str("%02d"%(minutes*(int(time[1])//minutes)))
                date_and_time = [date_and_time[0]] + time
                if current_segment_date == date_and_time:
                    count += int(line[2] =="Screen turned on")
                else:
                    current_segment_date = date_and_time[:]
                    to_write = " ".join(line[:2])
                    to_write = to_write + " " + "-".join(date_and_time)
                    F.write(to_write+" "+str(count) + '\n')
                    count = 0
            to_write = " ".join(line[:2])
            to_write = to_write + " " + "-".join(date_and_time)
            F.write(to_write+" "+str(count) + '\n')

with open("buffer.txt", "w") as F:
    with open(pow_filename, "r") as G:
        for line in G.readlines():
            if len(line.split(" ")) == 6:
                F.write(line)

os.remove(pow_filename)
os.rename("buffer.txt", pow_filename)
            
            
