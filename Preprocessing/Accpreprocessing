import os, sys, math

data_filepath = sys.argv[1] # "C:/Users/Patrick/Desktop/2016.10.20_justin_data"
patient = sys.argv[2]       # "jr7j7cmd"
minutes = int(sys.argv[3])  # 5
acc_filename = data_filepath+"/"+patient+"/appended_sheared_file_acc_"+str(minutes)+".txt"
pow_filename = data_filepath+"/"+patient+"/appended_sheared_file_pow_"+str(minutes)+".txt"
if not os.path.isfile(acc_filename):
    open(acc_filename, "a").close()
if not os.path.isfile(pow_filename):
    open(pow_filename, "a").close()

with open(acc_filename, "w") as F:
    F.write('timestamp UTC_time segment x y z\n')
    filenames = os.listdir(data_filepath+"/"+patient+"/accelerometer")
    for filename in filenames:
        with open(data_filepath+"/"+patient+"/accelerometer"+"/"+filename) as G:
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

with open(pow_filename, "w") as F:
    F.write('timestamp UTC_time segment screen_on_events\n')
    filenames = os.listdir(data_filepath+"/"+patient+"/power_state")
    for filename in filenames:
        with open(data_filepath+"/"+patient+"/power_state"+"/"+filename) as G:
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



















