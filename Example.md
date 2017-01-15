
Just source the GPS_preprocessing.R file located in the preprocessing folder and the following code shows how, give the data_directory and the patient_name, GPS will be proprocessed into flights and pauses, and then missingness will be replaced with imputed trajectories. The preprocessed GPS data is stored in the preprocessed_data folder in the patient's folder. The imputed files are stored in the processed_data folder.
```
source("GPS_preprocessing.R")
data_directory = "C:/Users/Ian/Documents/Work/JP/NeurosurgeryRecovery/Data"
patient_name="1jkumm9h"
GPS_preprocessing(patient_name,data_directory)
GPS_imputation(patient_name,data_directory)
```
