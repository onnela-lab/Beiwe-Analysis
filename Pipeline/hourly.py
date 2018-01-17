#!/usr/bin/python3

"""
This file will be run every hour using Python 3.5 in a Ubuntu server environment.

It has access to the following:
Programs:
    R
    Python 2.7
    Python 3.5
    Any python libraries listed in python3-requirements.txt
Environment variables (accessed using os.environ):
    server_url: The domain name of your Beiwe instance (e.g. https://mystudy.beiwe.org)
    study_object_id: The UUID of the study that the code is being run on
        (e.g. '584b042c2dd65714f0a8c3f4')
    study_name: The name of the study that the code is being run on (e.g. 'Diabetes Study 2018')
Files:
    The Beiwe-Analysis repository, located at /home/Beiwe-Analysis. This file is located
        inside that repository, at /home/Beiwe-Analysis/Pipeline/hourly.py.

If you experience difficulties with configuration, please contact msimoneau@hsph.harvard.edu.

Instructions for running this locally can be found in the README in this directory.

Commented out example code follows. If you uncomment it, you will have code that downloads
raw data, runs find_bursts.py on it and uploads the processed data. As is, the code only
runs on studies that have either 'Diabetes' or 'Test' in their names.
"""

# from datetime import datetime
# # Filesystem utilities
# import os
# from os.path import join
# import subprocess
#
# # Only run this code on Diabetes studies and test studies
# # Note that if you do not set study_name, or any of the other environment variables used in
# # download_s3_files.py or upload_s3_files.py, the code will fail to execute.
# study_name = os.environ['study_name'].lower()
# if 'diabetes' not in study_name and 'test' not in study_name:
#     exit(0)
#
# # Directory names, based on the location of this file. HOME is the parent directory of the
# # Beiwe-Analysis repository.
# BEIWE_ANALYSIS_DIR = os.path.dirname(os.path.dirname(os.path.realpath(__file__)))
# HOME = os.path.dirname(BEIWE_ANALYSIS_DIR)
# UTILS = join(os.path.dirname(os.path.realpath(__file__)), 'utils')
#
# now = datetime.now().strftime('%Y-%m-%dT%H-%M-%S-%f')
# RAW_DATA_DIR = join(HOME, 'raw-data-{}'.format(now))
# PROC_DATA_DIR = join(HOME, 'processed-data-{}'.format(now))
#
# # Make a folder for raw data, download all the files for this study from S3 and unzip them
# # into the new folder.
# os.mkdir(RAW_DATA_DIR)
# download_file = join(RAW_DATA_DIR, 'data.zip')
# subprocess.check_call(['python3', join(UTILS, 'download_s3_files.py'), download_file])
# subprocess.check_call(['unzip', '-q', download_file, '-d', RAW_DATA_DIR])
#
# # Collect some statistics about the raw data
# os.mkdir(PROC_DATA_DIR)
# folder_contents = os.listdir(RAW_DATA_DIR)
# if folder_contents:
#     mod_time = os.path.getmtime(join(RAW_DATA_DIR, folder_contents[0]))
# else:
#     mod_time = None
#
# # Save those statistics to a file
# with open(join(PROC_DATA_DIR, 'ls.txt'), 'w') as fn:
#     fn.write(repr(folder_contents))
# with open(join(PROC_DATA_DIR, 'mtime.txt'), 'w') as fn:
#     fn.write(repr(mod_time))
#
# # Find all the patients with raw accelerometer data
# find_bursts_input = []
# for patient_name in folder_contents:
#     patient_subfolder = join(RAW_DATA_DIR, patient_name)
#     accelerometer_subfolder = join(patient_subfolder, 'accelerometer')
#     if os.path.exists(accelerometer_subfolder):
#         # Get the inputs to find_bursts.py, of which there are five
#         results_filepath = join(PROC_DATA_DIR, patient_name, 'accelerometer_bursts.txt')
#         find_bursts_input.append((
#             RAW_DATA_DIR,
#             results_filepath,
#             patient_name,
#             'accelerometer',
#             '30000',  # subprocess requires string arguments
#         ))
#         os.mkdir(join(PROC_DATA_DIR, patient_name))
#
# # Run find_bursts on the raw accelerometer data
# for args in find_bursts_input:
#     command = ['python3', join(BEIWE_ANALYSIS_DIR, 'Preprocessing', 'find_bursts.py')]
#     command.extend(args)
#     logs = subprocess.check_output(command)
#
#     # Write the output of find_bursts to a log file. Note that we open the file in binary
#     # mode, because subprocess.check_output returns bytes.
#     with open(join(PROC_DATA_DIR, args[2], 'burst_log.txt'), 'wb') as fn:
#         fn.write(logs)
#
# # Zip all the files created above and upload them to S3
# local_file = join(PROC_DATA_DIR, 'data.zip')
# remote_file = 'pipeline-upload-{}.zip'.format(datetime.now().strftime('%Y-%m-%dT%H-%M-%S-%f'))
# subprocess.check_call(['zip', '-r', local_file, PROC_DATA_DIR])
# subprocess.check_call(['python3', join(UTILS, 'upload_s3_files.py'), local_file, remote_file])
#
# print('Complete!')
