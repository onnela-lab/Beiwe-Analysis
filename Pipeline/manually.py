#!/usr/bin/python3

"""
This file will be run manually using Python 3.5 in a Ubuntu server environment.

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
        inside that repository, at /home/Beiwe-Analysis/Pipeline/manually.py.

If you experience difficulties with configuration, please contact msimoneau@hsph.harvard.edu.

Instructions for running this locally can be found in the README in this directory.

BELOW IS AN EXAMPLE IMPLEMENTATION that, for four studies (studies with object IDs in the
studies_to_run_script_on list below), runs the script find_bursts.py on every patient ID that has
accelerometer data.  It then uploads each of those timestamped files to S3, along with two tags.

YOU ARE NOT MEANT TO RUN THIS EXACT CODE; IT IS ONLY A GUIDE FOR HOW YOU CAN SET UP YOUR PIPELINE.

"""

from datetime import datetime
# Filesystem utilities
import os
from os.path import join
import subprocess
from utils.upload_s3_files import upload_file_to_s3

# Note that if you do not set study_name, or any of the other environment variables used in
# download_s3_files.py or upload_s3_files.py, the code will fail to execute.

# Here's example code for how to only run this code on studies with certain IDs:
studies_to_run_script_on = ['584b042c2dd65714f0a8c3f4',
                            '59cac92457665803426798f8',
                            '59dd29e119979a4c872ab199',
                            '5873fe38644ad7557b168e43']
study_object_id = os.environ['study_object_id']
if study_object_id not in studies_to_run_script_on:
    exit(0)

# Directory names, based on the location of this file. HOME is the parent directory of the
# Beiwe-Analysis repository.
BEIWE_ANALYSIS_DIR = os.path.dirname(os.path.dirname(os.path.realpath(__file__)))
HOME = os.path.dirname(BEIWE_ANALYSIS_DIR)
UTILS = join(os.path.dirname(os.path.realpath(__file__)), 'utils')
now = datetime.now().strftime('%Y-%m-%dT%H-%M-%S-%f')

# Make a folder for raw data, download all the files for this study from S3 and unzip them
# into the new folder.
RAW_DATA_DIR = join(HOME, 'raw-data-{}'.format(now))
os.mkdir(RAW_DATA_DIR)
download_file = join(RAW_DATA_DIR, 'data.zip')
subprocess.check_call(['python3', join(UTILS, 'download_s3_files.py'), download_file])
subprocess.check_call(['unzip', '-q', download_file, '-d', RAW_DATA_DIR])

# Make a folder for processed data
PROC_DATA_DIR = join(HOME, 'processed-data-{}'.format(now))
os.mkdir(PROC_DATA_DIR)

# Iterate over all patient IDs that have raw accelerometer data
for patient_id in os.listdir(RAW_DATA_DIR):
    patient_subfolder = join(RAW_DATA_DIR, patient_id)
    accelerometer_subfolder = join(patient_subfolder, 'accelerometer')
    if os.path.exists(accelerometer_subfolder):
        # Run find_bursts.py on this patient's raw data
        output_filename = patient_id + '_accelerometer_bursts' + now + '.csv'
        output_filepath = join(PROC_DATA_DIR, output_filename)
        find_bursts_input = (
            RAW_DATA_DIR,
            output_filepath,
            patient_id,
            'accelerometer',
            '30000',  # subprocess requires string arguments
        )
        command = ['python3', join(BEIWE_ANALYSIS_DIR, 'Preprocessing', 'find_bursts.py')]
        command.extend(find_bursts_input)
        subprocess.call(command)

        # Upload the output file to S3, along with some tags
        tags = [patient_id, 'accelerometer_bursts']
        upload_file_to_s3(output_filepath, output_filename, tags)

# # Example code to zip all the files created above.  Using zip's '-j' flag means ignore directory
# # structure within the zip file
# local_file = join(PROC_DATA_DIR, 'data.zip')
# subprocess.check_call(['zip', '-r', '-j', local_file, PROC_DATA_DIR])
#
# # Upload the files to S3
# remote_file = 'pipeline-upload-{}.zip'.format(datetime.now().strftime('%Y-%m-%dT%H-%M-%S-%f'))
# upload_file_to_s3(local_file, remote_file, ['myFirstTag', 'mySecondTag', 'myThirdTag'])

print('Complete!')
