#!/usr/bin/python3
"""
This script is expected to run on a Linux machine (specifically debian-like disto, might be buggy on other)

This pipeline script does:
    1) Download all raw(chunked) data for a Beiwe study from (source) S3 Bucket
    2) Process downloaded data and generate statistical summaries(GPS, Text, Call, Power state, accelerometer)
    3) Upload summaries to (destination) S3 Bucket

Setup:
Beiwe-Analysis repo needs to exist in the user's home directory. This file is located at
    /home/Beiwe-Analysis/Pipeline/custom_pipeline.py

Environment variables:
    FREQ: (REQUIRED)  Frequency of running the pipeline (used to retrieve beiwe download creds from AWS SSM)
    access_key_ssm_name: (REQUIRED) Name of parameter stored in SSM (without frequency since its added at a later stage)
    secret_key_ssm_name: (REQUIRED) Name of parameter stored in SSM (without frequency since its added at a later stage)
    study_object_id: (REQUIRED) Study id of the Beiwe study (mainly used for downloading/uploading data)
    region_name: (REQUIRED) Region of AWS SSM service
    server_url: (REQUIRED) Beiwe backend server URL
"""

from datetime import datetime
import os
import subprocess
from custom_utils import upload_to_s3, download_raw_data


# # Here's example code for how to only run this code on studies with certain IDs:
# studies_to_run_script_on = ['584b042c2dd65714f0a8c3f4',
#                             '59cac92457665803426798f8',
#                             '59dd29e119979a4c872ab199',
#                             '5873fe38644ad7557b168e43']
# study_object_id = os.environ['study_object_id']
# if study_object_id not in studies_to_run_script_on:
#     exit(0)

# Load environment variables
env_vars = {
    "access_key_ssm_name": "{}-{}".format(os.environ["access_key_ssm_name"], os.environ["FREQ"]),
    "secret_key_ssm_name": "{}-{}".format(os.environ["secret_key_ssm_name"], os.environ["FREQ"]),
    "study_object_id": os.environ["study_object_id"],
    "region_name": os.environ["region_name"],
    "server_url": os.environ["server_url"]
}

# Globals
now = datetime.now().strftime('%Y-%m-%dT%H-%M-%S-%f')
BEIWE_ANALYSIS_DIR = os.path.dirname(os.path.dirname(os.path.realpath(__file__)))
os.environ["BEIWE_ANALYSIS_PROJECT_PATH"] = BEIWE_ANALYSIS_DIR
HOME = os.path.dirname(BEIWE_ANALYSIS_DIR)  # User's home directory
RAW_DATA_DIR = os.path.join(HOME, "raw-data-{}".format(now))  # Where raw data is saved
PROC_DATA_DIR = os.path.join(HOME, 'processed-data-{}'.format(now))  # Where summaries are saved
ZIPPED_DATA_FILE = os.path.join(RAW_DATA_DIR, "raw_data.zip")
PROC_ZIPPED_FILE_NAME = "processed-data-{}.zip".format(now)
PROC_ZIPPED_FILE_PATH = os.path.join(HOME, PROC_ZIPPED_FILE_NAME)

# Create data directories
os.mkdir(RAW_DATA_DIR)
os.mkdir(PROC_DATA_DIR)

# Download raw data and unzip it inside RAW_DATA_DIR
download_raw_data(ZIPPED_DATA_FILE, env_vars)
subprocess.check_call(["unzip", "-q", ZIPPED_DATA_FILE, "-d", RAW_DATA_DIR])

# Iterate over all patients and run the summary generation Rscript on each patient
for patient_id in os.listdir(RAW_DATA_DIR):
    # Run main.R RScript on current patient_id
    subprocess.call(["Rscript",
                     "--vanilla",
                     os.path.join(BEIWE_ANALYSIS_DIR, "Summary", "main.R"),
                     patient_id,
                     RAW_DATA_DIR,
                     PROC_DATA_DIR])
    summaries_base_dir = os.path.join(PROC_DATA_DIR, patient_id)
    summary_types = ["gps", "text", "call", "powerstate"]
    for summary_type in summary_types:
        tags = [patient_id, summary_type + "_summary"]
        summary_file_name = "%s_%s_summary_%s.csv" % (patient_id, summary_type, now)
        summary_file_path = os.path.join(summaries_base_dir, summary_file_name)
        if os.path.isfile(summary_file_path):
            upload_to_s3(summary_file_path, summary_file_name, tags, env_vars)
print("Done!")
