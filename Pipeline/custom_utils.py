from __future__ import print_function

import csv
import json
from datetime import datetime
from time import sleep

import boto3
import requests

COLUMNS_CALL_TEXT = [
    "id",
    "day",
    "outgoing_calls",
    "outgoing_calllengths",
    "call_outdegree",
    "incoming_calls",
    "incoming_calllengths",
    "call_indegree",
    "reciprocity",
    "responsiveness",
]

COLUMNS_POWERSTATE = ["id", "day", "total_screen_events", "total_power_events"]

CSV_METADATA = {
    "gps": ["id"],
    "accelerometer": ["id"],
    "call": COLUMNS_CALL_TEXT,
    "text": COLUMNS_CALL_TEXT,
    "powerstate": COLUMNS_POWERSTATE,
}


def upload_to_backend(local_file_path, file_name, env_vars, patient_id):
    """
    Uploads passed csv file to backend in JSON format
    :param local_file_path: path to the local csv file
    :param data_type: type of summary
    :param env_vars: a dictionary of env variables
    """
    access_key, secret_key = _get_beiwe_credentials(
        env_vars.get("region_name"),
        env_vars.get("access_key_ssm_name"),
        env_vars.get("secret_key_ssm_name")
    )

    # handle case where http/https not provided
    server_url = env_vars.get("server_url")
    if not server_url.startswith("http"):
        server_url = "https://" + server_url

    json_upload_url = '{}/pipeline-json-upload/v1'.format(server_url)

    payload = {
        'access_key': access_key,
        'secret_key': secret_key,
        'study_id': env_vars.get("study_object_id"),
        'file_name': file_name,
        "patient_id": patient_id,
    }

    with open(local_file_path, 'rU') as local_file:
        reader = csv.DictReader(local_file)
        # reader.fieldnames
        payload["summary_output"] = json.dumps([line for line in reader])

    resp = requests.post(json_upload_url, data=payload)

    # Raise an HTTP error if one occurred
    resp.raise_for_status()


def upload_to_s3(local_file_path, remote_file_path, tags_list, env_vars):
    """
    Uploads a local file to S3 bucket.

    :param local_file_path: path to the local file to be uploaded
    :param remote_file_path: name of the file this will be saved as remotely (on Amazon S3)
    :param env_vars: a dictionary of env variables
    """
    
    # Get the necessary credentials for pinging the Beiwe server
    access_key, secret_key = _get_beiwe_credentials(env_vars.get("region_name"),
                                                    env_vars.get("access_key_ssm_name"),
                                                    env_vars.get("secret_key_ssm_name"))

    pipeline_upload_url = '{}/pipeline-upload/v1'.format(env_vars.get("server_url"))
    payload = {
        'access_key': access_key,
        'secret_key': secret_key,
        'study_id': env_vars.get("study_object_id"),
        'file_name': remote_file_path,
        'tags': json.dumps(tags_list),  # endpoint expects JSON list
    }
    with open(local_file_path, 'rb') as local_file:
        resp = requests.post(
            pipeline_upload_url,
            files={'file': local_file},
            data=payload,
        )
    
    # Raise an HTTP error if one occurred
    resp.raise_for_status()


def download_raw_data(local_file, env_vars):
    """
    Downloads all available raw(chunked) data for a study.

    :param local_file: path to the local zip file where the downloaded files will go
    :param env_vars: a dictionary of env variables
    """

    # Get the necessary credentials for pinging the Beiwe server
    access_key, secret_key = _get_beiwe_credentials(env_vars.get("region_name"),
                                                    env_vars.get("access_key_ssm_name"),
                                                    env_vars.get("secret_key_ssm_name"))
    # handle case where http/https not provided
    server_url = env_vars.get("server_url")
    if not server_url.startswith("http"):
        server_url = "https://" + server_url

    data_access_api_url = '{}/get-data/v1'.format(server_url)
    payload = {
        'access_key': access_key,
        'secret_key': secret_key,
        'study_id': env_vars.get("study_object_id"),
        'web_form': 'true'  # Include this because it makes the backend return a zip file
    }

    resp = requests.post(data_access_api_url, data=payload)
    if resp.status_code > 399:
        print(resp.status_code)
        raise Exception(resp.status_code)
    with open(local_file, 'xb') as fn:
        fn.write(resp.content)



# Helper functions
def _get_beiwe_credentials(region_name, access_key_ssm_name, secret_key_ssm_name):
    ssm_client = boto3.client('ssm', region_name=region_name)
    resp = ssm_client.get_parameters(
        Names=(access_key_ssm_name, secret_key_ssm_name),
        WithDecryption=True,
    )['Parameters']
    access_key, secret_key = [p['Value'] for p in resp]
    return access_key, secret_key
