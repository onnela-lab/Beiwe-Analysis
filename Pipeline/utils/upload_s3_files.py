#!/usr/bin/python3
"""
Upload files pertaining to a particular study to the Beiwe Data Pipeline Upload API.

The relevant AWS object names and study information are passed as environment variables,
and the file names required are passed as function or command line arguments.
"""

import json
import os
import sys

import boto3
import requests


def run(local_file, remote_file):
    """
    Run the actual code.
    
    :param local_file: path to the local file to be uploaded
    :param remote_file: name of the file this will be saved as remotely (on Amazon S3)
    """
    
    # Grab environment variables
    freq = os.getenv('FREQ')
    access_key_ssm_name = '{}-{}'.format(os.getenv('access_key_ssm_name'), freq)
    secret_key_ssm_name = '{}-{}'.format(os.getenv('secret_key_ssm_name'), freq)
    study_object_id = os.getenv('study_object_id')
    region_name = os.getenv('region_name')
    server_url = os.getenv('server_url')
    
    # Get the necessary credentials for pinging the Beiwe server
    ssm_client = boto3.client('ssm', region_name=region_name)
    resp = ssm_client.get_parameters(
        Names=(access_key_ssm_name, secret_key_ssm_name),
        WithDecryption=True,
    )['Parameters']
    access_key, secret_key = [p['Value'] for p in resp]
    
    pipeline_upload_url = '{}/pipeline-upload/v1'.format(server_url)
    
    payload = {
        'access_key': access_key,
        'secret_key': secret_key,
        'study_id': study_object_id,
        'file_name': remote_file,
        'tags': json.dumps([remote_file]),  # endpoint expects JSON list
    }
    
    with open(local_file, 'rb') as fn:
        resp = requests.post(
            pipeline_upload_url,
            files={'file': fn},
            data=payload,
        )
    
    # Raise an HTTP error if one occurred
    resp.raise_for_status()


if __name__ == '__main__':
    _local_file = sys.argv[1]
    _remote_file = sys.argv[2]
    run(_local_file, _remote_file)
