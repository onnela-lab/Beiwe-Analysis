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


def upload_file_to_s3(local_file_path, remote_file_path, tags_list):
    """
    Run the actual code.
    
    :param local_file_path: path to the local file to be uploaded
    :param remote_file_path: name of the file this will be saved as remotely (on Amazon S3)
    """
    
    # Grab environment variables
    freq = os.environ['FREQ']
    access_key_ssm_name = '{}-{}'.format(os.environ['access_key_ssm_name'], freq)
    secret_key_ssm_name = '{}-{}'.format(os.environ['secret_key_ssm_name'], freq)
    study_object_id = os.environ['study_object_id']
    region_name = os.environ['region_name']
    server_url = os.environ['server_url']

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


if __name__ == '__main__':
    """
    You can call this file from a Bash shell with 3 command-line arguments:
    1. The filepath of a local file to upload
    2. The name you want the file to have once it's uploaded to the server
    3. A file containing a JSON list of tags
    
    Or, you can import the upload_file_to_s3() function in Python and call that function directly. 
    """
    _local_file_path = sys.argv[1]
    _remote_file_path = sys.argv[2]
    _tags_file = sys.argv[3]
    with open(_tags_file) as json_tags_file:
        tags_list = json.load(json_tags_file)
    upload_file_to_s3(_local_file_path, _remote_file_path, tags_list)
