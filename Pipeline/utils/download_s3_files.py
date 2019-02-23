#!/usr/bin/python3
"""
Download files pertaining to a particular study from the Beiwe Data Access API.

The relevant AWS object names and study information are passed as environment variables,
and the file names required are passed as function or command line arguments.
"""

import os
import sys

import boto3
import requests


def run(local_file):
    """
    Run the actual code.
    
    :param local_file: path to the local zip file where the downloaded files will go
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
    
    data_access_api_url = '{}/get-data/v1'.format(server_url)
    
    payload = {
        'access_key': access_key,
        'secret_key': secret_key,
        'study_id': study_object_id,
        'web_form': 'true'  # Include this because it makes the backend return a zip file
    }
    # TODO do this as a generator, if simple
    resp = requests.post(data_access_api_url, data=payload)
    byte_stream = resp.content
    with open(local_file, 'xb') as fn:
        fn.write(byte_stream)


if __name__ == '__main__':
    _local_file = sys.argv[1]
    run(_local_file)
