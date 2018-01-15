#!/usr/bin/python3
"""
This file will be run every hour using Python 3.5 in a Ubuntu server environment.

It has access to the following:
Programs:
    R
    Python 2.7
    Python 3.5
    Any python libraries listed in python3-requirements.txt
Environment variables (accessed using os.getenv):
    server_url: The domain name of your Beiwe instance (e.g. mystudy.beiwe.org)
    study_object_id: The UUID of the study that the code is being run on
        (e.g. '584b042c2dd65714f0a8c3f4')
    study_name: The name of the study that the code is being run on (e.g. 'Diabetes Study 2018')
Files:
    The Beiwe-Analysis repository, located at /home/Beiwe-Analysis. This file is located
        inside that repository, at /home/Beiwe-Analysis/Pipeline/hourly.py.
    /home/download_s3_files.py: A python3 script that downloads the files for the study and
        puts them in a zip file. The zip file name is specified as an command-line argument.
    /home/upload_s3_files.py: A python3 script that takes a file and uploads it to an S3
        bucket. The local and S3 file names are both specified as command-line arguments.
If you experience difficulties with configuration, please contact msimoneau@hsph.harvard.edu.

Commented out example code follows.
"""
