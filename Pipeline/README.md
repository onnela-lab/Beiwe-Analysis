## Instructions for writing code

*   There are five ways to run code: hourly, daily, weekly, monthly and manually. The first four are run regularly with no user interaction. The fifth is run by pressing a button on the View Study page.

*   The code that will be run on each of those schedules is the code in the appropriately named `.py` file in this directory (e.g. `daily.py` is run every day). Example code can be found in the `.py` files themselves, along with an explanation of what is provided in the runtime environment.

*   Any requirements for running any python2/python3 code must be put in the corresponding `pythonX_requirements.txt` file, again in this directory. The same requirements file will be used for the timed jobs and the manual jobs, so put *all* requirements there.

*   To download the raw data, use the `download_s3_files.py` script. It takes one argument: the name of a local `.zip` file which the raw data will be placed into. The file does not have to exist before calling the script.

*   To upload processed data, use the `upload_s3_files.py` script. It takes two arguments: the name of the local `.zip` file which the processed data is in, and the name of the remote file that it will be uploaded to. The local file needs to exist before calling the script, or it will error. The remote file name should be unique; the easiest way to achieve this is to include a timestamp.

## Instructions for running code locally

*   In order to run code locally, you must first set several environment variables. Example shell code to set these variables follows:
    ```bash
    export study_name="My Test Study"
    export study_object_id="584b042c2dd65714f0a8c3f4"
    export access_key_ssm_name="beiwe-daa-key-0"
    export secret_key_ssm_name="beiwe-daa-key-1"
    export region_name="us-west-2"
    export server_url="https://mystudy.beiwe.org"
    export FREQ="manually"
    ```
    You have to run this step every time that you run this code from a new shell. If you are unsure if you have to run this step, run `echo $FREQ` and see if it has a value; if it does, you probably don't have to.

*   You will also have to set up your Amazon AWS configuration in order to download and upload data. You do this by running `aws configure` and then entering your credentials at the prompts. You only have to do this once.

*   If you have not installed requirements, run:
    ```bash
    pip3 install -r Beiwe-Analysis/Pipeline/python3-requirements.txt
    ```
     You only have to do this once.

*   To run the actual code, run the following line of shell code from the directory containing the Beiwe-Analysis folder:
    ```bash
    /bin/bash Beiwe-Analysis/Pipeline/utils/runner.sh
    ```
