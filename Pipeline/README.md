*   There are five ways to run code: hourly, daily, weekly, monthly and manually. The first four are run regularly with no user interaction. The fifth is run by pressing a button on the View Study page.

*   The code that will be run on each of those schedules is the code in the appropriately named `.py` file in this directory (e.g. `daily.py` is run every day). Example code can be found in the `.py` files themselves, along with an explanation of what is provided in the runtime environment.

*   Any requirements for running any python2/python3 code must be put in the corresponding `pythonX_requirements.txt` file, again in this directory. The same requirements file will be used for the timed jobs and the manual jobs, so put *all* requirements there.

*   To download the raw data, use the `download_s3_files.py` script. It takes one argument: the name of a local `.zip` file which the raw data will be placed into. The file does not have to exist before calling the script.

*   To upload processed data, use the `upload_s3_files.py` script. It takes two arguments: the name of the local `.zip` file which the processed data is in, and the name of the remote file that it will be uploaded to. The local file needs to exist before calling the script, or it will error. The remote file name should be unique; the easiest way to achieve this is to include a timestamp.
