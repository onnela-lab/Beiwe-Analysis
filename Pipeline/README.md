*   There are five ways to run code: hourly, daily, weekly, monthly and manually. The first four are run regularly with no user interaction. The fifth is run by pressing a button on the View Study page.

*   To run code on any of those five schedules, put the bash command used to run that code in the appropriately named .sh file in this directory. The code will be run from the parent directory to Beiwe-Analysis, so any files must be referenced using their paths starting from Beiwe-Analysis. For example, to run `Beiwe-Analysis/Preprocessing/weekly_task.R` every week, add the line
    ```bash
    Rscript Beiwe-Analysis/Preprocessing/weekly_task.R
    ```
    to weekly.sh.

*   Any requirements for running any python2/python3 code must be put in the corresponding \_requirements.txt file, again in this directory. The same requirements file will be used for the timed jobs and the manual jobs, so put *all* requirements there.

*   To upload files at the end of processing, use the `upload_s3_files.py` script. You should call this directly in the .sh file:
    ```bash
    python3 upload_s3_files.py local_file remote_file
    ```
    where `local_file` is the path to the local file you wish to upload, and `remote_file` is what that file will be called when you download it.
