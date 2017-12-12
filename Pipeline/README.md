There are five ways to run code: hourly, daily, weekly, monthly and manually. The first four are run regularly with no user interaction. The fifth is run by pressing a button on the View Study page.

To run code on any of those five schedules, put the bash command used to run that code in the appropriately named .sh file in this directory. The full path to the file must be used, starting from Beiwe-Analysis. For example, to run `Beiwe-Analysis/Preprocessing/weekly_task.R` every week, add the line `Rscript Beiwe-Analysis/Preprocessing/weekly_task.R` to weekly.sh.

Any requirements for running any python2/python3 code must be put in the corresponding \_requirements.txt file, again in this directory. The same requirements file will be used for the timed jobs and the manual jobs, so put *all* requirements there.
