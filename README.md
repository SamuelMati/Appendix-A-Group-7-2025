# Group-7-Jobindex-Log-Analysis-R-

This is the repository of our log analysis conducted in R. This is supposed to act as an appendix material for our semester report titled: Understanding Recruiter Search Behaviour in Jobindex: A Mixed-Methods Analysis.

---===---
How to run:

- Download the "Jobindex Log Analysis" file
- Some .csv files had to be zipped, to run the scripts, pleas extract the .zip files containing those .csv files one by one (into the Jobindex Log Analysis folder), until you folder looks like the ExtractedFiles.png picture
- Run the "Jobindex Log Analysis Workspace"
- The scripts in which we clean the .csv files can be found in the Data Preparation Scripts folder, while the respective RQ (1,2 or 3) folder contains the Research Question Analysis scripts
- Open the desired folder and run the desired script (it should open it within the workspace, which will ensure the script can access the necessary .csv files)
- Run the installation command below, if you haven't previously installed the packages we are using
- Run the script

---===---
Package installation command:

install.packages(c(
  "tidyverse",
  "lubridate",
  "dplyr",
  "tidyr",
  "stringdist",
  "ggplot2"
))
