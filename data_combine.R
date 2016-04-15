#########################################################
#            Dynamic generation of core dataset         #
#########################################################


# Set working directory
setwd("~/Documents/CDA/collaborative_projects/Assignment3_P-P") # Unger

# Source PCE deflator 
source('deflator.R')

# Source CPS income percentile data from Bertrand (2013)'s replication file
# The file is only downloaded if not already present in working directory
if(!file.exists("CPS_Bertrand.rda")) {
  source('CPS_Bertrand.R')
}

# Opens CPS_Bertrand.rda if the previous step was skipped (to save computational power)
if(!exists("CPS.df" )) {
  load("CPS_Bertrand.rda")
}

# Source GSS downloader file if the dataset is not present on in the working directory

if(!file.exists("GSS.CS.rda")) {
  source('GSS_data_downloader.R')
}

# Open the GSS dataset if previous step was skipped, otherwise not to save computational power
if(!exists("GSS.CS.df" )) {
  load("GSS.CS.rda")
}

# Data-transformations

# The data transformations require substantial computational power. Thus after first 
# run it creates the file: "data_final.dta". The code below sources the transformations
# only if it hasn't been done priorly. 

if(!file.exists("data_final.rda")) {
  source('GSS_prep_dynamic.R')
}




