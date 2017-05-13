## CLEAN ENVIRONMENT
rm(list = ls())

##1. CHARGE PACKAGES
source("R_scripts/packages.R")

## 2. EXTRACTING_DATA
#calibration_data includes all the data we have
##negattion_data includes **only** data from the experiment
source("R_scripts/extracting_data.R")

## 3. CREATING A DATA.FRAME WITH POSITIONS TO PLOT (CALIBRATION DATA) + EXCLUSION OF WEIRD TRAJECTORIES
# excluded_trials: contains all subject-item pairs that are excluded from the calibration data 
source("R_scripts/calibration-trajectories.R")

## 4. LDA: Take LDA measure from calibration data, store. 
source("R_scripts/LDA-from-calibration.R")

## 5. Plot Calibration Trajectories 
source("R_scripts/plot-calibration-trajectories.R")

##6. LDA TEST with data
source("R_scripts/LDA-to-real-data.R")

##7. TRAJECTORIES with real data
source("R_scripts/negation-data-trajectories.R")


