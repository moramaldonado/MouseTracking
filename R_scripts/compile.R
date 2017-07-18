## 1. CLEAN ENVIRONMENT
rm(list = ls())

## 2. CHARGE PACKAGES
source("R_scripts/packages.R")

## 3. CHARGE FUNCTIONS FOR LDA TRAINING
source("R_scripts/LDA.R")

## 4. EXTRACTING and ORGANIZING CALIBRATION DATA
# Extracts data from calibration (last version: June 2017) into calibration_data
# Excludes innacurate trials and print the percentage
# Creates data frame normalized_positions.plot: each row contains one time.step with X.Position, Y.Position, Raw.Acceleration, Raw.Time columns among others 
source("R_scripts/extract_calibration_data.R")

## 5. Overall description of the results and computation of original/main LDA (subsection 2.2)





## LDAs (original)
source("R_scripts/LDA-run.R")

##COMPARE predictors and measures


## PLOT Trajectories
source("R_scripts/plot-calibration-trajectories.R")







##NEGATION
## 1. Extract data
source("R_scripts/extracting_data.R")

##6. LDA TEST with data
source("R_scripts/LDA-to-real-data.R")

##7. TRAJECTORIES with real data
source("R_scripts/negation-data-trajectories.R")


