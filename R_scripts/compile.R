## CLEAN ENVIRONMENT
rm(list = ls())

## CHARGE PACKAGES
source("R_scripts/packages.R")

## EXTRACTING_DATA
#Extracts data from calibration (last version: June 2017) into calibration_data
source("R_scripts/extracting_calibration_data.R")

## DATA FRAME FOR PLOT
# Creates data frame normalized_positions.plot: each row contains one time.step with X.Position, Y.Position, Raw.Acceleration, Raw.Time columns among others 
source("R_scripts/calibration-trajectories.R")

## LDAs 
source("R_scripts/LDA-training(Coord+Delta+DeltaDelta).R")

## 5. Plot Calibration Trajectories 
source("R_scripts/plot-calibration-trajectories.R")

##NEGATION
## 1. Extract data
source("R_scripts/extracting_data.R")

##6. LDA TEST with data
source("R_scripts/LDA-to-real-data.R")

##7. TRAJECTORIES with real data
source("R_scripts/negation-data-trajectories.R")


