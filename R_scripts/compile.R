
#### CALIBRATION DATA ####

## CLEAN ENVIRONMENT
rm(list = ls())

## CHARGE PACKAGES
source("R_scripts/packages.R")

## CHARGE FUNCTIONS FOR LDA TRAINING
source("R_scripts/calibration/LDA.R")

## EXTRACTING and ORGANIZING CALIBRATION DATA
# Extracts data from calibration (last version: June 2017) into calibration_data
# Excludes innacurate trials and print the percentage
# Creates data frame normalized_positions.plot: each row contains one time.step with X.Position, Y.Position, Raw.Acceleration, Raw.Time columns among others 
source("R_scripts/calibration/extract_calibration_data.R")

## CALIBRATION RESULTS 
### Overall description of the results and computation of original/main LDA (subsection 2.2)
source("R_scripts/calibration/calibration_results.R")
### Comparison with other measures normally used in mouse tracking (subsubsection 2.2.1)
source("R_scripts/calibration/comparisons_measures.R")
### Comparison with classifiers which take into account different sets of predictors(subsubsection 2.2.2)
source("R_scripts/calibration/comparisons_other_predictors.R")



#### NEGATION DATA #####

## CLEAN ENVIRONMENT
rm(list = ls())

## CHARGE PACKAGES
source("R_scripts/packages.R")








##NEGATION
## 1. Extract data
source("R_scripts/extracting_data.R")

##6. LDA TEST with data
source("R_scripts/LDA-to-real-data.R")

##7. TRAJECTORIES with real data
source("R_scripts/negation-data-trajectories.R")


