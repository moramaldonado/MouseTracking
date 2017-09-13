
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
source("R_scripts/calibration/comparison_other_predictors.R")

### Permutation tests
source("R_scripts/calibration/permutation_test.R")


#### NEGATION DATA #####

## CLEAN ENVIRONMENT
rm(list = ls())

## CHARGE PACKAGES
source("R_scripts/packages.R")

## EXTRACTING and ORGANIZING NEGATION DATA
# Extracts data from negation (last version: May 2017) into negation_data
# Excludes innacurate trials and print the percentage
# Creates data frame normalized_positions.plot: each row contains one time.step with X.Position, Y.Position, Raw.Acceleration, Raw.Time columns among others 

source("R_scripts/negation/extracting_negation_data.R")

## ANALIZE NEGATION DATA (subsection 3.2)
### Plotting the performance 
### Taking the lda-measure based on the training done with calibration data
### Run the stats
source("R_scripts/negation/negation_results.R")




