## CLEAN ENVIRONMENT
rm(list = ls())

##1. CHARGE PACKAGES
source("R_scripts/packages.R")

## 2. EXTRACTING_DATA
source("real_data/extracting_data.R")

## 3. EXCLUDING SUBJECTS
source("real_data/excluding_criteria.R")

## 4. EXCLUDING TRAJECTORIES (for plurals)
source("real_data/plural/exclusion_trajectories_plural.R")

## 5. LDA (only for plurals)
source("real_data/plural/LDA-Derivatives-plural-data.R")


## 6. PLOT TRAJECTORIES
