## CLEAN ENVIRONMENT
rm(list = ls())

##1. CHARGE PACKAGES
source("R_scripts/packages.R")

## 2. EXTRACTING_DATA
source("real_data/extracting_data.R")

## 3. EXCLUDING SUBJECTS
source("real_data/excluding_criteria.R")

## 4. EXCLUDING TRAJECTORIES (only for controls)
source("real_data/excluding_trajectories.R")

## 5. LDA (only for controls)
source("real_data/LDA-Derivatives-real-data.R")

## 6. PLOT TRAJECTORIES
source("real_data/trajectories.R")
