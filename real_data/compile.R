## CLEAN ENVIRONMENT
rm(list = ls())

##1. CHARGE PACKAGES
source("Analyses/R_Script/packages.R")

## 2. EXTRACTING_DATA
source("Analyses/R_Script/extracting_data.R")

## 3. EXCLUDING SUBJECTS
source("Analyses/R_Script/excluding_criteria.R")

## 4. EXCLUDING TRAJECTORIES (only for controls)
source("Analyses/R_Script/Control_data/trajectories_before.R")

## 5. LDA (only for controls)
source("Analyses/R_Script/Control_data/LDA.R")

## 6. PLOT TRAJECTORIES
source("Analyses/R_Script/Control_data/trajectories.R")
