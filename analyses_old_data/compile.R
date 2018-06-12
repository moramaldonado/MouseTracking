## CLEAN ENVIRONMENT
rm(list = ls())
x <- paste0('x', sprintf("%03d", c(1:101)))
y <- paste0('y', sprintf("%03d", c(1:101)))
v <- paste0('v', sprintf("%03d", c(1:101)))
a <- paste0('a', sprintf("%03d", c(1:101)))
t <- paste0('t', sprintf("%03d", c(1:101)))

##1. CHARGE PACKAGES
source("R_Script/packages.R")

## 2. EXTRACTING_DATA
source("R_Script/extracting_data.R")

## 3. EXCLUDING TRAJECTORIES AND SUBJECTS
source("R_Script/exclusion_trajectories_plural.R")
source("R_Script/excluding_criteria.R")


source("R_Script/controls.R")
source("R_Script/analyses.R")

