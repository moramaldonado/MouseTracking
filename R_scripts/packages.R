# Data cleaning / management
library(tidyverse)
library(plyr)
# Effect sizes
library(effsize)
library(powerAnalysis)

# Power analyses
library(pwr)

# Linear mixed effects models
library(lme4)

# Standard error of the mean
se <- function(x, ...) {
  n <- length(x)
  return(sd(x, ...)/sqrt(n))
}