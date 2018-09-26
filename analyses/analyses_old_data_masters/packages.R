
# Data cleaning / management
require(tidyverse)
require(plyr)
require(reshape2)

#LDA and analyses
require(lme4)
require(lda)
require(rocauc)
require(pROC)
source("R_Script/bins.R")
source("R_Script/LDA.R")
library(boot)

# Standard error of the mean
se <- function(x, ...) {
  n <- length(x)
  return(sd(x, ...)/sqrt(n))
}

#randomization within
random.within <- function (data, class) {
  data$Subject <- factor(data$Subject)
  vector.subject <- c('0')
  for (s in levels(data$Subject)){
    data.sub <- subset(data, Subject==s)
    tr <- unlist(data.sub[,class])
    tr.random <- as.character(sample(tr, length(tr), replace=FALSE))
    vector.subject <- c(vector.subject,tr.random)}
  vector.subject <- vector.subject[2:length(vector.subject)]
  data$Random <- vector.subject 
  return(data)
}

random <- function (data, class) {
  tr <- unlist(data[,class])
  data$Random<- as.character(sample(tr, length(tr), replace=FALSE))
   
  return(data)
}

#Plots
library("ggsci")
library("ggplot2")
library("gridExtra")
library("ggpubr")

plot_trajectory <- function(subject, item, data){
  plot<- ggplot(subset(data, Subject==subject & Item.number==item), aes(y=X.Position, x=Time.Step)) + geom_point(alpha=.4, size=1) 
  return(plot)}

source("R_Script/plot_measures.R")
source("R_Script/plot_measures_target.R")



