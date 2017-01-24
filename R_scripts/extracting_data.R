getwd()

## 1. INFORMATION FILE ##
info<- read.csv(file="data_R/Information.csv", header=TRUE, sep=",")

## 2. DATA FILE WITH ALL RESULTS ##
data <- read.csv(file="data_R/Data.csv", header=TRUE, sep=",")

## 3. ONLY CALIBRATION ##
calibration_data <- data %>% 
  filter(Sentence_Type=='calibration')
