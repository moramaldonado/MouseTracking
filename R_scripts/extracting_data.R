getwd()

## 1. INFORMATION FILE ##
info<- read.csv(file="data_R/Information.csv", header=TRUE, sep=",")
info$Subject<- mapvalues(info$Subject, from = c(0:max(info$Subject)), to = c(1:length(info$Subject)))

## 2. DATA FILE WITH ALL RESULTS ##
data <- read.csv(file="data_R/Data.csv", header=TRUE, sep=",")
data$Subject<- mapvalues(data$Subject, from = c(0:max(data$Subject)), to = c(1:length(info$Subject)))
data$Item.number<- mapvalues(data$Item.number, from = c(0:63), to = c(1:64))

## 3. ONLY CALIBRATION ##
calibration_data <- data %>% 
  filter(Sentence_Type=='calibration')
