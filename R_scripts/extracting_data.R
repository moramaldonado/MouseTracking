getwd()

## 1. INFORMATION FILE ##
info<- read.csv(file="data_R/Information.csv", header=TRUE, sep=",")
info$Subject<- mapvalues(info$Subject, from = c(0:max(info$Subject)), to = c(1:length(info$Subject)))

## 2. DATA FILE WITH ALL RESULTS ##
data <- read.csv(file="data_R/Data.csv", header=TRUE, sep=",")
data$Subject<- mapvalues(data$Subject, from = c(0:max(data$Subject)), to = c(1:length(info$Subject)))
data$Item.number<- mapvalues(data$Item.number, from = c(0:30), to = c(1:31))

data <- subset(data, Subject!=1)

## 3. ONLY CALIBRATION ##
calibration_data <- data %>% 
  filter(Sentence_Type=='calibration')%>%
  mutate(Subject = as.factor(Subject)) 

calibration_data$Expected_response = factor(calibration_data$Expected_response)
calibration_data <- calibration_data %>%  mutate(Accuracy = if_else(Expected_response==Response, TRUE, FALSE)) 

##4. ONLY REAL DATA
negation_data <- data %>% 
  filter(Sentence_Type=='Control')%>%
  mutate (Subject = as.factor(Subject))




