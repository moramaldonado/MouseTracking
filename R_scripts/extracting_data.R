getwd()

## 1. INFORMATION FILE ##
info_negation<- read.csv(file="data_R/Information.csv", header=TRUE, sep=",")
info_negation$Subject<- mapvalues(info_negation$Subject, from = c(0:max(info_negation$Subject)), to = c(1:length(info_negation$Subject)))

## 2. DATA FILE WITH ALL RESULTS ##
data_negation <- read.csv(file="data_R/Data.csv", header=TRUE, sep=",")
data_negation$Subject<- mapvalues(data_negation$Subject, from = c(0:max(data_negation$Subject)), to = c(1:length(info_negation$Subject)))
data_negation$Item.number<- mapvalues(data_negation$Item.number, from = c(0:35), to = c(1:36))

## 3. EXCLUDING not natives
natives <- subset(info_negation, grepl('en',info_negation$Language, ignore.case=TRUE))
not_natives <- subset(info_negation, !(Subject %in% natives$Subject))

info_negation <- subset(info_negation, !(Subject %in% not_natives$Subject))
data_negation <- subset(data_negation, !(Subject %in% not_natives$Subject))

## 4. ONLY CALIBRATION ##
#Extracting the calibration data for our three sources (the validation Mora did, the pilot and the experiment)
# calibration_data_validation <- read.csv(file="data_R/Data_validation.csv", header=TRUE, sep=",")
# subj <- paste0('V', c(1:9))
# calibration_data_validation <- calibration_data_validation  %>% 
#   filter(Sentence_Type=='calibration')%>%
#   mutate(Subject = as.factor(Subject)) %>%
#   mutate(Calibration= 'validation')
# calibration_data_validation$Subject<- mapvalues(calibration_data_validation$Subject, from = c( "1", "2", "3", "4" , "5", "6", "7", "8", "9"), to = subj)
# 
# calibration_data_pilot <- read.csv(file="data_R/Data_pilot.csv", header=TRUE, sep=",")
# subj <- paste0('P', c(1:9))
# calibration_data_pilot <- calibration_data_pilot %>% 
#   dplyr::select(-Sentence)%>%
#   filter(Sentence_Type=='calibration')%>%
#   mutate(Subject = as.factor(Subject)) %>%
#   mutate(Calibration= 'pilot')
# calibration_data_pilot$Subject<- mapvalues(calibration_data_pilot$Subject, from = c("0", "1", "2", "3", "4" , "5", "6", "7", "8"), to = subj)
# calibration_data_pilot<- subset(calibration_data_pilot, Subject!='P1')
# calibration_data_pilot$Subject <- factor(calibration_data_pilot$Subject)
# 
# calibration_data_experiment <- data_negation %>% 
#   dplyr::select(-Sentence)%>%
#   filter(Sentence_Type=='calibration')%>%
#   mutate(Subject = as.factor(Subject)) %>%
#   mutate(Calibration= 'experiment')
# 
# calibration_data <- rbind(calibration_data_experiment,calibration_data_pilot,calibration_data_validation)
# calibration_data$Expected_response = factor(calibration_data$Expected_response)
# calibration_data<- calibration_data %>%  mutate(Accuracy = if_else(Expected_response==Response, TRUE, FALSE)) 
# calibration_data$grp <- paste(calibration_data$Subject,calibration_data$Item.number)

##4. ONLY REAL DATA
negation_data <- data_negation %>% 
  filter(Sentence_Type=='Control')%>%
  mutate (Subject = as.factor(Subject))

rm(natives, not_natives)



