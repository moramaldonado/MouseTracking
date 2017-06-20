getwd()

## 1. INFORMATION FILE ##
info_calibration <- read.csv(file="data_R/calibration_pilot/Information.csv", header=TRUE, sep=",")
info_calibration$Subject<- mapvalues(info_calibration$Subject, from = c(0:max(info_calibration$Subject)), to = c(1:length(info_calibration$Subject)))

## 2. DATA FILE WITH ALL RESULTS ##
data_calibration <- read.csv(file="data_R/calibration_pilot/Data.csv", header=TRUE, sep=",")
data_calibration$Subject<- mapvalues(data_calibration$Subject, from = c(0:max(data_calibration$Subject)), to = c(1:length(info_calibration$Subject)))
data_calibration$Item.number<- mapvalues(data_calibration$Item.number, from = c(0:93), to = c(1:94))

## 3. EXCLUDING not natives
natives <- subset(info_calibration, grepl('en',info_calibration$Language, ignore.case=TRUE))
not_natives <- subset(info_calibration, !(Subject %in% natives$Subject))

info_calibration <- subset(info_calibration, !(Subject %in% not_natives$Subject))
data_calibration <- subset(data_calibration, !(Subject %in% not_natives$Subject))

calibration_data <- subset(data_calibration, Type !='practice')
calibration_data$grp <- paste(calibration_data$Subject,calibration_data$Item.number)


rm(natives, not_natives)