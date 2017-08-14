getwd()

## 1. INFORMATION FILE ##
info_calibration <- read.csv(file="data_R/calibration_pilot/Information.csv", header=TRUE, sep=",")
info_calibration$Subject<- mapvalues(info_calibration$Subject, from = c(0:max(info_calibration$Subject)), to = c(1:length(info_calibration$Subject)))

## 2. DATA FILE WITH ALL RESULTS ##
data_calibration <- read.csv(file="data_R/calibration_pilot/Data.csv", header=TRUE, sep=",")
data_calibration$Subject<- mapvalues(data_calibration$Subject, from = c(0:max(data_calibration$Subject)), to = c(1:length(info_calibration$Subject)))
data_calibration$Item.number<- mapvalues(data_calibration$Item.number, from = c(0:93), to = c(1:94))

## 3. EXCLUDING not natives (not for this calibration)
#natives <- subset(info_calibration, grepl('en',info_calibration$Language, ignore.case=TRUE))
#not_natives <- subset(info_calibration, !(Subject %in% natives$Subject))

#info_calibration <- subset(info_calibration, !(Subject %in% not_natives$Subject))
#data_calibration <- subset(data_calibration, !(Subject %in% not_natives$Subject))

calibration_data <- subset(data_calibration, Type !='practice')
calibration_data$grp <- paste(calibration_data$Subject,calibration_data$Item.number)
#rm(natives, not_natives)

##4. EXCLUDING inaccurate trials
innacurate_data <- subset (calibration_data, Accuracy==FALSE)
print('percentage of innacurate trials:')
print(nrow(innacurate_data)/nrow(calibration_data))
calibration_data <- subset(calibration_data, Accuracy==TRUE)


## 5. ORGANIZING CALIBRATION DATA
##Organizing the data a bit (creates cuts to visualize data afterwards)
calibration_data$MaxLogRatio_cut <- cut(calibration_data$MaxLogRatio, 5)
calibration_data$PointChange.Time_cut <- cut(calibration_data$PointChange.Time, breaks=c(0, 25, 50, 75, 100), include.lowest=TRUE)
calibration_data$PointChange.Time.Raw_cut <- cut(calibration_data$PointChange.Time.Raw, breaks=c(0, 300, 500, 800, 1100, 1700), dig.lab = 5, include.lowest=TRUE)
calibration_data$PointChange_cut <- cut(calibration_data$PointChange, breaks=c(0, 0.25, 0.5, 0.75, 1), dig.lab = 5 , include.lowest=TRUE)
calibration_data$RT_cut <- cut(calibration_data$RT, breaks=c(0, 500, 1000, 1500, 2000, 3500), dig.lab = 5, include.lowest=TRUE)

# NB: For calibration only
levels(calibration_data$Expected_response) <- c('blue','red')

## 6. CREATES NORMALIZED_POSITIONS.PLOT WITH INFORMATION OF X,Y COORDINATES, ACCELERATION, TIME AND LOGRATIO FOR EACH TIME STEP

## Include X.Positions as rows
normalized_positions.plot.X = calibration_data %>%
  dplyr::select(Subject,Polarity, Expected_response, Normalized.positions.X, Item.number, grp, MaxLogRatio, MaxLogRatio_cut, MaxRatio.Time.Norm, PointChange_cut, PointChange.Time_cut,PointChange.Time.Raw_cut, PointChange.Time.Raw, RT_cut, PointChange.Time) %>%
  separate(Normalized.positions.X, into= as.character(c(1:101)), sep = ",") %>%
  gather(Time.Step, X.Position, 4:104) 

## Include Y.Positions as rows
normalized_positions.plot.Y = calibration_data %>%
  dplyr::select(Subject,Polarity, Expected_response, Normalized.positions.Y,Item.number, grp, MaxLogRatio, MaxLogRatio_cut, MaxRatio.Time.Norm, PointChange_cut, PointChange.Time_cut,PointChange.Time.Raw_cut, PointChange.Time.Raw, RT_cut, PointChange.Time)%>%
  separate(Normalized.positions.Y, into= as.character(c(1:101)), sep = ",") %>%
  gather(Time.Step, Y.Position, 4:104) 

## Include Raw.Time as rows
rawtime = calibration_data %>% 
  dplyr::select(Subject,Polarity, Expected_response, RawTime, Item.number, grp, MaxLogRatio, MaxLogRatio_cut, MaxRatio.Time.Norm, PointChange_cut, PointChange.Time_cut,PointChange.Time.Raw_cut, PointChange.Time.Raw, RT_cut, PointChange.Time)%>%
  separate(RawTime, into= as.character(c(1:101)), sep = ",") %>%
  gather(Time.Step, RawTime, 4:104) 

## Include Raw.Acceleration as rows
acceleration_calibration = calibration_data %>%
  dplyr::select(Subject,Polarity, Expected_response, Acceleration, Item.number, grp, MaxLogRatio, MaxLogRatio_cut, MaxRatio.Time.Norm, PointChange_cut, PointChange.Time_cut,PointChange.Time.Raw_cut, PointChange.Time.Raw, RT_cut, PointChange.Time) %>%
  separate(Acceleration, into= as.character(c(1:101)), sep = ",") %>%
  gather(Time.Step, Acceleration, 4:104) 

## Include LogRatio as rows
log_ratio = calibration_data %>%
  dplyr::select(Subject,Polarity, Expected_response, LogRatio, Item.number, grp, MaxLogRatio, MaxLogRatio_cut, MaxRatio.Time.Norm, PointChange_cut, PointChange.Time_cut,PointChange.Time.Raw_cut, PointChange.Time.Raw, RT_cut, PointChange.Time) %>%
  separate(LogRatio, into= as.character(c(1:101)), sep = ",") %>%
  gather(Time.Step, LogRatio, 4:104) 


## Include SmoothAcceleration 
smooth_acceleration = calibration_data %>%
  dplyr::select(Subject,Polarity, Expected_response, Acceleration_Smooth, Acceleration_Smooth.Time, Item.number) %>%
  separate(Acceleration_Smooth, into= as.character(c(1:101)), sep = ",") %>%
  gather(Time.Step, Acceleration_Smooth, 4:104) 

smooth_acceleration$Acceleration_Smooth <- as.numeric(smooth_acceleration$Acceleration_Smooth)
smooth_acceleration$Time.Step <- as.numeric(smooth_acceleration$Time.Step)

## Put everything together
normalized_positions.plot <- merge(normalized_positions.plot.X,normalized_positions.plot.Y)
normalized_positions.plot <- merge(normalized_positions.plot, rawtime)
normalized_positions.plot <- merge(normalized_positions.plot, acceleration_calibration)
normalized_positions.plot <- merge(normalized_positions.plot, log_ratio)
normalized_positions.plot <- merge(normalized_positions.plot, smooth_acceleration)
rm(acceleration_calibration, rawtime, normalized_positions.plot.X, normalized_positions.plot.Y, log_ratio)

## Format data
normalized_positions.plot$X.Position<- as.numeric(normalized_positions.plot$X.Position)
normalized_positions.plot$Y.Position <- as.numeric(normalized_positions.plot$Y.Position)
normalized_positions.plot$Time.Step <- as.numeric(normalized_positions.plot$Time.Step)
normalized_positions.plot$PointChange.Time <- as.numeric(normalized_positions.plot$PointChange.Time)
normalized_positions.plot$RawTime <- as.numeric(normalized_positions.plot$RawTime)
normalized_positions.plot$Subject <- factor(normalized_positions.plot$Subject)
normalized_positions.plot$Polarity <- factor(normalized_positions.plot$Polarity )
normalized_positions.plot$Acceleration <- as.numeric(normalized_positions.plot$Acceleration)
normalized_positions.plot$LogRatio <- as.numeric(normalized_positions.plot$LogRatio)

## Take all X.Positions to the positive form
normalized_positions.plot.false = normalized_positions.plot %>%
  filter(Expected_response=='blue')%>%
  dplyr::mutate_at('X.Position', funs('-'))
normalized_positions.plot.true = normalized_positions.plot %>%
  filter(Expected_response=='red')
normalized_positions.plot <- rbind(normalized_positions.plot.false, normalized_positions.plot.true)
normalized_positions.plot$Subject <- factor(normalized_positions.plot$Subject)
rm(normalized_positions.plot.false, normalized_positions.plot.true)

## Take the time at which the color changed as onset in both raw time and time step
normalized_positions.plot$RawTime.Onset <- normalized_positions.plot$RawTime - normalized_positions.plot$PointChange.Time.Raw
normalized_positions.plot$Time.Step.Onset <- normalized_positions.plot$Time.Step - normalized_positions.plot$PointChange.Time

