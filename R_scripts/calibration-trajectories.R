## ORGANIZES THE DATA FRAME CALIBRATION_DATA
## CREATES NORMALIZED_POSITIONS.PLOT WITH INFORMATION OF X,Y COORDINATES, ACCELERATION, TIME AND LOGRATIO FOR EACH TIME STEP


##Organizing the data a bit
calibration_data <- subset(calibration_data, Accuracy==TRUE)
calibration_data$MaxLogRatio_cut <- cut(calibration_data$MaxLogRatio, 5)
calibration_data$PointChange.Time_cut <- cut(calibration_data$PointChange.Time, breaks=c(0, 25, 50, 75, 100), include.lowest=TRUE)
calibration_data$PointChange.Time.Raw_cut <- cut(calibration_data$PointChange.Time.Raw, breaks=c(0, 300, 500, 800, 1100, 1700), dig.lab = 5, include.lowest=TRUE)
calibration_data$PointChange_cut <- cut(calibration_data$PointChange, breaks=c(0, 0.25, 0.5, 0.75, 1), dig.lab = 5 , include.lowest=TRUE)
calibration_data$RT_cut <- cut(calibration_data$RT, breaks=c(0, 500, 1000, 1500, 2000, 3500), dig.lab = 5, include.lowest=TRUE)








## NB: For this calibration only
levels(calibration_data$Expected_response) <- c('blue','red')

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


AUC <- ddply(normalized_positions.plot, c("grp"),
             function(normalized_positions.plot)c(auc_2=auc(normalized_positions.plot$Time.Step, normalized_positions.plot$X.Position)))
XFLIPS <- ddply(normalized_positions.plot, c("grp"),
             function(normalized_positions.plot)c(Xflips_2=xflip(normalized_positions.plot$X.Position, 1)))

calibration_data <- merge(calibration_data, AUC, by='grp')
calibration_data <- merge(calibration_data, XFLIPS, by='grp')