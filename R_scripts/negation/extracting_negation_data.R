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

##4. Subsetting to negation data (without calibration or practice)
negation_data <- data_negation %>% 
  filter(Sentence_Type =='Control')%>%
  mutate (Subject = as.factor(Subject)) 
rm(natives, not_natives)


## 5. Excluding unaccurate trials
innacurate_data <- subset (negation_data, Accuracy==0)
print('percentage of innacurate trials:')
print(nrow(innacurate_data)/nrow(negation_data))
negation_data <- subset(negation_data, Accuracy==1)



## 6. CREATES NORMALIZED_POSITIONS.PLOT WITH INFORMATION OF X,Y COORDINATES, ACCELERATION, TIME AND LOGRATIO FOR EACH TIME STEP

## Include X.Positions as rows
normalized_positions.plot.X = negation_data %>%
  dplyr::select(Subject,Polarity, Expected_response, Normalized.positions.X, Item.number, Sentence) %>%
  separate(Normalized.positions.X, into= as.character(c(1:101)), sep = ",") %>%
  gather(Time.Step, X.Position, 4:104) 

## Include Y.Positions as rows
normalized_positions.plot.Y = negation_data %>%
  dplyr::select(Subject,Polarity, Expected_response, Normalized.positions.Y, Item.number, Sentence)%>%
  separate(Normalized.positions.Y, into= as.character(c(1:101)), sep = ",") %>%
  gather(Time.Step, Y.Position, 4:104) 

## Include Raw.Time as rows
rawtime = negation_data %>% 
  dplyr::select(Subject,Polarity, Expected_response, RawTime, Item.number, Sentence)%>%
  separate(RawTime, into= as.character(c(1:101)), sep = ",") %>%
  gather(Time.Step, RawTime, 4:104) 

## Include Raw.Acceleration as rows
acceleration_calibration = negation_data %>%
  dplyr::select(Subject,Polarity, Expected_response, Acceleration, Item.number, Sentence) %>%
  separate(Acceleration, into= as.character(c(1:101)), sep = ",") %>%
  gather(Time.Step, Acceleration, 4:104) 

## Include LogRatio as rows
log_ratio = negation_data %>%
  dplyr::select(Subject,Polarity, Expected_response, LogRatio, Item.number, Sentence)%>%
  separate(LogRatio, into= as.character(c(1:101)), sep = ",") %>%
  gather(Time.Step, LogRatio, 4:104) 


## Include SmoothAcceleration 
smooth_acceleration = negation_data %>%
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
normalized_positions.plot$RawTime <- as.numeric(normalized_positions.plot$RawTime)
normalized_positions.plot$Subject <- factor(normalized_positions.plot$Subject)
normalized_positions.plot$Polarity <- factor(normalized_positions.plot$Polarity )
normalized_positions.plot$Acceleration <- as.numeric(normalized_positions.plot$Acceleration)
normalized_positions.plot$LogRatio <- as.numeric(normalized_positions.plot$LogRatio)

## Take all X.Positions to the positive form
normalized_positions.plot.false = normalized_positions.plot %>%
  filter(Expected_response=='false')%>%
  dplyr::mutate_at('X.Position', funs('-'))
normalized_positions.plot.true = normalized_positions.plot %>%
  filter(Expected_response=='true')
normalized_positions.plot <- rbind(normalized_positions.plot.false, normalized_positions.plot.true)
normalized_positions.plot$Subject <- factor(normalized_positions.plot$Subject)
rm(normalized_positions.plot.false, normalized_positions.plot.true)


normalized_positions.plot$X.Position<- as.numeric(normalized_positions.plot$X.Position)
normalized_positions.plot$Y.Position <- as.numeric(normalized_positions.plot$Y.Position)
normalized_positions.plot$Time.Step <- as.numeric(normalized_positions.plot$Time.Step)
normalized_positions.plot$Subject <- factor(normalized_positions.plot$Subject)
normalized_positions.plot$Polarity <- factor(normalized_positions.plot$Polarity )
normalized_positions.plot$grp <- paste(normalized_positions.plot$Subject,normalized_positions.plot$Item.number)







