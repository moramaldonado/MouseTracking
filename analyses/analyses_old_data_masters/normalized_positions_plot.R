## 6. CREATES NORMALIZED_POSITIONS.PLOT WITH INFORMATION OF X,Y COORDINATES, ACCELERATION, TIME AND LOGRATIO FOR EACH TIME STEP

## Include X.Positions as rows
normalized_positions.plot.X = Data.Plurals %>%
  dplyr::select(Subject,Sentence_Type, Condition, X.Position, Item.number, Quantifier, MaxLogRatio, Truth.value, Response, RT, Accuracy) %>%
  separate(X.Position, into= as.character(c(1:101)), sep = ",") %>%
  gather(Time.Step, X.Position, 4:104) 

## Include Y.Positions as rows
normalized_positions.plot.Y = Data.Plurals %>%
  dplyr::select(Subject,Sentence_Type, Condition, Y.Position, Item.number, Quantifier, MaxLogRatio, Truth.value, Response, RT, Accuracy) %>%
  separate(Y.Position, into= as.character(c(1:101)), sep = ",") %>%
  gather(Time.Step, Y.Position, 4:104) 

## Include Raw.Time as rows
rawtime = Data.Plurals %>%
  dplyr::select(Subject,Sentence_Type, Condition, RawTime, Item.number, Quantifier, MaxLogRatio, Truth.value, Response, RT, Accuracy) %>%
  separate(RawTime, into= as.character(c(1:101)), sep = ",") %>%
  gather(Time.Step, RawTime, 4:104) 

## Include Raw.Acceleration as rows
acceleration_calibration = Data.Plurals %>%
  dplyr::select(Subject,Sentence_Type, Condition, Acceleration, Item.number, Quantifier, MaxLogRatio, Truth.value, Response, RT, Accuracy) %>%
  separate(Acceleration, into= as.character(c(1:101)), sep = ",") %>%
  gather(Time.Step, Acceleration, 4:104) 

## Include LogRatio as rows
log_ratio = Data.Plurals %>%
  dplyr::select(Subject,Sentence_Type, Condition, LogRatio, Item.number, Quantifier, MaxLogRatio, Truth.value, Response, RT, Accuracy) %>%
  separate(LogRatio, into= as.character(c(1:101)), sep = ",") %>%
  gather(Time.Step, LogRatio, 4:104) 

## Put everything together
normalized_positions.plot <- merge(normalized_positions.plot.X,normalized_positions.plot.Y)
normalized_positions.plot <- merge(normalized_positions.plot, rawtime)
normalized_positions.plot <- merge(normalized_positions.plot, acceleration_calibration)
normalized_positions.plot <- merge(normalized_positions.plot, log_ratio)
rm(acceleration_calibration, rawtime, normalized_positions.plot.X, normalized_positions.plot.Y, log_ratio)

## Format data
normalized_positions.plot$X.Position<- as.numeric(normalized_positions.plot$X.Position)
normalized_positions.plot$Y.Position <- as.numeric(normalized_positions.plot$Y.Position)
normalized_positions.plot$Time.Step <- as.numeric(normalized_positions.plot$Time.Step)
normalized_positions.plot$RawTime <- as.numeric(normalized_positions.plot$RawTime)
normalized_positions.plot$Subject <- factor(normalized_positions.plot$Subject)
normalized_positions.plot$Acceleration <- as.numeric(normalized_positions.plot$Acceleration)
normalized_positions.plot$LogRatio <- as.numeric(normalized_positions.plot$LogRatio)

## Take all X.Positions to the positive form
normalized_positions.plot.false = normalized_positions.plot %>%
  filter(Response=='false')%>%
  dplyr::mutate_at('X.Position', funs('-'))
normalized_positions.plot.true = normalized_positions.plot %>%
  filter(Response=='true')
normalized_positions.plot <- rbind(normalized_positions.plot.false, normalized_positions.plot.true)
normalized_positions.plot$Subject <- factor(normalized_positions.plot$Subject)
rm(normalized_positions.plot.false, normalized_positions.plot.true)


write.csv(normalized_positions.plot, file = "Data/Data_Plurals_positions.csv")
Data_Plurals.positions <- normalized_positions.plot 
rm(normalized_positions.plot)