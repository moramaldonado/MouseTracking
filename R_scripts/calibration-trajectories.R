##Organizing the data a bit
calibration_data <- subset(calibration_data, Accuracy==TRUE)
calibration_data$MaxLogRatio_cut <- cut(calibration_data$MaxLogRatio, 5)
calibration_data$PointChange.Time_cut <- cut(calibration_data$PointChange.Time, breaks=c(0, 25, 50, 75, 100), include.lowest=TRUE)
calibration_data$PointChange.Time.Raw_cut <- cut(calibration_data$PointChange.Time.Raw, breaks=c(0, 300, 500, 800, 1100, 1700), dig.lab = 5, include.lowest=TRUE)
calibration_data$PointChange_cut <- cut(calibration_data$PointChange, breaks=c(0, 0.25, 0.5, 0.75, 1), dig.lab = 5 , include.lowest=TRUE)
calibration_data$RT_cut <- cut(calibration_data$RT, breaks=c(0, 500, 1000, 1500, 2000, 3500), dig.lab = 5, include.lowest=TRUE)
levels(calibration_data$Expected_response) <- c('blue','red')

##First hand plots
normalized_positions.plot.X = calibration_data %>%
  dplyr::select(Subject,Polarity, Expected_response, Normalized.positions.X, Item.number, grp, MaxLogRatio, MaxLogRatio_cut, MaxRatio.Time.Norm, PointChange_cut, PointChange.Time_cut,PointChange.Time.Raw_cut, PointChange.Time.Raw, RT_cut, PointChange.Time) %>%
  separate(Normalized.positions.X, into= as.character(c(1:101)), sep = ",") %>%
  gather(Time.Step, X.Position, 4:104) 

normalized_positions.plot.Y = calibration_data %>%
  dplyr::select(Subject,Polarity, Expected_response, Normalized.positions.Y,Item.number, grp, MaxLogRatio, MaxLogRatio_cut, MaxRatio.Time.Norm, PointChange_cut, PointChange.Time_cut,PointChange.Time.Raw_cut, PointChange.Time.Raw, RT_cut, PointChange.Time)%>%
  separate(Normalized.positions.Y, into= as.character(c(1:101)), sep = ",") %>%
  gather(Time.Step, Y.Position, 4:104) 

rawtime = calibration_data %>% 
  dplyr::select(Subject,Polarity, Expected_response, RawTime, Item.number, grp, MaxLogRatio, MaxLogRatio_cut, MaxRatio.Time.Norm, PointChange_cut, PointChange.Time_cut,PointChange.Time.Raw_cut, PointChange.Time.Raw, RT_cut, PointChange.Time)%>%
  separate(RawTime, into= as.character(c(1:101)), sep = ",") %>%
  gather(Time.Step, RawTime, 4:104) 

acceleration_calibration = calibration_data %>%
  dplyr::select(Subject,Polarity, Expected_response, Acceleration, Item.number, grp, MaxLogRatio, MaxLogRatio_cut, MaxRatio.Time.Norm, PointChange_cut, PointChange.Time_cut,PointChange.Time.Raw_cut, PointChange.Time.Raw, RT_cut, PointChange.Time) %>%
  separate(Acceleration, into= as.character(c(1:101)), sep = ",") %>%
  gather(Time.Step, Acceleration, 4:104) 


normalized_positions.plot <- merge(normalized_positions.plot.X,normalized_positions.plot.Y)
normalized_positions.plot <- merge(normalized_positions.plot, rawtime)
normalized_positions.plot <- merge(normalized_positions.plot, acceleration_calibration)

normalized_positions.plot$X.Position<- as.numeric(normalized_positions.plot$X.Position)
normalized_positions.plot$Y.Position <- as.numeric(normalized_positions.plot$Y.Position)
normalized_positions.plot$Time.Step <- as.numeric(normalized_positions.plot$Time.Step)
normalized_positions.plot$PointChange.Time <- as.numeric(normalized_positions.plot$PointChange.Time)
normalized_positions.plot$RawTime <- as.numeric(normalized_positions.plot$RawTime)
normalized_positions.plot$Subject <- factor(normalized_positions.plot$Subject)
normalized_positions.plot$Polarity <- factor(normalized_positions.plot$Polarity )
normalized_positions.plot$Acceleration <- as.numeric(normalized_positions.plot$Acceleration)

normalized_positions.plot.false = normalized_positions.plot %>%
  filter(Expected_response=='blue')%>%
  dplyr::mutate_at('X.Position', funs('-'))
normalized_positions.plot.true = normalized_positions.plot %>%
  filter(Expected_response=='red')
normalized_positions.plot <- rbind(normalized_positions.plot.false, normalized_positions.plot.true)
normalized_positions.plot$Subject <- factor(normalized_positions.plot$Subject)
rm(normalized_positions.plot.false, normalized_positions.plot.true, normalized_positions.plot.X, normalized_positions.plot.Y)

normalized_positions.plot$RawTime.Onset <- normalized_positions.plot$RawTime - normalized_positions.plot$PointChange.Time.Raw
normalized_positions.plot$Time.Step.Onset <- normalized_positions.plot$Time.Step - normalized_positions.plot$PointChange.Time


##Subset normalized positions.plot to the -25/+25 time steps around the decision point 




# #Plot per subject
# for (s in levels(normalized_positions.plot$Subject)) {
#   print(s)
#   ggplot(subset(normalized_positions.plot,Subject==s), aes(x=X.Position, y=Y.Position, color=Polarity, group=grp, label=Time.Step)) +
#     geom_point(alpha=.2, size=.5) +
#     ggtitle('Calibration Trajectories per subject')+
#     theme(legend.position = "none") +
#     geom_text(aes(label=as.character(Time.Step)),hjust=0.2,vjust=0.3, size=1.5) +
#     expand_limits(x=c(-1.5,1.5), y=c(-0.5, 1.7)) +
#     facet_grid(.~Polarity)
#   name <- paste(s, '.png')
#   ggsave(name, plot = last_plot(), width=12, height=4, path='R_scripts/graphs/calibration_new/calibration_per_subject')
# }

#I am deciding by eye which trajectories to exclude (based on the grp)
#excluded_trajectories <- c('3 25', '3 27', '6 27', '6 30', '7 25', '10 25', '16 25', '23 25', '25 36', '31 25','31 28','17 33','31 26', '34 25', '48 27', '49 25')
#calibration_data <- subset(calibration_data, !(grp  %in% excluded_trajectories))
#normalized_positions.plot <- subset(normalized_positions.plot, !(grp  %in% excluded_trajectories))

