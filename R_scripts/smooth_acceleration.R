
# SMOOTH ACCELERATION (data frame)
## Creates data frame with smooth acceleration based on Dale and Duran (2011) method: avarage 7 y,x positions over a moving window.

smooth_acceleration = calibration_data %>%
  dplyr::select(Subject,Polarity, Expected_response, Acceleration_Smooth, Acceleration_Smooth.Time, Item.number, grp, MaxLogRatio, MaxLogRatio_cut, MaxRatio.Time.Norm, PointChange_cut, PointChange.Time_cut,PointChange.Time.Raw_cut, PointChange.Time.Raw, RT_cut, PointChange.Time) %>%
  separate(Acceleration_Smooth, into= as.character(c(1:94)), sep = ",") %>%
  gather(Time.Step, Acceleration_Smooth, 4:97) 
smooth_acceleration$Acceleration_Smooth <- as.numeric(smooth_acceleration$Acceleration_Smooth)
smooth_acceleration$Time.Step <- as.numeric(smooth_acceleration$Time.Step)
smooth_acceleration$Time.Step.Onset <- smooth_acceleration$Time.Step - smooth_acceleration$PointChange.Time


## Plotting
### NB: For plotting, I am taking a window of [-20,+50] Time steps
smooth_acceleration.subset <- subset(smooth_acceleration, Time.Step.Onset > -20 & Time.Step.Onset < 50)

acceleration_mean.subject <-   ddply(subset(smooth_acceleration.subset, Polarity!='uncertain'), c("Polarity", "Time.Step.Onset", "Subject"),
                                     function(smooth_acceleration.subset)c(response=mean(smooth_acceleration.subset$Acceleration_Smooth, na.rm=T)))
acceleration_mean.overall <-   ddply(acceleration_mean.subject, c("Polarity", "Time.Step.Onset"),
                                     function(acceleration_mean.subject)c(response=mean(acceleration_mean.subject$response, na.rm=T), se= se(acceleration_mean.subject$response, na.rm=T)))



ggplot(acceleration_mean.subject, aes(x=Time.Step.Onset, y=response, color=Subject, group=Subject)) +
  geom_line(alpha=.4) +
  geom_point(alpha=.4)+
   ggtitle('Acceleration subject: Smooth')+
  theme_minimal() +
  theme(legend.position = "top") + geom_vline(aes(xintercept=0)) + facet_grid(.~Polarity)

ggplot(acceleration_mean.overall, aes(x=Time.Step.Onset, y=response, color=Polarity)) +
  geom_line(alpha=.4) +
  geom_point(alpha=.4)+
  geom_errorbar(aes(ymin=response-se, ymax=response+se), width=.1, alpha=.2, position=position_dodge(0.05)) +
  ggtitle('Acceleration mean: Smooth')+
  theme_minimal() +
  theme(legend.position = "none") + geom_vline(aes(xintercept=0)) +  scale_colour_brewer(palette="Set1") 
ggsave('acceleration_overall_means.png', plot = last_plot(), scale = 1, dpi = 300, width = 10, path='R_scripts/graphs/calibration_new')



#complex plots
acceleration_mean.subject <-   ddply(subset(smooth_acceleration.subset, Polarity!='uncertain'), c("Polarity", "Time.Step.Onset", "Subject", "PointChange.Time_cut"),
                                     function(smooth_acceleration.subset)c(response=mean(smooth_acceleration.subset$Acceleration_Smooth, na.rm=T)))

acceleration_mean.overall <-   ddply(acceleration_mean.subject, c("Polarity", "Time.Step.Onset", "PointChange.Time_cut"),
                                     function(acceleration_mean.subject)c(response=mean(acceleration_mean.subject$response, na.rm=T), se= se(acceleration_mean.subject$response, na.rm=T)))

ggplot(acceleration_mean.overall, aes(x=Time.Step.Onset, y=response, color=Polarity)) +
  geom_line() + 
  geom_errorbar(aes(ymin=response-se, ymax=response+se), width=.1, alpha=.5, position=position_dodge(0.05)) +
  ggtitle('Acceleration mean')+
  theme_minimal() +
  theme(legend.position = "none") +
  facet_grid(.~PointChange.Time_cut) + geom_vline(aes(xintercept=0)) +  scale_colour_brewer(palette="Set1") 

ggsave('acceleration_means_overall_new_TIME.png', plot = last_plot(), scale = 1, dpi = 300, width = 10, path='R_scripts/graphs/calibration_new')



