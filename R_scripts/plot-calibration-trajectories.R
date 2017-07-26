## EXPLORATORY PLOTS (probably they won't go into the paper)

## 1. Mean Trajectory (as a function of the total duration of the trial in facets)
normalized_positions.means.subject <- ddply(subset(normalized_positions.plot, Polarity!='uncertain'), c("Polarity", "Time.Step", "RT_cut", "Subject"),
                                            function(normalized_positions.plot)c(X.Position.mean=mean(normalized_positions.plot$X.Position, na.rm=T), 
                                                                                 Y.Position.mean=mean(normalized_positions.plot$Y.Position, na.rm=T)))

normalized_positions.means.traj <- ddply(normalized_positions.means.subject, c("Polarity", "Time.Step", "RT_cut"),
                                         function(normalized_positions.means.subject)c(X.Position.mean=mean(normalized_positions.means.subject$X.Position, na.rm=T), 
                                                                                       X.Position.se=se(normalized_positions.means.subject$X.Position, na.rm=T),
                                                                                       Y.Position.mean=mean(normalized_positions.means.subject$Y.Position, na.rm=T), 
                                                                                       Y.Position.se=se(normalized_positions.means.subject$Y.Position, na.rm=T)))

ggplot(normalized_positions.means.traj, aes(x=X.Position.mean, y=Y.Position.mean, color=Polarity, group=Polarity)) + 
  geom_point(alpha=.6, size=1) + 
  ggtitle('Calibration Mean trajectories (Panels: Total duration of the trial)')+
  theme_minimal() +
  theme(legend.position = "none") +
  facet_grid(.~RT_cut)+
  scale_colour_brewer(palette="Set1") 
ggsave('calibration_mean_trajectory (RT).png', plot = last_plot(), scale = 1, dpi = 300, width = 6, height = 6,  path='R_scripts/graphs/calibration_new')


## 2. Mean X position with point change 
# Black line indicates the onset of color change as a function of trial total duration (facets)

normalized_positions.means.subject <-   ddply(subset(normalized_positions.plot,Polarity!='uncertain'), c("Polarity", "Time.Step.Onset", "RT_cut", "Subject"),
                                              function(normalized_positions.plot)c(X.Position.mean=mean(normalized_positions.plot$X.Position, na.rm=T)))

normalized_positions.means <- ddply(normalized_positions.means.subject, c("Polarity", "Time.Step.Onset", "RT_cut"),
                                    function(normalized_positions.means.subject)c(X.Position.mean=mean(normalized_positions.means.subject$X.Position, na.rm=T), X.Position.se=se(normalized_positions.means.subject$X.Position, na.rm=T)))

ggplot(normalized_positions.means, aes(x=Time.Step.Onset, y=X.Position.mean, color=Polarity, group=Polarity)) + 
  geom_point(alpha=.4) + geom_line(alpha=.4) + theme(legend.position = "none") +
  ggtitle('Calibration Data: Onset at change point, Panels: trial duration')+
  facet_wrap(~RT_cut) + 
  geom_errorbar(aes(ymin=X.Position.mean-X.Position.se, ymax=X.Position.mean+X.Position.se), width=.1, alpha=.4) +
  theme_minimal() + geom_vline(aes(xintercept=0)) +  scale_colour_brewer(palette="Set1")  +
  theme(legend.position = "none") 
ggsave('calibration_mean_XPosition_Time.png', plot = last_plot(), scale = 1, dpi = 300,width = 10, path='R_scripts/graphs/calibration_new')


## 3. Mean X position with point change
# Black line indicates the onset of color change as a function of the time step (trial percentage) where the change occured (facets)

normalized_positions.means.subject <-   ddply(subset(normalized_positions.plot,Polarity!='uncertain'), c("Polarity", "Time.Step.Onset", "PointChange_cut", "Subject"),
                                              function(normalized_positions.plot)c(X.Position.mean=mean(normalized_positions.plot$X.Position, na.rm=T)))

normalized_positions.means <- ddply(normalized_positions.means.subject, c("Polarity", "Time.Step.Onset", "PointChange_cut"),
                                    function(normalized_positions.means.subject)c(X.Position.mean=mean(normalized_positions.means.subject$X.Position, na.rm=T), X.Position.se=se(normalized_positions.means.subject$X.Position, na.rm=T)))

ggplot(normalized_positions.means, aes(x=Time.Step.Onset, y=X.Position.mean, color=Polarity, group=Polarity)) + 
  geom_point(alpha=.4) + geom_line(alpha=.4) + theme(legend.position = "none") +
  ggtitle('Calibration Data: Onset at change point, Panels: y coordinate of change')+
  facet_wrap(~PointChange_cut) + 
  geom_errorbar(aes(ymin=X.Position.mean-X.Position.se, ymax=X.Position.mean+X.Position.se), width=.1, alpha=.4) +
  theme_minimal() + geom_vline(aes(xintercept=0)) +  scale_colour_brewer(palette="Set1")  +
  theme(legend.position = "none") 
ggsave('calibration_mean_XPosition_ChangePoint.png', plot = last_plot(), scale = 1, dpi = 300,width = 10, path='R_scripts/graphs/calibration_new')

normalized_positions.means.subject <-   ddply(subset(normalized_positions.plot,Polarity!='uncertain'), c("Polarity", "Time.Step.Onset", "PointChange.Time_cut", "Subject"),
                                              function(normalized_positions.plot)c(X.Position.mean=mean(normalized_positions.plot$X.Position, na.rm=T)))

normalized_positions.means <- ddply(normalized_positions.means.subject, c("Polarity", "Time.Step.Onset", "PointChange.Time_cut"),
                                    function(normalized_positions.means.subject)c(X.Position.mean=mean(normalized_positions.means.subject$X.Position, na.rm=T), X.Position.se=se(normalized_positions.means.subject$X.Position, na.rm=T)))

ggplot(normalized_positions.means, aes(x=Time.Step.Onset, y=X.Position.mean, color=Polarity, group=Polarity)) + 
  geom_point(alpha=.4) + geom_line(alpha=.4) + theme(legend.position = "none") +
  ggtitle('Calibration Data: Onset at change point, Panels: Time step of change')+
  facet_wrap(~PointChange.Time_cut) + 
  geom_errorbar(aes(ymin=X.Position.mean-X.Position.se, ymax=X.Position.mean+X.Position.se), width=.1, alpha=.4) +
  theme_minimal() + geom_vline(aes(xintercept=0)) +  scale_colour_brewer(palette="Set1")  +
  theme(legend.position = "none")

ggsave('calibration_mean_XPosition_ChangePoint2.png', plot = last_plot(), scale = 1, dpi = 300,width = 10, height=6, path='R_scripts/graphs/calibration_new')



## 4. Mean Trajectory with change point 
# Actual mean trajectories as a function of the y coordinate when the color changed  (y facet) and the range of time when the color changed (x facet)

normalized_positions.means.subject <- ddply(subset(normalized_positions.plot,Polarity!='uncertain'), c("Polarity", "Time.Step", "PointChange_cut", "Subject", "PointChange.Time.Raw_cut"),
                                            function(normalized_positions.plot)c(X.Position.mean=mean(normalized_positions.plot$X.Position, na.rm=T), 
                                                                                 Y.Position.mean=mean(normalized_positions.plot$Y.Position, na.rm=T)))

normalized_positions.means.traj <- ddply(normalized_positions.means.subject, c("Polarity", "Time.Step", "PointChange_cut", "PointChange.Time.Raw_cut"),
                                         function(normalized_positions.means.subject)c(X.Position.mean=mean(normalized_positions.means.subject$X.Position, na.rm=T), 
                                                                                       X.Position.se=se(normalized_positions.means.subject$X.Position, na.rm=T),
                                                                                       Y.Position.mean=mean(normalized_positions.means.subject$Y.Position, na.rm=T), 
                                                                                       Y.Position.se=se(normalized_positions.means.subject$Y.Position, na.rm=T)))

ggplot(normalized_positions.means.traj, aes(x=X.Position.mean, y=Y.Position.mean, color=Polarity, group=Polarity)) + 
  geom_point(alpha=.6, size=1) + 
  ggtitle('Calibration Mean trajectories')+
  geom_errorbarh(aes(xmin=X.Position.mean-X.Position.se, xmax=X.Position.mean+X.Position.se)) + 
  theme_minimal() +
  theme(legend.position = "none") +
  facet_grid(PointChange_cut~PointChange.Time.Raw_cut) 
ggsave('calibration_mean_trajectory_ChangePoint.png', plot = last_plot(), scale = 1, dpi = 300, width = 6, height = 6,  path='R_scripts/graphs/calibration_new')




# Plotting real subjects
##Plotting trajectories (LDA)
ggplot(normalized_positions.plot, aes(x=X.Position, y=Y.Position, color=Polarity, group=grp)) +
  geom_point(alpha=.4, size=1) + 
  ggtitle('Calibration Trajectories per subject (LDA)')+
  theme_minimal()+
  theme(legend.position = "none") + 
  facet_grid(Polarity~lda_measure_cut) 
#ggsave('calibration_all_trajectories_lda.png', plot = last_plot(), scale = 1, dpi = 300, height=6, width = 10,  path='R_scripts/graphs/calibration_new')

##Plotting xposition agains time step (LDA)
ggplot(normalized_positions.plot, aes(x=Time.Step, y=X.Position, color=Polarity, group=grp)) + 
  geom_point(alpha=.4, size=1) + geom_line()+ theme_minimal()+ theme(legend.position = "none") + 
  ggtitle('Calibration Trajectories per subject (LDA)')+
  facet_grid(Polarity~lda_measure_cut) 
#ggsave('calibration_all_time_lda.png', plot = last_plot(), scale = 1, dpi = 300,  height=6, width = 10, path='R_scripts/graphs/calibration_new')

##Plotting xposition agains time step (RT)
ggplot(normalized_positions.plot, aes(x=Time.Step, y=X.Position, color=Polarity, group=grp)) + 
  geom_point(alpha=.4, size=1) + geom_line()+ theme(legend.position = "none") + 
  ggtitle('Calibration Trajectories per subject (RT)')+
  facet_grid(Polarity~RT_cut) 
#ggsave('calibration_all_time_RT.png', plot = last_plot(), scale = 1, dpi = 300,  height=6, width = 10, path='R_scripts/graphs/calibration_new')


##Plotting trajectories (Ratio)
ggplot(normalized_positions.plot, aes(x=Time.Step, y=X.Position, color=Polarity, group=grp)) + 
  geom_point(alpha=.4, size=1) + geom_line()+ theme(legend.position = "none") + 
  ggtitle('Calibration Time per subject (MaxLogRatio)')+
  facet_grid(Polarity~MaxLogRatio_cut) 
#ggsave('calibration_time_ratio.png', plot = last_plot(), scale = 1,  height=6, width = 10, dpi = 300, path='R_scripts/graphs/calibration_new')


ggplot(normalized_positions.plot, aes(x=X.Position, y=Y.Position, color=Polarity, group=grp)) + 
  geom_point(alpha=.4, size=1) + theme(legend.position = "none") + 
  ggtitle('Calibration Trajectories per subject (MaxLogRatio)')+
  facet_grid(Polarity~MaxLogRatio_cut) 
#ggsave('calibration_trajectories_ratio.png', plot = last_plot(), scale = 1,  height=6, width = 10, dpi = 300, path='R_scripts/graphs/calibration_new')
