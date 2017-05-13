#Plotting trajectories
normalized_positions.plot$MaxLogRatio_cut <- cut(normalized_positions.plot$MaxLogRatio, 7)
normalized_positions.plot$lda_measure_cut <- cut(normalized_positions.plot$lda_measure, 7)

#Plotting real subjects
ggplot(normalized_positions.plot, aes(x=X.Position, y=Y.Position, color=Polarity, group=grp)) +
  geom_point(alpha=.4, size=1) + 
  ggtitle('Calibration Trajectories per subject')+
  theme(legend.position = "none") + 
  facet_grid(lda_measure_cut~Expected_response) 
ggsave('calibration_XPosition.png', plot = last_plot(), scale = 1, dpi = 300, height=6,  path='R_scripts/graphs/calibration')

ggplot(normalized_positions.plot, aes(x=Time.Step, y=X.Position, color=Polarity, group=grp)) + 
  geom_point(alpha=.4, size=1) + geom_line()+ theme(legend.position = "none") + 
  ggtitle('Calibration Trajectories per subject (LDA)')+
  facet_grid(lda_measure_cut~Expected_response) 
ggsave('calibration_trajectories.png', plot = last_plot(), scale = 1, dpi = 300,  height=6, path='R_scripts/graphs/calibration')


ggplot(normalized_positions.plot, aes(x=Time.Step, y=X.Position, color=Polarity, group=grp)) + 
  geom_point(alpha=.4, size=1) + geom_line()+ theme(legend.position = "none") + 
  ggtitle('Calibration Trajectories per subject (MaxLogRatio)')+
  facet_grid(MaxLogRatio_cut~Expected_response) 
ggsave('ratio_calibration_trajectories.png', plot = last_plot(), scale = 1,  height=6, dpi = 300, path='R_scripts/graphs/calibration')


#Plotting real means
normalized_positions.means <- ddply(normalized_positions.plot, c("Polarity", "Time.Step", "Expected_response"),
                                    function(normalized_positions.means)c(X.Position.mean=mean(normalized_positions.means$X.Position, na.rm=T), X.Position.se=se(normalized_positions.means$X.Position, na.rm=T)))
ggplot(normalized_positions.means, aes(x=Time.Step, y=X.Position.mean, color=Polarity, group=Polarity)) + 
  geom_point(alpha=.6) + geom_line() + theme(legend.position = "none") +
  ggtitle('Calibration Mean X-Position x Time Step')+
  facet_grid(.~Expected_response) + 
  geom_errorbar(aes(ymin=X.Position.mean-X.Position.se, ymax=X.Position.mean+X.Position.se), width=.1) 

ggsave('calibration_mean_XPosition.png', plot = last_plot(), scale = 1, dpi = 300, path='R_scripts/graphs/calibration')



normalized_positions.means.traj <- ddply(normalized_positions.plot, c("Polarity", "Time.Step", "Expected_response"),
                                    function(normalized_positions.means)c(X.Position.mean=mean(normalized_positions.means$X.Position, na.rm=T), 
                                                                          X.Position.se=se(normalized_positions.means$X.Position, na.rm=T),
                                                                          Y.Position.mean=mean(normalized_positions.means$Y.Position, na.rm=T), 
                                                                          Y.Position.se=se(normalized_positions.means$Y.Position, na.rm=T)))

ggplot(normalized_positions.means.traj, aes(x=X.Position.mean, y=Y.Position.mean, color=Polarity, group=Polarity)) + 
  geom_point(alpha=.6) + 
  ggtitle('Calibration Mean trajectories')+
  theme(legend.position = "none") +
  facet_grid(.~Expected_response) 
ggsave('calibration_mean_trajectory.png', plot = last_plot(), scale = 1, dpi = 300, path='R_scripts/graphs/calibration')
