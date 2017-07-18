library(MASS) # NB: this will mask dplyr::select

## Subset to deviated and straight trials
calibration_data = subset(calibration_data, Polarity != 'uncertain')
calibration_data$Subject <- factor(calibration_data$Subject)
normalized_positions.plot$Subject <- factor(normalized_positions.plot$Subject)

## OVERALL PERFORMANCE (Plotting)
### Mean X position
normalized_positions.means.subject <-   ddply(normalized_positions.plot, c("Polarity", "Time.Step.Onset", "Subject"),
                                              function(normalized_positions.plot)c(X.Position.mean=mean(normalized_positions.plot$X.Position, na.rm=T)))

normalized_positions.means <- ddply(normalized_positions.means.subject, c("Polarity", "Time.Step.Onset"),
                                    function(normalized_positions.means.subject)c(X.Position.mean=mean(normalized_positions.means.subject$X.Position, na.rm=T), X.Position.se=se(normalized_positions.means.subject$X.Position, na.rm=T)))

ggplot(normalized_positions.means.subject, aes(x=Time.Step.Onset, y=X.Position.mean, color=Subject, group=Subject)) + 
  geom_point(size=0.5) + geom_line() +
  expand_limits(x=c(-1.5,1.5)) + theme_minimal()+geom_vline(aes(xintercept=0))+
  theme(legend.position = "none") + facet_grid(Polarity~.) 
ggsave('calibration_mean_subject_XPosition.png', plot = last_plot(), scale = 1, dpi = 300,width = 10, path='R_scripts/graphs/calibration_new')

ggplot(subset(normalized_positions.means, Polarity!='uncertain'), aes(x=Time.Step.Onset, y=X.Position.mean, color=Polarity, group=Polarity)) + 
  geom_point(alpha=.6) + geom_line(alpha=.6) + theme_minimal()+ theme(legend.position = "none") +
  ggtitle('Calibration Mean X-Position Onset at Change point') + geom_vline(aes(xintercept=0)) +  scale_colour_brewer(palette="Set1") +
  geom_errorbar(aes(ymin=X.Position.mean-X.Position.se, ymax=X.Position.mean+X.Position.se), width=.1, alpha=.4) 

ggsave('calibration_mean_XPosition.png', plot = last_plot(), scale = 1, dpi = 300,width = 10, path='R_scripts/graphs/calibration_new')


## Mean Trajectory
normalized_positions.means.subject <- ddply(subset(normalized_positions.plot, Polarity!='uncertain'), c("Polarity", "Time.Step", "Subject"),
                                            function(normalized_positions.plot)c(X.Position.mean=mean(normalized_positions.plot$X.Position, na.rm=T), 
                                                                                 Y.Position.mean=mean(normalized_positions.plot$Y.Position, na.rm=T)))

normalized_positions.means.traj <- ddply(normalized_positions.means.subject, c("Polarity", "Time.Step"),
                                         function(normalized_positions.means.subject)c(X.Position.mean=mean(normalized_positions.means.subject$X.Position, na.rm=T), 
                                                                                       X.Position.se=se(normalized_positions.means.subject$X.Position, na.rm=T),
                                                                                       Y.Position.mean=mean(normalized_positions.means.subject$Y.Position, na.rm=T), 
                                                                                       Y.Position.se=se(normalized_positions.means.subject$Y.Position, na.rm=T)))

ggplot(normalized_positions.means.subject, aes(x=X.Position.mean, y=Y.Position.mean, color=Subject, group=Subject)) +
  geom_point(size=0.5) + 
  ggtitle('Mean Trajectories per subject') +
  theme_minimal() +
  theme(legend.position = "top") + 
  expand_limits(x=c(-1.5,1.5)) + 
  facet_grid(Polarity~.)
ggsave('calibration_mean_subject_trajectory.png', plot = last_plot(), scale = 1, dpi = 300, width = 6, height = 6, path='R_scripts/graphs/calibration_new')


ggplot(normalized_positions.means.traj, aes(x=X.Position.mean, y=Y.Position.mean, color=Polarity, group=Polarity)) + 
  geom_point(alpha=.6, size=1) + 
  ggtitle('Calibration Mean trajectories')+
  geom_errorbarh(aes(xmin=X.Position.mean-X.Position.se, xmax=X.Position.mean+X.Position.se)) + 
  theme_minimal() +
  theme(legend.position = "none") +
  scale_colour_brewer(palette="Set1") 
ggsave('calibration_mean_trajectory.png', plot = last_plot(), scale = 1, dpi = 300, width = 6, height = 6,  path='R_scripts/graphs/calibration_new')











#LDA coords + delta + deltadelta based on coordinates
LDA_training.coord.delta.deltadelta(calibration_data)
save(m_pca, v_lda, b_lda, n_pca, all_data_columns, file="LDA(Coords+Delta+DeltaDelta).RData")
calibration_data <- dplyr::full_join(lda_measure.df, calibration_data, by=c("Subject", "Item.number", "Expected_response"))
normalized_positions.plot <- dplyr::full_join(lda_measure.df, normalized_positions.plot, by=c("Subject", "Item.number", "Expected_response"))
