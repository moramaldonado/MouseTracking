
##Plotting acceleration

# p1 <- ggplot(subset(acceleration_calibration, Subject==2 & Item.number ==20), aes(x=Time.Step, y=Smooth_Acceleration, color=Polarity, group=grp)) +
#   geom_line(alpha=.5) +
#   geom_point()+
#   ggtitle('')+
#   theme_minimal() +
#   theme(legend.position = "none") 
# 
# p1bis <- ggplot(subset(acceleration_calibration, Subject==2 & Item.number ==20), aes(x=Time.Step, y=Acceleration, color=Polarity, group=grp)) +
#   geom_line(alpha=.5) +
#   geom_point()+
#   ggtitle('')+
#   theme_minimal() +
#   theme(legend.position = "none") 
# 
# p2 <- ggplot(subset(normalized_positions.plot, Subject==2 & Item.number ==20), aes(x=Time.Step, y=X.Position, color=Polarity, group=grp)) +
#   geom_line(alpha=.5) +
#   geom_point(alpha=.5) +
#   ggtitle('')+
#   theme_minimal() +
#   theme(legend.position = "none") 
# 
# p4 <- ggplot(subset(normalized_positions.plot, Subject==2 & Item.number ==20), aes(x=X.Position, y=Y.Position, color=Polarity, group=Time.Step)) +
#   geom_point(alpha=.5) +
#   ggtitle('')+
#   theme_minimal() +
#   theme(legend.position = "none") 
# 
# p3 <- ggplot(subset(acceleration_calibration, Subject==2 & Item.number ==20), aes(x=Time.Step, y=Filtered_Acceleration, color=Polarity, group=grp)) +
#   geom_line(alpha=.5) +
#   geom_point()+
#   ggtitle('')+
#   theme_minimal() +
#   theme(legend.position = "none") 
# 
# multiplot(p1bis, p4, p1, p2, p3, cols=3)
# #ggsave('acceleration_per_subject.png', plot = last_plot(), scale = 1, dpi = 300, width = 10, height= 6, path='R_scripts/graphs/calibration_new')

#simple plots

acceleration_mean.subject <-   ddply(subset(normalized_positions.plot, Polarity!='uncertain'), c("Polarity", "Time.Step.Onset", "Subject"),
                                     function(normalized_positions.plot)c(response=mean(normalized_positions.plot$Smooth_Acceleration, na.rm=T)))

ggplot(acceleration_mean.subject, aes(x=Time.Step.Onset, y=response, color=Subject)) +
  geom_line(alpha=.5) + 
  ggtitle('Acceleration subject mean - Filtered ')+
  theme_minimal() +
  theme(legend.position = "none") + 
  facet_wrap(~Polarity) + geom_vline(aes(xintercept=0)) 
ggsave('acceleration_subject_means.png', plot = last_plot(), scale = 1, dpi = 300, width = 10, path='R_scripts/graphs/calibration_new')




acceleration_mean.overall <-   ddply(acceleration_mean.subject, c("Polarity", "Time.Step.Onset"),
                                     function(acceleration_mean.subject)c(response=mean(acceleration_mean.subject$response, na.rm=T), se= se(acceleration_mean.subject$response, na.rm=T)))


ggplot(acceleration_mean.overall, aes(x=Time.Step.Onset, y=response, color=Polarity)) +
  geom_line(alpha=.4) +
  geom_point(alpha=.4)+
  geom_errorbar(aes(ymin=response-se, ymax=response+se), width=.1, alpha=.2, position=position_dodge(0.05)) +
  ggtitle('Acceleration mean: Smooth')+
  theme_minimal() +
  theme(legend.position = "none") + geom_vline(aes(xintercept=0)) +  scale_colour_brewer(palette="Set1") 
ggsave('acceleration_overall_means.png', plot = last_plot(), scale = 1, dpi = 300, width = 10, path='R_scripts/graphs/calibration_new')



#complex plots

acceleration_mean.subject <-   ddply(subset(normalized_positions.plot, Polarity!='uncertain'), c("Polarity", "Time.Step.Onset", "Subject", "PointChange.Time_cut"),
                                     function(normalized_positions.plot)c(response=mean(normalized_positions.plot$Smooth_Acceleration, na.rm=T)))

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



