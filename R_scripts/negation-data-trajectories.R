#Plotting trajectories
normalized_positions.plot.X = negation_data %>%
  dplyr::select(Subject,Polarity, Expected_response, Normalized.positions.X, Item.number, lda_measure, Sentence) %>%
  separate(Normalized.positions.X, into= as.character(c(1:101)), sep = ",") %>%
  gather(Time.Step, X.Position, 4:104) 
normalized_positions.plot.Y = negation_data %>%
  dplyr::select(Subject,Polarity, Expected_response, Normalized.positions.Y,  Item.number, lda_measure, Sentence) %>%
  separate(Normalized.positions.Y, into= as.character(c(1:101)), sep = ",") %>%
  gather(Time.Step, Y.Position, 4:104) 
normalized_positions.plot <- merge(normalized_positions.plot.X,normalized_positions.plot.Y)

normalized_positions.plot$X.Position<- as.numeric(normalized_positions.plot$X.Position)
normalized_positions.plot$Y.Position <- as.numeric(normalized_positions.plot$Y.Position)
normalized_positions.plot$Time.Step <- as.numeric(normalized_positions.plot$Time.Step)
normalized_positions.plot$Subject <- factor(normalized_positions.plot$Subject)
normalized_positions.plot$Polarity <- factor(normalized_positions.plot$Polarity )
normalized_positions.plot$grp <- paste(normalized_positions.plot$Subject,normalized_positions.plot$Item.number)
normalized_positions.plot$lda_measure_cut <- cut(normalized_positions.plot$lda_measure, 5)

# Taking negative values for all FALSE items (so everything goes to 1 in the X position)
normalized_positions.plot.false = normalized_positions.plot %>%
  filter(Expected_response=='false')%>%
  dplyr::mutate_at('X.Position', funs('-'))
normalized_positions.plot.true = normalized_positions.plot %>%
  filter(Expected_response=='true')
normalized_positions.plot <- rbind(normalized_positions.plot.false, normalized_positions.plot.true)

#Plot each trajectory
ggplot(normalized_positions.plot, aes(x=X.Position, y=Y.Position, color=Polarity, group=grp)) + 
  geom_point(alpha=.4, size=1) + 
  theme(legend.position = "none") + 
  ggtitle('Negation data: All Trajectories') +
  facet_grid(lda_measure_cut~Expected_response) 
ggsave('negation-data-trajectory.png', plot = last_plot(), scale = 1, dpi = 300, path='R_scripts/graphs/negation_data')

#Plot each item as a function of X position 
ggplot(normalized_positions.plot, aes(x=Time.Step, y=X.Position, color=Polarity, group=grp)) +
  geom_point(alpha=.4, size=1) + geom_line()+ theme(legend.position = "none") + 
  ggtitle('Negation data: Xposition x Time step per subject') +
  expand_limits(x=c(-1.5,1.5)) + 
  facet_grid(lda_measure_cut~Expected_response) 
ggsave('negation-data-Xposition.png', plot = last_plot(), scale = 1, dpi = 300, path='R_scripts/graphs/negation_data')
#ggsave('negation-data-Xposition(withOLD).png', plot = last_plot(), scale = 1, dpi = 300, path='R_scripts/graphs')

#Take mean XPositions per subject and plot
normalized_positions.means.subject <-   ddply(normalized_positions.plot, c("Polarity", "Time.Step", "Expected_response", "Subject"),
                                              function(normalized_positions.plot)c(X.Position.mean=mean(normalized_positions.plot$X.Position, na.rm=T)))


ggplot(normalized_positions.means.subject, aes(x=Time.Step, y=X.Position.mean, color=Subject, group=Subject)) + 
  geom_point(alpha=.6) + geom_line() +
  expand_limits(x=c(-1.5,1.5)) + 
  theme(legend.position = "none") + facet_grid(Polarity~Expected_response) 
ggsave('negation-data-Xposition-subject.png', plot = last_plot(), scale = 1, dpi = 300, path='R_scripts/graphs/negation_data')

#Take mean of subject means
normalized_positions.means.overall <- ddply(normalized_positions.means.subject, c("Polarity", "Time.Step", "Expected_response"),
                                    function(normalized_positions.means.subject)c(X.Position.mean=mean(normalized_positions.means.subject$X.Position.mean, na.rm=T),
                                                                                 X.Position.se=se(normalized_positions.means.subject$X.Position, na.rm=T)))

ggplot(normalized_positions.means.overall, aes(x=Time.Step, y=X.Position.mean, color=Polarity, group=Polarity)) + 
  geom_point(alpha=.6) + geom_line() +
  ggtitle('Negation data: Mean Xposition x Time step') +
  expand_limits(x=c(-1.5,1.5)) + 
  theme(legend.position = "none") + facet_grid(.~Expected_response) + 
  geom_errorbar(aes(ymin=X.Position.mean-X.Position.se, ymax=X.Position.mean+X.Position.se), width=.1) 
ggsave('negation-data-mean-Xposition.png', plot = last_plot(), scale = 1, dpi = 300, path='R_scripts/graphs/negation_data')



#Take mean trajectories per subject
normalized_positions.means.subject <- ddply(normalized_positions.plot, c("Polarity", "Time.Step", "Expected_response", "Subject"),
                                         function(normalized_positions.plot)c(X.Position.mean=mean(normalized_positions.plot$X.Position, na.rm=T), 
                                                                               Y.Position.mean=mean(normalized_positions.plot$Y.Position, na.rm=T)))

ggplot(normalized_positions.means.subject, aes(x=X.Position.mean, y=Y.Position.mean, color=Subject, group=Subject)) +
  geom_point(alpha=.6) + 
  ggtitle('Negation data: Mean Trajectories') +
  theme(legend.position = "none") + 
  expand_limits(x=c(-1.5,1.5)) + 
  facet_grid(Polarity~Expected_response) 
ggsave('negation-data-subject-trajectory.png', plot = last_plot(), scale = 1, dpi = 300, path='R_scripts/graphs/negation_data')


normalized_positions.means.traj <- ddply(normalized_positions.means.subject, c("Polarity", "Time.Step", "Expected_response"),
                                         function(normalized_positions.means.subject)c(X.Position.mean=mean(normalized_positions.means.subject$X.Position, na.rm=T), 
                                                                               X.Position.se=se(normalized_positions.means.subject$X.Position, na.rm=T),
                                                                               Y.Position.mean=mean(normalized_positions.means.subject$Y.Position, na.rm=T), 
                                                                             Y.Position.se=se(normalized_positions.means.subject$Y.Position, na.rm=T)))

ggplot(normalized_positions.means.traj, aes(x=X.Position.mean, y=Y.Position.mean, color=Polarity, group=Polarity)) +
  geom_point(alpha=.6) + 
  ggtitle('Negation data: Mean Trajectories') +
  geom_errorbarh(aes(xmin=X.Position.mean-X.Position.se, xmax=X.Position.mean+X.Position.se), alpha=.8) +
  theme(legend.position = "none") + 
  expand_limits(x=c(-1.5,1.5)) + 
  facet_grid(.~Expected_response) 
ggsave('negation-data-mean-trajectory.png', plot = last_plot(), scale = 1, dpi = 300, path='R_scripts/graphs/negation_data')



#Just for fun- See trajectories per item

normalized_positions.means.items <- ddply(normalized_positions.plot, c("Polarity", "Time.Step", "Sentence"),
                                         function(normalized_positions.plot)c(X.Position.mean=mean(normalized_positions.plot$X.Position, na.rm=T), 
                                                                                       X.Position.se=se(normalized_positions.plot$X.Position, na.rm=T),
                                                                                       Y.Position.mean=mean(normalized_positions.plot$Y.Position, na.rm=T), 
                                                                                       Y.Position.se=se(normalized_positions.plot$Y.Position, na.rm=T)))

ggplot(normalized_positions.means.items, aes(x=X.Position.mean, y=Y.Position.mean, color=Polarity, group=Polarity)) +
  geom_point(alpha=.6) + 
  expand_limits(x=c(-1.5,1.5)) + 
  ggtitle('Negation data: Mean Trajectories per item') +
  geom_errorbarh(aes(xmin=X.Position.mean-X.Position.se, xmax=X.Position.mean+X.Position.se), alpha=.8) +
  theme(legend.position = "none") + 
  facet_wrap(~Sentence, ncol=4) 

ggsave('negation-data-mean-sentences.png', plot = last_plot(), scale = 1, dpi = 300, path='R_scripts/graphs/negation_data')


