
#Ordering
normalized_positions.plot$lda_measure_cut <- cut(normalized_positions.plot$lda_measure, 5)

#Taking negative values for false items
normalized_positions.plot.false = normalized_positions.plot %>%
  filter(Response=='false')%>%
  dplyr::mutate_at('X.Position', funs('-'))
normalized_positions.plot.true = normalized_positions.plot %>%
  filter(Response=='true')
normalized_positions.plot <- rbind(normalized_positions.plot.false, normalized_positions.plot.true)


trajectory_subject.X <- aggregate(X.Position~Subject+Condition+Time.Step+Item.number.cut, data= subset(normalized_positions.plot,Response=='true'), mean, na.rm=T)
ggplot(trajectory_subject.X, aes(y=X.Position, x=Time.Step, group=Subject, color=Condition)) +
  geom_point(alpha=.4, size=1) +
  theme(legend.position = "none") + 
  facet_grid(.~Item.number.cut) + ggtitle('Trajectories')
ggsave('plural_data_XPosition_Item.png', plot = last_plot(), scale = 1, dpi = 300, width=15, path='real_data/plural/graphs')

normalized_positions.means <- summarySE(trajectory_subject.X, measurevar="X.Position", groupvars=c("Condition", "Time.Step", 'Item.number.cut'))
ggplot(normalized_positions.means, aes(x=Time.Step, y=X.Position, color=Condition, group=Condition))  +
  geom_line(alpha=.8) + geom_errorbar(aes(ymin=X.Position-se, ymax=X.Position+se), width=.1, alpha=.2)+
  theme_light() + theme(legend.position = "top") + facet_grid(.~Item.number.cut) 

ggsave('plural_data_meanXPosition_Item.png', plot = last_plot(), scale = 1, dpi = 300, width = 10, path='real_data/plural/graphs')


#Ploting subject mean x-position per time step
trajectory_subject.X <- aggregate(X.Position~Subject+Response+Condition+Time.Step+lda_measure_cut, data= normalized_positions.plot, mean, na.rm=T)
ggplot(subset(trajectory_subject.X, Response=='true'), aes(y=X.Position, x=Time.Step, group=Subject, color=Condition)) +
geom_point(alpha=.4, size=1) +
theme_light()  + theme(legend.position = "top") + 
facet_grid(.~lda_measure_cut) + ggtitle('Trajectories (only for true responses)')
ggsave('plural_data_XPosition.png', plot = last_plot(), scale = 1, dpi = 300, width=15, path='real_data/plural/graphs')

#Plotting mean x-position per time step  
normalized_positions.means <- summarySE(trajectory_subject.X, measurevar="X.Position", groupvars=c("Condition", "Time.Step", "Response", 'lda_measure_cut'))
ggplot(subset(normalized_positions.means, Response=='true'), aes(x=Time.Step, y=X.Position, color=Condition, group=Condition))  +
geom_line(alpha=.8) + geom_errorbar(aes(ymin=X.Position-se, ymax=X.Position+se), width=.1, alpha=.2)+
theme_light()  + theme(legend.position = "top") + facet_grid(.~lda_measure_cut) 

ggsave('plural_data_meanXPosition.png', plot = last_plot(), scale = 1, dpi = 300, width = 10, path='real_data/plural/graphs')

#Plotting good trajectory aggregated by subject, truth value, sentence and lda_measure
trajectory_subject.X <- aggregate(X.Position~Subject+Response+Condition+Time.Step+lda_measure_cut, data= normalized_positions.plot, mean, na.rm=T)
trajectory_subject.Y <- aggregate(Y.Position~Subject+Response+Condition+Time.Step+lda_measure_cut, data= normalized_positions.plot, mean, na.rm=T)
trajectory_subject <- merge(trajectory_subject.X,trajectory_subject.Y) 

ggplot(trajectory_subject, aes(x=X.Position, y=Y.Position, color=Condition, group=Subject)) +geom_point(alpha=.4, size=.5) + 
  theme_light() + theme(legend.position = "none") + facet_grid(Response~lda_measure_cut) 

ggsave('plural_data_trajectories.png', plot = last_plot(), scale = 1, dpi = 300, width=10,path='real_data/plural/graphs')

normalized_positions.means.traj <- ddply(trajectory_subject, c("Condition", "Time.Step", "Response","lda_measure_cut"),
                                         function(normalized_positions.means)c(X.Position.mean=mean(normalized_positions.means$X.Position, na.rm=T), 
                                                                               se=se(normalized_positions.means$X.Position, na.rm=T),
                                                                               Y.Position.mean=mean(normalized_positions.means$Y.Position, na.rm=T)))

ggplot(normalized_positions.means.traj, aes(y=Y.Position.mean, x=X.Position.mean, color=Condition)) +geom_point(alpha=.4, size=2) + theme(legend.position = "top") + facet_grid(Response~lda_measure_cut)

ggsave('plural_data_mean_trajectories.png', plot = last_plot(), scale = 1, dpi = 300, width=10, path='real_data/plural/graphs')


#Only first part

first_part <- subset(normalized_positions.plot, Item.number<20)

trajectory_subject.X <- aggregate(X.Position~Subject+Response+Condition+Time.Step+lda_measure_cut, data= first_part, mean, na.rm=T)
ggplot(subset(trajectory_subject.X, Response=='true'), aes(y=X.Position, x=Time.Step, group=Subject, color=Condition)) +
  geom_point(alpha=.4, size=1) +
  theme_light()  + theme(legend.position = "top") + 
  facet_grid(.~lda_measure_cut) + ggtitle('Trajectories (only for true responses)')

normalized_positions.means <- summarySE(trajectory_subject.X, measurevar="X.Position", groupvars=c("Condition", "Time.Step", 'lda_measure_cut'))
ggplot(normalized_positions.means, aes(x=Time.Step, y=X.Position, color=Condition, group=Condition))  +
  geom_line(alpha=.8) + geom_errorbar(aes(ymin=X.Position-se, ymax=X.Position+se), width=.1, alpha=.2)+
  theme_light() + theme(legend.position = "top") + facet_grid(.~lda_measure_cut) 

#only for connected
connected <-  subset(normalized_positions.plot, Predicate=='connected')
trajectory_subject.X <- aggregate(X.Position~Subject+Response+Condition+Time.Step+lda_measure_cut, data= connected, mean, na.rm=T)
ggplot(subset(trajectory_subject.X, Response=='true'), aes(y=X.Position, x=Time.Step, group=Subject, color=Condition)) +
  geom_point(alpha=.4, size=1) +
  theme_light()  + theme(legend.position = "top") + 
  facet_grid(.~lda_measure_cut) + ggtitle('Trajectories (only for true responses)')


ggplot(experimental_items, aes(x=lda_measure, fill=Condition)) + 
  geom_histogram(binwidth=.5,  position="dodge")+ 
  theme(legend.position = "top") + 
  facet_grid(Predicate~Response)

