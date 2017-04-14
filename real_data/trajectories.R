
#Ordering
normalized_positions.plot$X.Position <- as.numeric(normalized_positions.plot$X.Position)
normalized_positions.plot$Y.Position <- as.numeric(normalized_positions.plot$Y.Position)
normalized_positions.plot$Time.Step <- as.numeric(normalized_positions.plot$Time.Step)
normalized_positions.plot$Subject <- factor(normalized_positions.plot$Subject)
normalized_positions.plot$Sentence_Type <- factor(normalized_positions.plot$Sentence_Type)
normalized_positions.plot$lda_measure_cut <- cut(normalized_positions.plot$lda_measure, 10)

#Taking negative values for false items
normalized_positions.plot.false = normalized_positions.plot %>%
  filter(Truth.value=='False')%>%
  dplyr::mutate_at('X.Position', funs('-'))
normalized_positions.plot.true = normalized_positions.plot %>%
  filter(Truth.value=='True')
normalized_positions.plot <- rbind(normalized_positions.plot.false, normalized_positions.plot.true)


#Weird trajectories (the ones with lda measures >10 or <-10)
weird <- subset(normalized_positions.plot, lda_measure < (-20) | lda_measure > 20)
weird$lda_measure_cut <- cut(weird$lda_measure, 10)
#how many trajectories
weird.number <- as.character(signif(nrow(weird)*100/nrow(normalized_positions.plot),3))

ggplot(weird, aes(y=Y.Position, x=X.Position, group=Subject, color=Sentence_Type)) + 
  geom_point(alpha=.4, size=1) + theme(legend.position = "none") + 
  facet_grid(Truth.value~lda_measure_cut) +
  ggtitle(paste('Number of Trajectories (out of total):', weird.number)) 
ggsave('Excluded trajectories_all.png', plot = last_plot(), scale = 1, dpi = 300, width=15)

trajectory_subject.X <- aggregate(X.Position~Subject+Truth.value+Sentence_Type+Time.Step+lda_measure_cut, data= weird, mean, na.rm=T)
ggplot(trajectory_subject.X, aes(y=X.Position, x=Time.Step, group=Subject, color=Truth.value)) + geom_point(alpha=.4, size=1) + theme(legend.position = "none") + facet_grid(Truth.value+Sentence_Type~lda_measure_cut) + ggtitle('Trajectories')
ggsave('Excluded X_all.png', plot = last_plot(), scale = 1, dpi = 300, width=15)



## Good trajectories 
better <- subset(normalized_positions.plot, lda_measure > (-20) & lda_measure < 20)
better$lda_measure_cut <- cut(better$lda_measure, 10)

#Plot good x-position per time step aggregated by subject, truth value, sentence and lda_measure
trajectory_subject.X <- aggregate(X.Position~Subject+Truth.value+Sentence_Type+Time.Step+lda_measure_cut, data= better, mean, na.rm=T)
ggplot(trajectory_subject.X, aes(y=X.Position, x=Time.Step, group=Subject, color=Truth.value)) + geom_point(alpha=.4, size=1) + theme(legend.position = "none") + facet_grid(Truth.value+Sentence_Type~lda_measure_cut) + ggtitle('Trajectories')

ggsave('X-Position_subjects(after exclusion)_all.png', plot = last_plot(), scale = 1, dpi = 300, width=10)

#Plotting mean x-position per time step  
normalized_positions.means <- summarySE(trajectory_subject.X, measurevar="X.Position", groupvars=c("Sentence_Type", "Time.Step", "Truth.value", 'lda_measure_cut'))
ggplot(normalized_positions.means, aes(x=Time.Step, y=X.Position, color=Sentence_Type, group=Sentence_Type)) + geom_line(alpha=.6) + geom_errorbar(aes(ymin=X.Position-se, ymax=X.Position+se), width=.1) + theme(legend.position = "top") + facet_grid(lda_measure_cut~Truth.value) 
ggsave('Mean_X-Position(after exclusion)_all.png', plot = last_plot(), scale = 1, dpi = 300, width = 10)

#Plot good trajectory aggregated by subject, truth value, sentence and lda_measure
trajectory_subject.X <- aggregate(X.Position~Subject+Truth.value+Sentence_Type+Time.Step+lda_measure_cut, data= better, mean, na.rm=T)
trajectory_subject.Y <- aggregate(Y.Position~Subject+Truth.value+Sentence_Type+Time.Step+lda_measure_cut, data=better, mean, na.rm=T)
trajectory_subject <- merge(trajectory_subject.X,trajectory_subject.Y) 
ggplot(trajectory_subject, aes(x=X.Position, y=Y.Position, color=Sentence_Type, group=Subject)) +geom_point(alpha=.4, size=.5) + theme(legend.position = "none") + facet_grid(Truth.value~lda_measure_cut) 
ggsave('Trajectory_subjects(after exclusion)_all.png', plot = last_plot(), scale = 1, dpi = 300, width=10)

normalized_positions.means.traj <- ddply(trajectory_subject, c("Sentence_Type", "Time.Step", "Truth.value","lda_measure_cut"),
                                         function(normalized_positions.means)c(X.Position.mean=mean(normalized_positions.means$X.Position, na.rm=T), 
                                                                               se=se(normalized_positions.means$X.Position, na.rm=T),
                                                                               Y.Position.mean=mean(normalized_positions.means$Y.Position, na.rm=T)))

ggplot(normalized_positions.means.traj, aes(y=Y.Position.mean, x=X.Position.mean, color=Sentence_Type)) +geom_point(alpha=.4, size=2) + theme(legend.position = "top") + facet_grid(Truth.value~lda_measure_cut)
ggsave('Mean_Trajectories(after exclusion)_all.png', plot = last_plot(), scale = 1, dpi = 300, width = 10)




#PLOT EVERYTHING TOGETHER
# trajectory_subject.X <- aggregate(X.Position~Subject+Truth.value+Sentence_Type+Time.Step, data= normalized_positions.plot, mean, na.rm=T)
# ggplot(trajectory_subject.X, aes(y=X.Position, x=Time.Step, group=Subject, color=Subject)) + geom_point(alpha=.4, size=1) + theme(legend.position = "none") + facet_grid(Sentence_Type~Truth.value)+ ggtitle('Trajectories')
# ggsave('Trajectories_subjects_X.png', plot = last_plot(), scale = 1, dpi = 300, width=10)
# 
# #Plotting means
# normalized_positions.means <- summarySE(trajectory_subject.X, measurevar="X.Position", groupvars=c("Sentence_Type", "Time.Step", "Truth.value"))
# ggplot(normalized_positions.means, aes(x=Time.Step, y=X.Position, color=Sentence_Type, group=Sentence_Type)) + geom_line(alpha=.6) + geom_errorbar(aes(ymin=X.Position-se, ymax=X.Position+se), width=.1) + theme(legend.position = "top") + facet_grid(.~Truth.value) 
# ggsave('Mean_Trajectories_X.png', plot = last_plot(), scale = 1, dpi = 300, width = 6)

# require(plotly)
# plotly::plot_ly(subset(normalized_positions.means.traj, Truth.value=='True'), x = ~X.Position.mean, y = ~Y.Position.mean, z = ~Time.Step, color = ~Sentence_Type, colors = c('#BF382A', '#0C4B8E'))
