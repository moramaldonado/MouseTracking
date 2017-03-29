#Plotting trajectories
normalized_positions.plot.X = calibration_data %>%
  dplyr::select(Subject,Polarity, Expected_response, Normalized.positions.X, lda_measure, Item.number) %>%
  separate(Normalized.positions.X, into= as.character(c(1:101)), sep = ",") %>%
  gather(Time.Step, X.Position, 4:104) 
normalized_positions.plot.Y = calibration_data %>%
  dplyr::select(Subject,Polarity, Expected_response, Normalized.positions.Y, lda_measure,  Item.number) %>%
  separate(Normalized.positions.Y, into= as.character(c(1:101)), sep = ",") %>%
  gather(Time.Step, Y.Position, 4:104) 
normalized_positions.plot <- merge(normalized_positions.plot.X,normalized_positions.plot.Y)
#rm(normalized_positions.plot.X,normalized_positions.plot.Y)

normalized_positions.plot$X.Position<- as.numeric(normalized_positions.plot$X.Position)
normalized_positions.plot$Y.Position <- as.numeric(normalized_positions.plot$Y.Position)
normalized_positions.plot$Time.Step <- as.numeric(normalized_positions.plot$Time.Step)
normalized_positions.plot$Subject <- factor(normalized_positions.plot$Subject)
normalized_positions.plot$Polarity <- factor(normalized_positions.plot$Polarity )
normalized_positions.plot$lda_measure_cut <- cut(normalized_positions.plot$lda_measure, 5)
normalized_positions.plot$grp <- paste(normalized_positions.plot$Subject,normalized_positions.plot$Item.number)

normalized_positions.plot.false = normalized_positions.plot %>%
  filter(Expected_response=='false')%>%
  dplyr::mutate_at('X.Position', funs('-'))
normalized_positions.plot.true = normalized_positions.plot %>%
  filter(Expected_response=='true')
normalized_positions.plot <- rbind(normalized_positions.plot.false, normalized_positions.plot.true)
summary(normalized_positions.plot)

#Plotting real subjects
ggplot(normalized_positions.plot, aes(x=X.Position, y=Y.Position, color=Polarity, group=grp)) + geom_point(alpha=.4, size=1) + theme(legend.position = "none") + 
  facet_grid(lda_measure_cut~Expected_response)     
ggsave('LDA-trajectories1.png', plot = last_plot(), scale = 1, dpi = 300)

ggplot(normalized_positions.plot, aes(x=Time.Step, y=X.Position, color=Polarity, group=grp)) + geom_point(alpha=.4, size=1) + geom_line()+ theme(legend.position = "none") + 
  facet_grid(lda_measure_cut~Expected_response) 
ggsave('LDA-trajectories2.png', plot = last_plot(), scale = 1, dpi = 300)


#Plotting real means
normalized_positions.means <- ddply(normalized_positions.plot, c("Polarity", "Time.Step", "Expected_response"),
                                    function(normalized_positions.means)c(X.Position.mean=mean(normalized_positions.means$X.Position, na.rm=T), X.Position.se=se(normalized_positions.means$X.Position, na.rm=T)))
ggplot(normalized_positions.means, aes(x=Time.Step, y=X.Position.mean, color=Polarity, group=Polarity)) + geom_point(alpha=.6) + geom_line() + theme(legend.position = "none") + facet_grid(.~Expected_response) + 
geom_errorbar(aes(ymin=X.Position.mean-X.Position.se, ymax=X.Position.mean+X.Position.se), width=.1) 

normalized_positions.means.traj <- ddply(normalized_positions.plot, c("Polarity", "Time.Step", "Expected_response"),
                                    function(normalized_positions.means)c(X.Position.mean=mean(normalized_positions.means$X.Position, na.rm=T), 
                                                                          X.Position.se=se(normalized_positions.means$X.Position, na.rm=T),
                                                                          Y.Position.mean=mean(normalized_positions.means$Y.Position, na.rm=T), 
                                                                          Y.Position.se=se(normalized_positions.means$Y.Position, na.rm=T)))

ggplot(normalized_positions.means, aes(x=Time.Step, y=X.Position.mean, color=Polarity, group=Polarity)) + geom_point(alpha=.6) + geom_line() + theme(legend.position = "none") + facet_grid(.~Expected_response) + 
  geom_errorbar(aes(ymin=X.Position.mean-X.Position.se, ymax=X.Position.mean+X.Position.se), width=.1) 

ggplot(normalized_positions.means.traj, aes(x=X.Position.mean, y=Y.Position.mean, color=Polarity, group=Polarity)) + geom_point(alpha=.6) + theme(legend.position = "none") + facet_grid(.~Expected_response) 

