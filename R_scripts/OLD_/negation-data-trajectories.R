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


#Plot each item as a function of X position 
ggplot(normalized_positions.plot, aes(x=Time.Step, y=X.Position, color=Polarity, group=grp)) +
  geom_point(alpha=.4, size=.5)  + geom_line(alpha=.4)+ theme(legend.position = "none") + 
  ggtitle('Negation data: Xposition x Time step per subject') +
  scale_colour_brewer(palette="Set1")+ 
  expand_limits(x=c(-1.5,1.5)) + 
  theme_minimal()+
  facet_grid(Expected_response+Polarity~lda_measure_cut) +
ggsave('negation-data-Xposition.png', plot = last_plot(), scale = 1, dpi = 300, path='R_scripts/graphs/negation_data')
#ggsave('negation-data-Xposition(withOLD).png', plot = last_plot(), scale = 1, dpi = 300, path='R_scripts/graphs')







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

negation_data$lda_measure_cut <- cut(negation_data$lda_measure, 5)
ggplot(subset(negation_data, MaxRatio.Time<3000), aes(x=MaxRatio.Time,fill=Polarity)) +
  geom_histogram(position='dodge')+
  facet_wrap(~lda_measure_cut)