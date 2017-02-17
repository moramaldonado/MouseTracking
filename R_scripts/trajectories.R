


normalized_positions = calibration_data %>%
  select(Subject, Normalized.positions.X, Polarity, Expected_response) %>%
  separate(Normalized.positions.X, into= as.character(c(1:101)), sep = ",") %>%
  gather(Time.Step, X.Position, 2:102)
normalized_positions$X.Position <- as.numeric(normalized_positions$X.Position)
normalized_positions$Time.Step <- as.numeric(normalized_positions$Time.Step)
normalized_positions$Subject <- factor(normalized_positions$Subject)
normalized_positions$Polarity <- factor(normalized_positions$Polarity )

differences  = calibration_data %>%
  select(Subject, Difference, Polarity) %>%
  separate(Difference, into= as.character(c(1:101)), sep = ",") %>%
  gather(Time.Step, Differences, 2:102) 

differences$Differences <- as.numeric(differences$Differences)
differences$Time.Step <- as.numeric(differences$Time.Step)
differences$Subject <- factor(differences$Subject)
differences$Polarity <- factor(differences$Polarity )



#Plotting real subjects
ggplot(normalized_positions, aes(x=Time.Step, y=X.Position, color=Subject, group=Subject)) + geom_point(alpha=.6) + geom_line() + theme(legend.position = "none") + facet_grid(Polarity~Expected_response) 
p1<- ggplot(normalized_positions, aes(x=Time.Step, y=X.Position, color=Polarity, group=Polarity)) + geom_point(alpha=.4, size=1) + theme(legend.position = "none") + facet_grid(.~Expected_response) + ggtitle('Trajectory X axis')
p2 <- ggplot(differences, aes(x=Time.Step, y=Differences, color=Polarity, group=Polarity)) + geom_point(alpha=.4, size=1) + theme(legend.position = "none") + ggtitle('Difference distance target and alternative')
multiplot(p1,p2)

#Plotting real means
normalized_positions.means <- ddply(normalized_positions, c("Polarity", "Time.Step", "Expected_response"),
                                    function(normalized_positions.means)c(X.Position.mean=mean(normalized_positions.means$X.Position, na.rm=T), X.Position.se=se(normalized_positions.means$X.Position, na.rm=T)))
ggplot(normalized_positions.means, aes(x=Time.Step, y=X.Position.mean, color=Polarity, group=Polarity)) + geom_point(alpha=.6) + geom_line() + theme(legend.position = "none") + facet_grid(.~Expected_response) + 
geom_errorbar(aes(ymin=X.Position.mean-X.Position.se, ymax=X.Position.mean+X.Position.se), width=.1) 


differences.means <- ddply(differences, c("Polarity", "Time.Step"),
                                    function(differences.means)c(Differences.mean=mean(differences.means$Difference, na.rm=T), Differences.se=se(differences.means$Differences, na.rm=T)))

ggplot(differences.means, aes(x=Time.Step, y=Differences.mean, color=Polarity, group=Polarity)) + geom_point(alpha=.6) + geom_line() + theme(legend.position = "none") + 
  geom_errorbar(aes(ymin=Differences.mean-Differences.se, ymax=Differences.mean+Differences.se), width=.1) 





#Normalized positions for fake data
normalized_positions.fakedata = calibration_data_new_subjects %>%
  select(randomID, Normalized.positions.X, Polarity, Expected_response) %>%
  separate(Normalized.positions.X, into= as.character(c(1:101)), sep = ",") %>%
  gather(Time.Step, X.Position, 2:102)
normalized_positions.fakedata$X.Position <- as.numeric(normalized_positions.fakedata$X.Position)
normalized_positions.fakedata$Time.Step <- as.numeric(normalized_positions.fakedata$Time.Step)
normalized_positions.fakedata$Subject <- factor(normalized_positions.fakedata$randomID)
normalized_positions.fakedata$Polarity <- factor(normalized_positions.fakedata$Polarity )


