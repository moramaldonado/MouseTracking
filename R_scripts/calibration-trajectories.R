##Excluding weird trajectories from calibration before taking the LDA measure
calibration_data <- subset(calibration_data, Accuracy==TRUE)

normalized_positions.plot.X = calibration_data %>%
  dplyr::select(Subject,Polarity, Expected_response, Normalized.positions.X, Item.number, grp, MaxLogRatio) %>%
  separate(Normalized.positions.X, into= as.character(c(1:101)), sep = ",") %>%
  gather(Time.Step, X.Position, 4:104) 
normalized_positions.plot.Y = calibration_data %>%
  dplyr::select(Subject,Polarity, Expected_response, Normalized.positions.Y,Item.number,grp, MaxLogRatio) %>%
  separate(Normalized.positions.Y, into= as.character(c(1:101)), sep = ",") %>%
  gather(Time.Step, Y.Position, 4:104) 
normalized_positions.plot <- merge(normalized_positions.plot.X,normalized_positions.plot.Y)

normalized_positions.plot$X.Position<- as.numeric(normalized_positions.plot$X.Position)
normalized_positions.plot$Y.Position <- as.numeric(normalized_positions.plot$Y.Position)
normalized_positions.plot$Time.Step <- as.numeric(normalized_positions.plot$Time.Step)
normalized_positions.plot$Subject <- factor(normalized_positions.plot$Subject)
normalized_positions.plot$Polarity <- factor(normalized_positions.plot$Polarity )

normalized_positions.plot.false = normalized_positions.plot %>%
  filter(Expected_response=='false')%>%
  dplyr::mutate_at('X.Position', funs('-'))
normalized_positions.plot.true = normalized_positions.plot %>%
  filter(Expected_response=='true')
normalized_positions.plot <- rbind(normalized_positions.plot.false, normalized_positions.plot.true)
normalized_positions.plot$Subject <- factor(normalized_positions.plot$Subject)


# #Plot per subject
# for (s in levels(normalized_positions.plot$Subject)) {
#   print(s)
#   ggplot(subset(normalized_positions.plot,Subject==s), aes(x=X.Position, y=Y.Position, color=Polarity, group=grp, label=Time.Step)) +
#     geom_point(alpha=.2, size=.5) +
#     ggtitle('Calibration Trajectories per subject')+
#     theme(legend.position = "none") +
#     geom_text(aes(label=as.character(Time.Step)),hjust=0.2,vjust=0.3, size=1.5) +
#     expand_limits(x=c(-1.5,1.5), y=c(-0.5, 1.7)) +
#     facet_grid(.~Item.number)
#   name <- paste(s, '.png')
#   ggsave(name, plot = last_plot(), width=12, height=4, path='R_scripts/graphs/calibration/calibration_per_subject')
# }

#I am deciding by eye which trajectories to exclude (based on the grp)
excluded_trajectories <- c('3 25', '3 27', '6 27', '6 30', '7 25', '10 25', '16 25', '23 25', '25 36', '31 25','31 28','17 33','31 26', '34 25', '48 27', '49 25')
calibration_data <- subset(calibration_data, !(grp  %in% excluded_trajectories))
normalized_positions.plot <- subset(normalized_positions.plot, !(grp  %in% excluded_trajectories))

