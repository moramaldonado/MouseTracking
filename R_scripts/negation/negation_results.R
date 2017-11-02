library(MASS) # NB: this will mask dplyr::select
require(car)
#### PERFORMANCE ####

#Plot each trajectory
ggplot(normalized_positions.plot, aes(x=X.Position, y=Y.Position, color=Polarity, group=grp)) + 
  geom_point(alpha=.7, size=.5) + 
  theme(legend.position = "none") + 
  ggtitle('') +
  scale_colour_brewer(palette="Set1")+ 
  theme_minimal()+
  facet_grid(Polarity~Expected_response) +
  theme(legend.position = "top") 
ggsave('negation-data-trajectory.png', plot = last_plot(), scale = 1, dpi = 300, width=10, path='R_scripts/graphs/negation')

# Mean X Position
normalized_positions.means.subject <-   ddply(normalized_positions.plot, c("Polarity", "Time.Step", "Expected_response", "Subject"),
                                              function(normalized_positions.plot)c(X.Position.mean=mean(normalized_positions.plot$X.Position, na.rm=T)))

normalized_positions.means.overall <- ddply(normalized_positions.means.subject, c("Polarity", "Time.Step", "Expected_response"),
                                            function(normalized_positions.means.subject)c(X.Position.mean=mean(normalized_positions.means.subject$X.Position.mean, na.rm=T),
                                                                                          X.Position.se=se(normalized_positions.means.subject$X.Position, na.rm=T)))

#Mean X position per subject
ggplot(normalized_positions.means.subject, aes(x=Time.Step, y=X.Position.mean, color=Subject, group=Subject)) + 
  geom_point(alpha=.4) + geom_line(alpha=.4) +
  expand_limits(x=c(-1.5,1.5)) + 
  theme_minimal() +
  ggtitle('Negation data: Subject means XPosition x TimeStep')+
  theme(legend.position = "none") + facet_grid(Polarity~Expected_response) 
ggsave('negation-data-Xposition-subject.png', plot = last_plot(), scale = 1, dpi = 300, width=7, path='R_scripts/graphs/negation')

#Overall means
ggplot(normalized_positions.means.overall, aes(x=Time.Step, y=X.Position.mean, color=Polarity, group=Polarity)) + 
  geom_point(alpha=.6) + geom_line() +
  ggtitle('Negation data: Mean Xposition x Time step') +
  scale_colour_brewer(palette="Set1")+ 
  expand_limits(x=c(-1.5,1.5)) + 
  theme_minimal() +
  theme(legend.position = "none") + facet_grid(.~Expected_response) + 
  geom_errorbar(aes(ymin=X.Position.mean-X.Position.se, ymax=X.Position.mean+X.Position.se), width=.1) 
ggsave('negation-data-mean-Xposition.png', plot = last_plot(), scale = 1, dpi = 300, width=6, path='R_scripts/graphs/negation')


## Mean Trajectory
normalized_positions.means.subject <- ddply(normalized_positions.plot, c("Polarity", "Time.Step", "Expected_response", "Subject"),
                                            function(normalized_positions.plot)c(X.Position.mean=mean(normalized_positions.plot$X.Position, na.rm=T), 
                                                                                 Y.Position.mean=mean(normalized_positions.plot$Y.Position, na.rm=T)))

normalized_positions.means.traj <- ddply(normalized_positions.means.subject, c("Polarity", "Time.Step", "Expected_response"),
                                         function(normalized_positions.means.subject)c(X.Position.mean=mean(normalized_positions.means.subject$X.Position, na.rm=T), 
                                                                                       X.Position.se=se(normalized_positions.means.subject$X.Position, na.rm=T),
                                                                                       Y.Position.mean=mean(normalized_positions.means.subject$Y.Position, na.rm=T), 
                                                                                       Y.Position.se=se(normalized_positions.means.subject$Y.Position, na.rm=T)))

#Take mean trajectories per subject
ggplot(normalized_positions.means.subject, aes(x=X.Position.mean, y=Y.Position.mean, color=Subject, group=Subject)) +
  geom_point(alpha=.4, size=.6) + 
  ggtitle('Negation data: Subject Mean Trajectories') +
  theme_minimal()+
  theme(legend.position = "none") + 
  expand_limits(x=c(-1.5,1.5)) + 
  facet_grid(Polarity~Expected_response) 
ggsave('negation-data-subject-trajectory.png', plot = last_plot(), scale = 1, dpi = 300, path='R_scripts/graphs/negation')


#Plot mean trajectories
ggplot(normalized_positions.means.traj, aes(x=X.Position.mean, y=Y.Position.mean, color=Polarity, group=Polarity)) +
  geom_point(alpha=.5) + 
  ggtitle('') +
  xlab('X Coordinate') +
  ylab('Y Coordinate') +
  geom_errorbarh(aes(xmin=X.Position.mean-X.Position.se, xmax=X.Position.mean+X.Position.se), alpha=.4) + 
  theme_minimal()+
  expand_limits(x=c(-1.5,1.5)) + 
  scale_colour_brewer(palette="Set1", 
                      name="Polarity",
                      breaks=c("N", "P"),
                      labels=c("Negative", "Affirmative"))+ 
  facet_grid(.~Expected_response) 
ggsave('negation-data-mean-trajectory.png', plot = last_plot(), scale = 1, dpi = 300, path='R_scripts/graphs/negation', width = 7, height = 5)





#### LOADING THE LDA FROM CALIBRATION ####

# LDA FULL (coordinates, velocity and euclidean-based acceleration) ####
load('LDA-Full.RData')
# negation_data = subset(negation_data, Adjective!=5)

#Create data frame for LDA
x <- paste0('x', sprintf("%03d", c(1:101)))
y <- paste0('y', sprintf("%03d", c(1:101)))
a <- paste0('a', sprintf("%03d", c(1:101)))
v <- paste0('v', sprintf("%03d", c(1:101)))
t <- paste0('t', sprintf("%03d", c(1:101)))

# Each x and y coordenate into two columns (101 coordenates per trial) 
normalized_positions = negation_data %>%
  dplyr::select(Subject, Item.number, Polarity, Response, Normalized.positions.X,Normalized.positions.Y, Velocity, Acceleration, RawTime) %>%
  separate(Normalized.positions.Y, into= y, sep = ",") %>%
  separate(Normalized.positions.X, into= x, sep = ",") %>%
  separate(Velocity, into= v, sep = ",") %>%
  separate(RawTime, into= t, sep = ",") %>%
  separate(Acceleration, into= a, sep = ",")
  
  
normalized_positions[y] <- sapply(normalized_positions[y],as.numeric)
normalized_positions[x] <- sapply(normalized_positions[x],as.numeric)
normalized_positions[v] <- sapply(normalized_positions[v],as.numeric)
normalized_positions[a] <- sapply(normalized_positions[a],as.numeric)
normalized_positions[t] <- sapply(normalized_positions[t],as.numeric)

# Taking the negative of false items, to have everything in the same scale
normalized_positions_false = normalized_positions%>%
  filter(Response=='false')%>%
  dplyr::mutate_at(vars(starts_with('x')), funs('-'))
normalized_positions_true = filter(normalized_positions, Response=='true')
normalized_positions = bind_rows(normalized_positions_false,normalized_positions_true)
rm(normalized_positions_true, normalized_positions_false)


#More about classes
normalized_positions$Subject <- factor(normalized_positions$Subject)
normalized_positions$Polarity <- factor(normalized_positions$Polarity)
normalized_positions$Response <- factor(normalized_positions$Response)


normalized_positions.new <- normalized_positions %>%
  dplyr::select(Subject, Item.number, Polarity, Response, one_of(all_data_columns))

normalized_positions.new_pca <- bind_cols(normalized_positions.new,
                                          as.data.frame(predict(m_pca, normalized_positions.new)[,1:n_pca]))

lda_measure.new.df <- data_frame(
  lda_measure=c(as.matrix(dplyr::select(normalized_positions.new_pca, starts_with("PC"))) %*% v_lda- b_lda),
  Subject = normalized_positions.new_pca$Subject, 
  Item.number = normalized_positions.new_pca$Item.number, 
  Polarity = normalized_positions.new_pca$Polarity, 
  Response = normalized_positions.new_pca$Response)

##Including the relevant lda_measure in the data
negation_data$Subject <- factor(negation_data$Subject)
negation_data$Response <- factor(negation_data$Response)
negation_data$Polarity <- factor(negation_data$Polarity)
negation_data <- dplyr::full_join(lda_measure.new.df, negation_data, by=c("Subject", "Item.number", "Polarity", "Response"))
normalized_positions.plot <- dplyr::full_join(lda_measure.new.df, normalized_positions.plot, by=c("Subject", "Item.number", "Polarity"))
normalized_positions.plot$lda_measure_cut <- cut(normalized_positions.plot$lda_measure, 5)

## Plotting the distribution of LDA measure in negation data (only for true responses)
negation_data_true <- filter(negation_data, Response=='true')
png(filename='R_scripts/graphs/negation/LDA-negation.png', width = 7, height = 7, units = 'in', res = 300)
plot_measure(negation_data_true, "lda_measure", "Polarity")
dev.off()



# LDA only coordinates ####
load('LDA-Coords.RData')

normalized_positions.new <- normalized_positions %>%
  dplyr::select(Subject, Item.number, Polarity, Response, one_of(all_data_columns))

normalized_positions.new_pca <- bind_cols(normalized_positions.new,
                                          as.data.frame(predict(m_pca, normalized_positions.new)[,1:n_pca]))

lda_measure.new.df <- data_frame(
  lda_measure_coords=c(as.matrix(dplyr::select(normalized_positions.new_pca, starts_with("PC"))) %*% v_lda- b_lda),
  Subject = normalized_positions.new_pca$Subject, 
  Item.number = normalized_positions.new_pca$Item.number, 
  Polarity = normalized_positions.new_pca$Polarity, 
  Response = normalized_positions.new_pca$Response)

##Including the relevant lda_measure in the data
negation_data <- dplyr::full_join(lda_measure.new.df, negation_data, by=c("Subject", "Item.number", "Polarity", "Response"))
normalized_positions.plot <- dplyr::full_join(lda_measure.new.df, normalized_positions.plot, by=c("Subject", "Item.number", "Polarity"))

negation_data_true <- filter(negation_data, Response=='true')
png(filename='R_scripts/graphs/negation/LDA-coords-negation.png', width = 7, height = 7, units = 'in', res = 300)
plot_measure(negation_data_true, "lda_measure_coords", "Polarity")
dev.off()

# LDA time ####
load('LDA-Coords-Time.RData')

normalized_positions.new <- normalized_positions %>%
  dplyr::select(Subject, Item.number, Polarity, Response, one_of(all_data_columns))

normalized_positions.new_pca <- bind_cols(normalized_positions.new,
                                          as.data.frame(predict(m_pca, normalized_positions.new)[,1:n_pca]))

lda_measure.new.df <- data_frame(
  lda_measure_coords_time=c(as.matrix(dplyr::select(normalized_positions.new_pca, starts_with("PC"))) %*% v_lda- b_lda),
  Subject = normalized_positions.new_pca$Subject, 
  Item.number = normalized_positions.new_pca$Item.number, 
  Polarity = normalized_positions.new_pca$Polarity, 
  Response = normalized_positions.new_pca$Response)

##Including the relevant lda_measure in the data
negation_data <- dplyr::full_join(lda_measure.new.df, negation_data, by=c("Subject", "Item.number", "Polarity", "Response"))
normalized_positions.plot <- dplyr::full_join(lda_measure.new.df, normalized_positions.plot, by=c("Subject", "Item.number", "Polarity"))

negation_data_true <- filter(negation_data, Response=='true')
png(filename='R_scripts/graphs/negation/LDA-coords-time-negation.png', width = 7, height = 7, units = 'in', res = 300)
plot_measure(negation_data_true, "lda_measure_coords_time", "Polarity")
dev.off()







## Plotting the distribution of alternative measures in negation data ####
### Max log ratio
png(filename='R_scripts/graphs/negation/MaxLogRatio-negation.png', width = 7, height = 7, units = 'in', res = 300)
plot_measure(negation_data_true, "MaxLogRatio", "Polarity")
dev.off()
### Max deviation
png(filename='R_scripts/graphs/negation/MaxDeviation-negation.png', width = 7, height = 7, units = 'in', res = 300)
plot_measure(negation_data_true, "MaxDeviation", "Polarity")
dev.off()
### AUC
png(filename='R_scripts/graphs/negation/AUC-negation.png', width = 7, height = 7, units = 'in', res = 300)
plot_measure(negation_data_true, "AUC", "Polarity")
dev.off()
### Xflips
png(filename='R_scripts/graphs/negation/Xflips-negation.png', width = 7, height = 7, units = 'in', res = 300)
plot_measure(negation_data_true, "X.flips", "Polarity")
dev.off()
### Acc flips
png(filename='R_scripts/graphs/negation/Accflips-negation.png', width = 7, height = 7, units = 'in', res = 300)
plot_measure(negation_data_true, "Acc.flips", "Polarity")
dev.off()



#### STATS ####

mydata <- negation_data
mydata$Expected_response <- factor(mydata$Expected_response)
mydata$Polarity <- factor(mydata$Polarity)
#mydata$Expected_response = factor(mydata$Expected_response,levels(mydata$Expected_response)[c(2,1)])
#mydata$Polarity = factor(mydata$Polarity,levels(mydata$Polarity)[c(2,1)])


contrasts(mydata$Expected_response) <- c(-0.5, 0.5)
contrasts(mydata$Polarity) <- c(0.5, -0.5)

mydata$Interaction<-factor(contrasts(mydata$Polarity)[mydata$Polarity]*
                             contrasts(mydata$Expected_response)[mydata$Expected_response]) 
#Mean values

xflips.means.subj <- ddply(mydata, c("Polarity", "Expected_response", "Subject"),
                      function(mydata)c(mean=mean(mydata$X.flips, na.rm=T)))

xflips.means <- ddply(xflips.means.subj, c("Polarity", "Expected_response"),
                                            function(xflips.means.subj)c(mean=mean(xflips.means.subj$mean, na.rm=T), se = se(xflips.means.subj$mean)))

### LdaMeasure: Differences between conditions and truth values
control_model.lda <- lmer(X.flips ~ Polarity*Expected_response + (1+Polarity*Expected_response|Subject), data = mydata, REML=FALSE)
summary(control_model.lda)
#Effect of Interaction
m0.interaction.lda <- lmer(X.flips~ Polarity+Expected_response+ (1+Polarity*Expected_response|Subject), data = mydata, REML=FALSE) #the value of intercept is not exactly the same as the one in my aggregate function, why?
anova(control_model.lda, m0.interaction.lda)
#Anova(control_model.lda, type=2)


control_model.lda <- lmer(X.flips ~ Polarity + Expected_response + Interaction + (1+Polarity*Expected_response|Subject), data = mydata, REML=FALSE)

#Main Effect: Polarity (Affirmative vs. Negative)
m0.sentence.lda <- lmer(X.flips ~ Expected_response  +Interaction+ (1+Polarity*Expected_response|Subject), data = mydata, REML=FALSE) #the value of intercept is not exactly the same as the one in my aggregate function, why?
anova(control_model.lda, m0.sentence.lda)

#Main Effect :Expected_response (True vs. False)
m0.response.lda <- lmer(X.flips ~ Polarity  + Interaction+ (1+Polarity*Expected_response|Subject), data = mydata, REML=FALSE) #the value of intercept is not exactly the same as the one in my aggregate function, why?
anova(control_model.lda, m0.response.lda)
















### AUC FOR ALL MEASURES ####

auc_measures <- data.frame (lda.full=1, 
                            lda.coord=1,
                            lda.time=1,
                            logratio=1, 
                            xflips=1, 
                            maxdeviation=1,
                            accflips=1, 
                            auc=1)

xflips.score.te <- negation_data_true$X.flips 
xflips.label.te <- factor(negation_data_true$Polarity)
xflips.roc.te <- roc(xflips.label.te, xflips.score.te)
auc_measures$xflips <- xflips.roc.te$auc

maxdeviation.score.te <- negation_data_true$MaxDeviation 
maxdeviation.label.te <- factor(negation_data_true$Polarity)
maxdeviation.roc.te <- roc(maxdeviation.label.te,maxdeviation.score.te)
auc_measures$maxdeviation <- maxdeviation.roc.te$auc


lda.score.te <- negation_data_true$lda_measure_coords
lda.label.te <- factor(negation_data_true$Polarity)
lda.roc.te <- roc(lda.label.te,lda.score.te)
auc_measures$lda.coord <- lda.roc.te$auc

lda.score.te <- negation_data_true$lda_measure_coords_time
lda.label.te <- factor(negation_data_true$Polarity)
lda.roc.te <- roc(lda.label.te,lda.score.te)
auc_measures$lda.time <- lda.roc.te$auc

lda.score.te <- negation_data_true$lda_measure
lda.label.te <- factor(negation_data_true$Polarity)
lda.roc.te <- roc(lda.label.te,lda.score.te)
auc_measures$lda.full <- lda.roc.te$auc

logratio.score.te <- negation_data_true$MaxLogRatio
logratio.label.te <- factor(negation_data_true$Polarity)
logratio.roc.te <- roc(logratio.label.te,logratio.score.te)
auc_measures$logratio <- logratio.roc.te$auc

auc.score.te <- negation_data_true$AUC
auc.label.te <- factor(negation_data_true$Polarity)
auc.roc.te <- roc(logratio.label.te,logratio.score.te)
auc_measures$auc <- logratio.roc.te$auc

accflip.score.te <- negation_data_true$Acc.flips
accflip.label.te <- factor(negation_data_true$Polarity)
accflip.roc.te <- roc(accflip.label.te, accflip.score.te)
auc_measures$accflips <- accflip.roc.te$auc


  

# AUC for each measure - table for latex
require(xtable)
xtable(auc_measures)



