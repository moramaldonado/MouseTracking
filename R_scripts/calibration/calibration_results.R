##CALIBRATION RESULTS
## Overall description of the results and computation of original/main LDA (subsection 2.2)
library(MASS) # NB: this will mask dplyr::select

## Subset to deviated and straight trials
#calibration_data = subset(calibration_data, Polarity != 'uncertain')
calibration_data$Subject <- factor(calibration_data$Subject)
normalized_positions.plot$Subject <- factor(normalized_positions.plot$Subject)

## OVERALL PERFORMANCE (Plotting) ####

### Mean X position
normalized_positions.means.subject <-   ddply(normalized_positions.plot, c("Polarity", "Time.Step.Onset", "Subject"),
                                              function(normalized_positions.plot)c(X.Position.mean=mean(normalized_positions.plot$X.Position, na.rm=T)))

normalized_positions.means <- ddply(normalized_positions.means.subject, c("Polarity", "Time.Step.Onset"),
                                    function(normalized_positions.means.subject)c(X.Position.mean=mean(normalized_positions.means.subject$X.Position, na.rm=T), X.Position.se=se(normalized_positions.means.subject$X.Position, na.rm=T)))

## Plot subjects
# ggplot(normalized_positions.means.subject, aes(x=Time.Step.Onset, y=X.Position.mean, color=Subject, group=Subject)) + 
#   geom_point(size=0.5) + geom_line() +
#   expand_limits(x=c(-1.5,1.5)) + theme_minimal()+geom_vline(aes(xintercept=0))+
#   theme(legend.position = "none") + facet_grid(Polarity~.) 
# ggsave('calibration_mean_subject_XPosition.png', plot = last_plot(), scale = 1, dpi = 300,width = 10, path='R_scripts/graphs/calibration')

ggplot(subset(normalized_positions.means, Polarity!='uncertain'), aes(x=Time.Step.Onset, y=X.Position.mean, color=Polarity, group=Polarity)) + 
  geom_point(alpha=.6) + geom_line(alpha=.6) + theme_minimal()+ theme(legend.position = "none") +
  ggtitle('Calibration Mean X-Position Onset at Change point') + geom_vline(aes(xintercept=0)) +  scale_colour_brewer(palette="Set1") +
  geom_errorbar(aes(ymin=X.Position.mean-X.Position.se, ymax=X.Position.mean+X.Position.se), width=.1, alpha=.4) 
ggsave('calibration_mean_XPosition.png', plot = last_plot(), scale = 1, dpi = 300,width = 10, path='R_scripts/graphs/calibration')








## Mean Trajectory
normalized_positions.means.subject <- ddply(subset(normalized_positions.plot, Polarity!='uncertain'), c("Polarity", "Time.Step", "Subject"),
                                            function(normalized_positions.plot)c(X.Position.mean=mean(normalized_positions.plot$X.Position, na.rm=T), 
                                                                                 Y.Position.mean=mean(normalized_positions.plot$Y.Position, na.rm=T)))

normalized_positions.means.traj <- ddply(normalized_positions.means.subject, c("Polarity", "Time.Step"),
                                         function(normalized_positions.means.subject)c(X.Position.mean=mean(normalized_positions.means.subject$X.Position, na.rm=T), 
                                                                                       X.Position.se=se(normalized_positions.means.subject$X.Position, na.rm=T),
                                                                                       Y.Position.mean=mean(normalized_positions.means.subject$Y.Position, na.rm=T), 
                                                                                       Y.Position.se=se(normalized_positions.means.subject$Y.Position, na.rm=T)))
# 
# ggplot(normalized_positions.means.subject, aes(x=X.Position.mean, y=Y.Position.mean, color=Subject, group=Subject)) +
#   geom_point(size=0.5) + 
#   ggtitle('Mean Trajectories per subject') +
#   theme_minimal() +
#   theme(legend.position = "none") + 
#   expand_limits(x=c(-1.5,1.5)) + 
#   facet_grid(Polarity~.)
# ggsave('calibration_mean_subject_trajectory.png', plot = last_plot(), scale = 1, dpi = 300, width = 6, height = 6, path='R_scripts/graphs/calibration')


ggplot(normalized_positions.means.traj, aes(x=X.Position.mean, y=Y.Position.mean, color=Polarity, group=Polarity)) + 
  geom_point(alpha=.6, size=1) + 
  ggtitle('')+
  geom_errorbarh(aes(xmin=X.Position.mean-X.Position.se, xmax=X.Position.mean+X.Position.se)) + 
  theme_minimal() +
  theme(legend.position = "top") +
  expand_limits(x=c(-1.5,1.5)) + 
  scale_colour_brewer(palette="Set1") 
ggsave('calibration_mean_trajectory.png', plot = last_plot(), scale = 1, dpi = 300, width = 6, height = 6,  path='R_scripts/graphs/calibration')


##Mean trajectory per point change 

normalized_positions.means.subject <- ddply(subset(normalized_positions.plot, Polarity!='uncertain'), c("PointChange", "Time.Step", "Subject"),
                                            function(normalized_positions.plot)c(X.Position.mean=mean(normalized_positions.plot$X.Position, na.rm=T), 
                                                                                 Y.Position.mean=mean(normalized_positions.plot$Y.Position, na.rm=T)))

normalized_positions.means.traj <- ddply(normalized_positions.means.subject, c("PointChange", "Time.Step"),
                                         function(normalized_positions.means.subject)c(X.Position.mean=mean(normalized_positions.means.subject$X.Position, na.rm=T), 
                                                                                       X.Position.se=se(normalized_positions.means.subject$X.Position, na.rm=T),
                                                                                       Y.Position.mean=mean(normalized_positions.means.subject$Y.Position, na.rm=T), 
                                                                                       Y.Position.se=se(normalized_positions.means.subject$Y.Position, na.rm=T)))

normalized_positions.means.traj$PointChange <- factor(normalized_positions.means.traj$PointChange)

myPalette <- c("#000099",  "#FF3333", "#CC0000", "#990000")

ggplot(normalized_positions.means.traj, aes(x=X.Position.mean, y=Y.Position.mean, color=PointChange, group=PointChange)) + 
  geom_point(alpha=.4, size=1) + 
  ggtitle('')+
  xlab('X Coordinate') +
  ylab('Y Coordinate') +
  geom_errorbarh(aes(xmin=X.Position.mean-X.Position.se, xmax=X.Position.mean+X.Position.se), alpha=0.3) + 
  theme_minimal() +
  expand_limits(x=c(-1.5,1.5)) + 
  scale_colour_manual(values=myPalette,
                      name="Decision point",
                      breaks=c("0", "0.4", "0.7", "0.9"),
                      labels=c("Straight", "Early (y=0.4)", "Middle (y=0.7)", "Late (y=0.9)")) 
ggsave('calibration_mean_trajectory_pointchang.png', plot = last_plot(), scale = 1, dpi = 300, width = 6, height = 5,  path='R_scripts/graphs/calibration')



### Mean X position per point change
normalized_positions.means.subject <-   ddply(normalized_positions.plot, c("Time.Step.Onset", "Subject", "PointChange"),
                                              function(normalized_positions.plot)c(X.Position.mean=mean(normalized_positions.plot$X.Position, na.rm=T)))

normalized_positions.means <- ddply(normalized_positions.means.subject, c("Time.Step.Onset", "PointChange"),
                                    function(normalized_positions.means.subject)c(X.Position.mean=mean(normalized_positions.means.subject$X.Position, na.rm=T), X.Position.se=se(normalized_positions.means.subject$X.Position, na.rm=T)))

normalized_positions.means$PointChange <- factor(normalized_positions.means$PointChange)

myPalette <- c("#000099",  "#FF3333", "#CC0000", "#990000")



ggplot(normalized_positions.means, aes(x=Time.Step.Onset, y=X.Position.mean, color=PointChange, group=PointChange)) + 
  geom_point(alpha=.4) + geom_line(alpha=.6) + theme_minimal()+ 
  ggtitle('Calibration Mean X-Position Onset at Change point') + geom_vline(aes(xintercept=0)) +  
  scale_colour_manual(values=myPalette,
                      name="Decision point",
                      breaks=c("0", "0.4", "0.7", "0.9"),
                      labels=c("Straight", "Early (y=0.4)", "Middle (y=0.7)", "Late (y=0.9)")) +
  geom_errorbar(aes(ymin=X.Position.mean-X.Position.se, ymax=X.Position.mean+X.Position.se), width=.1, alpha=.4)

ggsave('calibration_mean_XPosition_CP.png', plot = last_plot(), scale = 1, dpi = 300,width = 10, path='R_scripts/graphs/calibration')



#### LDA CLASSIFIER ####

#LDA coords + delta + deltadelta based on coordinates
LDA_training.coord.dist(calibration_data)
save(m_pca, v_lda, b_lda, n_pca, all_data_columns, file="LDA-Full.RData")
calibration_data <- dplyr::full_join(lda_measure.df, calibration_data, by=c("Subject", "Item.number", "Expected_response"))
normalized_positions.plot <- dplyr::full_join(lda_measure.df, normalized_positions.plot, by=c("Subject", "Item.number", "Expected_response"))

ggplot(calibration_data, aes(x=lda_measure_full, fill=Polarity)) + geom_density(alpha=.3) + facet_grid(.~Expected_response)
ggsave('try.png')

#Plots
png(filename='R_scripts/graphs/calibration/LDA-Distribution.png', width = 7, height = 7, units = 'in', res = 300)
plot_measure(calibration_data, "lda_measure_full", "Polarity")
dev.off()

#LDA coords
LDA_training.coord(calibration_data)
save(m_pca, v_lda, b_lda, n_pca, all_data_columns, file="LDA-Coords.RData")
calibration_data <- dplyr::full_join(lda_measure.df, calibration_data, by=c("Subject", "Item.number", "Expected_response"))
normalized_positions.plot <- dplyr::full_join(lda_measure.df, normalized_positions.plot, by=c("Subject", "Item.number", "Expected_response"))

#Plots
png(filename='R_scripts/graphs/calibration/LDA-Distribution-coords.png', width = 7, height = 7, units = 'in', res = 300)
plot_measure(calibration_data, "lda_measure_coords", "Polarity")
dev.off()


#Taking LDA measure to cut
normalized_positions.plot$lda_measure_full_cut <- cut(normalized_positions.plot$lda_measure_full, 5)
calibration_data$lda_measure_full_cut <- cut(calibration_data$lda_measure_full, 5)


#LDA coords + time
LDA_training.coord.time(calibration_data)
save(m_pca, v_lda, b_lda, n_pca, all_data_columns, file="LDA-Coords-Time.RData")


## CLASSIFIER PERFORMANCE

#Preparing bins for crossvalidation
calibration_data$id <- 1:nrow(calibration_data)

deviated = calibration_data %>% 
  filter(Polarity=='deviated')%>%
  dplyr::select(id)
deviated$id <- sample(deviated$id) 

straight = calibration_data %>% 
  filter(Polarity=='straight')%>%
  dplyr::select(id)
straight$id <- sample(straight$id) 


bins  <- rep(1:10, nrow(straight) / 10)
try <- split(straight, bins)

bin1 <- try$`1`
bin2 <- try$`2`
bin3 <- try$`3`
bin4 <- try$`4`
bin5 <- try$`5`
bin6 <- try$`6`
bin7 <- try$`7`
bin8 <- try$`8`
bin9 <- try$`9`
bin10 <- try$`10`

bins  <- rep(1:10, nrow(deviated) / 10)
try <- split(deviated, bins)
bin1 <- rbind(bin1, try$`1`)
bin2 <- rbind(bin2, try$`2`)
bin3 <- rbind(bin3, try$`3`)
bin4 <- rbind(bin4, try$`4`)
bin5 <- rbind(bin5, try$`5`)
bin6 <- rbind(bin6, try$`6`)
bin7 <- rbind(bin7, try$`7`)
bin8 <- rbind(bin8, try$`8`)
bin9 <- rbind(bin9, try$`9`)
bin10 <- rbind(bin10, try$`10`)
rm(try)
bins <- list(bin1, bin2, bin3, bin4, bin5, bin6, bin7, bin8, bin9, bin10)
x <- paste0('x', sprintf("%03d", c(1:101)))
y <- paste0('y', sprintf("%03d", c(1:101)))
auc.bins <- data.frame(bins = c(1:10),
                       lda.full=c(1:10),
                       lda.vel.acc=c(1:10),
                       lda.coord.vel=c(1:10), 
                       lda.vel = c(1:10),
                       lda.acc=c(1:10), 
                       lda.coord=c(1:10),
                       lda.coord.delta.deltadelta=c(1:10),
                       lda.coord.delta=c(1:10),
                       lda.delta=c(1:10),
                       lda.deltadelta=c(1:10),
                       lda.logratio=c(1:10), 
                       logratio=c(1:10), 
                       xflips=c(1:10), 
                       maxdeviation=c(1:10),
                       accflips=c(1:10), 
                       topline=c(1:10))


#Testing classifier per bin and obtaining ROC and AUC
for (b in 1: length(bins)) {
  calibrationTrain <- subset(calibration_data, !(id %in% bins[[b]]$id))
  calibrationTest <- subset(calibration_data, id %in% bins[[b]]$id)

  ## LDA with Coordinates, Delta, DeltaDelta
  ###  TRAINING + TEST
  LDA_training.coord.dist(calibrationTrain)
  LDA_test.coord.dist(calibrationTest, v_lda, b_lda, m_pca, all_data_columns, n_pca)
  
  #ROC and AUC
  lda.score.te <- lda_measure_te.df$lda_measure 
  lda.label.te <- lda_measure_te.df$Deviation
  lda.roc.te <- roc(lda.label.te, lda.score.te)
  auc.bins$lda.full[b] <- lda.roc.te$auc
  assign(paste0('lda_full.roc.te',b), lda.roc.te)
}

#plot ROC and aUC
png(filename='R_scripts/graphs/calibration/ROC:LDA-FULL.png', width = 7, height = 7, units = 'in', res = 300)
plot.roc(smooth(lda_full.roc.te1), print.auc = FALSE, col="red", main='Original Linear Discriminant Analysis (Coord, Vel, Acc)')
plot.roc(smooth(lda_full.roc.te2), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_full.roc.te3), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_full.roc.te4), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_full.roc.te5), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_full.roc.te6), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_full.roc.te7), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_full.roc.te8), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_full.roc.te9), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_full.roc.te10), print.auc = FALSE, col="red", add=TRUE)
text(0.5, 0, paste("MEAN AUC=", round(mean(auc.bins$lda.full), digits=3)),
     cex = 1)
dev.off()

rm(lda_full.roc.te1,lda_full.roc.te10,lda_full.roc.te2,lda_full.roc.te3,lda_full.roc.te4,lda_full.roc.te5,lda_full.roc.te6, lda_full.roc.te7,lda_full.roc.te8,lda_full.roc.te9)

#Topline and Random classifiers
source("R_scripts/calibration/topline.R")
source("R_scripts/calibration/random_classifier.R")
auc.bins$random_classifier <- rowMeans(random_classifier.df)

