require(MASS)
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

##loop with bins
bins <- list(bin1, bin2, bin3, bin4, bin5, bin6, bin7, bin8, bin9, bin10)
x <- paste0('x', sprintf("%03d", c(1:101)))
y <- paste0('y', sprintf("%03d", c(1:101)))
auc.bins <- data.frame(lda.coord.delta.deltadelta=c(1:10), 
                       lda.coord.delta=c(1:10), 
                       lda.coord=c(1:10), 
                       lda.coord.accdist=c(1:10), 
                       lda.logratio=c(1:10), 
                       logratio=c(1:10), 
                       xflips=c(1:10), 
                       maxdeviation=c(1:10))

#Other measures
for (b in 1: length(bins)) {
  print(b) 
  calibrationTrain <- subset(calibration_data, !(id %in% bins[[b]]$id))
  calibrationTest <- subset(calibration_data, id %in% bins[[b]]$id)
  
  #MaxLogRatio
  maxlogratio.score.te <- calibrationTest$MaxLogRatio 
  maxlogratio.label.te <- factor(calibrationTest$Polarity)
  maxlogratio.roc.te <- roc(maxlogratio.label.te, maxlogratio.score.te)
  auc.bins$logratio[b] <- maxlogratio.roc.te$auc
  assign(paste0('maxlogratio.roc.te',b), maxlogratio.roc.te)
  
  #XFlips
  xflips.score.te <- calibrationTest$X.flips 
  xflips.label.te <- factor(calibrationTest$Polarity)
  xflips.roc.te <- roc(xflips.label.te, xflips.score.te)
  auc.bins$xflips[b] <- xflips.roc.te$auc
  assign(paste0('xflips.roc.te',b), xflips.roc.te)
  
  #MaxDeviation
  maxdeviation.score.te <- calibrationTest$X.flips 
  maxdeviation.label.te <- factor(calibrationTest$Polarity)
  maxdeviation.roc.te <- roc(maxdeviation.label.te,maxdeviation.score.te)
  auc.bins$maxdeviation[b] <- maxdeviation.roc.te$auc
  assign(paste0('maxdeviation.roc.te',b), maxdeviation.roc.te)
  
  
  #AUC
  auc.score.te <- calibrationTest$AUC 
  auc.label.te <- factor(calibrationTest$Polarity)
  auc.roc.te <- roc(maxdeviation.label.te,auc.score.te)
  auc.bins$auc[b] <- auc.roc.te$auc
  assign(paste0('auc.roc.te',b), auc.roc.te)
  
}



#Different features in the classifier
for (b in 1: length(bins)) {
print(b) 
calibrationTrain <- subset(calibration_data, !(id %in% bins[[b]]$id))
calibrationTest <- subset(calibration_data, id %in% bins[[b]]$id)

## LDA with Coordinates, Delta, DeltaDelta
###  TRAINING + TEST
LDA_training.coord.delta.deltadelta(calibrationTrain)
LDA_test.coord.delta.deltadelta(calibrationTest, v_lda, b_lda, m_pca, all_data_columns, n_pca)

#ROC and AUC
lda.score.te <- lda_measure_te.df$lda_measure 
lda.label.te <- lda_measure_te.df$Deviation
lda.roc.te <- roc(lda.label.te, lda.score.te)
auc.bins$lda.coord.delta.deltadelta[b] <- lda.roc.te$auc
assign(paste0('lda_full.roc.te',b), lda.roc.te)


## LDA with Coordinates
###  TRAINING + TEST
LDA_training.coord(calibrationTrain)
LDA_test.coord(calibrationTest, v_lda, b_lda, m_pca, all_data_columns, n_pca)

#ROC and AUC
lda.score.te <- lda_measure_te.df$lda_measure 
lda.label.te <- lda_measure_te.df$Deviation
lda.roc.te <- roc(lda.label.te, lda.score.te)
auc.bins$lda.coord[b] <- lda.roc.te$auc
assign(paste0('lda_coord.roc.te',b), lda.roc.te)


## LDA with Coordinates, Acceleration based on distance
###  TRAINING + TEST
LDA_training.coord.accdist(calibrationTrain)
LDA_test.coord.accdist(calibrationTest, v_lda, b_lda, m_pca, all_data_columns, n_pca)

#ROC and AUC
lda.score.te <- lda_measure_te.df$lda_measure 
lda.label.te <- lda_measure_te.df$Deviation
lda.roc.te <- roc(lda.label.te, lda.score.te)
auc.bins$lda.coord.accdist[b] <- lda.roc.te$auc
assign(paste0('lda_coord_acc.roc.te',b), lda.roc.te)


## LDA with LogRatio
###  TRAINING + TEST
LDA_training.logratio(calibrationTrain)
LDA_test.logratio(calibrationTest, v_lda, b_lda, m_pca, all_data_columns, n_pca)

#ROC and AUC
lda.score.te <- lda_measure_te.df$lda_measure 
lda.label.te <- lda_measure_te.df$Deviation
lda.roc.te <- roc(lda.label.te, lda.score.te)
auc.bins$lda.logratio[b] <- lda.roc.te$auc
assign(paste0('lda_logratio.roc.te',b), lda.roc.te)

## LDA with DeltaDelta
###  TRAINING + TEST
LDA_training.deltadelta(calibrationTrain)
LDA_test.deltadelta(calibrationTest, v_lda, b_lda, m_pca, all_data_columns, n_pca)

#ROC and AUC
lda.score.te <- lda_measure_te.df$lda_measure 
lda.label.te <- lda_measure_te.df$Deviation
lda.roc.te <- roc(lda.label.te, lda.score.te)
auc.bins$lda.logratio[b] <- lda.roc.te$auc
assign(paste0('lda_deltadelta.roc.te',b), lda.roc.te)


}








#Plots

png(filename='ROC:AUC.png', width = 7, height = 7, units = 'in', res = 300)
plot.roc(smooth(auc.roc.te1), print.auc = FALSE, col="red", main='AUC')
plot.roc(smooth(auc.roc.te2), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(auc.roc.te3), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(auc.roc.te4), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(auc.roc.te5), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(auc.roc.te6), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(auc.roc.te7), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(auc.roc.te8), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(auc.roc.te9), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(auc.roc.te10), print.auc = FALSE, col="red", add=TRUE)
text(0.5, 0, paste("MEAN AUC=", round(mean(auc.bins$auc), digits=3)),
     cex = .8)
dev.off()


png(filename='ROC:LDA-FULL.png', width = 7, height = 7, units = 'in', res = 300)
plot.roc(smooth(lda_full.roc.te1), print.auc = FALSE, col="red", main='LDA with Coords, Delta, DeltaDelta')
plot.roc(smooth(lda_full.roc.te2), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_full.roc.te3), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_full.roc.te4), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_full.roc.te5), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_full.roc.te6), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_full.roc.te7), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_full.roc.te8), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_full.roc.te9), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_full.roc.te10), print.auc = FALSE, col="red", add=TRUE)
text(0.5, 0, paste("MEAN AUC=", round(mean(auc.bins$lda.coord.delta.deltadelta), digits=3)),
     cex = .8)
dev.off()

png(filename='ROC:LDA-Coords.png', width = 7, height = 7, units = 'in', res = 300)
plot.roc(smooth(lda_coord.roc.te1), print.auc = FALSE, col="red", main='LDA with Coordinates')
plot.roc(smooth(lda_coord.roc.te2), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord.roc.te3), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord.roc.te4), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord.roc.te5), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord.roc.te6), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord.roc.te7), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord.roc.te8), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord.roc.te9), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord.roc.te10), print.auc = FALSE, col="red", add=TRUE)
text(0.5, 0, paste("MEAN AUC=", round(mean(auc.bins$lda.coord), digits=3)),
     cex = .8)
dev.off()


png(filename='ROC:LDA-LogRatio.png', width = 7, height = 7, units = 'in', res = 300)
plot.roc(smooth(lda_logratio.roc.te1), print.auc = FALSE, col="red", main='LDA with LogRatio')
plot.roc(smooth(lda_logratio.roc.te2), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_logratio.roc.te3), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_logratio.roc.te4), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_logratio.roc.te5), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_logratio.roc.te6), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_logratio.roc.te7), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_logratio.roc.te8), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_logratio.roc.te9), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_logratio.roc.te10), print.auc = FALSE, col="red", add=TRUE)
text(0.5, 0, paste("MEAN AUC=", round(mean(auc.bins$lda.coord), digits=3)),
     cex = .8)
dev.off()


png(filename='ROC:LDA-CoordsAcc.png', width = 7, height = 7, units = 'in', res = 300)
plot.roc(smooth(lda_coord_acc.roc.te1), print.auc = FALSE, col="red", main='LDA with Coordinates and Acceleration ')
plot.roc(smooth(lda_coord_acc.roc.te2), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord_acc.roc.te3), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord_acc.roc.te4), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord_acc.roc.te5), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord_acc.roc.te6), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord_acc.roc.te7), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord_acc.roc.te8), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord_acc.roc.te9), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord_acc.roc.te10), print.auc = FALSE, col="red", add=TRUE)
text(0.5, 0, paste("MEAN AUC=", round(mean(auc.bins$lda.coord.accdist), digits=3)),
     cex = .8)
dev.off()


png(filename='ROC:LDA-DeltaDelta.png', width = 7, height = 7, units = 'in', res = 300)
plot.roc(smooth(lda_deltadelta.roc.te1), print.auc = FALSE, col="red", main='LDA with DeltaDelta ')
plot.roc(smooth(lda_deltadelta.roc.te2), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_deltadelta.roc.te3), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_deltadelta.roc.te4), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_deltadelta.roc.te5), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_deltadelta.roc.te6), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_deltadelta.roc.te7), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_deltadelta.roc.te8), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_deltadelta.roc.te9), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_deltadelta.roc.te10), print.auc = FALSE, col="red", add=TRUE)
text(0.5, 0, paste("MEAN AUC=", round(mean(auc.bins$lda.coord.accdist), digits=3)),
     cex = .8)
dev.off()


png(filename='ROC:MaxLogRatio.png', width = 7, height = 7, units = 'in', res = 300)
plot.roc(smooth(maxlogratio.roc.te1), print.auc = FALSE, col="red", main=' ROC: maxlogratio')
plot.roc(smooth(maxlogratio.roc.te2), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(maxlogratio.roc.te3), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(maxlogratio.roc.te4), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(maxlogratio.roc.te5), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(maxlogratio.roc.te6), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(maxlogratio.roc.te7), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(maxlogratio.roc.te8), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(maxlogratio.roc.te9), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(maxlogratio.roc.te10), print.auc = FALSE, col="red", add=TRUE)
text(0.5, 0, paste("MEAN AUC=",round(mean(auc.bins$logratio), digits=3)),
     cex = .8)
dev.off()

png(filename='ROC:XFlips.png', width = 7, height = 7, units = 'in', res = 300)
plot.roc(smooth(xflips.roc.te1), print.auc = FALSE, col="red", main=' ROC:XFlips')
plot.roc(smooth(xflips.roc.te2), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(xflips.roc.te3), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(xflips.roc.te4), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(xflips.roc.te5), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(xflips.roc.te6), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(xflips.roc.te7), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(xflips.roc.te8), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(xflips.roc.te9), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(xflips.roc.te10), print.auc = FALSE, col="red", add=TRUE)
text(0.5, 0, paste("MEAN AUC=",round(mean(auc.bins$xflips), digits=3)),
     cex = .8)
dev.off()







 


 