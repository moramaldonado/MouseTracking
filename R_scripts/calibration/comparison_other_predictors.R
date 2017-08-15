##2.2.3 Other predictors for the LDA
## Training and testing LDA with different predictors
#Different features in the classifier

for (b in 1: length(bins)) {
  calibrationTrain <- subset(calibration_data, !(id %in% bins[[b]]$id))
  calibrationTest <- subset(calibration_data, id %in% bins[[b]]$id)

    ## LDA *only* with Coordinates
      ###  TRAINING + TEST
      LDA_training.coord(calibrationTrain)
      LDA_test.coord(calibrationTest, v_lda, b_lda, m_pca, all_data_columns, n_pca)
      
      #ROC and AUC
      lda.score.te <- lda_measure_te.df$lda_measure 
      lda.label.te <- lda_measure_te.df$Deviation
      lda.roc.te <- roc(lda.label.te, lda.score.te)
      auc.bins$lda.coord[b] <- lda.roc.te$auc
      assign(paste0('lda_coord.roc.te',b), lda.roc.te)
  
    ## LDA with Coordinates and Delta
      ###  TRAINING + TEST
      LDA_training.coord.delta(calibrationTrain)
      LDA_test.coord.delta(calibrationTest, v_lda, b_lda, m_pca, all_data_columns, n_pca)
      
      #ROC and AUC
      lda.score.te <- lda_measure_te.df$lda_measure 
      lda.label.te <- lda_measure_te.df$Deviation
      lda.roc.te <- roc(lda.label.te, lda.score.te)
      auc.bins$lda.coord.delta[b] <- lda.roc.te$auc
      assign(paste0('lda_coord.delta.roc.te',b), lda.roc.te)

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
    auc.bins$lda.deltadelta[b] <- lda.roc.te$auc
    assign(paste0('lda_deltadelta.roc.te',b), lda.roc.te)



  

  
}


#Plots
png(filename='R_scripts/graphs/calibration/ROC:LDA-Coords.png', width = 7, height = 7, units = 'in', res = 300)
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
rm(lda_coord.roc.te1,lda_coord.roc.te2,lda_coord.roc.te10,lda_coord.roc.te3, lda_coord.roc.te4, lda_coord.roc.te5,lda_coord.roc.te6,lda_coord.roc.te7,lda_coord.roc.te8,lda_coord.roc.te9)



png(filename='R_scripts/graphs/calibration/ROC:LDA-CoordsDelta.png', width = 7, height = 7, units = 'in', res = 300)
plot.roc(smooth(lda_coord.delta.roc.te1), print.auc = FALSE, col="red", main='LDA with Coordinates')
plot.roc(smooth(lda_coord.delta.roc.te2), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord.delta.roc.te3), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord.delta.roc.te4), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord.delta.roc.te5), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord.delta.roc.te6), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord.delta.roc.te7), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord.delta.roc.te8), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord.delta.roc.te9), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord.delta.roc.te10), print.auc = FALSE, col="red", add=TRUE)
text(0.5, 0, paste("MEAN AUC=", round(mean(auc.bins$lda.coord.delta), digits=3)),
     cex = .8)
dev.off()
rm(lda_coord.delta.roc.te1,lda_coord.delta.roc.te2,lda_coord.delta.roc.te3,lda_coord.delta.roc.te4,lda_coord.delta.roc.te5,lda_coord.delta.roc.te6,lda_coord.delta.roc.te7,lda_coord.delta.roc.te8,lda_coord.delta.roc.te9,lda_coord.delta.roc.te10)


png(filename='R_scripts/graphs/calibration/ROC:LDA-LogRatio.png', width = 7, height = 7, units = 'in', res = 300)
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
rm(lda_logratio.roc.te1,lda_logratio.roc.te2,lda_logratio.roc.te3,lda_logratio.roc.te4,lda_logratio.roc.te5,lda_logratio.roc.te6,lda_logratio.roc.te7,lda_logratio.roc.te8,lda_logratio.roc.te9,lda_logratio.roc.te10)


png(filename='R_scripts/graphs/calibration/ROC:LDA-CoordsAcc.png', width = 7, height = 7, units = 'in', res = 300)
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
rm(lda_coord_acc.roc.te1,lda_coord_acc.roc.te2,lda_coord_acc.roc.te3,lda_coord_acc.roc.te4,lda_coord_acc.roc.te5,lda_coord_acc.roc.te6,lda_coord_acc.roc.te7,lda_coord_acc.roc.te8,lda_coord_acc.roc.te9,lda_coord_acc.roc.te10)


png(filename='R_scripts/graphs/calibration/ROC:LDA-DeltaDelta.png', width = 7, height = 7, units = 'in', res = 300)
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
text(0.5, 0, paste("MEAN AUC=", round(mean(auc.bins$lda.deltadelta), digits=3)),
     cex = .8)
dev.off()
rm(lda_deltadelta.roc.te1,lda_deltadelta.roc.te2,lda_deltadelta.roc.te3,lda_deltadelta.roc.te4,lda_deltadelta.roc.te5,lda_deltadelta.roc.te6,lda_deltadelta.roc.te7,lda_deltadelta.roc.te8,lda_deltadelta.roc.te9,lda_deltadelta.roc.te10)













