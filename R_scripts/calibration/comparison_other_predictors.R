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
      
    ## LDA with Coordinates, Velocity
      ###  TRAINING + TEST
      LDA_training.coord.vel(calibrationTrain)
      LDA_test.coord.vel(calibrationTest, v_lda, b_lda, m_pca, all_data_columns, n_pca)
      
      #ROC and AUC
      lda.score.te <- lda_measure_te.df$lda_measure
      lda.label.te <- lda_measure_te.df$Deviation
      lda.roc.te <- roc(lda.label.te, lda.score.te)
      auc.bins$lda.coord.vel[b] <- lda.roc.te$auc
      assign(paste0('lda_coord_vel.roc.te',b), lda.roc.te)
    
    ## LDA with Acceleration
      ###  TRAINING + TEST
      LDA_training.dist.acc(calibrationTrain)
      LDA_test.dist.acc(calibrationTest, v_lda, b_lda, m_pca, all_data_columns, n_pca)
      
      #ROC and AUC
      lda.score.te <- lda_measure_te.df$lda_measure
      lda.label.te <- lda_measure_te.df$Deviation
      lda.roc.te <- roc(lda.label.te, lda.score.te)
      auc.bins$lda.acc[b] <- lda.roc.te$auc
      assign(paste0('lda_acc.roc.te',b), lda.roc.te)
      
    ## LDA with Partial derivatives (Coords, Delta, DeltaDelta)
      ###  TRAINING + TEST
      LDA_training.coord.delta.deltadelta(calibrationTrain)
      LDA_test.coord.delta.deltadelta(calibrationTest, v_lda, b_lda, m_pca, all_data_columns, n_pca)
      
      #ROC and AUC
      lda.score.te <- lda_measure_te.df$lda_measure
      lda.label.te <- lda_measure_te.df$Deviation
      lda.roc.te <- roc(lda.label.te, lda.score.te)
      auc.bins$lda.coord.delta.deltadelta[b] <- lda.roc.te$auc
      assign(paste0('lda_coord.delta.deltadelta.roc.te',b), lda.roc.te)
      
      
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
#Only coords
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

# Coords + Velocity
png(filename='R_scripts/graphs/calibration/ROC:LDA-CoordsVelocity.png', width = 7, height = 7, units = 'in', res = 300)
plot.roc(smooth(lda_coord_vel.roc.te1), print.auc = FALSE, col="red", main='LDA with Coordinates+Velocity')
plot.roc(smooth(lda_coord_vel.roc.te2), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord_vel.roc.te3), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord_vel.roc.te4), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord_vel.roc.te5), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord_vel.roc.te6), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord_vel.roc.te7), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord_vel.roc.te8), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord_vel.roc.te9), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord_vel.roc.te10), print.auc = FALSE, col="red", add=TRUE)
text(0.5, 0, paste("MEAN AUC=", round(mean(auc.bins$lda.coord.vel), digits=3)),
     cex = .8)
dev.off()
rm(lda_coord_vel.roc.te1,lda_coord_vel.roc.te2,lda_coord_vel.roc.te10,lda_coord_vel.roc.te3, lda_coord_vel.roc.te4, lda_coord_vel.roc.te5,lda_coord_vel.roc.te6,lda_coord_vel.roc.te7,lda_coord_vel.roc.te8,lda_coord_vel.roc.te9)


#Acceleration
png(filename='R_scripts/graphs/calibration/ROC:LDA-Acceleration.png', width = 7, height = 7, units = 'in', res = 300)
plot.roc(smooth(lda_acc.roc.te1), print.auc = FALSE, col="red", main='LDA with Acceleration ')
plot.roc(smooth(lda_acc.roc.te2), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_acc.roc.te3), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_acc.roc.te4), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_acc.roc.te5), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_acc.roc.te6), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_acc.roc.te7), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_acc.roc.te8), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_acc.roc.te9), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_acc.roc.te10), print.auc = FALSE, col="red", add=TRUE)
text(0.5, 0, paste("MEAN AUC=", round(mean(auc.bins$lda.acc), digits=3)),
     cex = .8)
dev.off()
rm(lda_acc.roc.te1,lda_acc.roc.te2,lda_acc.roc.te3,lda_acc.roc.te4,lda_acc.roc.te5,lda_acc.roc.te6,lda_acc.roc.te7,lda_acc.roc.te8,lda_acc.roc.te9,lda_acc.roc.te10)


# Coords + Delta + DeltaDelta
png(filename='R_scripts/graphs/calibration/ROC:LDA-CoordsDeltaDeltaDelta.png', width = 7, height = 7, units = 'in', res = 300)
plot.roc(smooth(lda_coord.delta.deltadelta.roc.te1), print.auc = FALSE, col="red", main='LDA with Coordinates+Partial derivatives')
plot.roc(smooth(lda_coord.delta.deltadelta.roc.te2), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord.delta.deltadelta.roc.te3), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord.delta.deltadelta.roc.te4), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord.delta.deltadelta.roc.te5), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord.delta.deltadelta.roc.te6), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord.delta.deltadelta.roc.te7), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord.delta.deltadelta.roc.te8), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord.delta.deltadelta.roc.te9), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(lda_coord.delta.deltadelta.roc.te10), print.auc = FALSE, col="red", add=TRUE)
text(0.5, 0, paste("MEAN AUC=", round(mean(auc.bins$lda.coord.delta), digits=3)),
     cex = .8)
dev.off()
rm(lda_coord.delta.deltadelta.roc.te1,lda_coord.delta.deltadelta.roc.te2,lda_coord.delta.deltadelta.roc.te3,lda_coord.delta.deltadelta.roc.te4,lda_coord.delta.deltadelta.roc.te5,lda_coord.delta.deltadelta.roc.te6,lda_coord.delta.deltadelta.roc.te7,lda_coord.delta.deltadelta.roc.te8,lda_coord.delta.deltadelta.roc.te9,lda_coord.delta.deltadelta.roc.te10)




# Coords + Delta
png(filename='R_scripts/graphs/calibration/ROC:LDA-CoordsDelta.png', width = 7, height = 7, units = 'in', res = 300)
plot.roc(smooth(lda_coord.delta.roc.te1), print.auc = FALSE, col="red", main='LDA with Coordinates+Delta')
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

# LogRatio
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


# DeltaDelta
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













