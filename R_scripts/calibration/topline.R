library(MASS) # NB: this will mask dplyr::select

##TOPLINE (the best we can do)
calibrationTrain <- calibration_data
calibrationTest <- calibration_data

for (b in 1: length(bins)) {
  calibrationTrain <- subset(calibration_data, !(id %in% bins[[b]]$id))
  calibrationTest <- subset(calibration_data, !(id %in% bins[[b]]$id)) #NB: Same data for training and testing

  ###  TRAINING + TEST
  LDA_training.coord.dist(calibrationTrain)
  LDA_test.coord.dist(calibrationTest, v_lda, b_lda, m_pca, all_data_columns, n_pca)
  
  lda.score.te <- lda_measure_te.df$lda_measure 
  lda.label.te <- lda_measure_te.df$Deviation
  lda.roc.topline <- roc(lda.label.te, lda.score.te)
  lda.topline.auc <- lda.roc.topline$auc

  auc.bins$topline[b] <- lda.roc.topline$auc
  #assign(paste0('lda_full.roc.te',b), lda.roc.te)
}