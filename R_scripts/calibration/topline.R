library(MASS) # NB: this will mask dplyr::select

##TOPLINE (the best we can do)
calibrationTrain <- calibration_data
calibrationTest <- calibration_data

###  TRAINING + TEST
LDA_training.coord.dist(calibrationTrain)
LDA_test.coord.dist(calibrationTest, v_lda, b_lda, m_pca, all_data_columns, n_pca)

lda.score.te <- lda_measure_te.df$lda_measure 
lda.label.te <- lda_measure_te.df$Deviation
lda.roc.topline <- roc(lda.label.te, lda.score.te)
lda.topline.auc <- lda.roc.topline$auc

