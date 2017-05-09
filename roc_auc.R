


#LDA
lda.score <- calibration_data$lda_measure 
lda.label <- calibration_data$Deviation
lda.roc <- roc(lda.label, lda.score)

#MaxRatio
logratio.score <- calibration_data$MaxLogRatio
logratio.label <- calibration_data$Deviation
logratio.roc <- roc(logratio.label, logratio.score)


#Plots
png(filename='LDA vs. LogRatio', width=950, height=500, units='px', pointsize = 14)
par(mfrow=c(1,2))
plot.roc(lda.roc)
plot.roc(smooth(lda.roc), print.auc = TRUE, col="red", add=TRUE, main='lda')
legend("bottomright", legend=c("Empirical", "Smoothed"),
       col=c(par("fg"), "red"), lwd=2)
title("ROC: LDA \n (trained: new calibration, tested: new calibration)", line=2, cex.main=1)
plot.roc(logratio.roc)
plot.roc(smooth(logratio.roc), print.auc = TRUE, col="blue", add=TRUE)
legend("bottomright", legend=c("Empirical", "Smoothed"),
       col=c(par("fg"), "blue"), lwd=2)
title("ROC: MaxLogRatio", line=2.5, cex.main=1)
dev.off()


## Train= new calibration data, test=old calibration data
load('transformation_all.RData')
normalized_positions_te <- normalized_positions_validation %>%
  dplyr::select(Subject, Item.number, Polarity, Expected_response, Deviation, one_of(all_data_columns))
normalized_positions_te_pca <- bind_cols(normalized_positions_te,as.data.frame(predict(m_pca, normalized_positions_te)[,1:n_pca]))

lda_measure_te.df <- data_frame(
  lda_measure=c(as.matrix(dplyr::select(normalized_positions_te_pca, starts_with("PC"))) %*% v_lda- b_lda),
  Deviation=normalized_positions_te$Deviation,
  Subject = normalized_positions_te$Subject,
  Expected_response = normalized_positions_te$Expected_response,
  Item.number = normalized_positions_te$Item.number)

lda.score.te <- lda_measure_te.df$lda_measure 
lda.label.te <- lda_measure_te.df$Deviation
lda.roc.te <- roc(lda.label.te, lda.score.te)


## Train= old calibration data, test=new calibration data
load('OLD_transformation_all.RData')
normalized_positions_te <- normalized_positions %>%
  dplyr::select(Subject, Item.number, Polarity, Expected_response, Deviation, one_of(all_data_columns))
normalized_positions_te_pca <- bind_cols(normalized_positions_te,as.data.frame(predict(m_pca, normalized_positions_te)[,1:n_pca]))

 lda_measure_te.df_2 <- data_frame(
  lda_measure=c(as.matrix(dplyr::select(normalized_positions_te_pca, starts_with("PC"))) %*% v_lda- b_lda),
  Deviation=normalized_positions_te$Deviation,
  Subject = normalized_positions_te$Subject,
  Expected_response = normalized_positions_te$Expected_response,
  Item.number = normalized_positions_te$Item.number)

 lda.score.te_2 <- lda_measure_te.df_2$lda_measure 
 lda.label.te_2 <- lda_measure_te.df_2$Deviation
 lda.roc.te_2 <- roc(lda.label.te_2, lda.score.te_2)
 
 
 png(filename='LDA.png', width = 6, height = 3, units = 'in', res = 300)
 par(mfrow=c(1,3))
 plot.roc(lda.roc)
 plot.roc(smooth(lda.roc), print.auc = TRUE, col="red", add=TRUE, main='lda')
 legend("bottomright", legend=c("Empirical", "Smoothed"),
        col=c(par("fg"), "red"), lwd=2, cex=.5)
 title("trained: new calibration, \n tested: new calibration", line=2.5, cex.main=.7)
 
 plot.roc(lda.roc.te_2)
 plot.roc(smooth(lda.roc.te_2), print.auc = TRUE, col="green", add=TRUE, main='lda')
 legend("bottomright", legend=c("Empirical", "Smoothed"),
        col=c(par("fg"), "green"), lwd=2, cex=.5)
 title("trained: old calibration, \n tested: new calibration", line=2.5, cex.main=.7)
 
 plot.roc(lda.roc.te)
 plot.roc(smooth(lda.roc.te), print.auc = TRUE, col="blue", add=TRUE, main='lda')
 legend("bottomright", legend=c("Empirical", "Smoothed"),
        col=c(par("fg"), "blue"), lwd=2, cex=.5)
 title("trained: new calibration, \n tested: old calibration", line=2.5, cex.main=.7)
  dev.off()

 
 
 #Train with part of the data, test with another part of the data 
 
 
 