## 2.2.1 Calibration results: Comparison with other measures

## plots
png(filename='R_scripts/graphs/calibration/MaxLogRatio.png', width = 8, height = 6, units = 'in', res = 300)
plot_measure(calibration_data, "MaxLogRatio", "Polarity")
dev.off()

png(filename='R_scripts/graphs/calibration/MaxDeviation.png', width = 8, height = 6, units = 'in', res = 300)
plot_measure(calibration_data, "MaxDeviation", "Polarity")
dev.off()

png(filename='R_scripts/graphs/calibration/AccFlips.png', width = 8, height = 6, units = 'in', res = 300)
plot_measure(calibration_data, "Acc.flips", "Polarity")
dev.off()

png(filename='R_scripts/graphs/calibration/XFlips.png', width = 8, height = 6, units = 'in', res = 300)
plot_measure(calibration_data, "X.flips", "Polarity")
dev.off()

png(filename='R_scripts/graphs/calibration/AUC.png', width = 8, height = 6, units = 'in', res = 300)
plot_measure(calibration_data, "AUC", "Polarity")
dev.off()

## Correlation between measures
calibration_data.corr = calibration_data %>%
  dplyr::select(MaxLogRatio, MaxDeviation, X.flips, Acc.flips, lda_measure_full)

# Pearson by default
calibration_corr.matrix = cor(calibration_data.corr)
round(calibration_corr.matrix, 2)

#plotcorr(calibration_corr.matrix, mar = c(0.1, 0.1, 0.1, 0.1))

ggpairs(calibration_data.corr)
ggsave('correlations_measures.png', plot = last_plot(), scale = 1, dpi = 300,width = 10, path='R_scripts/graphs/calibration')


## Cross validation
for (b in 1: length(bins)) {
  calibrationTrain <- subset(calibration_data, !(id %in% bins[[b]]$id))
  calibrationTest <- subset(calibration_data, id %in% bins[[b]]$id)

  #AccFlips
  accflips.score.te <- calibrationTest$Acc.flips 
  accflips.label.te <- factor(calibrationTest$Polarity)
  accflips.roc.te <- roc(accflips.label.te, accflips.score.te)
  auc.bins$accflips[b] <- accflips.roc.te$auc
  assign(paste0('accflips.roc.te',b), accflips.roc.te)
    
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

#MAX LOG RATIO
png(filename='R_scripts/graphs/calibration/ROC:MaxLogRatio.png', width = 7, height = 7, units = 'in', res = 300)
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
rm(maxlogratio.roc.te1, maxlogratio.roc.te2, maxlogratio.roc.te3, maxlogratio.roc.te4,maxlogratio.roc.te5,maxlogratio.roc.te6,maxlogratio.roc.te7,maxlogratio.roc.te8,maxlogratio.roc.te9,maxlogratio.roc.te10)

#X FLIPS
png(filename='R_scripts/graphs/calibration/ROC:XFlips.png', width = 7, height = 7, units = 'in', res = 300)
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
rm(xflips.roc.te1,xflips.roc.te2,  xflips.roc.te3, xflips.roc.te4, xflips.roc.te5, xflips.roc.te6,xflips.roc.te7, xflips.roc.te8,xflips.roc.te9, xflips.roc.te10)


#AUC
png(filename='R_scripts/graphs/calibration/ROC:AUC.png', width = 7, height = 7, units = 'in', res = 300)
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
rm(auc.roc.te1, auc.roc.te2,  auc.roc.te3, auc.roc.te4, auc.roc.te5, auc.roc.te6, auc.roc.te7, auc.roc.te8, auc.roc.te9, auc.roc.te10)


#ACC FLIPS
png(filename='R_scripts/graphs/calibration/ROC:AccFlips.png', width = 7, height = 7, units = 'in', res = 300)
plot.roc(smooth(accflips.roc.te1), print.auc = FALSE, col="red", main='AUC')
plot.roc(smooth(accflips.roc.te2), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(accflips.roc.te3), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(accflips.roc.te4), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(accflips.roc.te5), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(accflips.roc.te6), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(accflips.roc.te7), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(accflips.roc.te8), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(accflips.roc.te9), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(accflips.roc.te10), print.auc = FALSE, col="red", add=TRUE)
text(0.5, 0, paste("MEAN AUC=", round(mean(auc.bins$accflips), digits=3)),
     cex = .8)
dev.off()
rm(accflips.roc.te1, accflips.roc.te2,  accflips.roc.te3, accflips.roc.te4, accflips.roc.te5, accflips.roc.te6, accflips.roc.te7, accflips.roc.te8, accflips.roc.te9, accflips.roc.te10)


#MAX DEVIATION
png(filename='R_scripts/graphs/calibration/ROC:MaxDeviation.png', width = 7, height = 7, units = 'in', res = 300)
plot.roc(smooth(maxdeviation.roc.te1), print.auc = FALSE, col="red", main=' ROC: maxlogratio')
plot.roc(smooth(maxdeviation.roc.te2), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(maxdeviation.roc.te3), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(maxdeviation.roc.te4), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(maxdeviation.roc.te5), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(maxdeviation.roc.te6), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(maxdeviation.roc.te7), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(maxdeviation.roc.te8), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(maxdeviation.roc.te9), print.auc = FALSE, col="red", add=TRUE)
plot.roc(smooth(maxdeviation.roc.te10), print.auc = FALSE, col="red", add=TRUE)
text(0.5, 0, paste("MEAN AUC=",round(mean(auc.bins$maxdeviation), digits=3)),
     cex = .8)
dev.off()
rm(maxdeviation.roc.te1, maxdeviation.roc.te2,maxdeviation.roc.te3, maxdeviation.roc.te4,maxdeviation.roc.te5,maxdeviation.roc.te6,maxdeviation.roc.te7,maxdeviation.roc.te8,maxdeviation.roc.te9,maxdeviation.roc.te10)


