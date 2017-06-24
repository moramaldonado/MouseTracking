calibration_data.corr = calibration_data %>%
  dplyr::select(MaxLogRatio, MaxDeviation, X.flips, Acc.flips, lda_measure )

# Pearson by default
calibration_corr.matrix = cor(calibration_data.corr)
round(calibration_corr.matrix, 2)

plotcorr(calibration_corr.matrix, mar = c(0.1, 0.1, 0.1, 0.1))

ggpairs(calibration_data.corr)
ggsave('correlations_measures.png', plot = last_plot(), scale = 1, dpi = 300,width = 10, path='R_scripts/graphs/calibration_new')
