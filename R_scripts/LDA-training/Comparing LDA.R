##PLOTTING

#LDA(s)
plot_measure(calibration_data, "lda_measure_full", "Polarity")
plot_measure(calibration_data, "lda_measure_logratio", "Polarity")
plot_measure(calibration_data, "lda_measure_deltadelta", "Polarity")
plot_measure(calibration_data, "lda_measure_coords", "Polarity")
plot_measure(calibration_data, "lda_measure_coord.acc", "Polarity")

#Other standard measures
plot_measure(calibration_data, "MaxLogRatio", "Polarity")
plot_measure(calibration_data, "MaxDeviation", "Polarity")
plot_measure(calibration_data, "Acc.flips", "Polarity")
plot_measure(calibration_data, "X.flips", "Polarity")
plot_measure(calibration_data, "AUC", "Polarity")
plot_measure(calibration_data, "auc_2", "Polarity")
plot_measure(calibration_data, "Xflips_2", "Polarity")


