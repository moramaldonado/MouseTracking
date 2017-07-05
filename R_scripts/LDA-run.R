
library(MASS) # NB: this will mask dplyr::select

##Subset to deviated and straight trials
calibration_data = subset(calibration_data, Polarity != 'uncertain')
calibration_data$Subject <- factor(calibration_data$Subject)
normalized_positions.plot$Subject <- factor(normalized_positions.plot$Subject)

#LDA only with coords
LDA_training.coord(calibration_data)
save(m_pca, v_lda, b_lda, n_pca, all_data_columns, file="LDA(Coords).RData")
calibration_data <- dplyr::full_join(lda_measure.df, calibration_data, by=c("Subject", "Item.number", "Expected_response"))
normalized_positions.plot <- dplyr::full_join(lda_measure.df, normalized_positions.plot, by=c("Subject", "Item.number", "Expected_response"))

#LDA coords + acceleration based on distance
LDA_training.coord.accdist(calibration_data)
save(m_pca, v_lda, b_lda, n_pca, all_data_columns, file="LDA(Coord+AccDist).RData")
calibration_data <- dplyr::full_join(lda_measure.df, calibration_data, by=c("Subject", "Item.number", "Expected_response"))
normalized_positions.plot <- dplyr::full_join(lda_measure.df, normalized_positions.plot, by=c("Subject", "Item.number", "Expected_response"))

#LDA coords + delta based on coordinates
LDA_training.coord.delta(calibration_data)
save(m_pca, v_lda, b_lda, n_pca, all_data_columns, file="LDA(Coord+Delta).RData")
calibration_data <- dplyr::full_join(lda_measure.df, calibration_data, by=c("Subject", "Item.number", "Expected_response"))
normalized_positions.plot <- dplyr::full_join(lda_measure.df, normalized_positions.plot, by=c("Subject", "Item.number", "Expected_response"))

#LDA coords + delta + deltadelta based on coordinates
LDA_training.coord.delta.deltadelta(calibration_data)
save(m_pca, v_lda, b_lda, n_pca, all_data_columns, file="LDA(Coords+Delta+DeltaDelta).RData")
calibration_data <- dplyr::full_join(lda_measure.df, calibration_data, by=c("Subject", "Item.number", "Expected_response"))
normalized_positions.plot <- dplyr::full_join(lda_measure.df, normalized_positions.plot, by=c("Subject", "Item.number", "Expected_response"))

#LDA deltadelta
LDA_training.deltadelta(calibration_data)
save(m_pca, v_lda, b_lda, n_pca, all_data_columns, file="LDA(DeltaDelta).RData")
calibration_data <- dplyr::full_join(lda_measure.df, calibration_data, by=c("Subject", "Item.number", "Expected_response"))
normalized_positions.plot <- dplyr::full_join(lda_measure.df, normalized_positions.plot, by=c("Subject", "Item.number", "Expected_response"))

#LDA with LogRatio
LDA_training.logratio(calibration_data)
save(m_pca, v_lda, b_lda, n_pca, all_data_columns, file="LDA(LogRatio).RData")
calibration_data <- dplyr::full_join(lda_measure.df, calibration_data, by=c("Subject", "Item.number", "Expected_response"))
normalized_positions.plot <- dplyr::full_join(lda_measure.df, normalized_positions.plot, by=c("Subject", "Item.number", "Expected_response"))





normalized_positions.plot$lda_measure_coords_cut <- cut(normalized_positions.plot$lda_measure_coords, 5)
calibration_data$lda_measure_coords_cut <- cut(calibration_data$lda_measure_coords, 5)

normalized_positions.plot$lda_measure_coord_delta_cut <- cut(normalized_positions.plot$lda_measure_coord_delta, 5)
calibration_data$lda_measure_coord_delta_cut <- cut(calibration_data$lda_measure_coord_delta, 5)

normalized_positions.plot$lda_measure_coord.acc_cut <- cut(normalized_positions.plot$lda_measure_coord.acc, 5)
calibration_data$lda_measure_coord.acc_cut <- cut(calibration_data$lda_measure_coord.acc, 5)

normalized_positions.plot$lda_measure_logratio_cut <- cut(normalized_positions.plot$lda_measure_logratio, 5)
calibration_data$lda_measure_logratio_cut <- cut(calibration_data$lda_measure_logratio, 5)

normalized_positions.plot$lda_measure_full_cut <- cut(normalized_positions.plot$lda_measure_full, 5)
calibration_data$lda_measure_full_cut <- cut(calibration_data$lda_measure_full, 5)

