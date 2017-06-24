library(MASS) # NB: this will mask dplyr::select

##Subset to deviated and straight trials
calibration_data = subset(calibration_data, Polarity != 'uncertain')

### ORDERING DATA
r <- paste0('r', sprintf("%03d", c(1:101)))

# Each x and y coordenate into two columns (101 coordenates per trial) 
normalized_positions = calibration_data %>%
  dplyr::select(Subject, Item.number, Polarity, Expected_response, LogRatio) %>%
  separate(LogRatio, into= r, sep = ",")

normalized_positions[r] <- sapply(normalized_positions[r],as.numeric)

#last arrangements
normalized_positions$Subject <- factor(normalized_positions$Subject)
normalized_positions$Polarity <- factor(normalized_positions$Polarity)
normalized_positions$Expected_response <- factor(normalized_positions$Expected_response)
normalized_positions$Deviation <- ifelse(normalized_positions$Polarity == "deviated",
                                         "NonCentral", "Central")

#Training with the whole data set, commented: training with all data set but subject 1, test on subject 1
normalized_positions_tr <- normalized_positions
#normalized_positions_tr <- normalized_positions[!(normalized_positions$Subject %in% c("1")),]
#commented testing
#normalized_positions_te <- normalized_positions[(normalized_positions$Subject %in% c("1")),]

# Remove dimensions that are constant within groups
constant_columns_ctl <- normalized_positions_tr %>%
  filter(Deviation == "Central") %>%
  dplyr::select(starts_with("r")) %>%
  find_constant
constant_columns_nctl <- normalized_positions_tr %>%
  filter(Deviation == "NonCentral") %>%
  dplyr::select(starts_with("r")) %>%
  find_constant
constant_columns <- c(constant_columns_ctl, constant_columns_nctl)

normalized_positions_tr <- dplyr::select(normalized_positions_tr,
                                         -one_of(constant_columns))

all_data_columns <- names(dplyr::select(normalized_positions_tr,
                                        starts_with("r")))
##PCA in training set
m_pca <- normalized_positions_tr %>%
  dplyr::select(one_of(all_data_columns)) %>%
  as.matrix %>%
  prcomp(center = TRUE, scale = TRUE)

n_pca <- 13
normalized_positions_tr_pca <- bind_cols(normalized_positions_tr,
                                         as.data.frame(m_pca$x[,1:n_pca]))
### LDA
m_lda <- lda(factor(Deviation) ~ .,
             data=dplyr::select(normalized_positions_tr_pca,
                                starts_with("PC"), Deviation))

#linear discriminating coefficient for each PCA
v_lda <- m_lda$scaling

#overall bias
b_lda <- mean(as.matrix(dplyr::select(normalized_positions_tr_pca, starts_with("PC"))) %*% v_lda)

#save(v_lda, b_lda, x.subset, y.subset, file="transformation_all.RData")
save(m_pca, v_lda, b_lda, n_pca, all_data_columns, file="LDA-training-LogRatio.RData")

#Creating matrix with the lda meaure
lda_measure.df <- data_frame(
  lda_measure_logratio=c(as.matrix(dplyr::select(normalized_positions_tr_pca, starts_with("PC"))) %*% v_lda- b_lda),
  Deviation=normalized_positions_tr$Deviation, 
  Subject = normalized_positions_tr$Subject, 
  Expected_response = normalized_positions_tr$Expected_response,
  Item.number = normalized_positions_tr$Item.number)


###SAVING THIS DATA
calibration_data$Subject <- factor(calibration_data$Subject)
calibration_data <- dplyr::full_join(lda_measure.df, calibration_data, by=c("Subject", "Item.number", "Expected_response"))
normalized_positions.plot <- dplyr::full_join(lda_measure.df, normalized_positions.plot, by=c("Subject", "Item.number", "Expected_response"))
normalized_positions.plot$lda_measure_logratio_cut <- cut(normalized_positions.plot$lda_measure_logratio, 5)
calibration_data$lda_measure_logratio_cut <- cut(calibration_data$lda_measure_logratio, 5)

rm(all_data_columns, lda_measure.df, constant_columns, constant_columns_ctl,constant_columns_nctl,i,name_ddx,name_ddy,name_dx,name_dx_last, name_dy, name_x_last, name_y, name_x, name_y_last, name_dy_last, x, y, normalized_positions)
