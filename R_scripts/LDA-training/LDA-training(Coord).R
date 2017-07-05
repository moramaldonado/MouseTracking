library(MASS) # NB: this will mask dplyr::select


### ORDERING DATA
x <- paste0('x', sprintf("%03d", c(1:101)))
y <- paste0('y', sprintf("%03d", c(1:101)))

# Each x and y coordenate into two columns (101 coordenates per trial) 
normalized_positions = calibration_data %>%
  dplyr::select(Subject, Item.number, Polarity, Expected_response, Normalized.positions.X,Normalized.positions.Y) %>%
  separate(Normalized.positions.Y, into= y, sep = ",") %>%
  separate(Normalized.positions.X, into= x, sep = ",")
normalized_positions[y] <- sapply(normalized_positions[y],as.numeric)
normalized_positions[x] <- sapply(normalized_positions[x],as.numeric)

# Taking the negative of false items, to have everything in the same scale
normalized_positions_false = normalized_positions%>%
  filter(Expected_response=='blue')%>%
  dplyr::mutate_at(vars(starts_with('x')), funs('-'))
normalized_positions_true = filter(normalized_positions, Expected_response=='red')
normalized_positions = bind_rows(normalized_positions_false,normalized_positions_true)
rm(normalized_positions_true, normalized_positions_false)

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
find_constant <- function(d, epsilon=1e-4) {
  names(d)[apply(d,2,function(x) is.na(var(x)) || sqrt(var(x)) < epsilon)]
}
constant_columns_ctl <- normalized_positions_tr %>%
  filter(Deviation == "Central") %>%
  dplyr::select(starts_with("x"), starts_with("y")) %>%
  find_constant
constant_columns_nctl <- normalized_positions_tr %>%
  filter(Deviation == "NonCentral") %>%
  dplyr::select(starts_with("x"), starts_with("y")) %>%
  find_constant
constant_columns <- c(constant_columns_ctl, constant_columns_nctl)

normalized_positions_tr <- dplyr::select(normalized_positions_tr,
                                         -one_of(constant_columns))

# Remove correlated dimensions
find_uncorrelated <- function(d, data_columns, cutoff=0.95,
                              keep_long_distance=T) {
  cor_m <- cor(d[,data_columns])
  above_cutoff_m <- (cor_m > cutoff) & upper.tri(cor_m, diag=F)
  for (i in 1:(nrow(above_cutoff_m))) {
    above_cutoff_m[above_cutoff_m[i,],] <- F
  }
  if (!keep_long_distance) {
    lt_or_above_cutoff_m <- above_cutoff_m | lower.tri(above_cutoff_m, diag=T)
    for (i in 1:(nrow(above_cutoff_m)-1)) {
      above_cutoff_m[i,(i+1):ncol(above_cutoff_m)] <-
        as.logical(cumprod(lt_or_above_cutoff_m[i,(i+1):ncol(above_cutoff_m)]))
    }
  }
  is_correlated <- apply(above_cutoff_m, 2, any)
  result <- data_columns[!is_correlated]
  return(result)
}
#uncorrelated_columns <- find_uncorrelated(normalized_positions_tr,
#                                          all_data_columns,
#                                          cutoff=0.95)

all_data_columns <- names(dplyr::select(normalized_positions_tr,
                                        starts_with("x"),
                                        starts_with("y")))
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

#Saving data
save(m_pca, v_lda, b_lda, n_pca, all_data_columns, file="LDA(Coords).RData")
  
#Creating matrix with the lda meaure
lda_measure.df <- data_frame(
  lda_measure_coords =c(as.matrix(dplyr::select(normalized_positions_tr_pca, starts_with("PC"))) %*% v_lda- b_lda),
  Deviation=normalized_positions_tr$Deviation, 
  Subject = normalized_positions_tr$Subject, 
  Expected_response = normalized_positions_tr$Expected_response,
  Item.number = normalized_positions_tr$Item.number)


###SAVING THIS DATA
calibration_data$Subject <- factor(calibration_data$Subject)
calibration_data <- dplyr::full_join(lda_measure.df, calibration_data, by=c("Subject", "Item.number", "Expected_response", "Deviation"))
normalized_positions.plot <- dplyr::full_join(lda_measure.df, normalized_positions.plot, by=c("Subject", "Item.number", "Expected_response","Deviation"))
normalized_positions.plot$lda_measure_coords_cut <- cut(normalized_positions.plot$lda_measure_coords, 5)
calibration_data$lda_measure_coords_cut <- cut(calibration_data$lda_measure_coords, 5)

rm(all_data_columns, lda_measure.df, constant_columns, constant_columns_ctl,constant_columns_nctl, x, y, normalized_positions)
