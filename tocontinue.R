total.subj <- length(levels(calibration_data$Subject))
subj.te <- factor(sample(calibration_data$Subject, 1/3*total.subj))

#normalized_positions_tr <- normalized_positions[!(normalized_positions$Subject %in% subj.te),]
#commented testing
#normalized_positions_te <- normalized_positions[(normalized_positions$Subject %in% subj.te),]


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

v_lda <- m_lda$scaling
b_lda <- mean(as.matrix(dplyr::select(normalized_positions_tr_pca, starts_with("PC"))) %*% v_lda)

#save(v_lda, b_lda, x.subset, y.subset, file="transformation_all.RData")
save(m_pca, v_lda, b_lda, n_pca, all_data_columns, file="transformation_all.RData")

#Creating matrix with the lda meaure
lda_measure.df <- data_frame(
  lda_measure=c(as.matrix(dplyr::select(normalized_positions_tr_pca, starts_with("PC"))) %*% v_lda- b_lda),
  Deviation=normalized_positions_tr$Deviation, 
  Subject = normalized_positions_tr$Subject, 
  Expected_response = normalized_positions_tr$Expected_response,
  Item.number = normalized_positions_tr$Item.number)

ggplot(lda_measure.df, aes(x=lda_measure, fill=Deviation)) +
  geom_histogram(bins=10,  position="dodge")+
  theme(legend.position = "top") +
  facet_grid(.~Expected_response)
ggsave('LDA_all_calibration.png', path='R_scripts/graphs')