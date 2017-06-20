require(MASS)
calibration_data$id <- 1:nrow(calibration_data)

deviated = calibration_data %>% 
  filter(Polarity=='deviated')%>%
  dplyr::select(id)
deviated$id <- sample(deviated$id) 

straight = calibration_data %>% 
  filter(Polarity=='straight')%>%
  dplyr::select(id)
straight$id <- sample(straight$id) 

#try <- split(straight, sample(1:10, nrow(straight), replace=T))
bins  <- rep(1:10, nrow(straight) / 10)
try <- split(straight, bins)

bin1 <- try$`1`
bin2 <- try$`2`
bin3 <- try$`3`
bin4 <- try$`4`
bin5 <- try$`5`
bin6 <- try$`6`
bin7 <- try$`7`
bin8 <- try$`8`
bin9 <- try$`9`
bin10 <- try$`10`

bins  <- rep(1:10, nrow(deviated) / 10)
try <- split(deviated, bins)
bin1 <- rbind(bin1, try$`1`)
bin2 <- rbind(bin2, try$`2`)
bin3 <- rbind(bin3, try$`3`)
bin4 <- rbind(bin4, try$`4`)
bin5 <- rbind(bin5, try$`5`)
bin6 <- rbind(bin6, try$`6`)
bin7 <- rbind(bin7, try$`7`)
bin8 <- rbind(bin8, try$`8`)
bin9 <- rbind(bin9, try$`9`)
bin10 <- rbind(bin10, try$`10`)



##loop with bins
bins <- list(bin1, bin2, bin3, bin4, bin5, bin6, bin7, bin8, bin9, bin10)
x <- paste0('x', sprintf("%03d", c(1:101)))
y <- paste0('y', sprintf("%03d", c(1:101)))
auc.bins <- c(1:10)

for (b in 1: length(bins)) {
print(b) 
calibrationTrain <- subset(calibration_data, !(id %in% bins[[b]]$id))
calibrationTest <- subset(calibration_data, id %in% bins[[b]]$id)
### COMPLETE TRAINING 
# Each x and y coordenate into two columns (101 coordenates per trial) 
normalized_positions = calibrationTrain %>%
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

# Deltas
for(i in 2:101)
{ 
  
  name_x <- x[i]
  name_y <- y[i]
  name_x_last <- x[i-1]
  name_y_last <- y[i-1]
  name_dx <- paste0(name_x,'_delta')
  name_dy <- paste0(name_y,'_delta')
  name_ddx <- paste0(name_x,'_ddelta')
  name_ddy <- paste0(name_y,'_ddelta')
  normalized_positions[[name_dx]] <-  normalized_positions[[name_x]] -
    normalized_positions[[name_x_last]]
  normalized_positions[[name_dy]] <-  normalized_positions[[name_y]] -
    normalized_positions[[name_y_last]]
  if (i > 2) {
    name_dx_last <- paste0(name_x_last, '_delta')
    name_dy_last <- paste0(name_y_last, '_delta')
    normalized_positions[[name_ddx]] <-  normalized_positions[[name_dx]] -
      normalized_positions[[name_dx_last]]
    normalized_positions[[name_ddy]] <-  normalized_positions[[name_dy]] -
      normalized_positions[[name_dy_last]]
  }
}

#Training with the whole data set, commented: training with all data set but subject 1, test on subject 1
normalized_positions_tr <- normalized_positions

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





## TEST 
normalized_positions = calibrationTest %>%
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

# Deltas
for(i in 2:101)
{ 
  name_x <- x[i]
  name_y <- y[i]
  name_x_last <- x[i-1]
  name_y_last <- y[i-1]
  name_dx <- paste0(name_x,'_delta')
  name_dy <- paste0(name_y,'_delta')
  name_ddx <- paste0(name_x,'_ddelta')
  name_ddy <- paste0(name_y,'_ddelta')
  normalized_positions[[name_dx]] <-  normalized_positions[[name_x]] -
    normalized_positions[[name_x_last]]
  normalized_positions[[name_dy]] <-  normalized_positions[[name_y]] -
    normalized_positions[[name_y_last]]
  if (i > 2) {
    name_dx_last <- paste0(name_x_last, '_delta')
    name_dy_last <- paste0(name_y_last, '_delta')
    normalized_positions[[name_ddx]] <-  normalized_positions[[name_dx]] -
      normalized_positions[[name_dx_last]]
    normalized_positions[[name_ddy]] <-  normalized_positions[[name_dy]] -
      normalized_positions[[name_dy_last]]
  }
}


normalized_positions_te <- normalized_positions %>%
  dplyr::select(Subject, Item.number, Polarity, Expected_response, Deviation, one_of(all_data_columns))
normalized_positions_te_pca <- bind_cols(normalized_positions_te,as.data.frame(predict(m_pca, normalized_positions_te)[,1:n_pca]))

lda_measure_te.df <- data_frame(
  lda_measure=c(as.matrix(dplyr::select(normalized_positions_te_pca, starts_with("PC"))) %*% v_lda- b_lda),
  Deviation=normalized_positions_te$Deviation,
  Subject = normalized_positions_te$Subject,
  Expected_response = normalized_positions_te$Expected_response,
  Item.number = normalized_positions_te$Item.number)


#ROC and AUC

lda.score.te <- lda_measure_te.df$lda_measure 
lda.label.te <- lda_measure_te.df$Deviation
lda.roc.te <- roc(lda.label.te, lda.score.te)

auc.bins[b] <- lda.roc.te$auc


assign(paste0('lda.roc.te',b), lda.roc.te)
}


#Plots
png(filename='AUC-ROC-LDAvRatio.png', width = 7, height = 7, units = 'in', res = 300)
title("ROC: LDA \n (trained: 90% calibration, tested: 10% calibration)", line=2, cex.main=1)
par(mfrow=c(4,3), mai = c(1, 0.05, 0.1, 0.1))
plot.roc(lda.roc.te1)
plot.roc(smooth(lda.roc.te1), print.auc = TRUE, col="red", add=TRUE, main='lda')
plot.roc(lda.roc.te2)
plot.roc(smooth(lda.roc.te2), print.auc = TRUE, col="red", add=TRUE, main='lda')

plot.roc(lda.roc.te3)
plot.roc(smooth(lda.roc.te3), print.auc = TRUE, col="red", add=TRUE, main='lda')

plot.roc(lda.roc.te4)
plot.roc(smooth(lda.roc.te4), print.auc = TRUE, col="red", add=TRUE, main='lda')

plot.roc(lda.roc.te5)
plot.roc(smooth(lda.roc.te5), print.auc = TRUE, col="red", add=TRUE, main='lda')

plot.roc(lda.roc.te6)
plot.roc(smooth(lda.roc.te6), print.auc = TRUE, col="red", add=TRUE, main='lda')

plot.roc(lda.roc.te7)
plot.roc(smooth(lda.roc.te7), print.auc = TRUE, col="red", add=TRUE, main='lda')

plot.roc(lda.roc.te8)
plot.roc(smooth(lda.roc.te8), print.auc = TRUE, col="red", add=TRUE, main='lda')

plot.roc(lda.roc.te9)
plot.roc(smooth(lda.roc.te9), print.auc = TRUE, col="red", add=TRUE, main='lda')

plot.roc(lda.roc.te10)
plot.roc(smooth(lda.roc.te10), print.auc = TRUE, col="red", add=TRUE, main='lda')

dev.off()

rm(name_ddx,name_ddy,name_dx,name_dx_last, name_dy, name_x_last, name_y, name_x, name_y_last, name_dy_last, normalized_positions)


 


 


 