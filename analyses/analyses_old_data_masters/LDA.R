# TRAINING LDA 
## Functions that TRAIN different LDAs classifier with different predictors
### INPUT: calibration_data
### OUTPUT: v_lda, b_lda, m_lda, lda_measure.df, n_pca, all_data_columns > these data is stored on a .R file 

library(MASS)

find_constant <- function(d, epsilon=1e-4) {
  names(d)[apply(d,2,function(x) is.na(var(x)) || sqrt(var(x)) < epsilon)]
}

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


#Coordinates, velocity and acceleration (based on gradients/distance)
LDA_training.coord.dist <- function(data){
  ### ORDERING DATA
  x <- paste0('x', sprintf("%03d", c(1:101)))
  y <- paste0('y', sprintf("%03d", c(1:101)))
  v <- paste0('v', sprintf("%03d", c(1:101)))
  a <- paste0('a', sprintf("%03d", c(1:101)))
  
  # Each x and y coordenate into two columns (101 coordenates per trial) 
  normalized_positions = data %>%
    dplyr::select(Subject, Item.number, Condition, Response, X.Position, Y.Position, Velocity, Acceleration) %>%
    separate(X.Position, into= x, sep = ",") %>%
    separate(Y.Position, into= y, sep = ",") %>%
    separate(Velocity, into= v, sep = ",") %>%
    separate(Acceleration, into= a, sep = ",")
  
  normalized_positions[y] <- sapply(normalized_positions[y],as.numeric)
  normalized_positions[x] <- sapply(normalized_positions[x],as.numeric)
  normalized_positions[v] <- sapply(normalized_positions[v],as.numeric)
  normalized_positions[a] <- sapply(normalized_positions[a],as.numeric)
  
  # Taking the negative of false items, to have everything in the same scale
  normalized_positions_false = normalized_positions%>%
    filter(Response=='false')%>%
    dplyr::mutate_at(vars(starts_with('x')), funs('-'))
  normalized_positions_true = filter(normalized_positions, Response=='true')
  normalized_positions = bind_rows(normalized_positions_false,normalized_positions_true)
  rm(normalized_positions_true, normalized_positions_false)
  
  #last arrangements
  normalized_positions$Subject <- factor(normalized_positions$Subject)
  normalized_positions$Condition <- factor(normalized_positions$Condition)
  normalized_positions$Response <- factor(normalized_positions$Response)
  normalized_positions$Deviation <- ifelse(normalized_positions$Condition == "Neg",
                                           "NonCentral", "Central")
  
  
  normalized_positions_tr <- normalized_positions

  # Remove dimensions that are constant within groups
    constant_columns_ctl <- normalized_positions_tr %>%
    filter(Deviation == "Central") %>%
    dplyr::select(starts_with("x"), starts_with("y"), starts_with('v'), starts_with('a')) %>%
    find_constant
  constant_columns_nctl <- normalized_positions_tr %>%
    filter(Deviation == "NonCentral") %>%
    dplyr::select(starts_with("x"), starts_with("y"), starts_with('v'),starts_with('a')) %>%
    find_constant
  constant_columns <- c(constant_columns_ctl, constant_columns_nctl)
  
  normalized_positions_tr <- dplyr::select(normalized_positions_tr,
                                           -one_of(constant_columns))

  all_data_columns <<- names(dplyr::select(normalized_positions_tr,
                                          starts_with("x"),
                                          starts_with("y"),
                                          starts_with("v"),
                                          starts_with("a")))
  ##PCA in training set
  m_pca <<- normalized_positions_tr %>%
    dplyr::select(one_of(all_data_columns)) %>%
    as.matrix %>%
    prcomp(center = TRUE, scale = TRUE)
  
  n_pca <<- 13
  normalized_positions_tr_pca <- bind_cols(normalized_positions_tr,
                                           as.data.frame(m_pca$x[,1:n_pca]))
  ### LDA
  m_lda <<- lda(factor(Deviation) ~ .,
               data=dplyr::select(normalized_positions_tr_pca,
                                  starts_with("PC"), Deviation))
  
  #linear discriminating coefficient for each PCA
  v_lda <<- m_lda$scaling
  
  #overall bias
  b_lda <<- mean(as.matrix(dplyr::select(normalized_positions_tr_pca, starts_with("PC"))) %*% v_lda)
  
  lda_measure.df <<- data_frame(
    lda_measure_full=c(as.matrix(dplyr::select(normalized_positions_tr_pca, starts_with("PC"))) %*% v_lda- b_lda),
    Subject = normalized_positions_tr$Subject, 
    Response = normalized_positions_tr$Response,
    Item.number = normalized_positions_tr$Item.number,
  Deviation = normalized_positions_tr$Deviation)
  
  
}



# TESTING LDA 
## Functions that TEST different LDAs classifier with different predictor

LDA_test.coord.dist <- function(data, v_lda, b_lda, m_pca, all_data_columns, n_pca) {

  ## TEST 
  normalized_positions = data %>%
    dplyr::select(Subject, Item.number, Condition, Response, X.Position,Y.Position, Velocity, Acceleration) %>%
    separate(Y.Position, into= y, sep = ",") %>%
    separate(X.Position, into= x, sep = ",") %>%
    separate(Velocity, into= v, sep = ",") %>%
    separate(Acceleration, into= a, sep = ",")
  
  normalized_positions[y] <- sapply(normalized_positions[y],as.numeric)
  normalized_positions[x] <- sapply(normalized_positions[x],as.numeric)
  normalized_positions[v] <- sapply(normalized_positions[v],as.numeric)
  normalized_positions[a] <- sapply(normalized_positions[a],as.numeric)
  
  # Taking the negative of false items, to have everything in the same scale
  normalized_positions_false = normalized_positions%>%
    filter(Response=='false')%>%
    dplyr::mutate_at(vars(starts_with('x')), funs('-'))
  normalized_positions_true = filter(normalized_positions, Response=='true')
  normalized_positions = bind_rows(normalized_positions_false,normalized_positions_true)
  rm(normalized_positions_true, normalized_positions_false)
  
  #last arrangements
  normalized_positions$Subject <- factor(normalized_positions$Subject)
  normalized_positions$Condition <- factor(normalized_positions$Condition)
  normalized_positions$Expected_response <- factor(normalized_positions$Response)
  normalized_positions$Deviation <- ifelse(normalized_positions$Condition == "Neg",
                                           "NonCentral", "Central")
  
  normalized_positions_te <- normalized_positions %>%
    dplyr::select(Subject, Item.number, Deviation, Response, one_of(all_data_columns))
  normalized_positions_te_pca <- bind_cols(normalized_positions_te,as.data.frame(predict(m_pca, normalized_positions_te)[,1:n_pca]))
  
  lda_measure_te.df <<- data_frame(
    lda_measure=c(as.matrix(dplyr::select(normalized_positions_te_pca, starts_with("PC"))) %*% v_lda- b_lda),
    Subject = normalized_positions_te$Subject,
    Response = normalized_positions_te$Response,
    Item.number = normalized_positions_te$Item.number, 
    Deviation = normalized_positions_te$Deviation)
  
  
}
