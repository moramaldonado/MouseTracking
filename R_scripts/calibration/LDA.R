# TRAINING LDA 
## Functions that TRAIN different LDAs classifier with different predictors
### INPUT: calibration_data
### OUTPUT: v_lda, b_lda, m_lda, lda_measure.df, n_pca, all_data_columns > these data is stored on a .R file 

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

LDA_training.coord <- function(calibration_data){
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
  
  normalized_positions_tr <- normalized_positions

  # Remove dimensions that are constant within groups

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

  #uncorrelated_columns <- find_uncorrelated(normalized_positions_tr,
  #                                          all_data_columns,
  #                                          cutoff=0.95)
  
  all_data_columns <<- names(dplyr::select(normalized_positions_tr,
                                          starts_with("x"),
                                          starts_with("y")))
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
    lda_measure_coords =c(as.matrix(dplyr::select(normalized_positions_tr_pca, starts_with("PC"))) %*% v_lda- b_lda),
    Subject = normalized_positions_tr$Subject, 
    Expected_response = normalized_positions_tr$Expected_response,
    Item.number = normalized_positions_tr$Item.number)

}

LDA_training.coord.accdist <- function(calibration_data){
  ### ORDERING DATA
  x <- paste0('x', sprintf("%03d", c(1:101)))
  y <- paste0('y', sprintf("%03d", c(1:101)))
  a <- paste0('a', sprintf("%03d", c(1:101)))
  
  # Each x and y coordenate into two columns (101 coordenates per trial) 
  normalized_positions = calibration_data %>%
    dplyr::select(Subject, Item.number, Polarity, Expected_response, Normalized.positions.X,Normalized.positions.Y, Acceleration) %>%
    separate(Normalized.positions.Y, into= y, sep = ",") %>%
    separate(Normalized.positions.X, into= x, sep = ",") %>%
    separate(Acceleration, into= a, sep = ",")
  
  normalized_positions[y] <- sapply(normalized_positions[y],as.numeric)
  normalized_positions[x] <- sapply(normalized_positions[x],as.numeric)
  normalized_positions[a] <- sapply(normalized_positions[a],as.numeric)
  
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
  
  constant_columns_ctl <- normalized_positions_tr %>%
    filter(Deviation == "Central") %>%
    dplyr::select(starts_with("x"), starts_with("y"), starts_with('a')) %>%
    find_constant
  constant_columns_nctl <- normalized_positions_tr %>%
    filter(Deviation == "NonCentral") %>%
    dplyr::select(starts_with("x"), starts_with("y"), starts_with('a')) %>%
    find_constant
  constant_columns <- c(constant_columns_ctl, constant_columns_nctl)
  
  normalized_positions_tr <- dplyr::select(normalized_positions_tr,
                                           -one_of(constant_columns))
  
  #uncorrelated_columns <- find_uncorrelated(normalized_positions_tr,
  #                                          all_data_columns,
  #                                          cutoff=0.95)
  
  all_data_columns <<- names(dplyr::select(normalized_positions_tr,
                                          starts_with("x"),
                                          starts_with("y"), 
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
    lda_measure_coord.acc=c(as.matrix(dplyr::select(normalized_positions_tr_pca, starts_with("PC"))) %*% v_lda- b_lda),
    Subject = normalized_positions_tr$Subject, 
    Expected_response = normalized_positions_tr$Expected_response,
    Item.number = normalized_positions_tr$Item.number)
  
  
}

LDA_training.coord.delta <- function(calibration_data){
  
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
  
  # Deltas
  for(i in 2:101)
  { 
    name_x <- x[i]
    name_y <- y[i]
    name_x_last <- x[i-1]
    name_y_last <- y[i-1]
    name_dx <- paste0(name_x,'_delta')
    name_dy <- paste0(name_y,'_delta')
    normalized_positions[[name_dx]] <-  normalized_positions[[name_x]] -
      normalized_positions[[name_x_last]]
    normalized_positions[[name_dy]] <-  normalized_positions[[name_y]] -
      normalized_positions[[name_y_last]]
  }

  normalized_positions_tr <- normalized_positions

    # Remove dimensions that are constant within groups
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
  


  all_data_columns <<- names(dplyr::select(normalized_positions_tr,
                                          starts_with("x"),
                                          starts_with("y")))
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
  
  #Creating matrix with the lda meaure
  lda_measure.df <<- data_frame(
    lda_measure_coord_delta =c(as.matrix(dplyr::select(normalized_positions_tr_pca, starts_with("PC"))) %*% v_lda- b_lda),
    Subject = normalized_positions_tr$Subject, 
    Expected_response = normalized_positions_tr$Expected_response,
    Item.number = normalized_positions_tr$Item.number)
  
  
}

LDA_training.coord.delta.deltadelta <- function(calibration_data){
  
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
  

  normalized_positions_tr <- normalized_positions

  # Remove dimensions that are constant within groups
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
  

  all_data_columns <<- names(dplyr::select(normalized_positions_tr,
                                          starts_with("x"),
                                          starts_with("y")))
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
    lda_measure_full =c(as.matrix(dplyr::select(normalized_positions_tr_pca, starts_with("PC"))) %*% v_lda- b_lda),
    Subject = normalized_positions_tr$Subject, 
    Expected_response = normalized_positions_tr$Expected_response,
    Item.number = normalized_positions_tr$Item.number)
  
  
  }

LDA_training.deltadelta <- function(calibration_data){
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
  
  all_data_columns <<- names(dplyr::select(normalized_positions_tr,
                                          ends_with('ddelta')))
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
    lda_measure_deltadelta=c(as.matrix(dplyr::select(normalized_positions_tr_pca, starts_with("PC"))) %*% v_lda- b_lda),
    Subject = normalized_positions_tr$Subject, 
    Expected_response = normalized_positions_tr$Expected_response,
    Item.number = normalized_positions_tr$Item.number)
  
}

LDA_training.logratio<- function(calibration_data){
  
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
  
  all_data_columns <<- names(dplyr::select(normalized_positions_tr,
                                          starts_with("r")))
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
  
  #lda measure
  lda_measure.df <<- data_frame(
    lda_measure_logratio=c(as.matrix(dplyr::select(normalized_positions_tr_pca, starts_with("PC"))) %*% v_lda- b_lda),
    Subject = normalized_positions_tr$Subject, 
    Expected_response = normalized_positions_tr$Expected_response,
    Item.number = normalized_positions_tr$Item.number)
  
}


# TESTING LDA 
## Functions that TEST different LDAs classifier with different predictor

LDA_test.coord.delta.deltadelta <- function(data, v_lda, b_lda, m_pca, all_data_columns, n_pca) {
  
  ## TEST 
  normalized_positions = data %>%
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
  
  lda_measure_te.df <<- data_frame(
    lda_measure=c(as.matrix(dplyr::select(normalized_positions_te_pca, starts_with("PC"))) %*% v_lda- b_lda),
    Deviation=normalized_positions_te$Deviation,
    Subject = normalized_positions_te$Subject,
    Expected_response = normalized_positions_te$Expected_response,
    Item.number = normalized_positions_te$Item.number)
  
  
}


LDA_test.coord.delta <- function(data, v_lda, b_lda, m_pca, all_data_columns, n_pca) {
  
  ## TEST 
  normalized_positions = data %>%
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
    normalized_positions[[name_dx]] <-  normalized_positions[[name_x]] -
      normalized_positions[[name_x_last]]
    normalized_positions[[name_dy]] <-  normalized_positions[[name_y]] -
      normalized_positions[[name_y_last]]

  }
  
  
  normalized_positions_te <- normalized_positions %>%
    dplyr::select(Subject, Item.number, Polarity, Expected_response, Deviation, one_of(all_data_columns))
  normalized_positions_te_pca <- bind_cols(normalized_positions_te,as.data.frame(predict(m_pca, normalized_positions_te)[,1:n_pca]))
  
  lda_measure_te.df <<- data_frame(
    lda_measure=c(as.matrix(dplyr::select(normalized_positions_te_pca, starts_with("PC"))) %*% v_lda- b_lda),
    Deviation=normalized_positions_te$Deviation,
    Subject = normalized_positions_te$Subject,
    Expected_response = normalized_positions_te$Expected_response,
    Item.number = normalized_positions_te$Item.number)
  
  
}



LDA_test.coord <- function(data, v_lda, b_lda, m_pca, all_data_columns, n_pca) {
  
  ## TEST 
  normalized_positions = data %>%
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
  
  
  normalized_positions_te <- normalized_positions %>%
    dplyr::select(Subject, Item.number, Polarity, Expected_response, Deviation, one_of(all_data_columns))
  normalized_positions_te_pca <- bind_cols(normalized_positions_te,as.data.frame(predict(m_pca, normalized_positions_te)[,1:n_pca]))
  
  lda_measure_te.df <<- data_frame(
    lda_measure=c(as.matrix(dplyr::select(normalized_positions_te_pca, starts_with("PC"))) %*% v_lda- b_lda),
    Deviation=normalized_positions_te$Deviation,
    Subject = normalized_positions_te$Subject,
    Expected_response = normalized_positions_te$Expected_response,
    Item.number = normalized_positions_te$Item.number)
  
  
}

LDA_test.coord.accdist <- function(data, v_lda, b_lda, m_pca, all_data_columns, n_pca) {
  a <- paste0('a', sprintf("%03d", c(1:101)))
  
  ## TEST 
  normalized_positions = data %>%
    dplyr::select(Subject, Item.number, Polarity, Expected_response, Normalized.positions.X,Normalized.positions.Y, Acceleration) %>%
    separate(Normalized.positions.Y, into= y, sep = ",") %>%
    separate(Normalized.positions.X, into= x, sep = ",") %>%
    separate(Acceleration, into= a, sep = ",")
  
  normalized_positions[y] <- sapply(normalized_positions[y],as.numeric)
  normalized_positions[x] <- sapply(normalized_positions[x],as.numeric)
  normalized_positions[a] <- sapply(normalized_positions[a],as.numeric)
  
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
  
  
  normalized_positions_te <- normalized_positions %>%
    dplyr::select(Subject, Item.number, Polarity, Expected_response, Deviation, one_of(all_data_columns))
  normalized_positions_te_pca <- bind_cols(normalized_positions_te,as.data.frame(predict(m_pca, normalized_positions_te)[,1:n_pca]))
  
  lda_measure_te.df <<- data_frame(
    lda_measure=c(as.matrix(dplyr::select(normalized_positions_te_pca, starts_with("PC"))) %*% v_lda- b_lda),
    Deviation=normalized_positions_te$Deviation,
    Subject = normalized_positions_te$Subject,
    Expected_response = normalized_positions_te$Expected_response,
    Item.number = normalized_positions_te$Item.number)
  
  
}

LDA_test.logratio <- function(data, v_lda, b_lda, m_pca, all_data_columns, n_pca) {
  r <- paste0('r', sprintf("%03d", c(1:101)))
  
  # Each x and y coordenate into two columns (101 coordenates per trial) 
  normalized_positions = data %>%
    dplyr::select(Subject, Item.number, Polarity, Expected_response, LogRatio) %>%
    separate(LogRatio, into= r, sep = ",")
  
  normalized_positions[r] <- sapply(normalized_positions[r],as.numeric)
  
  #last arrangements
  normalized_positions$Subject <- factor(normalized_positions$Subject)
  normalized_positions$Polarity <- factor(normalized_positions$Polarity)
  normalized_positions$Expected_response <- factor(normalized_positions$Expected_response)
  normalized_positions$Deviation <- ifelse(normalized_positions$Polarity == "deviated",
                                           "NonCentral", "Central")
  
  
  normalized_positions_te <- normalized_positions %>%
    dplyr::select(Subject, Item.number, Polarity, Expected_response, Deviation, one_of(all_data_columns))
  normalized_positions_te_pca <- bind_cols(normalized_positions_te,as.data.frame(predict(m_pca, normalized_positions_te)[,1:n_pca]))
  
  lda_measure_te.df <<- data_frame(
    lda_measure=c(as.matrix(dplyr::select(normalized_positions_te_pca, starts_with("PC"))) %*% v_lda- b_lda),
    Deviation=normalized_positions_te$Deviation,
    Subject = normalized_positions_te$Subject,
    Expected_response = normalized_positions_te$Expected_response,
    Item.number = normalized_positions_te$Item.number)
  
  
}

LDA_test.deltadelta <- function(data, v_lda, b_lda, m_pca, all_data_columns, n_pca) {
  
  ## TEST 
  normalized_positions = data %>%
    dplyr::select(Subject, Item.number, Polarity, Expected_response, Normalized.positions.X,Normalized.positions.Y, Acceleration) %>%
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
  
  lda_measure_te.df <<- data_frame(
    lda_measure=c(as.matrix(dplyr::select(normalized_positions_te_pca, starts_with("PC"))) %*% v_lda- b_lda),
    Deviation=normalized_positions_te$Deviation,
    Subject = normalized_positions_te$Subject,
    Expected_response = normalized_positions_te$Expected_response,
    Item.number = normalized_positions_te$Item.number)
  
  
}

