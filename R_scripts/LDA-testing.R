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
