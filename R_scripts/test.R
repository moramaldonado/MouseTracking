k <- 20

library(MASS) # NB: this will mask dplyr::select

### ORDERING DATA
x <- paste('x', as.character(c(1:101)), sep='')
y <- paste('y', as.character(c(1:101)), sep='')

# Each x and y coordenate into two columns (101 coordenates per trial) 
normalized_positions = calibration_data %>%
  dplyr::select(Subject, Item.number, Polarity, Expected_response, Normalized.positions.X,Normalized.positions.Y) %>%
  separate(Normalized.positions.Y, into= y, sep = ",") %>%
  separate(Normalized.positions.X, into= x, sep = ",")  
normalized_positions[y] <- sapply(normalized_positions[y],as.numeric)
normalized_positions[x] <- sapply(normalized_positions[x],as.numeric)
sapply(normalized_positions, class)

normalized_positions_false = normalized_positions%>%
  filter(Expected_response=='false')%>%
  dplyr::mutate_at(vars(starts_with('x')), funs('-'))
normalized_positions_true = filter(normalized_positions, Expected_response=='true')
normalized_positions = bind_rows(normalized_positions_false,normalized_positions_true)
rm(normalized_positions_true, normalized_positions_false)
normalized_positions$Subject <- factor(normalized_positions$Subject)
normalized_positions$Polarity <- factor(normalized_positions$Polarity)
normalized_positions$Expected_response <- factor(normalized_positions$Expected_response)


for (i in 1:20){
  training <- sample_frac(normalized_positions, size = .93, replace = FALSE)
  test <- anti_join(normalized_positions, training)
  x_correlations <- cor(dplyr::select(training, starts_with("x"))) > 0.95
  y_correlations <- cor(dplyr::select(training, starts_with("y"))) > 0.95

  
  ##Remove points after 3 correlation > .99 (otherwise I'll be removing too many points)
  for (r in 1:100){
    if (x[r]!='SACAR'){
      for (c in (r+1):101){
        m <- paste('x', as.character(c), sep='')
        if(x_correlations[r,m] == TRUE)
        {x[c]='SACAR'}
        else{break}
      }}}
  
    for (r in 1:100){
    if (y[r]!='SACAR'){
      for (c in (r+1):101){
        m <- paste('y', as.character(c), sep='')
        if(y_correlations[r,m] == TRUE)
        {y[c]='SACAR'}
        else{break}}}}
  
  #Taking out elements
  x.subset <- x[x != "SACAR"];
  y.subset <- y[y != "SACAR"];
  
  
  
  ### LDA
  training <- training %>%
    dplyr::select(Subject, Item.number, Polarity, Expected_response, one_of(x.subset), one_of(y.subset))%>%
    #  dplyr::select(Subject, Item.number, Polarity, Expected_response, one_of(x.subset))%>%  
    mutate(Deviation=ifelse(Polarity == "deviated", "NonCentral", "Central"))
  
  m_lda <- lda(x=dplyr::select(training, starts_with("x"), starts_with("y")),
               grouping=training$Deviation)
  
  v_lda <- m_lda$scaling
  b_lda <- mean(as.matrix(dplyr::select(training, starts_with("x"), starts_with("y"))) %*% v_lda)

  lda_measure.df <- data_frame(
    lda_measure=c(as.matrix(dplyr::select(training, starts_with("x"), starts_with("y"))) %*% v_lda- b_lda),
    Deviation=training$Deviation, 
    Subject = training$Subject, 
    Expected_response = training$Expected_response,
    Item.number = training$Item.number)
  
  name <- paste('LDAtraining',as.character(i))name <- paste(name,'.png')
  
    
  ggplot(lda_measure.df, aes(x=lda_measure, fill=Deviation)) + 
    geom_histogram(position="dodge")+ 
    ggtitle(name)+
    theme(legend.position = "top") + 
    facet_grid(.~Expected_response)
  ggsave(name, plot = last_plot(), scale = 1, dpi = 300)
  
  #test
  
  test <- test %>%
    dplyr::select(Subject, Item.number, Polarity, Expected_response, one_of(x.subset), one_of(y.subset))%>%
    mutate(Deviation=ifelse(Polarity == "deviated", "NonCentral", "Central"))
  
  lda_measure.df <- data_frame(
    lda_measure=c(as.matrix(dplyr::select(test, starts_with("x"), starts_with("y"))) %*% v_lda- b_lda),
    Deviation=test$Deviation, 
    Subject = test$Subject, 
    Expected_response = test$Expected_response,
    Item.number = test$Item.number)
  
  name <- paste('LDAtest',as.character(i))
  name <- paste(name,'.png')
  
  ggplot(lda_measure.df, aes(x=lda_measure, fill=Deviation)) + 
    ggtitle(name)+
    geom_histogram(position="dodge")+ 
    theme(legend.position = "top") + 
    facet_grid(.~Expected_response)
  ggsave(name, plot = last_plot(), scale = 1, dpi = 300)
  
  assign(paste0('training',as.character(i)), training)
  assign(paste0('test',as.character(i)), test)
  rm(training)
  rm(test)
  
}


