# LDA FULL (coordinates, velocity and euclidean-based acceleration) ####
load('LDA-Full.RData')
# negation_data = subset(negation_data, Adjective!=5)
negation_data <- filter(negation_data, Response=='true')
normalized_positions.plot <- filter(normalized_positions.plot, Response=='true')


#Create data frame for LDA
x <- paste0('x', sprintf("%03d", c(1:101)))
y <- paste0('y', sprintf("%03d", c(1:101)))
a <- paste0('a', sprintf("%03d", c(1:101)))
v <- paste0('v', sprintf("%03d", c(1:101)))

# Each x and y coordenate into two columns (101 coordenates per trial) 
normalized_positions = negation_data %>%
  dplyr::select(Subject, Item.number, Polarity, Normalized.positions.X,Normalized.positions.Y, Velocity, Acceleration) %>%
  separate(Normalized.positions.Y, into= y, sep = ",") %>%
  separate(Normalized.positions.X, into= x, sep = ",") %>%
  separate(Velocity, into= v, sep = ",") %>%
  separate(Acceleration, into= a, sep = ",")


normalized_positions[y] <- sapply(normalized_positions[y],as.numeric)
normalized_positions[x] <- sapply(normalized_positions[x],as.numeric)
normalized_positions[v] <- sapply(normalized_positions[v],as.numeric)
normalized_positions[a] <- sapply(normalized_positions[a],as.numeric)



#More about classes
normalized_positions$Subject <- factor(normalized_positions$Subject)
normalized_positions$Polarity <- factor(normalized_positions$Polarity)
normalized_positions.new <- normalized_positions %>%
  dplyr::select(Subject, Item.number, Polarity, one_of(all_data_columns))

normalized_positions.new_pca <- bind_cols(normalized_positions.new,
                                          as.data.frame(predict(m_pca, normalized_positions.new)[,1:n_pca]))

lda_measure.new.df <- data_frame(
  lda_measure=c(as.matrix(dplyr::select(normalized_positions.new_pca, starts_with("PC"))) %*% v_lda- b_lda),
  Subject = normalized_positions.new_pca$Subject, 
  Item.number = normalized_positions.new_pca$Item.number, 
  Polarity = normalized_positions.new_pca$Polarity)

##Including the relevant lda_measure in the data
negation_data$Subject <- factor(negation_data$Subject)
negation_data$Polarity <- factor(negation_data$Polarity)
negation_data <- dplyr::full_join(lda_measure.new.df, negation_data, by=c("Subject", "Item.number", "Polarity"))


## Plotting the distribution of LDA measure in negation data (only for true responses)
