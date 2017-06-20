library(MASS) # NB: this will mask dplyr::select
##LOADING THE DATA FROM CALIBRATION
load('transformation_all_coord.RData')

negation_data = subset(negation_data, Adjective!=5)

x <- paste0('x', sprintf("%03d", c(1:101)))
y <- paste0('y', sprintf("%03d", c(1:101)))

# Each x and y coordenate into two columns (101 coordenates per trial) 
normalized_positions = negation_data %>%
  dplyr::select(Subject, Item.number, Polarity, Response, Normalized.positions.X,Normalized.positions.Y) %>%
  separate(Normalized.positions.Y, into= y, sep = ",") %>%
  separate(Normalized.positions.X, into= x, sep = ",")
normalized_positions[y] <- sapply(normalized_positions[y],as.numeric)
normalized_positions[x] <- sapply(normalized_positions[x],as.numeric)

# Taking the negative of false items, to have everything in the same scale
normalized_positions_false = normalized_positions%>%
  filter(Response=='false')%>%
  dplyr::mutate_at(vars(starts_with('x')), funs('-'))
normalized_positions_true = filter(normalized_positions, Response=='true')
normalized_positions = bind_rows(normalized_positions_false,normalized_positions_true)
rm(normalized_positions_true, normalized_positions_false)

#More about classes
normalized_positions$Subject <- factor(normalized_positions$Subject)
normalized_positions$Polarity <- factor(normalized_positions$Polarity)
normalized_positions$Response <- factor(normalized_positions$Response)

normalized_positions.new <- normalized_positions %>%
  dplyr::select(Subject, Item.number, Polarity, Response, one_of(all_data_columns))

normalized_positions.new_pca <- bind_cols(normalized_positions.new,
                                          as.data.frame(predict(m_pca, normalized_positions.new)[,1:n_pca]))

lda_measure.new.df <- data_frame(
  lda_measure=c(as.matrix(dplyr::select(normalized_positions.new_pca, starts_with("PC"))) %*% v_lda- b_lda),
  Subject = normalized_positions.new_pca$Subject, 
  Item.number = normalized_positions.new_pca$Item.number, 
  Polarity = normalized_positions.new_pca$Polarity, 
  Response = normalized_positions.new_pca$Response)


##Including the relevant lda_measure in the data
negation_data$Subject <- factor(negation_data$Subject)
negation_data$Response <- factor(negation_data$Response)
negation_data$Polarity <- factor(negation_data$Polarity)
negation_data <- dplyr::full_join(lda_measure.new.df, negation_data, by=c("Subject", "Item.number", "Polarity", "Response"))


#Plotting LDA and MaxLogRatio

##Subsetting to accurate trials
not_accurate_negation_data <- subset(negation_data, Accuracy==0)
negation_data <- subset(negation_data, Accuracy==1)

ggplot(negation_data, aes(x=lda_measure, fill=Polarity)) +
 geom_histogram(alpha=.4, position='dodge')+
  scale_fill_brewer(palette="Set1")+ 
  theme_minimal() +
  xlab('LDA: Coord')+
  facet_grid(.~Response)


mydata.agreggated.lda <- ddply(negation_data, c("Polarity", "Subject", "Response"),
                               function(negation_data)c(lda_measure=mean(negation_data$lda_measure, na.rm=T)))

mydata.agreggated.overall.lda <- ddply(mydata.agreggated.lda, c("Polarity", "Response"),
                                       function(mydata.agreggated.lda)c(mean=mean(mydata.agreggated.lda$lda_measure, na.rm=T), se=se(mydata.agreggated.lda$lda_measure, na.rm=T) ))

p1 <- ggplot(mydata.agreggated.overall.lda, aes(x=Polarity, y=mean, fill=Polarity)) +
  geom_bar(position=position_dodge(), stat="identity") +
  scale_fill_brewer(palette="Set1")+  
  xlab(' ')  +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 1.5, by = 0.1)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
  facet_grid(.~Response)


multiplot(lda_means, p1, cols=2)
multiplot(lda_density, p2, cols=2)

