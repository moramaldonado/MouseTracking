library(MASS) # NB: this will mask dplyr::select
##LOADING THE DATA FROM CALIBRATION
load('transformation_all.RData')

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

##Plotting distributions
ggplot(negation_data, aes(x=lda_measure, fill=Polarity)) +
  geom_histogram(alpha=.4, position='dodge')+
  scale_fill_brewer(palette="Set1")+ 
  theme_minimal() +
  xlab('LDA: Coord+Deltas+DeltaDelta (with uncertain)')+
  facet_grid(.~Response)




#MaxLogRatio

lda_density <-
  ggplot(negation_data, aes(x=lda_measure, fill=Polarity)) +
  geom_density(alpha=.5)+
  theme(legend.position = "top") +
  scale_fill_brewer(palette="Set1") +
  theme_minimal() +
  facet_grid(.~Response)

mydata.agreggated.lda <- ddply(negation_data, c("Polarity", "Subject", "Response"),
                               function(negation_data)c(lda_measure=mean(negation_data$lda_measure, na.rm=T)))

mydata.agreggated.overall.lda <- ddply(mydata.agreggated.lda, c("Polarity", "Response"),
                                       function(mydata.agreggated.lda)c(mean=mean(mydata.agreggated.lda$lda_measure, na.rm=T), se=se(mydata.agreggated.lda$lda_measure, na.rm=T) ))

lda_means <-ggplot(mydata.agreggated.overall.lda, aes(x=Polarity, y=mean, fill=Polarity)) +
  geom_bar(position=position_dodge(), stat="identity") +
  scale_fill_brewer(palette="Set1")+  
  xlab(' ')  +
  scale_y_continuous(breaks = seq(0, 1.5, by = 0.1)) +
  theme_minimal() +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
  facet_grid(.~Response)

png(filename= "R_scripts/graphs/negation_data/LDA.png", width=800, height=480, unit='px')
multiplot(lda_density, lda_means, cols=2)
dev.off()

#MaxLogRatio

logratio_density <-
  ggplot(negation_data, aes(x=MaxLogRatio, fill=Polarity)) +
  geom_density(alpha=.5)+
  theme(legend.position = "top") +
  scale_fill_brewer(palette="Set1") +
  theme_minimal() +
  facet_grid(.~Response)


mydata.agreggated.maxratio <- ddply(negation_data, c("Polarity", "Subject", "Response"),
                                    function(negation_data)c(MaxLogRatio=mean(negation_data$MaxLogRatio, na.rm=T)))

mydata.agreggated.overall.maxratio <- ddply(mydata.agreggated.maxratio, c("Polarity", Response),
                                            function(mydata.agreggated.maxratio)c(mean=mean(mydata.agreggated.maxratio$MaxLogRatio, na.rm=T), se=se(mydata.agreggated.maxratio$MaxLogRatio, na.rm=T) ))

logratio_mean <- ggplot(mydata.agreggated.overall.maxratio, aes(x=Polarity, y=mean, fill=Polarity)) +
  geom_bar(position=position_dodge(), stat="identity") +
  scale_fill_brewer(palette="Set1")+  
  xlab(' ')  +
  theme_minimal()+
  facet_grid(.~Response) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9))

png(filename= "R_scripts/graphs/negation_data/MaxLogRatio.png", width=800, height=480, unit='px')
multiplot(logratio_density, logratio_mean, cols=2)
dev.off()



#MaxDeviation

maxdev_density <-
  ggplot(negation_data, aes(x=MaxDeviation, fill=Polarity)) +
  geom_density(alpha=.5)+
  theme(legend.position = "top") +
  scale_fill_brewer(palette="Set1") +
  theme_minimal() +
  facet_grid(.~Response)


mydata.agreggated.maxdev <- ddply(negation_data, c("Polarity", "Subject", "Response"),
                                    function(negation_data)c(MaxDeviation=mean(negation_data$MaxDeviation, na.rm=T)))

mydata.agreggated.overall.maxdev <- ddply(mydata.agreggated.maxdev, c("Polarity", "Response"),
                                            function(mydata.agreggated.maxdev)c(mean=mean(mydata.agreggated.maxdev$MaxDeviation, na.rm=T), se=se(mydata.agreggated.maxdev$MaxDeviation, na.rm=T) ))

maxdev_mean <- ggplot(mydata.agreggated.overall.maxdev, aes(x=Polarity, y=mean, fill=Polarity)) +
  geom_bar(position=position_dodge(), stat="identity") +
  scale_fill_brewer(palette="Set1")+  
  xlab(' ')  +
  theme_minimal()+
  facet_grid(.~Response) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9))

png(filename= "R_scripts/graphs/negation_data/MaxDeviation.png", width=800, height=480, unit='px')
multiplot(maxdev_density, maxdev_mean, cols=2)
dev.off()


#XFlips

xflips_density <-
  ggplot(negation_data, aes(x=X.flips, fill=Polarity)) +
  geom_density(alpha=.5)+
  theme(legend.position = "top") +
  scale_fill_brewer(palette="Set1") +
  theme_minimal() +
  facet_grid(.~Response)


mydata.agreggated.xflips <- ddply(negation_data, c("Polarity", "Subject", "Response"),
                                  function(negation_data)c(X.flips=mean(negation_data$X.flips, na.rm=T)))

mydata.agreggated.overall.xflips <- ddply(mydata.agreggated.xflips, c("Polarity", "Response"),
                                          function(mydata.agreggated.xflips)c(mean=mean(mydata.agreggated.xflips$X.flips, na.rm=T), se=se(mydata.agreggated.xflips$X.flip, na.rm=T) ))

xflips_mean <- ggplot(mydata.agreggated.overall.xflips, aes(x=Polarity, y=mean, fill=Polarity)) +
  geom_bar(position=position_dodge(), stat="identity") +
  scale_fill_brewer(palette="Set1")+  
  xlab(' ')  +
  theme_minimal()+
  facet_grid(.~Response) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9))

png(filename= "R_scripts/graphs/negation_data/MaxDeviation.png", width=800, height=480, unit='px')
multiplot(xflips_density, xflips_mean, cols=2)
dev.off()