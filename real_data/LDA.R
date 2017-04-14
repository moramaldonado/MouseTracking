library(MASS) # NB: this will mask dplyr::select
##LOADING THE DATA FROM CALIBRATION
load('transformation_all.RData')
#load('transformation.RData')

#Ordering the data to have coordenates in columns
x <- paste('x', as.character(c(1:101)), sep='')
y <- paste('y', as.character(c(1:101)), sep='')
normalized_positions = controls %>%
  dplyr::select(Subject, Item.number, Sentence_Type, Truth.value, X.Position,Y.Position) %>%
  separate(X.Position, into= x, sep = ",") %>%
  separate(Y.Position, into= y, sep = ",")

#making sure everything has the right class
normalized_positions[y] <- sapply(normalized_positions[y],as.numeric)
normalized_positions[x] <- sapply(normalized_positions[x],as.numeric)
summary(normalized_positions)

# Taking the negative of false items, to have everything in the same scale
normalized_positions_false = normalized_positions%>%
  filter(Truth.value=='False') %>% 
  dplyr::mutate_at(vars(starts_with('x')), funs('-'))
normalized_positions_true = filter(normalized_positions, Truth.value=='True')
normalized_positions = bind_rows(normalized_positions_false,normalized_positions_true)

rm(normalized_positions_false, normalized_positions_true)
#View(filter(normalized_positions, Truth.value=='False')%>% dplyr::select(starts_with('x')))
#View(normalized_positions_false%>% dplyr::select(starts_with('x')))

#More about classes
normalized_positions$Subject <- factor(normalized_positions$Subject)
normalized_positions$Sentence_Type <- factor(normalized_positions$Sentence_Type)
normalized_positions$Truth.value <- factor(normalized_positions$Truth.value)

#Taking out correlated positions
normalized_positions.new <- normalized_positions %>%
 dplyr::select(Subject, Item.number, Sentence_Type, Truth.value, one_of(x.subset), one_of(y.subset))
#dplyr::select(Subject, Item.number, Sentence_Type, Truth.value, one_of(x.subset))

jpeg('histograms_positions(after exclusion).jpg')
par(mfrow=c(2,2))
hist(normalized_positions.new$x13)
hist(normalized_positions.new$x35)
hist(normalized_positions.new$x68)
hist(normalized_positions.new$x95)
dev.off()

#LDA
lda_measure.df <- data_frame(
  lda_measure=c(as.matrix(dplyr::select(normalized_positions.new, starts_with("x"), starts_with("y"))) %*% v_lda- b_lda),
  #lda_measure=c(as.matrix(dplyr::select(normalized_positions.new, starts_with("x"))) %*% v_lda - b_lda),
  Truth.value= normalized_positions.new$Truth.value, 
  Subject = normalized_positions.new$Subject, 
  Sentence_Type = normalized_positions.new$Sentence_Type,
  Item.number = normalized_positions.new$Item.number)

#Plotting LDA
ggplot(lda_measure.df, aes(fill=Sentence_Type, x=lda_measure)) +
geom_histogram(binwidth=10, alpha=.5, position='dodge') +
theme(legend.position = "top") 
ggsave('LDA_all.png', plot = last_plot(), scale = 1, dpi = 300)

##Including the relevant lda_measure in the data
controls$Subject <- factor(controls$Subject)
controls$Truth.value <- factor(controls$Truth.value)
controls$Sentence_Type <- factor(controls$Sentence_Type)

normalized_positions.plot$Subject <- factor(normalized_positions.plot$Subject)
normalized_positions.plot$Truth.value <- factor(normalized_positions.plot$Truth.value)
normalized_positions.plot$Sentence_Type <- factor(normalized_positions.plot$Sentence_Type)
controls <- dplyr::full_join(lda_measure.df, controls, by=c("Subject", "Item.number", "Sentence_Type", "Truth.value"))
normalized_positions.plot <- dplyr::full_join(lda_measure.df, normalized_positions.plot, by=c("Subject", "Item.number", "Sentence_Type", "Truth.value"))




#TEST
mymatrix <- as.matrix(dplyr::select(normalized_positions.new, starts_with("x"), starts_with("y")))
test.df <- t(t(mymatrix)  * as.vector(v_lda)) # first optio

library(reshape2)
ggplot(data = melt(as.data.frame(test.df)), mapping = aes(x = value)) + ggtitle('real data')+
  geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')+ theme(strip.text = element_text(size=7), axis.text=element_text(size=7))



