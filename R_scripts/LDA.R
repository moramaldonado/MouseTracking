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

par(mfrow=c(2,2))
hist(normalized_positions$x10)
hist(normalized_positions$x30)
hist(normalized_positions$x60)
hist(normalized_positions$x90)



# Taking the negative of false items, to have everything in the same scale
normalized_positions_false = normalized_positions%>%
  filter(Expected_response=='false')%>%
  dplyr::mutate_at(vars(starts_with('x')), funs('-'))
normalized_positions_true = filter(normalized_positions, Expected_response=='true')
normalized_positions = bind_rows(normalized_positions_false,normalized_positions_true)
rm(normalized_positions_true, normalized_positions_false)

#last arrangements
normalized_positions$Subject <- factor(normalized_positions$Subject)
normalized_positions$Polarity <- factor(normalized_positions$Polarity)
normalized_positions$Expected_response <- factor(normalized_positions$Expected_response)

#Correlation matrix
x_correlations <- cor(dplyr::select(normalized_positions, starts_with("x"))) > 0.95
y_correlations <- cor(dplyr::select(normalized_positions, starts_with("y"))) > 0.95

# Remove points after 2 correlation > .99 (otherwise I'll be removing too many points)
x_correlations.sums <- colSums(x_correlations)
y_correlations.sums <- colSums(y_correlations)

##Remove points after 2 correlation > .99 (otherwise I'll be removing too many points)
# for(c in 3:101) {
#   for (r in 1:101) {
#     m <- paste('x', as.character(c), sep='')
#     l <- paste('x', as.character(c-1), sep='')
#     if(x_correlations[r,m] == TRUE & x_correlations[r,l] == TRUE) 
#     {x[r]='SACAR'}
#   }}
# for(c in 3:101) {
#   for (r in 1:101) {
#     m <- paste('y', as.character(c), sep='')
#     l <- paste('y', as.character(c-1), sep='')
#     if(y_correlations[r,m] == TRUE & y_correlations[r,l] == TRUE) 
#     {y[r]='SACAR'}
#   }}


######
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
      else{break}
    }}}



#Taking out elements
x.subset <- x[x != "SACAR"];
y.subset <- y[y != "SACAR"];


### LDA
normalized_positions.new <- normalized_positions %>%
dplyr::select(Subject, Item.number, Polarity, Expected_response, one_of(x.subset), one_of(y.subset))%>%
#  dplyr::select(Subject, Item.number, Polarity, Expected_response, one_of(x.subset))%>%  
  mutate(Deviation=ifelse(Polarity == "deviated", "NonCentral", "Central"))

jpeg('histograms_positions(after exclusion).jpg')
par(mfrow=c(2,2))
hist(normalized_positions.new$x13)
hist(normalized_positions.new$x35)
hist(normalized_positions.new$x68)
hist(normalized_positions.new$x95)
dev.off()

m_lda <- lda(x=dplyr::select(normalized_positions.new, starts_with("x"), starts_with("y")),
#m_lda <- lda(x=dplyr::select(normalized_positions.new, starts_with("x")),
             grouping=normalized_positions.new$Deviation)

v_lda <- m_lda$scaling
b_lda <- mean(as.matrix(dplyr::select(normalized_positions.new, starts_with("x"), starts_with("y"))) %*% v_lda)
#b_lda <- mean(as.matrix(dplyr::select(normalized_positions.new, starts_with("x"))) %*% v_lda)
save(v_lda, b_lda, x.subset, y.subset, file="transformation_all.RData")

#Creating matrix with the lda meaur
lda_measure.df <- data_frame(
  lda_measure=c(as.matrix(dplyr::select(normalized_positions.new, starts_with("x"), starts_with("y"))) %*% v_lda- b_lda),
  #lda_measure=c(as.matrix(dplyr::select(normalized_positions.new, starts_with("x"))) %*% v_lda- b_lda),
  Deviation=normalized_positions.new$Deviation, 
  Subject = normalized_positions.new$Subject, 
  Expected_response = normalized_positions.new$Expected_response,
  Item.number = normalized_positions.new$Item.number)
 
ggplot(lda_measure.df, aes(x=lda_measure, fill=Deviation)) + 
  geom_histogram(binwidth=.5,  position="dodge")+ 
  theme(legend.position = "top") + 
  facet_grid(.~Expected_response)
ggsave('LDA-classification_all.png', plot = last_plot(), scale = 1, dpi = 300)

###SAVING THIS DATA
calibration_data <- dplyr::full_join(lda_measure.df, calibration_data, by=c("Subject", "Item.number", "Expected_response"))


#Last tests
mymatrix <- as.matrix(dplyr::select(normalized_positions.new, starts_with("x"), starts_with("y")))
test <- t(t(mymatrix)  * as.vector(v_lda)) # first optono

dev.off()
hist(test.df)


ggplot(data = melt(as.data.frame(test)), mapping = aes(x = value)) + ggtitle('calibration')+
  geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')+ theme(strip.text = element_text(size=7), axis.text=element_text(size=7))


ggplot(data = melt(as.data.frame(mymatrix)), mapping = aes(x = value)) + ggtitle('calibration')+
  geom_histogram(bins = 20) + facet_wrap(~variable)+ theme(strip.text = element_text(size=7), axis.text=element_text(size=7))




ggplot(data = dplyr::select(normalized_positions.new, y96, Deviation), aes(x=y96, fill=Deviation))+geom_histogram()

ggplot(data = dplyr::select(normalized_positions.new, y99, Deviation), aes(x=y99, fill=Deviation))+geom_histogram()



