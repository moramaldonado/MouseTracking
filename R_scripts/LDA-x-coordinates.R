
###JUST WITH X-COORDINATES

library(MASS) # NB: this will mask dplyr::select

### ORDERING DATA
x <- paste('x', as.character(c(1:101)), sep='')

# Each x coordenate into different columns (101 coordenates per trial) 
normalized_positions = calibration_data %>%
  dplyr::select(Subject, Item.number, Polarity, Expected_response, Normalized.positions.X) %>%
  separate(Normalized.positions.X, into= x, sep = ",", convert=T) 

# Taking the negative of false items, to have everything in the same scale
normalized_positions_false = normalized_positions%>%
  filter(Expected_response=='false')%>%
  dplyr::mutate_at(vars(starts_with('x')), funs('-'))
normalized_positions_true = filter(normalized_positions, Expected_response=='true')
normalized_positions = bind_rows(normalized_positions_false,normalized_positions_true)
rm(normalized_positions_true, normalized_positions_false)

#last arrangements
normalized_positions$Subject <- factor(normalized_positions$Subject)
normalized_positions$Item.number <- factor(normalized_positions$Item.number)
normalized_positions$Polarity <- factor(normalized_positions$Polarity)
normalized_positions$Expected_response <- factor(normalized_positions$Expected_response)

#Correlation matrix
x_correlations <- cor(dplyr::select(normalized_positions, starts_with("x"))) > 0.99

# Remove points after 2 correlation > .99 (otherwise I'll be removing too many points)
#x_correlations.sums <- colSums(x_correlations)

for(c in 3:101) {
  for (r in 1:101) {
    m <- paste('x', as.character(c), sep='')
    l <- paste('x', as.character(c-1), sep='')
    k <- paste('x', as.character(c-2), sep='')
    if(x_correlations[r,m] == TRUE & x_correlations[r,l] == TRUE & x_correlations[r,k] == TRUE) 
    {x[r]='SACAR'}
  }}

#Taking out elements
x.subset <- x[x != "SACAR"];

### LDA
normalized_positions.new <- normalized_positions %>%
  dplyr::select(Subject, Item.number, Polarity, Expected_response, one_of(x.subset))%>%
  mutate(Deviation=ifelse(Polarity == "deviated", "NonCentral", "Central"))

m_lda <- lda(x=dplyr::select(normalized_positions.new, starts_with("x")),
             grouping=normalized_positions.new$Deviation)

v_lda <- m_lda$scaling
b_lda <- mean(as.matrix(dplyr::select(normalized_positions.new, starts_with("x"))) %*% v_lda)

#save(m_pca, v_lda, b_lda, file="transformation.RData")

#Creating matrix with the lda meaur
lda_measure.df <- data_frame(
  lda_measure=c(as.matrix(dplyr::select(normalized_positions.new, starts_with("x"))) %*% v_lda- b_lda),
  Deviation=c(normalized_positions.new$Deviation), 
  Subject = c(normalized_positions.new$Subject), 
  Expected_response = normalized_positions.new$Expected_response,
  Item.number = c(normalized_positions.new$Item.number))

ggplot(lda_measure.df, aes(x=lda_measure, fill=Deviation)) + 
  geom_histogram(binwidth=.5,  position="dodge")+ 
  theme(legend.position = "top") + 
  facet_grid(.~Expected_response)
ggsave('LDA-classification.png', plot = last_plot(), scale = 1, dpi = 300)

###SAVING THIS DATA
calibration_data <- dplyr::full_join(lda_measure.df, calibration_data, by=c("Subject", "Item.number", "Expected_response"))





