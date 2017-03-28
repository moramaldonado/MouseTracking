library(MASS) # NB: this will mask dplyr::select

### ORDERING DATA
x <- paste('x', as.character(c(1:101)), sep='')
y <- paste('y', as.character(c(1:101)), sep='')

# Each x and y coordenate into two columns (101 coordenates per trial) 
normalized_positions = calibration_data %>%
  dplyr::select(Subject, Item.number, Polarity, Expected_response, Normalized.positions.X,Normalized.positions.Y) %>%
  separate(Normalized.positions.Y, into= y, sep = ",", convert=T) %>%
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

##PCA
m_pca <- normalized_positions %>%
  dplyr::select(starts_with("x"), starts_with("y")) %>%
  as.matrix %>%
  prcomp(center = TRUE, scale = TRUE)

normalized_positions_pca <- bind_cols(normalized_positions,
                                      as.data.frame(m_pca$x[,1:10]))

normalized_positions_pca <- normalized_positions_pca %>%
  mutate(Deviation=ifelse(Polarity == "deviated", "NonCentral", "Central"))

m_lda <- lda(x=dplyr::select(normalized_positions_pca, starts_with("PC")),
             grouping=normalized_positions_pca$Deviation)

# left <- normalized_positions_pca %>%
#   filter(Expected_response=='false', Polarity == "deviated") %>%
#   mutate(Deviation=ifelse(Polarity == "deviated", "Left", "WTF"))

## Third version: combined classifier
v_lda <- m_lda$scaling
b_lda <- mean(as.matrix(dplyr::select(normalized_positions_pca, starts_with("PC"))) %*% v_lda)
save(m_pca, v_lda, b_lda, file="transformation.RData")


##new
lda_measure.df <- data_frame(
  lda_measure=c(as.matrix(dplyr::select(normalized_positions_pca, starts_with("PC"))) %*% v_lda - b_lda),
  Deviation=c(normalized_positions_pca$Deviation), 
  Subject = c(normalized_positions_pca$Subject), 
  Expected_response = normalized_positions_pca$Expected_response,
  Item.number = c(normalized_positions_pca$Item.number))

ggplot(lda_measure.df, aes(fill=Deviation, x=lda_measure, fill=Deviation)) + 
  geom_histogram(binwidth=1,  position="dodge")+ 
  theme(legend.position = "top") + 
  facet_grid(.~Expected_response)

ggsave('LDA-classification.png', plot = last_plot(), scale = 1, dpi = 300)

ggplot(calibration_data, aes(fill=Polarity, x=MaxLogRatio, fill=Polarity)) + geom_histogram(binwidth=1,  position="dodge")+ theme(legend.position = "top")
ggsave('MaxLogRatio.png', plot = last_plot(), scale = 1, dpi = 300)


calibration_data <- dplyr::full_join(lda_measure.df, calibration_data, by=c("Subject", "Item.number", "Expected_response"))





##Fake data (just in case)
# normalized_positions.fake = calibration_data_new_subjects %>%
#   dplyr::select(Subject, Polarity, Expected_response, Normalized.positions.X,Normalized.positions.Y) %>%
#   separate(Normalized.positions.Y, into= y, sep = ",", convert=T) %>%
#   separate(Normalized.positions.X, into= x, sep = ",", convert=T)
# normalized_positions.fake$Subject <- factor(normalized_positions.fake$Subject)
# normalized_positions.fake$Polarity <- factor(normalized_positions.fake$Polarity)

## PCA should be on the whole data.
## For LDA, add in the central trials from the "false" side. (We want
## central trials to be treated all of a piece regardless of where they wind up)