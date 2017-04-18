library(MASS) # NB: this will mask dplyr::select
##LOADING THE DATA FROM CALIBRATION
load('transformation_all.RData')
#load('transformation.RData')

x <- paste0('x', sprintf("%03d", c(1:101)))
y <- paste0('y', sprintf("%03d", c(1:101)))

# Each x and y coordenate into two columns (101 coordenates per trial) 
normalized_positions = experimental_items %>%
  dplyr::select(Subject, Condition, Response, Picture, Predicate, RT.Log,Item.number, X.Position, Y.Position) %>%
  separate(X.Position, into= x, sep = ",") %>%
  separate(Y.Position, into= y, sep = ",")
normalized_positions[y] <- sapply(normalized_positions[y],as.numeric)
normalized_positions[x] <- sapply(normalized_positions[x],as.numeric)


# Taking the negative of false items, to have everything in the same scale
normalized_positions_false = normalized_positions%>%
  filter(Response=='false') %>% 
  dplyr::mutate_at(vars(starts_with('x')), funs('-'))
normalized_positions_true = filter(normalized_positions, Response=='true')
normalized_positions = bind_rows(normalized_positions_false,normalized_positions_true)

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
 dplyr::select(Subject, Condition, Response, Picture, Predicate, RT.Log,Item.number, one_of(all_data_columns))

normalized_positions.new_pca <- bind_cols(normalized_positions.new,
                                         as.data.frame(predict(m_pca, normalized_positions.new)[,1:n_pca]))

lda_measure.new.df <- data_frame(
  lda_measure=c(as.matrix(dplyr::select(normalized_positions.new_pca, starts_with("PC"))) %*% v_lda- b_lda),
  Subject = normalized_positions.new_pca$Subject, 
  Item.number = normalized_positions.new_pca$Item.number, 
  Condition = normalized_positions.new_pca$Condition, 
  Response = normalized_positions.new_pca$Response, 
  Predicate = normalized_positions.new_pca$Predicate,
  Picture = normalized_positions.new_pca$Picture,
  RT.Log = normalized_positions.new_pca$RT.Log)


#Plotting LDA
ggplot(lda_measure.new.df, aes(x=lda_measure, fill=Condition)) + 
  geom_histogram(binwidth=.5,  position="dodge")+ 
  theme(legend.position = "top") + 
  facet_grid(.~Response)
ggsave('LDA_plural_data.png', plot = last_plot(), scale = 1, dpi = 300, path='real_data/plural/graphs')


lda_measure.new.df$Item.number.cut <- cut(lda_measure.new.df$Item.number, 10)
ggplot(lda_measure.new.df, aes(x=lda_measure, fill=Condition)) + 
  geom_histogram(binwidth=3,  position="dodge")+ 
  theme(legend.position = "top") + 
  facet_grid(Response~Item.number.cut, scale='free_x')
ggsave('LDA_plural_data_across_exp.png', plot = last_plot(), scale = 1, dpi = 300, width=10, path='real_data/plural/graphs')


experimental_items <- dplyr::full_join(lda_measure.new.df, experimental_items, by=c("Subject", "Item.number", "Response", "Condition", "Predicate", "Picture", "RT.Log"))
normalized_positions.plot <- dplyr::full_join(lda_measure.new.df, normalized_positions.plot, by=c("Subject", "Item.number", "Response", "Condition", "Predicate", "Picture", "RT.Log"))

