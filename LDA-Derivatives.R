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


#Taking the derivatives (EVERYTHING is here but I commented the delta_delta just to see if it works better, spoiler alert: it doesn't)

#first derivative delta
normalized_positions_x <- dplyr::select(normalized_positions, starts_with("x"))
normalized_positions_x$x1_delta <-  normalized_positions_x$x1 - 0
normalized_positions_y <- dplyr::select(normalized_positions, starts_with("y"))
normalized_positions_y$y1_delta <-  normalized_positions_y$y1 - 0
#x
for(c in 2:101)
  { 
  name <- paste(x[c],'_delta', sep='')  
  normalized_positions_x[, ncol(normalized_positions_x) + 1] <-  normalized_positions_x[c] - normalized_positions_x[c-1]
  names(normalized_positions_x)[ncol(normalized_positions_x)] <- name
  }

#y
for(c in 2:101)
{ 
  name <- paste(y[c],'_delta', sep='')  
  normalized_positions_y[, ncol(normalized_positions_y) + 1] <-  normalized_positions_y[c] - normalized_positions_y[c-1]
  names(normalized_positions_y)[ncol(normalized_positions_y)] <- name
}


# 
# # second derivative delta-delta
# #x
# normalized_positions_x$x1_delta_delta <-  normalized_positions_x$x1_delta - 0
# for(c in 103:202)
# {
#   name <- paste(colnames(normalized_positions_x)[c],'_delta', sep='')  
#   normalized_positions_x[, ncol(normalized_positions_x) + 1] <-  normalized_positions_x[c] - normalized_positions_x[c-1]
#   names(normalized_positions_x)[ncol(normalized_positions_x)] <- name
# }
# 
# #y
# normalized_positions_y$y1_delta_delta <-  normalized_positions_y$y1_delta - 0
# for(c in 103:202)
# {
#   name <- paste(colnames(normalized_positions_y)[c],'_delta', sep='')  
#   normalized_positions_y[, ncol(normalized_positions_y) + 1] <-  normalized_positions_y[c] - normalized_positions_y[c-1]
#   names(normalized_positions_y)[ncol(normalized_positions_y)] <- name
# }
 

#this are all zeros so I remove them
normalized_positions_x$x101_delta <- NULL
 normalized_positions_y$y101_delta <- NULL
 normalized_positions_y$y100_delta <- NULL
# normalized_positions_y$y101_delta_delta <- NULL
# 

normalized_positions <- merge(normalized_positions, normalized_positions_x)
normalized_positions <- merge(normalized_positions, normalized_positions_y)



#Correlation matrix
x_correlations <- cor(dplyr::select(normalized_positions, starts_with("x"))) > 0.95
y_correlations <- cor(dplyr::select(normalized_positions, starts_with("y"))) > 0.95

#Include all the values to remove correlated dimensions
x <- c(x, paste(x, '_delta', sep=''))
y <- c(y, paste(y, '_delta', sep=''))
#x <- c(x, paste(x, '_delta', sep=''), paste(x, '_delta_delta', sep=''))
#y <- c(y, paste(y, '_delta', sep=''), paste(y, '_delta_delta', sep=''))


##Remove points after 3 correlation > .95 (otherwise I'll be removing too many points)
for (r in 1:200){
  if (x[r]!='SACAR'){
    for (c in (r+1):201){
      m <- paste('x', as.character(c), sep='')
      if(x_correlations[r,c] == TRUE)
      {x[c]='SACAR'}
      else{break}
    }}}


for (r in 1:199){
  if (y[r]!='SACAR'){
    for (c in (r+1):200){
      m <- paste('y', as.character(c), sep='')
      if(y_correlations[r,c] == TRUE)
      {y[c]='SACAR'}
      else{break}
    }}}



#Taking out elements
x.subset <- x[x != "SACAR"];
x.subset <- x.subset[x.subset != "x101_delta"];
y.subset <- y[y != "SACAR"];
y.subset <- y.subset[y.subset != "y101_delta"];
y.subset <- y.subset[y.subset != "y100_delta"];
#y.subset <- y.subset[y.subset != "y101_delta_delta"];

### LDA
normalized_positions.new <- normalized_positions %>%
  dplyr::select(Subject, Item.number, Polarity, Expected_response, one_of(x.subset), one_of(y.subset))%>%
  #  dplyr::select(Subject, Item.number, Polarity, Expected_response, one_of(x.subset))%>%  
  mutate(Deviation=ifelse(Polarity == "deviated", "NonCentral", "Central"))


#look at predictors that have near zero variance
x = nearZeroVar(normalized_positions.new, saveMetrics = TRUE)
x[x[,"zeroVar"] + x[,"nzv"] > 0, ] 
normalized_positions.new <- normalized_positions.new%>%dplyr::select(-x2_delta,-x6_delta,-x7_delta,-x9_delta,-x97_delta,-x99_delta,-x100_delta,-y95_delta) 

###THIS IS FAILING!!!
m_lda <- lda(x=dplyr::select(normalized_positions.new, starts_with("x"), starts_with("y")),
             #m_lda <- lda(x=dplyr::select(normalized_positions.new, starts_with("x")),
             grouping=normalized_positions.new$Deviation)




v_lda <- m_lda$scaling
b_lda <- mean(as.matrix(dplyr::select(normalized_positions.new, starts_with("x"), starts_with("y"))) %*% v_lda)
#b_lda <- mean(as.matrix(dplyr::select(normalized_positions.new, starts_with("x"))) %*% v_lda)

#save(v_lda, b_lda, x.subset, y.subset, file="transformation_all.RData")

#Creating matrix with the lda meaure
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