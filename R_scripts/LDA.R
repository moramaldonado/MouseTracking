x <- paste('x', as.character(c(1:101)), sep='')
y <- paste('y', as.character(c(1:101)), sep='')

normalized_positions = calibration_data %>%
  select(Subject, Polarity, Expected_response, Normalized.positions.X,Normalized.positions.Y) %>%
  separate(Normalized.positions.Y, into= y, sep = ",") %>%
  separate(Normalized.positions.X, into= x, sep = ",")
normalized_positions$Subject <- factor(normalized_positions$Subject)
normalized_positions$Polarity <- factor(normalized_positions$Polarity )


my_data = normalized_positions %>%
  filter(Expected_response=='true')%>%
  select(-Expected_response,-Subject) 

#PCA
my_data1 <- my_data %>% select(-Polarity)
my_data1 <- as.data.frame(sapply(my_data1, as.numeric))
res <- prcomp(my_data1, center = TRUE, scale = FALSE)
summary(res)
plot(cumsum(res$sdev^2/sum(res$sdev^2)))
trunc <- res$x[,1:3]
trunc <- as.data.frame(trunc)
Polarity <- my_data$Polarity
trunc <- cbind(Polarity, trunc)

#Part of data (done by hand): Still collinear!
ex.x <- paste('x', as.character(c(25:75)), sep='') 
ex.y <- paste('y', as.character(c(25:75)), sep='')
my_data2 <- my_data %>%
  select(one_of(ex.x),one_of(ex.y), Polarity)  
  
#LDA for real data 
library(MASS)
fit <- lda(formula = Polarity ~ ., 
           data = trunc, CV=TRUE)

ct <- table(my_data$Polarity, fit$class)
ct
diag(prop.table(ct, 1))


fit2 <- lda(formula = Polarity ~ Differences*Time.Step, 
            data = differences, CV=TRUE)

ct2 <- table(differences$Polarity, fit2$class)
ct2
diag(prop.table(ct2, 1))

