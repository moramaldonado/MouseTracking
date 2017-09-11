#Random classifier
random_classifier <- function(training, test, iterations){
# 1. Extract empirical frequency in training set for each class
class1 <<- sum(training$Polarity=="straight")/length(training$Polarity)
class2 <<- sum(training$Polarity=="deviated")/length(training$Polarity)

# 2. Assign labels randomly based on probability
labels <- c('straight', 'deviated')
#test$random_classifier <- sample(labels, length(test$Polarity), replace=TRUE, prob=c(class1,class2))
parameters <- estBetaParams(class1, 0.1)
#test$random_classifier <- rbeta(length(test$Polarity), parameters$alpha, parameters$beta)
my <<- rbeta(length(test$Polarity)*iterations, parameters$alpha, parameters$beta)
calibrationTest <<- test
}

calibration_data <- subset(calibration_data, Polarity!='uncertain')

# Data frame for iteration of different bins
iterations = 10000
random_classifier.df <- data.frame(matrix(ncol=iterations, nrow=10))

# Create bins for crossvalidation
# calibration_data$id <- 1:nrow(calibration_data)
# deviated <- calibration_data %>% 
#   filter(Polarity=='deviated')%>%
#   dplyr::select(id)
# deviated$id <- sample(deviated$id) 
# 
# straight <- calibration_data %>% 
#   filter(Polarity=='straight')%>%
#   dplyr::select(id)
# straight$id <- sample(straight$id) 
# 
# bins<- rep(1:10, nrow(straight) / 10)
# try <- split(straight, bins)
# 
# bin1 <- try$`1`
# bin2 <- try$`2`
# bin3 <- try$`3`
# bin4 <- try$`4`
# bin5 <- try$`5`
# bin6 <- try$`6`
# bin7 <- try$`7`
# bin8 <- try$`8`
# bin9 <- try$`9`
# bin10 <- try$`10`
# 
# bins  <- rep(1:10, nrow(deviated) / 10)
# try <- split(deviated, bins)
# 
# bin1 <- rbind(bin1, try$`1`)
# bin2 <- rbind(bin2, try$`2`)
# bin3 <- rbind(bin3, try$`3`)
# bin4 <- rbind(bin4, try$`4`)
# bin5 <- rbind(bin5, try$`5`)
# bin6 <- rbind(bin6, try$`6`)
# bin7 <- rbind(bin7, try$`7`)
# bin8 <- rbind(bin8, try$`8`)
# bin9 <- rbind(bin9, try$`9`)
# bin10 <- rbind(bin10, try$`10`)
# rm(try)
# bins <- list(bin1, bin2, bin3, bin4, bin5, bin6, bin7, bin8, bin9, bin10) 

# ROC and AUC for each bin
for (b in 1: length(bins)) {
  calibrationTrain <- subset(calibration_data, !(id %in% bins[[b]]$id))
  calibrationTest <- subset(calibration_data, id %in% bins[[b]]$id)
  random_classifier(calibrationTrain, calibrationTest, iterations)
  
  for (i in 1:iterations){      
    point1 <- if_else(i==1, 1, (length(calibrationTest$Polarity)*(i-1))+1)
    point2 <- length(calibrationTest$Polarity)*i
    #print(c(point1, point2))
    random_classifier.iteration <- my[point1:point2]
    
    #score <- calibrationTest$random_classifier#predictor 
    score <- random_classifier.iteration #predictor 
    label <- factor(calibrationTest$Polarity) #response
    roc <- roc(label, score)
    random_classifier.df[b,i] <- roc$auc
  }
}





  
