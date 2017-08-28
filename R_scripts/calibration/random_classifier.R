#Random classifier
random_classifier <- function(training, test){
# 1. Extract empirical frequency in training set for each class
class1 <<- sum(training$Polarity=="straight")/length(training$Polarity)
class2 <<- sum(training$Polarity=="deviated")/length(training$Polarity)

# 2. Assign labels randomly based on probability
labels <- c('straight', 'deviated')
test$random_classifier <- sample(labels, length(test$Polarity), replace=TRUE, prob=c(class1,class2))
calibrationTest <<- test
}

calibration_data <- subset(calibration_data, Polarity!='uncertain')

# Data frame for iteration of different bins
iterations = 100
random_classifier.df <- data.frame(matrix(ncol=iterations, nrow=10))

for (i in 1:iterations){
  # Create bins for crossvalidation
    calibration_data$id <- 1:nrow(calibration_data)
    deviated <- calibration_data %>% 
      filter(Polarity=='deviated')%>%
      dplyr::select(id)
    deviated$id <- sample(deviated$id) 
    
    straight <- calibration_data %>% 
      filter(Polarity=='straight')%>%
      dplyr::select(id)
    straight$id <- sample(straight$id) 
    
    bins<- rep(1:10, nrow(straight) / 10)
    try <- split(straight, bins)
    
    bin1 <- try$`1`
    bin2 <- try$`2`
    bin3 <- try$`3`
    bin4 <- try$`4`
    bin5 <- try$`5`
    bin6 <- try$`6`
    bin7 <- try$`7`
    bin8 <- try$`8`
    bin9 <- try$`9`
    bin10 <- try$`10`
    
    bins  <- rep(1:10, nrow(deviated) / 10)
    try <- split(deviated, bins)
    
    bin1 <- rbind(bin1, try$`1`)
    bin2 <- rbind(bin2, try$`2`)
    bin3 <- rbind(bin3, try$`3`)
    bin4 <- rbind(bin4, try$`4`)
    bin5 <- rbind(bin5, try$`5`)
    bin6 <- rbind(bin6, try$`6`)
    bin7 <- rbind(bin7, try$`7`)
    bin8 <- rbind(bin8, try$`8`)
    bin9 <- rbind(bin9, try$`9`)
    bin10 <- rbind(bin10, try$`10`)
    rm(try)
    bins <- list(bin1, bin2, bin3, bin4, bin5, bin6, bin7, bin8, bin9, bin10) 
    
    # ROC and AUC for each bin
    for (b in 1: length(bins)) {
      calibrationTrain <- subset(calibration_data, !(id %in% bins[[b]]$id))
      calibrationTest <- subset(calibration_data, id %in% bins[[b]]$id)
      random_classifier(calibrationTrain, calibrationTest)
      score <- factor(calibrationTest$random_classifier, ordered=TRUE) 
      label <- factor(calibrationTest$Polarity)
      roc <- roc(label, score)
      random_classifier.df[b,i] <- roc$auc
      }
    }
  
