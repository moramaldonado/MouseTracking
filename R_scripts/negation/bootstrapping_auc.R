library(boot)
library(reshape)


#Function for AUC 
auc_roc <- function(data, indices, score, label, n){
  n_data <- if(n!=FALSE) subset(data, Subject %in% sample(data$Subject, n)) else data 
  data <- n_data[indices,]
  score.te <- unlist(data[,score])  #Measure
  label.te <- unlist(data[,label]) #Polarity
  roc.te <- roc(label.te, score.te)
  return(roc.te$auc)
}
power <-function(value){length(which(value> .5))/1000}

#Data frame

ns <- c(10,15,20,25,30,35,40,FALSE)
auc_xflips <- data.frame (N10=c(1:1000), 
                          N15=c(1:1000), 
                          N20=c(1:1000), 
                          N25=c(1:1000), 
                          N30=c(1:1000),
                          N35=c(1:1000),
                          N40=c(1:1000),
                          TOTAL = c(1:1000),
                          measure= 'xflips')

auc_lda <- data.frame (N10=c(1:1000), 
                          N15=c(1:1000), 
                          N20=c(1:1000), 
                          N25=c(1:1000), 
                          N30=c(1:1000),
                          N35=c(1:1000),
                          N40=c(1:1000),
                          TOTAL = c(1:1000),
                          measure= 'lda')

auc_maxdeviation <- data.frame (N10=c(1:1000), 
                       N15=c(1:1000), 
                       N20=c(1:1000), 
                       N25=c(1:1000), 
                       N30=c(1:1000),
                       N35=c(1:1000),
                       N40=c(1:1000),
                       TOTAL = c(1:1000),
                       measure= 'maxdeviation')

auc_maxratio<- data.frame (N10=c(1:1000), 
                                N15=c(1:1000), 
                                N20=c(1:1000), 
                                N25=c(1:1000), 
                                N30=c(1:1000),
                                N35=c(1:1000),
                                N40=c(1:1000),
                                TOTAL = c(1:1000),
                                measure= 'maxratio')


auc_lda_coords<- data.frame (N10=c(1:1000), 
                           N15=c(1:1000), 
                           N20=c(1:1000), 
                           N25=c(1:1000), 
                           N30=c(1:1000),
                           N35=c(1:1000),
                           N40=c(1:1000),
                           TOTAL = c(1:1000),
                           measure= 'lda_coords')


for (i in 1:length(ns)) { 
  results_xflips <- boot(data=negation_data_true, statistic=auc_roc, R=1000, score='X.flips', label= 'Polarity', n=ns[i])
  auc_xflips[i]<- results_xflips$t
  
  results_lda <- boot(data=negation_data_true, statistic=auc_roc, R=1000, score='lda_measure', label= 'Polarity', n=ns[i])
  auc_lda[i]<- results_lda$t
  
  results_lda_coords <- boot(data=negation_data_true, statistic=auc_roc, R=1000, score='lda_measure_coords', label= 'Polarity', n=ns[i])
  auc_lda_coords[i]<- results_lda_coords$t
  
  results_maxdeviation <- boot(data=negation_data_true, statistic=auc_roc, R=1000, score='MaxDeviation', label= 'Polarity', n=ns[i])
  auc_maxdeviation[i]<- results_maxdeviation$t
  
  results_maxratio <- boot(data=negation_data_true, statistic=auc_roc, R=1000, score='MaxLogRatio', label= 'Polarity', n=ns[i])
  auc_maxratio[i]<- results_maxratio$t
  
  }

auc_all <- rbind(auc_lda, auc_xflips, auc_maxdeviation, auc_lda_coords, auc_maxratio) 
auc_all_melt <- melt(auc_all, id="measure")



auc_power <- ddply(auc_all_melt, c("measure", "variable"),
                   function(auc_all_melt){power(auc_all_melt$value)})

auc_power.2 <- ddply(auc_all_melt, c("measure", "variable"),
                   function(auc_all_melt){c(mean=mean(auc_all_melt$value, na.rm=T), se= se(auc_all_melt$value, na.rm=T))})




ggplot(data=auc_power, aes(x=variable, y=V1, group=measure, colour=measure)) +
  geom_line(alpha=.5) +
  geom_point(alpha=.5) +
  ylab('% AUC > .5') +
  theme_minimal()

ggplot(data=auc_power.2, aes(x=variable, y=mean, group=measure, colour=measure)) +
  geom_line() +
  geom_point() +
  ylab('mean AUC') + geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) 








## bootstrapping ####
# bootstrapping complete set of data
#results of AUC are stored in results$t
results_xflips <- boot(data=negation_data_true, statistic=auc_roc, R=1000, score='X.flips', label= 'Polarity', n=FALSE)
results_lda <- boot(data=negation_data_true, statistic=auc_roc, R=1000, score='lda_measure', label= 'Polarity', n=FALSE)
results_lda_coords <- boot(data=negation_data_true, statistic=auc_roc, R=1000, score='lda_measure_coords', label= 'Polarity', n=FALSE)
results_maxdeviation <- boot(data=negation_data_true, statistic=auc_roc, R=1000, score='MaxDeviation', label= 'Polarity', n=FALSE)


#try to see if works
hist(results_maxdeviation$t)
abline(v=results_maxdeviation$t0, col='red')




