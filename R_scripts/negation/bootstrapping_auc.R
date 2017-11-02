library(boot)
library(reshape)


#Function for AUC 
auc_roc <- function(data, indices, score, label, n){
  n_data <- if(n!=FALSE) subset(data, Subject %in% sample(data$Subject, n)) else data 
  data <- n_data[indices,]
  score.te <- unlist(data[,score])  #Measureen
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


auc_accflips<- data.frame (N10=c(1:1000), 
                             N15=c(1:1000), 
                             N20=c(1:1000), 
                             N25=c(1:1000), 
                             N30=c(1:1000),
                             N35=c(1:1000),
                             N40=c(1:1000),
                             TOTAL = c(1:1000),
                             measure= 'accflips')


auc_auc <- data.frame (N10=c(1:1000), 
                           N15=c(1:1000), 
                           N20=c(1:1000), 
                           N25=c(1:1000), 
                           N30=c(1:1000),
                           N35=c(1:1000),
                           N40=c(1:1000),
                           TOTAL = c(1:1000),
                           measure= 'AUC')



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
  
  results_accflips <- boot(data=negation_data_true, statistic=auc_roc, R=1000, score='Acc.flips', label= 'Polarity', n=ns[i])
  auc_accflips[i]<- results_accflips$t
  
  results_auc <- boot(data=negation_data_true, statistic=auc_roc, R=1000, score='AUC', label= 'Polarity', n=ns[i])
  auc_auc[i]<- results_auc$t
  
  }

auc_all <- rbind(auc_lda, auc_xflips, auc_maxdeviation, auc_lda_coords, auc_maxratio, auc_accflips, auc_auc) 
auc_all_melt <- melt(auc_all, id="measure")



auc_power <- ddply(auc_all_melt, c("measure", "variable"),
                   function(auc_all_melt){power(auc_all_melt$value)})
levels(auc_power$measure) <- c('Original LDA', "X-coord flips", "Maximal Deviation", "Coords LDA", "Maximal LogRatio", "Acc flips", "AUC")

auc_power.2 <- ddply(auc_all_melt, c("measure", "variable"),
                   function(auc_all_melt){c(mean=mean(auc_all_melt$value, na.rm=T), se= se(auc_all_melt$value, na.rm=T))})
levels(auc_power.2$measure) <- c('Original LDA', "X-coord flips", "Maximal Deviation", "Coords LDA", "Maximal LogRatio", "Acc flips", "AUC")



ggplot(data=subset(auc_power, measure!='Acc flips'), aes(x=variable, y=V1, group=measure, colour=measure)) +
  geom_line(alpha=.7) +
  geom_point(alpha=.5) +
  ylab('% AUC > .5') +
  xlab('Number of subjects') +
  theme_minimal() + labs(colour='Measure')   + theme(legend.position = 'none')
ggsave('MeasurePower.png', plot = last_plot(), scale = 1, dpi = 300, width=6, path='R_scripts/graphs/negation')

ggplot(data=subset(auc_power.2, measure!='Acc flips'), aes(x=variable, y=mean, group=measure, colour=measure)) +
  geom_line() +
  geom_point() +
  ylab('Mean Area Under ROC Curve') + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
  xlab('Number of subjects') +
  theme_minimal() + labs(colour='Measure')  
ggsave('MeasureAUC_means.png', plot = last_plot(), scale = 1, dpi = 300, width=7, path='R_scripts/graphs/negation')





