## CROSS-VALIDATION ####

#Data frame
ns <- c(10,15,20,25,30,35,40,FALSE)

auc_lda <- data.frame (N10=c(1:1000), N15=c(1:1000), N20=c(1:1000), N25=c(1:1000), 
                       N30=c(1:1000), N35=c(1:1000), N40=c(1:1000), TOTAL = c(1:1000),
                       measure= 'lda')
permutation_lda <- data.frame (N10=0, N15=0, N20=0, N25=0, N30=0, N35=0, N40=0,TOTAL = 0, measure = 'permutation_lda')

auc_maxdeviation <- data.frame (N10 = c(1:1000), N15 = c(1:1000), N20 = c(1:1000),
                                N25 = c(1:1000), N30 = c(1:1000), N35 = c(1:1000), N40 = c(1:1000), TOTAL = c(1:1000),
                                measure = 'maxdeviation')
permutation_maxdeviation <- data.frame (N10=0, N15=0, N20=0, N25=0, N30=0, N35=0, N40=0,TOTAL = 0, measure = 'permutation_maxdeviation')


auc_maxratio <- data.frame (N10 = c(1:1000), N15 = c(1:1000), N20 = c(1:1000),
                            N25 = c(1:1000), N30 = c(1:1000), N35 = c(1:1000), N40 = c(1:1000), TOTAL = c(1:1000),
                            measure = 'maxratio')
permutation_maxratio <- data.frame (N10=0, N15=0, N20=0, N25=0, N30=0, N35=0, N40=0,TOTAL = 0, measure = 'permutation_maxratio')


#LDA: cross validation

#Other MT measures and baseline


controls.true <- random.within(controls.true, "Condition")

results_lda <- boot(data=controls.true, statistic=auc_roc, R=1000, score='lda_measure', label= 'Condition', n=FALSE)
results_lda.random <- boot(data=controls.true, statistic=auc_roc, R=1000, score='lda_measure', label= 'Random', n=FALSE)
data <- rbind(data.frame(value=results_lda$t, variable='original'), data.frame(value=results_lda.random$t, variable='null'))  
roc.te <- roc(data$variable, data$value)
permutation<- roc.te$auc 
roc.te <- roc(data$variable, data$value)
permutation <- roc.te$auc 

p1 <- ggplot(data, aes(x=value, fill=variable))  + xlab('LDA') +
  geom_density(alpha=.5) + scale_fill_nejm()+ ggtitle(as.character(permutation))


results_maxdeviation <- boot(data=controls.true, statistic=auc_roc, R=1000, score='MaxDeviation', label= 'Condition', n=FALSE)
results_maxdeviation.random <- boot(data=controls.true, statistic=auc_roc, R=1000, score='MaxDeviation', label= 'Random', n=FALSE)
data <- rbind(data.frame(value=results_maxdeviation$t, variable='original'), data.frame(value=results_maxdeviation.random$t, variable='null'))  
roc.te <- roc(data$variable, data$value)
permutation <- roc.te$auc 

p2 <- ggplot(data, aes(x=value, fill=variable))  + xlab('MD') +
  geom_density(alpha=.5) + scale_fill_nejm()+ ggtitle(as.character(permutation))

results_maxratio <- boot(data=controls.true, statistic=auc_roc, R=1000, score='MaxLogRatio', label= 'Condition', n=FALSE)
results_maxratio.random <- boot(data=controls.true, statistic=auc_roc, R=1000, score='MaxLogRatio', label= 'Random', n=FALSE)
data <- rbind(data.frame(value=results_maxratio$t, variable='original'), data.frame(value=results_maxratio.random$t, variable='null'))  
roc.te <- roc(data$variable, data$value)
permutation <- roc.te$auc 

p3 <- ggplot(data, aes(x=value, fill=variable)) + xlab('MaxLogRatio') +
  geom_density(alpha=.5) + scale_fill_nejm()+ ggtitle(as.character(permutation))


p4 <- grid.arrange(p1,p2,p3, ncol=3)




ggsave('crossvalidation1.png', plot = p4, scale = 1, dpi = 300, width = 10, height = 5, path='fig/')


controls.true.subset <- subset(controls.true, Rank<25)
controls.true.subset <- random.within(controls.true.subset, "Condition")

results_lda <- boot(data=controls.true.subset, statistic=auc_roc, R=1000, score='lda_measure', label= 'Condition', n=FALSE)
results_lda.random <- boot(data=controls.true.subset, statistic=auc_roc, R=1000, score='lda_measure', label= 'Random', n=FALSE)
data <- rbind(data.frame(value=results_lda$t, variable='original'), data.frame(value=results_lda.random$t, variable='null'))  
roc.te <- roc(data$variable, data$value)
permutation <- roc.te$auc 


p1 <- ggplot(data, aes(x=value, fill=variable))  + xlab('LDA') +
  geom_density(alpha=.5) + scale_fill_nejm() + ggtitle(as.character(permutation))


results_maxdeviation <- boot(data=controls.true.subset, statistic=auc_roc, R=1000, score='MaxDeviation', label= 'Condition', n=FALSE)
results_maxdeviation.random <- boot(data=controls.true.subset, statistic=auc_roc, R=1000, score='MaxDeviation', label= 'Random', n=FALSE)
data <- rbind(data.frame(value=results_maxdeviation$t, variable='original'), data.frame(value=results_maxdeviation.random$t, variable='null'))  
roc.te <- roc(data$variable, data$value)
permutation <- roc.te$auc 


p2 <- ggplot(data, aes(x=value, fill=variable))  + xlab('MD') +
  geom_density(alpha=.5) + scale_fill_nejm()+ ggtitle(as.character(permutation))

results_maxratio <- boot(data=controls.true.subset, statistic=auc_roc, R=1000, score='MaxLogRatio', label= 'Condition', n=FALSE)
results_maxratio.random <- boot(data=controls.true.subset, statistic=auc_roc, R=1000, score='MaxLogRatio', label= 'Random', n=FALSE)
data <- rbind(data.frame(value=results_maxratio$t, variable='original'), data.frame(value=results_maxratio.random$t, variable='null'))  
roc.te <- roc(data$variable, data$value)
permutation <- roc.te$auc 


p3 <- ggplot(data, aes(x=value, fill=variable)) + xlab('MaxLogRatio') +
  geom_density(alpha=.5) + scale_fill_nejm()+ ggtitle(as.character(permutation))


p4 <- grid.arrange(p1,p2,p3, ncol=3)

ggsave('crossvalidation1_subset.png', plot = p4, scale = 1, dpi = 300, width = 10, height = 5, path='fig/')








for (i in 1:length(ns)) { 
  
  #Include null hypothesis (labels shuffled in Random column)
  controls.true <- random.within(controls.true, "Condition")

  results_lda <- boot(data=controls.true, statistic=auc_roc, R=1000, score='lda_measure', label= 'Condition', n=ns[i])
  results_lda.random <- boot(data=controls.true, statistic=auc_roc, R=1000, score='lda_measure', label= 'Random', n=ns[i])
  data <- rbind(data.frame(value=results_lda$t, variable='original'), data.frame(value=results_lda.random$t, variable='null'))  
  roc.te <- roc(data$variable, data$value)
  permutation_lda[i]<- roc.te$auc 
  auc_lda[i]<- results_lda$t  
  
  results_maxdeviation <- boot(data=controls.true, statistic=auc_roc, R=1000, score='MaxDeviation', label= 'Condition', n=ns[i])
  results_maxdeviation.random <- boot(data=controls.true, statistic=auc_roc, R=1000, score='MaxDeviation', label= 'Random', n=ns[i])
  data <- rbind(data.frame(value=results_maxdeviation$t, variable='original'), data.frame(value=results_maxdeviation.random$t, variable='null'))  
  roc.te <- roc(data$variable, data$value)
  permutation_maxdeviation[i]<- roc.te$auc 
  auc_maxdeviation[i]<- results_maxdeviation$t
  
  results_maxratio <- boot(data=controls.true, statistic=auc_roc, R=1000, score='MaxLogRatio', label= 'Condition', n=ns[i])
  results_maxratio.random <- boot(data=controls.true, statistic=auc_roc, R=1000, score='MaxLogRatio', label= 'Random', n=ns[i])
  data <- rbind(data.frame(value=results_maxratio$t, variable='original'), data.frame(value=results_maxratio.random$t, variable='null'))  
  roc.te <- roc(data$variable, data$value)
  permutation_maxratio[i]<- roc.te$auc 
  auc_maxratio[i]<- results_maxratio$t
  
  
}


##Cross validation
auc_all <- rbind(auc_lda, auc_maxdeviation, auc_maxratio) 
auc_all_melt <- melt(auc_all, id="measure")
permutations <- rbind(permutation_lda, permutation_maxdeviation, permutation_maxratio)
permutations <- melt(permutations, id="measure")

save(auc_all, auc_all_melt, permutations, file = "crossvalidation_controls.RData")



permutations$measure <- revalue(permutations$measure, c("permutation_lda"="LDA", "permutation_maxdeviation"="Maximal Deviation",
                                                        "permutation_maxratio"="Maximal LogRatio"))

auc_means <- ddply(auc_all_melt, c("measure", "variable"),
                   function(auc_all_melt){c(mean=mean(auc_all_melt$value, na.rm=T), se= se(auc_all_melt$value, na.rm=T))})

auc_means$measure <- revalue(auc_means$measure, c("lda"="Original LDA", "maxdeviation"="Maximal Deviation",
                                                  "maxratio"="Maximal LogRatio"))

auc_power <- ddply(auc_all_melt, c("measure", "variable"),
                   function(auc_all_melt){power(auc_all_melt$value)})
auc_power$measure <- revalue(auc_power$measure, c("lda"="Original LDA",  "maxdeviation"="Maximal Deviation",
                                                  "maxratio"="Maximal LogRatio"))


p1 <- ggplot(data=auc_means, aes(x=variable, y=mean, group=measure, colour=measure)) +
  geom_line(alpha=.7) +
  geom_point(alpha=.5, size=2) +
  ylab('Mean AUC') +
  xlab('') +
  ylim(0.4, 1) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1)+
  theme_minimal() + labs(colour='Classifier') + theme(legend.position = 'none') + 
  theme(legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14), 
        axis.title = element_text(size = 13), 
        axis.text = element_text(size = 13))

p2 <- ggplot(data=permutations, aes(x=variable, y=value, group=measure, colour=measure)) +
  geom_line(alpha=.7) +
  geom_point(alpha=.5, size=2) +
  ylab('Separability (AUC)') +
  xlab('') +
  ylim(0.6, 1) +
  theme_minimal() + labs(colour='Classifier')  + 
  theme(legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14), 
        axis.title = element_text(size = 13), 
        axis.text = element_text(size = 13))

ggarrange(p1, p2, 
          ncol = 2, nrow = 1,  align = "hv", 
          widths = c(1, 1), heights = c(1,1), common.legend = TRUE, labels = c("A", "B"), legend='right')

