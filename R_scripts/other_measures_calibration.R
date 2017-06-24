
##PLOTTING different measures
plot_measure <- function(data, measure){
  histogram <- ggplot(data, aes(x=measure, fill=Polarity)) +
    geom_histogram(bins=12,  position="dodge")+
    scale_fill_brewer(palette="Set1")+
    theme(legend.position = "top") +
    theme_minimal()
  
  density <-ggplot(data, aes(x=measure, fill=Polarity)) +
    geom_density(alpha=.5)+
    scale_fill_brewer(palette="Set1")+
    theme_minimal() +theme(legend.position = "none")
  
  mydata.agreggated <- ddply(data, c("Polarity", "Subject"),
                                 function(data)c(mean=mean(data$measure, na.rm=T)))
  mydata.agreggated.overall <- ddply(mydata.agreggated, c("Polarity"),
                                         function(mydata.agreggated)c(mean=mean(mydata.agreggated$mean, na.rm=T), se=se(mydata.agreggated$mean, na.rm=T) ))
  
  
  mean <- ggplot(mydata.agreggated.overall, aes(x=Polarity, y=mean, fill=Polarity)) +
    geom_bar(position=position_dodge(), stat="identity") +
    scale_fill_brewer(palette="Set1")+  
    theme_minimal()+
    xlab(' ')  +
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) 
  

  return(multiplot(density, histogram, mean, cols=2))  
}



#LDA
lda_histo <- ggplot(calibration_data, aes(x=lda_measure, fill=Polarity)) +
  geom_histogram(bins=12,  position="dodge")+
  scale_fill_brewer(palette="Set1")+
  theme(legend.position = "top") +
  theme_minimal()
lda_density <-ggplot(calibration_data, aes(x=lda_measure_logratio, fill=Polarity)) +
  geom_density(alpha=.5)+
  scale_fill_brewer(palette="Set1")+
   theme_minimal() +theme(legend.position = "none")
mydata.agreggated.lda <- ddply(calibration_data, c("Polarity", "Subject"),
                               function(calibration_data)c(lda_measure=mean(calibration_data$lda_measure, na.rm=T)))
mydata.agreggated.overall.lda <- ddply(mydata.agreggated.lda, c("Polarity"),
                                       function(mydata.agreggated.lda)c(mean=mean(mydata.agreggated.lda$lda_measure, na.rm=T), se=se(mydata.agreggated.lda$lda_measure, na.rm=T) ))

lda_mean <- ggplot(mydata.agreggated.overall.lda, aes(x=Polarity, y=mean, fill=Polarity)) +
  geom_bar(position=position_dodge(), stat="identity") +
  scale_fill_brewer(palette="Set1")+  
  theme_minimal()+
  xlab(' ')  +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) 

png(filename= "R_scripts/graphs/calibration_new/LDA.png", width=800, height=480, unit='px')
multiplot(lda_density, lda_mean, cols=2)
dev.off()



#MAXLOGRATIO
logratio_histo <-ggplot(calibration_data, aes(x=MaxLogRatio, fill=Polarity)) +
  geom_histogram(bins=15,  position="dodge")+
  theme(legend.position = "top") +
  theme_minimal()

logratio_density <-ggplot(calibration_data, aes(x=MaxLogRatio, fill=Polarity)) +
  geom_density(alpha=.5)+
  theme(legend.position = "top") +
  scale_fill_brewer(palette="Set1") +
  theme_minimal()


mydata.agreggated.maxratio <- ddply(calibration_data, c("Polarity", "Subject"),
                                    function(calibration_data)c(MaxLogRatio=mean(calibration_data$MaxLogRatio, na.rm=T)))

mydata.agreggated.overall.maxratio <- ddply(mydata.agreggated.maxratio, c("Polarity"),
                                            function(mydata.agreggated.maxratio)c(mean=mean(mydata.agreggated.maxratio$MaxLogRatio, na.rm=T), se=se(mydata.agreggated.maxratio$MaxLogRatio, na.rm=T) ))

logratio_mean <- ggplot(mydata.agreggated.overall.maxratio, aes(x=Polarity, y=mean, fill=Polarity)) +
  geom_bar(position=position_dodge(), stat="identity") +
  scale_fill_brewer(palette="Set1")+  
  xlab(' ')  +
  theme_minimal()+ theme(legend.position = "none")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9))

png(filename= "R_scripts/graphs/calibration_new/MaxLogRatio.png", width=800, height=480, unit='px')
multiplot(logratio_density, logratio_mean, cols=2)
dev.off()





ggplot(calibration_data, aes(x=AUC, fill=Polarity)) +
  geom_histogram(bins=15,  position="dodge")+
  theme(legend.position = "top") +
  theme_minimal() +
  facet_grid(.~Expected_response)
ggsave('AUC_histo_SvD.png',  width = 6, path='R_scripts/graphs/calibration_new')

ggplot(calibration_data, aes(x=AUC, fill=Polarity)) +
  geom_density(alpha=.5)+
  theme(legend.position = "top") +
  theme_minimal()+
  facet_grid(.~Expected_response)
ggsave('AUC_density_SvD.png',  width = 6, path='R_scripts/graphs/calibration_new')


# MAX DEVIATION
ggplot(calibration_data, aes(x=MaxDeviation, fill=Polarity)) +
  geom_histogram(bins=15,  position="dodge")+
  theme(legend.position = "top") +
  theme_minimal() 

maxdev.density <-  ggplot(calibration_data, aes(x=MaxDeviation, fill=Polarity)) +
  geom_density(alpha=.5)+
  theme(legend.position = "none") +
  theme_minimal() + theme(legend.position = "none")+
  scale_fill_brewer(palette="Set1")
  
mydata.agreggated.maxdev <- ddply(calibration_data, c("Polarity", "Subject"),
                                    function(calibration_data)c(MaxDeviation=mean(calibration_data$MaxDeviation, na.rm=T)))

mydata.agreggated.overal.maxdev <- ddply(mydata.agreggated.maxdev, c("Polarity"),
                                         function(mydata.agreggated.maxdev)c(mean=mean(mydata.agreggated.maxdev$MaxDeviation, na.rm=T), se=se(mydata.agreggated.maxdev$MaxDeviation, na.rm=T) ))

maxdev.mean <- ggplot(mydata.agreggated.overal.maxdev, aes(x=Polarity, y=mean, fill=Polarity)) +
  geom_bar(position=position_dodge(), stat="identity") +
  scale_fill_brewer(palette="Set1")+
  ggtitle('maxdev')+
  xlab(' ')  +
  theme_minimal()+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9))


png(filename= "R_scripts/graphs/calibration_new/maxdev.png", width=800, height=480, unit='px')
multiplot(maxdev.density, maxdev.mean, cols=2)
dev.off()



# X FLIPS
ggplot(calibration_data, aes(x=X.flips, fill=Polarity)) +
  geom_histogram(bins=15,  position="dodge")+
  theme(legend.position = "top") +
  theme_minimal() +
  facet_grid(.~Expected_response)

xflips.density <-  ggplot(calibration_data, aes(x=X.flips, fill=Polarity)) +
  geom_density(alpha=.5)+
  theme(legend.position = "none") +
  theme_minimal()  + theme(legend.position = "none")+
  scale_fill_brewer(palette="Set1")

mydata.agreggated.xflips <- ddply(calibration_data, c("Polarity", "Subject"),
                                  function(calibration_data)c(X.flips=mean(calibration_data$X.flips, na.rm=T)))

mydata.agreggated.overal.xflips <- ddply(mydata.agreggated.xflips, c("Polarity"),
                                         function(mydata.agreggated.xflips)c(mean=mean(mydata.agreggated.xflips$X.flips, na.rm=T), se=se(mydata.agreggated.xflips$X.flips, na.rm=T) ))

xflips.mean <- ggplot(mydata.agreggated.overal.xflips, aes(x=Polarity, y=mean, fill=Polarity)) +
  geom_bar(position=position_dodge(), stat="identity") +
  scale_fill_brewer(palette="Set1")+
  ggtitle('XFlips')+
  xlab(' ')  +
  theme_minimal()+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9))


png(filename= "R_scripts/graphs/calibration_new/XFlips.png", width=800, height=480, unit='px')
multiplot(xflips.density, xflips.mean, cols=2)
dev.off()



# ACC FLIPS
ggplot(subset(calibration_data, Polarity!='uncertain'), aes(x=Acc.flips, fill=Polarity)) +
  geom_histogram(bins=15,  position="dodge")+
  theme(legend.position = "top") +
  theme_minimal() +
  facet_grid(.~Expected_response)
ggsave('AccFlips_histo_SvD.png',  width = 6, path='R_scripts/graphs/calibration_new')

ggplot(subset(calibration_data, Polarity!='uncertain'), aes(x=Acc.flips, fill=Polarity)) +
  geom_density(alpha=.5)+
  theme(legend.position = "top") +
  theme_minimal()
ggsave('AccFlips_densit_SvD.png',  width = 6, path='R_scripts/graphs/calibration_new')




mydata.agreggated.accflips <- ddply(calibration_data, c("Polarity", "Subject"),
                                  function(calibration_data)c(Acc.flips=mean(calibration_data$Acc.flips, na.rm=T)))

mydata.agreggated.overal.accflips <- ddply(mydata.agreggated.accflips, c("Polarity"),
                                         function(mydata.agreggated.accflips)c(mean=mean(mydata.agreggated.accflips$Acc.flips, na.rm=T), se=se(mydata.agreggated.accflips$Acc.flips, na.rm=T) ))


