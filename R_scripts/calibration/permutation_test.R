library(lmPerm)

auc.bins$random_classifier <- rowMeans(random_classifier.df)

auc.bins_change <- melt(auc.bins, id=c('bins'))


m <- subset(auc.bins_change, variable=='lda.full'|variable=='logratio')
mod2 <- aovp(value ~ bins + variable, data=m)
summary(mod2)


