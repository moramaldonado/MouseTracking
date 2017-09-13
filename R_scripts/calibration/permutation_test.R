
#Prepare the data
auc.bins_change <- melt(auc.bins, id=c('bins'))
auc.pvalues <- data.frame(matrix(ncol=length(levels(auc.bins_change$variable))-1, nrow = 2))
colnames(auc.pvalues) <-  levels(auc.bins_change$variable)[2:15]
row.names(auc.pvalues) <- c('pvalue', 'tvalue')
len <- length(levels(auc.bins_change$variable))-1

#Do permutation test for LDA vs. all the other measures
for (l in 1:len)
{
  print(levels(auc.bins_change$variable)[l+1])
  auc.bins_subset <- subset(auc.bins_change,variable=='lda.full'| variable==levels(auc.bins_change$variable)[l+1])
  tr <- factor(auc.bins_subset$variable)
  y <- auc.bins_subset$value
  
  #Mean differences
  diff_original <- diff(by(y, tr, mean))
  dist <- replicate(2000, diff(by(y, sample(tr, length(tr), FALSE), mean)))
  #hist(dist, xlim = c(-0.5, 0.5), col = "black", breaks = 100)
  #abline(v = diff(by(y, tr, mean)), col = "blue", lwd = 2)
  
  # Is the attested difference different from what we would expect by chance?
  p.value <- t.test(dist, y = NULL, alternative = 'two.sided', mu = diff_original)$p.value
  t <- t.test(dist, y = NULL, alternative = 'two.sided', mu = diff_original)$statistic
  
  #Save in DF
  auc.pvalues[1,l] <- p.value
  auc.pvalues[2,l] <- t
  
}

rm(auc.bins_subset)

require(xtable)
xtable(auc.pvalues)





