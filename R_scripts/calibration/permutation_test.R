
#Prepare the data
auc.bins_change <- melt(auc.bins, id=c('bins'))
auc.pvalues <- data.frame(matrix(ncol=length(levels(auc.bins_change$variable))-1, nrow = 2))
colnames(auc.pvalues) <-  levels(auc.bins_change$variable)[2:15]
row.names(auc.pvalues) <- c('pvalue', 'tvalue')
len <- length(levels(auc.bins_change$variable))-1


diff_mean <- function(y, tr, group1, group2) {
  mean(y[tr == group1]) - mean(y[tr == group2])
}

ITERATIONS <- 100

#Do permutation test for LDA vs. all the other measures
for (l in 1:len)
{
  baseline <- 'lda.full'
  comparison <- levels(auc.bins_change$variable)[l+1]
  print(comparison)
  auc.bins_subset <- subset(auc.bins_change,variable %in% c(baseline, comparison))
  tr <- factor(auc.bins_subset$variable)
  y <- auc.bins_subset$value
  #Mean differences
  diff_original <- diff_mean(y, tr, baseline, comparison)
  dist <- replicate(ITERATIONS, diff_mean(y, sample(tr, length(tr), FALSE),  baseline, comparison))
  #hist(dist, xlim = c(-0.5, 0.5), col = "black", breaks = 100)
  #abline(v = diff(by(y, tr, mean)), col = "blue", lwd = 2)
  
  
  p.value <- sum(dist > diff_original)/ITERATIONS  # one-tailed test
  
  
  # Is the attested difference different from what we would expect by chance?
  #p.value <- t.test(dist, y = NULL, alternative = 'two.sided', mu = diff_original)$p.value
  #t <- t.test(dist, y = NULL, alternative = 'two.sided', mu = diff_original)$statistic
  
  #Save in DF
  auc.pvalues[1,l] <- p.value
  auc.pvalues[2,l] <- diff_original
  
}

rm(auc.bins_subset)

require(xtable)
xtable(auc.pvalues)





