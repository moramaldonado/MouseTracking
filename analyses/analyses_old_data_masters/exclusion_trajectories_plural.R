

pdf('histograms_positions(before exclusion)_plurals.pdf')
par(mfrow=c(1,2))
hist(Data_Plurals.positions$Y.Position)
hist(Data_Plurals.positions$X.Position)
dev.off()

#Calculating maximal and minimal x.values for exclusion
dat <- ddply(Data_Plurals.positions, c("Subject", "Item.number"),
             function(Data_Plurals.positions)c(min=min(Data_Plurals.positions$X.Position, na.rm=T),
                                                  max=max(Data_Plurals.positions$X.Position, na.rm=T))) 

lower.bound <- -1.7
upper.bound <- 1.7

#doubt: there are in particular 3 subjects that have this pattern of results, maybe it's better just to take them out (instead of )
#Excluding trials with extreme values.

Data.Plurals  <- merge(Data.Plurals, dat, by=c("Subject", "Item.number"))
Data_Plurals.positions <- merge(Data_Plurals.positions, dat, by=c("Subject", "Item.number"))
bad.controls <- subset(Data.Plurals, min < lower.bound | max > upper.bound) 
print(nrow(bad.controls)/nrow(Data.Plurals))


Data.Plurals <- subset(Data.Plurals, min > lower.bound & max < upper.bound )
Data_Plurals.positions <- subset(Data_Plurals.positions, min > lower.bound & max < upper.bound)





