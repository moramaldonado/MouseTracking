
natives <- subset(information, grepl('en',information$language, ignore.case=TRUE))
not_natives <- subset(information, !(subject %in% natives$subject))

accuracy_controls <- ddply(subset(control, Condition != 'Both-C' & Condition!='Both-D'), c("subject"), 
                           function(control)c(mean=mean(control$Accuracy, na.rm=T)))
rt_total_control <- ddply(control, c("subject"), 
                  function(control)c(rt=mean(control$rt, na.rm=T)))

speed_accuracy <- merge(accuracy_controls,rt_total_control )
exclusion_controls <- subset(speed_accuracy, mean<0.75)


control <-subset(control, !(subject %in% exclusion_controls$subject))
experimental_items <-subset(experimental_items, !(subject %in% exclusion_controls$subject))
results <-subset(results, !(subject %in% exclusion_controls$subject))
information <- subset(information, !(subject %in% exclusion_controls$subject))

hist(log(results$rt))