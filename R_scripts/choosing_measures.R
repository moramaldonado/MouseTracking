## WHICH MEASURE IS A BETTER PREDICTOR FOR THE CALIBRATION ##

## See the distribution in real data
ggplot(calibration_data, aes(fill=Polarity, x=MaxLogRatio, color=Polarity)) + geom_density(alpha=.2)
#calibration_data$deviated <- if_else(calibration_data$Polarity=='deviated', 1, 0)          
#calibration_ratio.lmer <- glmer(deviated ~ MaxLogRatio  + (1|Subject), data = calibration_data, family=binomial)


#Take the sd for each measure, each condition
#1. MaxLogRatio 
MaxLogRatio.sd <- aggregate(MaxLogRatio~Polarity, data=calibration_data, FUN=sd)
#2. MaxDifference
MaxDifference.sd <- aggregate(MaxDifference~Polarity, data=calibration_data, FUN=sd)
#3. MaxDeviation
MaxDeviation.sd <- aggregate(MaxDeviation~Polarity, data=calibration_data, FUN=sd)

##SIMULATION FAKE DATA: Create fake data and run model 
n = 50
# Make new subjects
new_subjects = calibration_data %>%
# Select 2 unique subjects
select(Subject) %>%
distinct() %>%
# Sample subjects n times with replacement
sample_n(n, replace = T) %>%
# Order by original subject ID
arrange(Subject) %>%
# Assign subject IDs 1 to n for sampled subjects
mutate(randomID = paste("randomID", 1:n, sep = "_"))
  # Add some variance per subject

# Make data for new subjects
calibration_data_new_subjects = calibration_data %>%
# Combine original data with list of new subjects to get
inner_join(new_subjects) %>%
  arrange(randomID) %>%
# Make a new column for dependent variable with added variance
mutate(MaxLogRatio_NEW = MaxLogRatio + if_else(Polarity=='straight',
                                               rnorm(1, 0, MaxLogRatio.sd[1,2]),
                                               if_else(Polarity=='deviated',
                                                       rnorm(1, 0, MaxLogRatio.sd[2,2]),
                                                       rnorm(1, 0, MaxLogRatio.sd[3,2])))) %>%
  mutate(MaxDifference_NEW = MaxDifference + if_else(Polarity=='straight',
                                                 rnorm(1, 0, MaxDifference.sd[1,2]),
                                                 if_else(Polarity=='deviated',
                                                         rnorm(1, 0, MaxDifference.sd[2,2]),
                                                         rnorm(1, 0, MaxDifference.sd[3,2])))) %>%
  
  mutate(MaxDeviation_NEW = MaxDeviation + if_else(Polarity=='straight',
                                                     rnorm(1, 0, MaxDeviation.sd[1,2]),
                                                     if_else(Polarity=='deviated',
                                                             rnorm(1, 0, MaxDeviation.sd[2,2]),
                                                             rnorm(1, 0, MaxDeviation.sd[3,2]))))
  

# Run model and save result
calibration_data_new_subjects$Polarity <- factor(calibration_data_new_subjects$Polarity)
calibration.lmer <- glmer(Polarity ~ MaxLogRatio_NEW  +
                            (1|randomID), data = calibration_data_new_subjects, family=binomial)


calibration_data_new_subjects$straight <- if_else(calibration_data_new_subjects$Polarity=='straight', 
                                                  1,
                                                  0)
calibration_data_new_subjects$deviated <- if_else(calibration_data_new_subjects$Polarity=='deviated', 
                                                  1,
                                                  0)
calibration_data_new_subjects$uncertain <- if_else(calibration_data_new_subjects$Polarity=='uncertain', 
                                                   1,
                                                   0)

calibration_deviated_ratio.lmer <- glmer(deviated ~ MaxLogRatio_NEW  +
                            (1|randomID), data = calibration_data_new_subjects, family=binomial)


#visualizing and plotting
calibration_data.summary <- ddply(calibration_data_new_subjects, c("Polarity"),
                                  function(calibration_data.summary)c(MaxLogRatio.mean=mean(calibration_data.summary$MaxLogRatio_NEW, na.rm=T), MaxLogRatio.se=se(calibration_data.summary$MaxLogRatio_NEW, na.rm=T), MaxDifference.mean=mean(calibration_data.summary$MaxDifference_NEW, na.rm=T), MaxDifference.se=se(calibration_data.summary$MaxDifference_NEW, na.rm=T), MaxDeviation.mean=mean(calibration_data.summary$MaxDeviation_NEW, na.rm=T), MaxDeviation.se=se(calibration_data.summary$MaxDeviation_NEW, na.rm=T)))
p1 <- ggplot(calibration_data_new_subjects, aes(x=Polarity, y=MaxLogRatio_NEW, color=Polarity)) + geom_point(alpha=.2)
p2 <-  ggplot(calibration_data_new_subjects, aes(x=Polarity, y=MaxDifference_NEW, color=Polarity)) + geom_point(alpha=.2)
p3 <- ggplot(calibration_data_new_subjects, aes(x=Polarity, y=MaxDeviation_NEW, color=Polarity)) + geom_point(alpha=.2) 
multiplot(p1,p2,p3)


#Another possibility
mod <- multinom(Polarity ~ MaxLogRatio_NEW, calibration_data_new_subjects)
predict(mod)


