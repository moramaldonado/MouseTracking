

## WHICH MEASURE IS A BETTER PREDICTOR FOR THE CALIBRATION ##

## See the distribution in real data
ggplot(calibration_data, aes(fill=Polarity, x=MaxLogRatio, fill=Polarity)) + geom_histogram(binwidth=1,  position="dodge")+ theme(legend.position = "top")
ggsave('MaxLogRatio.png', plot = last_plot(), scale = 1, dpi = 300)

#Take the sd for each measure, each condition
#1. MaxLogRatio 
MaxLogRatio.sd <- aggregate(MaxLogRatio~Polarity, data=calibration_data, FUN=sd)
#2. MaxDifference
MaxDifference.sd <- aggregate(MaxDifference~Polarity, data=calibration_data, FUN=sd)
#3. MaxDeviation
MaxDeviation.sd <- aggregate(MaxDeviation~Polarity, data=calibration_data, FUN=sd)

##SIMULATION FAKE DATA: Create fake data and run model 
n = 150
# Make new subjects
new_subjects = calibration_data %>%
# Select 2 unique subjects
dplyr::select(Subject) %>%
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
mutate(MaxLogRatio_NEW = MaxLogRatio + if_else(Polarity=='deviated',
                                               rnorm(1, 0, MaxLogRatio.sd[1,2]),
                                               if_else(Polarity=='straight',
                                                       rnorm(1, 0, MaxLogRatio.sd[2,2]),
                                                       rnorm(1, 0, MaxLogRatio.sd[3,2])))) %>%
  
  mutate(MaxDifference_NEW = MaxDifference + if_else(Polarity=='deviated',
                                                 rnorm(1, 0, MaxDifference.sd[1,2]),
                                                 if_else(Polarity=='straight',
                                                         rnorm(1, 0, MaxDifference.sd[2,2]),
                                                         rnorm(1, 0, MaxDifference.sd[3,2])))) %>%
  
  mutate(MaxDeviation_NEW = MaxDeviation + if_else(Polarity=='deviated',
                                                     rnorm(1, 0, MaxDeviation.sd[1,2]),
                                                     if_else(Polarity=='straight',
                                                             rnorm(1, 0, MaxDeviation.sd[2,2]),
                                                             rnorm(1, 0, MaxDeviation.sd[3,2]))))
  

#SEE the same as before but with FAKE data: visualizing and plotting 
calibration_data.summary <- ddply(calibration_data_new_subjects, c("Polarity"),
                                  function(calibration_data.summary)c(MaxLogRatio.mean=mean(calibration_data.summary$MaxLogRatio_NEW, na.rm=T), MaxLogRatio.se=se(calibration_data.summary$MaxLogRatio_NEW, na.rm=T), MaxDifference.mean=mean(calibration_data.summary$MaxDifference_NEW, na.rm=T), MaxDifference.se=se(calibration_data.summary$MaxDifference_NEW, na.rm=T), MaxDeviation.mean=mean(calibration_data.summary$MaxDeviation_NEW, na.rm=T), MaxDeviation.se=se(calibration_data.summary$MaxDeviation_NEW, na.rm=T)))
ggplot(calibration_data_new_subjects, aes(fill=Polarity, x=MaxLogRatio_NEW, color=Polarity)) + geom_histogram(alpha=.2, binwidth=.05)



# Testing model FAKE DATA for MAX LOG RATIO


calibration_data_new_subjects$straight <- if_else(calibration_data_new_subjects$Polarity=='straight', 
                                                  1,
                                                  0)
calibration_data_new_subjects$deviated <- if_else(calibration_data_new_subjects$Polarity=='deviated', 
                                                  1,
                                                  0)
calibration_data_new_subjects$uncertain <- if_else(calibration_data_new_subjects$Polarity=='uncertain', 
                                                   1,
                                                   0)


## FIND A THRESHOLD

# iff MaxLogRatio > 2, 'deviated'; else 'uncertain'
classified_data <- calibration_data_new_subjects %>%
  dplyr::select(randomID, MaxLogRatio_NEW, Polarity) %>%
  mutate(classification= if_else(MaxLogRatio_NEW>2,
                                 'class.deviation', 
                                 if_else(MaxLogRatio_NEW>0.1 & MaxLogRatio_NEW <= 2, 
                                         'class.uncertain', 
                                         'class.straight')))
ggplot(classified_data, aes(x=MaxLogRatio_NEW, y=classification, color=Polarity)) +
  geom_point(shape=1, alpha=.5,      # Use hollow circles
             position=position_jitter(width=1,height=.5)) + scale_x_continuous(breaks=seq(-1, 8, 0.5)) 
  


## TEST how it does with the data I have from previous experiment
classified_data <-  data %>% filter(Sentence_Type=='EI') %>%
  dplyr::select(Subject, MaxLogRatio, Polarity) %>%
  mutate(classification= if_else(MaxLogRatio>2,
                                 'class.deviation', 
                                 if_else(MaxLogRatio>0.1 & MaxLogRatio<=2, 
                                         'class.uncertain', 
                                         'class.straight')))
ggplot(classified_data, aes(color= Polarity, y=MaxLogRatio, x=classification))+ geom_point()


