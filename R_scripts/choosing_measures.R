
## WHICH MEASURE IS A BETTER PREDICTOR FOR THE CALIBRATION ##

#1. MaxLogRatio
#2. MaxDifference
#3. MaxDeviation

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
mutate(randomID = paste("randomID", 1:n, sep = "_"))%>%
# Add some variance per subject
mutate(MaxLogRatio_sd = rnorm(n, 0, sd(calibration_data$MaxLogRatio)))%>%
mutate(MaxDifference_sd = rnorm(n, 0, sd(calibration_data$MaxDifference)))%>%  
mutate(MaxDeviation_sd = rnorm(n, 0, sd(calibration_data$MaxDeviation)))

# Make data for new subjects
calibration_data_new_subjects = calibration_data %>%
# Combine original data with list of new subjects to get
inner_join(new_subjects) %>%
# Make a new column for dependent variable with added variance
mutate(MaxLogRatio_NEW = MaxLogRatio + MaxLogRatio_sd) %>%
mutate(MaxDifference_NEW = MaxDifference + MaxDifference_sd) %>%
mutate(MaxDeviation_NEW = MaxLogRatio + MaxDeviation_sd) 


# Run model and save result
calibration_data_new_subjects$Polarity_num <- as.numeric(factor(calibration_data_new_subjects$Polarity))-1
calibration.lmer <- lmer(Polarity_num ~ MaxLogRatio_NEW  +
                            (1|randomID), data = calibration_data_new_subjects)
calibration.lmer_sum = summary(calibration.lmer)
calibration.lmer_anova = anova(calibration.lmer)

calibration_data.summary <- ddply(calibration_data_new_subjects, c("Polarity"),
                                  function(calibration_data.summary)c(MaxLogRatio.mean=mean(calibration_data.summary$MaxLogRatio_NEW, na.rm=T), MaxLogRatio.se=se(calibration_data.summary$MaxLogRatio_NEW, na.rm=T), MaxDifference.mean=mean(calibration_data.summary$MaxDifference_NEW, na.rm=T), MaxDifference.se=se(calibration_data.summary$MaxDifference_NEW, na.rm=T), MaxDeviation.mean=mean(calibration_data.summary$MaxDeviation_NEW, na.rm=T), MaxDeviation.se=se(calibration_data.summary$MaxDeviation_NEW, na.rm=T)))

#plotting
p1 <- ggplot(calibration_data.summary, aes(x=Polarity, y=MaxLogRatio.mean, color=Polarity)) + geom_point() + geom_errorbar(aes(ymin=MaxLogRatio.mean-MaxLogRatio.se,   ymax=MaxLogRatio.mean+MaxLogRatio.se), width=.1) 
p2 <- ggplot(calibration_data.summary, aes(x=Polarity, y=MaxDifference.mean, color=Polarity)) + geom_point()  + geom_point() + geom_errorbar(aes(ymin=MaxDifference.mean-MaxDifference.se,   ymax=MaxDifference.mean+MaxDifference.se), width=.1) 
p3 <- ggplot(calibration_data.summary, aes(x=Polarity, y=MaxDeviation.mean , color=Polarity)) + geom_point() + geom_point() + geom_errorbar(aes(ymin=MaxDeviation.mean-MaxDeviation.se,   ymax=MaxDeviation.mean+MaxDeviation.se), width=.1) 
multiplot(p1,p2,p3)
