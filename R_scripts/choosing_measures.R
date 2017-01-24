
## WHICH MEASURE IS A BETTER PREDICTOR FOR THE CALIBRATION ##
calibration_data.summary <- ddply(calibration_data, c("Polarity"),
                                  function(calibration_data.summary)c(MaxLogRatio.mean=mean(calibration_data.summary$MaxLogRatio, na.rm=T), MaxLogRatio.se=se(calibration_data.summary$MaxLogRatio, na.rm=T), MaxDifference.mean=mean(calibration_data.summary$MaxDifference, na.rm=T), MaxDifference.se=se(calibration_data.summary$MaxDifference, na.rm=T)))

ggplot(calibration_data, aes(x=Polarity, y=) )

#1. MaxLogRatio
#2. MaxDifference
#3. MaxDeviation

calibration_ratio.lmer <- lmer(MaxLogRatio~Polarity+(1|Subject), data = calibration_data)
calibration_data.lmer_sum <- summary(calibration_ratio.lmer)

calibration_ratio.lmer <- lmer(MaxDeviation~Polarity+(1|Subject), data = calibration_data)
calibration_data.lmer_sum <- summary(calibration_ratio.lmer)



##Bootstrapping the number of calibration
n = 100

## Linear regression
calibration.lmer <- lmer()