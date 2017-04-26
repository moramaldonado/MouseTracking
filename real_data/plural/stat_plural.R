
ggplot(data=experimental_items, aes(x=MaxLogRatio, fill=Condition)) + 
  geom_histogram(position='dodge') + 
  facet_grid(.~ Response) + theme_bw() + theme(legend.position='none')
ggsave('MaxLogRatio_plural_data.png', plot = last_plot(), scale = 1, dpi = 300, width=10, path='real_data/plural/graphs')



##STATS
mydata <- subset(experimental_items, Response=='true')

### LdaMeasure: Differences between conditions and truth values
plurals_model.lda <- lmer(lda_measure ~ Condition + (1+Condition|Subject), data = mydata, REML=FALSE)
summary(plurals_model.lda)

#Main Effect: truth value (False vs. True)
m0.condition.lda <- lmer(lda_measure ~ 1 + (1+Condition|Subject), data = mydata, REML=FALSE) #the value of intercept is not exactly the same as the one in my aggregate function, why?
anova(plurals_model.lda, m0.condition.lda)

### LdaMeasure: Differences between conditions and truth values
plurals_model.ratio <- lmer( MaxLogRatio~Condition + (1+Condition|Subject), data = mydata, REML=FALSE)
summary(plurals_model.ratio)

#Main Effect: truth value (False vs. True)
m0.condition.ratio <- lmer(MaxLogRatio ~ 1 + (1+Condition|Subject), data = mydata, REML=FALSE) #the value of intercept is not exactly the same as the one in my aggregate function, why?
anova(plurals_model.ratio, m0.condition.ratio)

