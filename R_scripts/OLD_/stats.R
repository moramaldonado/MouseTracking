##STATS
mydata <- negation_data
mydata$Expected_response <- factor(mydata$Expected_response)
mydata$Interaction<-factor(contrasts(mydata$Polarity)[mydata$Polarity]*
  contrasts(mydata$Expected_response)[mydata$Expected_response]) 


### LdaMeasure: Differences between conditions and truth values
control_model.lda <- lmer(lda_measure ~ Polarity + Expected_response + Interaction * Item.number + (1+Polarity*Expected_response|Subject), data = mydata, REML=FALSE)
summary(control_model.lda)

#Main Effect: Polarity (Affirmative vs. Negative)
m0.sentence.lda <- lmer(lda_measure ~ Expected_response + Interaction + (1+Polarity*Expected_response|Subject), data = mydata, REML=FALSE) #the value of intercept is not exactly the same as the one in my aggregate function, why?
anova(control_model.lda, m0.sentence.lda)

#Main Effect :Expected_response (Affirmative vs. Negative)
m0.response.lda <- lmer(lda_measure ~ Polarity + Interaction + (1+Polarity*Expected_response|Subject), data = mydata, REML=FALSE) #the value of intercept is not exactly the same as the one in my aggregate function, why?
anova(control_model.lda, m0.response.lda)

#Effect of Interaction
m0.interaction.lda <- lmer(lda_measure~ Polarity+Expected_response+ (1+Polarity*Expected_response|Subject), data = mydata, REML=FALSE) #the value of intercept is not exactly the same as the one in my aggregate function, why?
anova(control_model.lda, m0.interaction.lda)



