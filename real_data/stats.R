##STATS
ggplot(data=controls, aes(x=MaxLogRatio, fill=Sentence_Type)) + 
  geom_histogram(position='dodge') + 
  scale_colour_brewer(palette="Paired", name="Type of sentence",  breaks=c("controln", "controlp"), labels=c("Negative Control", "Affirmative Control")) +
  facet_grid(.~ Truth.value) + theme_bw() + theme(legend.position='none')
ggsave('MaxLogRatio_real_data.png', plot = last_plot(), scale = 1, dpi = 300, width=10, path='real_data/graphs')

ggplot(data=controls, aes(x=MaxLogRatio, fill=Sentence_Type)) + 
  geom_histogram(position='dodge') + 
  scale_colour_brewer(palette="Paired", name="Type of sentence",  breaks=c("controln", "controlp"), labels=c("Negative Control", "Affirmative Control")) +
  facet_grid(Truth.value~Item.number.cut, scale='free_x') + theme_bw() + theme(legend.position='none')
ggsave('MaxLogRatio_real_data_items.png', plot = last_plot(), scale = 1, dpi = 300, width=10, path='real_data/graphs')

mydata <- controls
mydata$Interaction<-factor(contrasts(mydata$Sentence_Type)[mydata$Sentence_Type]*
  contrasts(mydata$Truth.value)[mydata$Truth.value]) 
##following Levy 2014; controlp:true, controln:false=1; controlp:false, controln:true=0


### MaxLogRatio: Differences between conditions and truth values
control_model.logratio <- lmer(MaxLogRatio ~ Sentence_Type + Truth.value + Interaction + (1+Sentence_Type*Truth.value|Subject), data = mydata, REML=FALSE)
summary(control_model.logratio)

#Main Effect: truth value (False vs. True)
m0.truth <- lmer(MaxLogRatio ~ Sentence_Type + Interaction + (1+Sentence_Type*Truth.value|Subject), data = mydata, REML=FALSE) #the value of intercept is not exactly the same as the one in my aggregate function, why?
anova(control_model.logratio, m0.truth)

#Main Effect: sentence type (Affirmative vs. Negative)
m0.sentence <- lmer(MaxLogRatio ~ Truth.value + Interaction + (1+Sentence_Type*Truth.value|Subject), data = mydata, REML=FALSE) #the value of intercept is not exactly the same as the one in my aggregate function, why?
anova(control_model.logratio, m0.sentence)

#Effect of Interaction
m0.interaction <- lmer(MaxLogRatio~ Sentence_Type+Truth.value+ (1+Sentence_Type*Truth.value|Subject), data = mydata, REML=FALSE) #the value of intercept is not exactly the same as the one in my aggregate function, why?
anova(control_model.logratio, m0.interaction)

### LdaMeasure: Differences between conditions and truth values
control_model.lda <- lmer(lda_measure ~ Sentence_Type + Truth.value + Interaction * Item.number + (1+Sentence_Type*Truth.value|Subject), data = mydata, REML=FALSE)
summary(control_model.lda)

#Main Effect: truth value (False vs. True)
m0.truth.lda <- lmer(lda_measure ~ Sentence_Type + Interaction + (1+Sentence_Type*Truth.value|Subject), data = mydata, REML=FALSE) #the value of intercept is not exactly the same as the one in my aggregate function, why?
anova(control_model.lda, m0.truth.lda)

#Main Effect: sentence type (Affirmative vs. Negative)
m0.sentence.lda <- lmer(lda_measure ~ Truth.value + Interaction + Item.number+ (1+Sentence_Type*Truth.value|Subject), data = mydata, REML=FALSE) #the value of intercept is not exactly the same as the one in my aggregate function, why?
anova(control_model.lda, m0.sentence.lda)

#Effect of Interaction
m0.interaction.lda <- lmer(lda_measure~ Sentence_Type+Truth.value+ Item.number+(1+Sentence_Type*Truth.value|Subject), data = mydata, REML=FALSE) #the value of intercept is not exactly the same as the one in my aggregate function, why?
anova(control_model.lda, m0.interaction.lda)



