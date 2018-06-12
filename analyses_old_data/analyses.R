## TRAJECTORIES for target items
auc_roc <- function(data, indices, score, label, n){
  n_data <- if(n!=FALSE) subset(data, Subject %in% sample(data$Subject, n)) else data 
  data <- n_data[indices,]
  score.te <- unlist(data[,score])  #Measureen
  label.te <- unlist(data[,label]) #Polarity
  roc.te <- roc(label.te, score.te, direction = ">")
  return(roc.te$auc)
}

##establish n for subset
n <- 25

experimental_items$Rank <- with(experimental_items, ave(Item.number, Subject, FUN=rank)) 
experimental_items[do.call(order, experimental_items[c('Subject', 'Rank')]),]

m <- experimental_items %>% dplyr::select(Rank,Subject,Item.number)

experimental_items.positions <- merge(experimental_items.positions, m, by=c('Subject','Item.number'))
identical(levels(experimental_items$Rank), levels(experimental_items.positions$Rank))
experimental_items.positions.subset <- subset(experimental_items.positions, Rank <= n)

experimental_items$Condition <- factor(experimental_items$Condition)
experimental_items$Condition  = factor(experimental_items$Condition ,levels(experimental_items$Condition )[c(2,1)])



### trajectories 
positions_subject.means <- ddply(subset(experimental_items.positions,  Sentence_Type=='starget'), c("Condition", "Response", "Time.Step", "Subject"),
                                 function(experimental_items.positions)c(X.Position.mean=mean(experimental_items.positions$X.Position, na.rm=T), 
                                                                         Y.Position.mean=mean(experimental_items.positions$Y.Position, na.rm=T)))

positions_overall.means <- ddply(positions_subject.means, c("Condition", "Response", "Time.Step"),
                                 function(positions_subject.means)c(X.Position.mean=mean(positions_subject.means$X.Position, na.rm=T), 
                                                                    X.Position.se=se(positions_subject.means$X.Position, na.rm=T),
                                                                    Y.Position.mean=mean(positions_subject.means$Y.Position, na.rm=T), 
                                                                    Y.Position.se=se(positions_subject.means$Y.Position, na.rm=T)))
ggplot(positions_overall.means, aes(x=X.Position.mean, y=Y.Position.mean, color=Condition, group=Condition)) + 
  geom_point(alpha=.6, size=1.5) + 
  xlab('X Coordinate') +
  ylab('Y Coordinate') +
  geom_errorbarh(aes(xmin=X.Position.mean-X.Position.se, xmax=X.Position.mean+X.Position.se), alpha=0.3) + 
  theme_bw() +
  expand_limits(x=c(-1.5,1.5)) + 
  theme(legend.position='top') +
  facet_grid(.~Response) + scale_color_aaas()

ggsave('mean-trajectories.png',  scale = 1, dpi = 300, width = 6, height = 4, path='fig/')





# positions_subject.means <- ddply(subset(experimental_items.positions.subset,  Sentence_Type=='starget'), c("Condition", "Response", "Time.Step", "Subject"),
#                                  function(experimental_items.positions.subset)c(X.Position.mean=mean(experimental_items.positions.subset$X.Position, na.rm=T), 
#                                                                                 Y.Position.mean=mean(experimental_items.positions.subset$Y.Position, na.rm=T)))
# 
# positions_overall.means <- ddply(positions_subject.means, c("Condition", "Response", "Time.Step"),
#                                  function(positions_subject.means)c(X.Position.mean=mean(positions_subject.means$X.Position, na.rm=T), 
#                                                                     X.Position.se=se(positions_subject.means$X.Position, na.rm=T),
#                                                                     Y.Position.mean=mean(positions_subject.means$Y.Position, na.rm=T), 
#                                                                     Y.Position.se=se(positions_subject.means$Y.Position, na.rm=T)))
# 
# 
# ggplot(positions_overall.means, aes(x=X.Position.mean, y=Y.Position.mean, color=Condition, group=Condition)) + 
#   geom_point(alpha=.6, size=1.5) + 
#   ggtitle('Targets')+
#   xlab('X Coordinate') +
#   ylab('Y Coordinate') +
#   geom_errorbarh(aes(xmin=X.Position.mean-X.Position.se, xmax=X.Position.mean+X.Position.se), alpha=0.3) + 
#   theme_bw() +
#   expand_limits(x=c(-1.5,1.5)) + 
#   theme(legend.position='top') +
#   facet_grid(.~Response) + scale_color_aaas()
# 
# ggsave('mean-trajectories-subset.png',  scale = 1, dpi = 300, width = 6, height = 5, path='fig/')

## ACCEPTABILITY
experimental_items$Response.num <- mapvalues(experimental_items$Response, c("false", "true"), c(FALSE, TRUE))
experimental_items$Response.num <- as.logical(experimental_items$Response.num)

mydata.agreggated <- ddply(experimental_items, c("Condition", "Subject"),
                           function(experimental_items){mean=mean(experimental_items$Response.num, na.rm=T)})

mydata.agreggated.overall <- ddply(mydata.agreggated, c("Condition"),
                                   function(mydata.agreggated)c(mean=mean(mydata.agreggated$V1, na.rm=T), se=se(mydata.agreggated$V1, na.rm=T) ))

ggbarplot(mydata.agreggated.overall, y = "mean", x = "Condition", xlab=FALSE,
          color = "Condition", fill= "Condition",
          alpha = 0.8) +   geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) + border() + ylab('Proportion of true responses') +
  scale_colour_aaas() +  scale_fill_aaas()  +  coord_cartesian(ylim = c(0,.25,0.5,0.75,1))
ggsave('acceptability.png',  scale = 1, dpi = 300, width = 6, height = 6, path='fig/')

## RT times
ggplot(experimental_items, aes(x=RT, fill=Condition)) +
  geom_histogram(position="dodge") +
  scale_colour_aaas() +  scale_fill_aaas() +
  theme_bw() +theme(legend.position = "top") + facet_grid(.~Response) 

mydata.agreggated <- ddply(experimental_items, c("Condition", "Subject", "Response"),
                           function(experimental_items){mean=mean(experimental_items$RT.Log, na.rm=T)})

mydata.agreggated.overall <- ddply(mydata.agreggated, c("Condition", "Response"),
                                   function(mydata.agreggated)c(mean=mean(mydata.agreggated$V1, na.rm=T), se=se(mydata.agreggated$V1, na.rm=T) ))

ggbarplot(mydata.agreggated.overall, y = "mean", x = "Condition", ylab= FALSE, 
          color = "Condition", fill= "Condition", alpha = 0.8) +      coord_cartesian(ylim = c(7.75,8.25))+  ylab('Log Response Times') +
  scale_y_continuous(breaks=c(7.75,8,8.25)) +  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) + border() +
  scale_colour_aaas() +  scale_fill_aaas() +  facet_grid(.~Response) 

ggsave('responsetimes-targets.png',  scale = 1, dpi = 300, width = 6, height = 6, path='fig/')


## simple model for stats 

###restrict analyses to TRUE items
mydata <- subset(experimental_items, Response=='true')

m1 <- lmer(RT.Log ~ Condition +  (1+Condition|Subject), data = mydata, REML=FALSE)
summary(m1)
m0 <- lmer(RT.Log ~ 1 +  (1+Condition|Subject), data = mydata, REML=FALSE)
anova(m1,m0)

##LDA

load('LDA-Full.RData')
LDA_test.coord.dist(experimental_items,v_lda,b_lda,m_pca,all_data_columns,n_pca)
experimental_items <- dplyr::full_join(lda_measure_te.df, experimental_items, by=c("Subject", "Item.number", "Response"))

experimental_items.true <- subset(experimental_items, Response=='true')
experimental_items.false <- subset(experimental_items, Response=='false')

plot_measure_target(experimental_items.true,'MaxLogRatio','Condition')
ggsave('MaxLogRatio-experimental-true.png', plot = last_plot(), scale = 1, dpi = 300, width = 7, height = 5, path='fig/')

#plot_measure_target(subset(experimental_items.true, Rank<50),'MaxLogRatio','Condition')
#ggsave('MaxLogRatio-experimental-true-subset.png', plot = last_plot(), scale = 1, dpi = 300, width = 7, height = 5, path='fig/')

#plot_measure_target(experimental_items.false, 'MaxLogRatio','Condition')
#ggsave('MaxLogRatio-experimental-false.png', plot = last_plot(), scale = 1, dpi = 300, width = 7, height = 5, path='fig/')

plot_measure_target(experimental_items.true,'MaxDeviation','Condition')
ggsave('MaxDeviation-experimental-true.png', plot = last_plot(), scale = 1, dpi = 300, width = 7, height = 5, path='fig/')

#plot_measure_target(experimental_items.false, 'MaxDeviation','Condition')
#ggsave('MaxDeviation-experimental-false.png', plot = last_plot(), scale = 1, dpi = 300, width = 7, height = 5, path='fig/')

#plot_measure_target(subset(experimental_items.true, Rank<50),'MaxDeviation','Condition')
#ggsave('MaxDeviation-experimental-true-subset.png', plot = last_plot(), scale = 1, dpi = 300, width = 7, height = 5, path='fig/')


plot_measure_target(experimental_items.true,'lda_measure','Condition')
ggsave('LDA-experimental-true.png', plot = last_plot(), scale = 1, dpi = 300, width = 7, height = 5, path='fig/')

# plot_measure_target(experimental_items.false, 'lda_measure','Condition')
# ggsave('lda_measure-experimental-false.png', plot = last_plot(), scale = 1, dpi = 300, width = 7, height = 5, path='fig/')
# 
# 
# plot_measure_target(subset(experimental_items.true, Rank<50),'lda_measure','Condition')
# ggsave('LDA-experimental-true-subset.png', plot = last_plot(), scale = 1, dpi = 300, width = 7, height = 5, path='fig/')
# 

## cross validation
experimental_items.true <- random.within(experimental_items.true, "Condition")
permutation_values <- c(1,2,3)


results_lda <- boot(data=experimental_items.true, statistic=auc_roc, R=1000, score='lda_measure', label= 'Condition', n=FALSE)
results_lda.random <- boot(data=experimental_items.true, statistic=auc_roc, R=1000, score='lda_measure', label= 'Random', n=FALSE)
data <- rbind(data.frame(value=results_lda$t, variable='original'), data.frame(value=results_lda.random$t, variable='null'))  
roc.te <- roc(data$variable, data$value)
permutation_values[1]<- roc.te$auc 
permutation_value_lda<- roc.te$auc 
cdat_lda <- ddply(data, "variable", summarise, rating.mean=mean(value))
save(data,cdat_lda,permutation_value_lda, file='LDA-targets-test.RData')

ggplot(data, aes(x=value, fill=variable, color=variable))  + xlab('AUC for bootstrapped samples') + 
  geom_density(alpha=.5) + scale_fill_uchicago() + theme_bw() + scale_color_uchicago() + coord_cartesian(xlim = c(0.4, 0.6)) + 
  geom_vline(data=cdat_lda, aes(xintercept=rating.mean, color=variable),size=0.6) + theme(legend.position = 'none') +
  geom_text(aes(x=cdat_lda$rating.mean[1]-.003, label=as.character(round(cdat_lda$rating.mean[1],2)), y=2), colour="black", angle=90, text=element_text(size=10)) + 
  geom_text(aes(x=cdat_lda$rating.mean[2]-.003, label=as.character(round(cdat_lda$rating.mean[2],2)), y=2), colour="black", angle=90, text=element_text(size=10)) 
ggsave('LDA-targets-test.png', plot = last_plot(), scale = 1, dpi = 300, width = 5, height = 5, path='fig/')


results_maxdeviation <- boot(data=experimental_items.true, statistic=auc_roc, R=1000, score='MaxDeviation', label= 'Condition', n=FALSE)
results_maxdeviation.random <- boot(data=experimental_items.true, statistic=auc_roc, R=1000, score='MaxDeviation', label= 'Random', n=FALSE)
data <- rbind(data.frame(value=results_maxdeviation$t, variable='original'), data.frame(value=results_maxdeviation.random$t, variable='null'))  
roc.te <- roc(data$variable, data$value)

permutation_values[2]<- roc.te$auc  
permutation_value_MD<- roc.te$auc  
cdat_md<- ddply(data, "variable", summarise, rating.mean=mean(value))
save(data,cdat_md,permutation_value_MD, file='MD-targets-test.RData')

ggplot(data, aes(x=value, fill=variable, color=variable))  + xlab('AUC for bootstrapped samples') + 
  geom_density(alpha=.5) + scale_fill_uchicago() + theme_bw() +   scale_color_uchicago() + coord_cartesian(xlim = c(0.4, 0.6)) + 
  geom_vline(data=cdat_md, aes(xintercept=rating.mean,  color=variable),size=1) + theme(legend.position = 'none') +
  geom_text(aes(x=cdat_md$rating.mean[1]-.003, label=as.character(round(cdat_md$rating.mean[1],2)), y=2), colour="black", angle=90, text=element_text(size=10)) + 
  geom_text(aes(x=cdat_md$rating.mean[2]-.003, label=as.character(round(cdat_md$rating.mean[2],2)), y=2), colour="black", angle=90, text=element_text(size=10)) 

ggsave('MD-targets-test.png', plot = last_plot(), scale = 1, dpi = 300, width = 5, height = 5, path='fig/')


results_maxratio <- boot(data=experimental_items.true, statistic=auc_roc, R=1000, score='MaxLogRatio', label= 'Condition', n=FALSE)
results_maxratio.random <- boot(data=experimental_items.true, statistic=auc_roc, R=1000, score='MaxLogRatio', label= 'Random', n=FALSE)
data <- rbind(data.frame(value=results_maxratio$t, variable='original'), data.frame(value=results_maxratio.random$t, variable='null'))  
roc.te <- roc(data$variable, data$value)
permutation_values[3]<- roc.te$auc  
permutation_value_maxlogratio<- roc.te$auc  
cdat_max_ratio<- ddply(data, "variable", summarise, rating.mean=mean(value))
save(data,cdat_max_ratio,permutation_value_maxlogratio, file='MaxLogRatio-targets-test.RData')

ggplot(data, aes(x=value, fill=variable, color=variable))  + xlab('AUC for bootstrapped samples') + 
  geom_density(alpha=.5) + scale_fill_uchicago() + theme_bw() + scale_color_uchicago() + coord_cartesian(xlim = c(0.4, 0.6)) + 
  geom_vline(data=cdat_max_ratio, aes(xintercept=rating.mean, color=variable),size=0.6) + theme(legend.position = 'none') +
  geom_text(aes(x=cdat_max_ratio$rating.mean[1]-.003, label=as.character(round(cdat_max_ratio$rating.mean[1],2)), y=2), colour="black", angle=90, text=element_text(size=10)) + 
  geom_text(aes(x=cdat_max_ratio$rating.mean[2]-.003, label=as.character(round(cdat_max_ratio$rating.mean[2],2)), y=2), colour="black", angle=90, text=element_text(size=10)) 

ggsave('MaxLogRatio-targets-test.png', plot = last_plot(), scale = 1, dpi = 300, width = 5, height = 5, path='fig/')

