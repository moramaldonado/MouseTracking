## controls


# area under the ROC curve
auc_roc <- function(data, indices, score, label, n){
  n_data <- if(n!=FALSE) subset(data, Subject %in% sample(data$Subject, n)) else data 
  data <- n_data[indices,]
  score.te <- unlist(data[,score])  #Measureen
  label.te <- unlist(data[,label]) #Polarity
  roc.te <- roc(label.te, score.te, direction='<')
  return(roc.te$auc)
}


##Add rank
n <- 25
controls$Rank <- with(controls, ave(Item.number, Subject, FUN=rank)) 
controls[do.call(order, controls[c('Subject', 'Rank')]),]
m <- controls %>% dplyr::select(Rank,Subject,Item.number)
controls.positions <- merge(controls.positions, m, by=c('Subject','Item.number'))
identical(levels(controls$Rank), levels(controls.positions$Rank))
controls.positions.subset <- subset(controls.positions, Rank <= n)

controls$Condition <- factor(controls$Condition)
controls$Condition  = factor(controls$Condition ,levels(controls$Condition )[c(2,1)])

controls.positions$Condition <- factor(controls.positions$Condition)
controls.positions$Condition  = factor(controls.positions$Condition ,levels(controls.positions$Condition )[c(2,1)])



## trajectories ####
controls_positions_subject.means <- ddply(controls.positions, c("Condition", "Truth.value", "Time.Step", "Subject"),
                                          function(controls.positions)c(X.Position.mean=mean(controls.positions$X.Position, na.rm=T), 
                                                                        Y.Position.mean=mean(controls.positions$Y.Position, na.rm=T)))

controls_positions_overall.means <- ddply(controls_positions_subject.means, c("Condition", "Truth.value", "Time.Step"),
                                          function(controls_positions_subject.means)c(X.Position.mean=mean(controls_positions_subject.means$X.Position, na.rm=T), 
                                                                                      X.Position.se=se(controls_positions_subject.means$X.Position, na.rm=T),
                                                                                      Y.Position.mean=mean(controls_positions_subject.means$Y.Position, na.rm=T), 
                                                                                      Y.Position.se=se(controls_positions_subject.means$Y.Position, na.rm=T)))

ggplot(controls_positions_overall.means, aes(x=X.Position.mean, y=Y.Position.mean, color=Condition, group=Condition)) + 
  geom_point(alpha=.8, size=1.5) + 
  xlab('X Coordinate') +
  ylab('Y Coordinate') +
  geom_errorbarh(aes(xmin=X.Position.mean-X.Position.se, xmax=X.Position.mean+X.Position.se), alpha=0.3) + 
  theme_bw() +
  expand_limits(x=c(-1.5,1.5)) + 
  theme(legend.position='none') +
  facet_grid(.~Truth.value) + scale_colour_aaas()
ggsave('mean-trajectories-controls.png', scale = 1, dpi = 300, width = 6, height = 4, path='fig/')


##timestep ####
# controls_positions_subject.means <- ddply(controls.positions, c("Condition", "Truth.value", "Time.Step", "Subject"),
#                                           function(controls.positions)c(X.Position.mean=mean(controls.positions$X.Position, na.rm=T)))
# 
# 
# 
# 
# 
# 
# controls_positions_overall.means <- ddply(controls_positions_subject.means, c("Condition", "Truth.value", "Time.Step"),
#                                           function(controls_positions_subject.means)c(X.Position.mean=mean(controls_positions_subject.means$X.Position, na.rm=T), 
#                                                                                     X.Position.se=se(controls_positions_subject.means$X.Position, na.rm=T)))
# ggplot(controls_positions_subject.means, aes(x=Time.Step, y=X.Position.mean, color=Condition, group=Subject)) + 
#   geom_line(alpha=.2, size=1) + 
#   xlab('X Coordinate') +
#   theme_bw() +
#   expand_limits(y=c(-1.5,1.5)) + 
#   theme(legend.position='top') +
#   facet_grid(Condition~Truth.value) + scale_color_jco()
# 
# ggsave('subjects-timestepx-controls.png', scale = 1, dpi = 300, width = 6, height = 5, path='fig/')
# 
# ggplot(controls_positions_overall.means, aes(x=Time.Step, y=X.Position.mean, color=Condition, group=Condition)) + 
#   geom_point(alpha=1, size=1.5) + 
#   xlab('X Coordinate') +
#   geom_errorbar(aes(ymin=X.Position.mean-X.Position.se, ymax=X.Position.mean+X.Position.se), alpha=0.3) + 
#   theme_bw() +
#   expand_limits(y=c(-1.5,1.5)) + 
#   theme(legend.position='none') +
#   facet_grid(Condition~Truth.value) + scale_color_jco()
# 
# ggsave('mean-timestepx-controls.png', scale = 1, dpi = 300, width = 6, height = 5, path='fig/')




###only 30 first

# controls_positions_subject.means <- ddply(controls.positions.subset, c("Condition", "Truth.value", "Time.Step", "Subject"),
#                                           function(controls.positions.subset)c(X.Position.mean=mean(controls.positions.subset$X.Position, na.rm=T), 
#                                                                                Y.Position.mean=mean(controls.positions.subset$Y.Position, na.rm=T)))
# 
# 
# 
# 
# 
# controls_positions_overall.means <- ddply(controls_positions_subject.means, c("Condition", "Truth.value", "Time.Step"),
#                                           function(controls_positions_subject.means)c(X.Position.mean=mean(controls_positions_subject.means$X.Position, na.rm=T), 
#                                                                                       X.Position.se=se(controls_positions_subject.means$X.Position, na.rm=T),
#                                                                                       Y.Position.mean=mean(controls_positions_subject.means$Y.Position, na.rm=T), 
#                                                                                       Y.Position.se=se(controls_positions_subject.means$Y.Position, na.rm=T)))
# 
# 
# 
# ggplot(controls_positions_overall.means, aes(x=X.Position.mean, y=Y.Position.mean, color=Condition, group=Condition)) + 
#   geom_point(alpha=.8, size=1.5) + 
#   xlab('X Coordinate') +
#   ylab('Y Coordinate') +
#   geom_errorbarh(aes(xmin=X.Position.mean-X.Position.se, xmax=X.Position.mean+X.Position.se), alpha=0.3) + 
#   theme_bw() +
#   expand_limits(x=c(-1.5,1.5)) + 
#   theme(legend.position='top') +
#   facet_grid(.~Truth.value) + scale_color_jco()
# 
# ggsave('mean-trajectories-controls-subset.png', scale = 1, dpi = 300, width = 6, height = 5, path='fig/')


##LDA running ####
load('LDA-Full.RData')
LDA_test.coord.dist(controls,v_lda,b_lda,m_pca,all_data_columns,n_pca)
controls <- dplyr::full_join(lda_measure_te.df, controls, by=c("Subject", "Item.number", "Response"))
controls.true <- subset(controls, Response=='true')
controls.false <- subset(controls, Response=='false')



### Measures plots ####

plot_measure(controls.true,'MaxLogRatio','Condition')
ggsave('MaxLogRatio-controls-true.png', plot = last_plot(), scale = 1, dpi = 300, width = 7, height = 5, path='fig/')

#plot_measure(subset(controls.true,Rank<=25),'MaxLogRatio','Condition')
#ggsave('MaxLogRatio-controls-true-subset.png', plot = last_plot(), scale = 1, dpi = 300, width = 7, height = 5, path='fig/')

plot_measure(controls.true,'MaxDeviation','Condition')
ggsave('MaxDeviation-controls-true.png', plot = last_plot(), scale = 1, dpi = 300, width = 7, height = 5, path='fig/')

#plot_measure(subset(controls.true,Rank<=25),'MaxDeviation','Condition')
#ggsave('MaxDeviation-controls-true-subset.png', plot = last_plot(), scale = 1, dpi = 300, width = 7, height = 5, path='fig/')

plot_measure(controls.true,'lda_measure','Condition')
ggsave('LDA-controls-true.png', plot = last_plot(), scale = 1, dpi = 300, width = 7, height = 5, path='fig/')

#plot_measure(subset(controls.true, Rank<25),'lda_measure','Condition')
#ggsave('LDA-controls-true-subset.png', plot = last_plot(), scale = 1, dpi = 300, width = 7, height = 5, path='fig/')

### test crossvalidation ####
permutation_values <- c(1,2,3)
controls.true <- random.within(controls.true, "Condition")


results_lda <- boot(data=controls.true, statistic=auc_roc, R=1000, score='lda_measure', label= 'Condition', n=FALSE)
results_lda.random <- boot(data=controls.true, statistic=auc_roc, R=1000, score='lda_measure', label= 'Random', n=FALSE)
data <- rbind(data.frame(value=results_lda$t, variable='original'), data.frame(value=results_lda.random$t, variable='null'))  
roc.te <- roc(data$variable, data$value)
permutation_values[1]<- roc.te$auc 
permutation_value_lda<- roc.te$auc 
cdat_lda <- ddply(data, "variable", summarise, rating.mean=mean(value))
save(data,cdat_lda,permutation_value_lda, file='LDA-controls-test.RData')

ggplot(data, aes(x=value, fill=variable, color=variable))  + xlab('AUC for bootstrapped samples') + 
  geom_density(alpha=.5) + scale_fill_uchicago() + theme_bw() +  scale_color_uchicago() +     coord_cartesian(xlim = c(0.4, 0.7)) + 
  geom_vline(data=cdat_lda, aes(xintercept=rating.mean, color=variable),size=0.6) + theme(legend.position = 'none') +
  geom_text(aes(x=cdat_lda$rating.mean[1]-.003, label=as.character(round(cdat_lda$rating.mean[1],2)), y=2), colour="black", angle=90, text=element_text(size=10)) + 
  geom_text(aes(x=cdat_lda$rating.mean[2]-.003, label=as.character(round(cdat_lda$rating.mean[2],2)), y=2), colour="black", angle=90, text=element_text(size=10)) 

  ggsave('LDA-controls-test.png', plot = last_plot(), scale = 1, dpi = 300, width = 5, height = 5, path='fig/')

  
  results_maxdeviation <- boot(data=controls.true, statistic=auc_roc, R=1000, score='MaxDeviation', label= 'Condition', n=FALSE)
  results_maxdeviation.random <- boot(data=controls.true, statistic=auc_roc, R=1000, score='MaxDeviation', label= 'Random', n=FALSE)
  data <- rbind(data.frame(value=results_maxdeviation$t, variable='original'), data.frame(value=results_maxdeviation.random$t, variable='null'))  
  roc.te <- roc(data$variable, data$value)
  
permutation_values[2]<- roc.te$auc  
permutation_value_MD<- roc.te$auc  
cdat_md<- ddply(data, "variable", summarise, rating.mean=mean(value))
save(data,cdat_md,permutation_value_MD, file='MD-controls-test.RData')

ggplot(data, aes(x=value, fill=variable, color=variable))  + xlab('AUC for bootstrapped samples') + 
  geom_density(alpha=.5) + scale_fill_uchicago() + theme_bw() + scale_color_uchicago() +     coord_cartesian(xlim = c(0.4, 0.7)) + 
  geom_vline(data=cdat_md, aes(xintercept=rating.mean, color=variable),size=0.6) + theme(legend.position = 'none') +
  geom_text(aes(x=cdat_md$rating.mean[1]-.003, label=as.character(round(cdat_md$rating.mean[1],2)), y=2), colour="black", angle=90, text=element_text(size=10)) + 
  geom_text(aes(x=cdat_md$rating.mean[2]-.003, label=as.character(round(cdat_md$rating.mean[2],2)), y=2), colour="black", angle=90, text=element_text(size=10)) 

ggsave('MD-controls-test.png', plot = last_plot(), scale = 1, dpi = 300, width = 5, height = 5, path='fig/')


results_maxratio <- boot(data=controls.true, statistic=auc_roc, R=1000, score='MaxLogRatio', label= 'Condition', n=FALSE)
results_maxratio.random <- boot(data=controls.true, statistic=auc_roc, R=1000, score='MaxLogRatio', label= 'Random', n=FALSE)
data <- rbind(data.frame(value=results_maxratio$t, variable='original'), data.frame(value=results_maxratio.random$t, variable='null'))  
roc.te <- roc(data$variable, data$value)
permutation_values[3]<- roc.te$auc  
permutation_value_maxlogratio<- roc.te$auc  
cdat_max_ratio<- ddply(data, "variable", summarise, rating.mean=mean(value))
save(data,cdat_max_ratio,permutation_value_maxlogratio, file='MaxLogRatio-controls-test.RData')

ggplot(data, aes(x=value, fill=variable, color=variable))  + xlab('AUC for bootstrapped samples') + 
  geom_density(alpha=.5) + scale_fill_uchicago() + theme_bw() + scale_color_uchicago() +    coord_cartesian(xlim = c(0.4, 0.7)) +  
  geom_vline(data=cdat_max_ratio, aes(xintercept=rating.mean, color=variable),size=0.6) + theme(legend.position = 'none') +
  geom_text(aes(x=cdat_max_ratio$rating.mean[1]-.003, label=as.character(round(cdat_max_ratio$rating.mean[1],2)), y=2), colour="black", angle=90, text=element_text(size=10)) + 
  geom_text(aes(x=cdat_max_ratio$rating.mean[2]-.003, label=as.character(round(cdat_max_ratio$rating.mean[2],2)), y=2), colour="black", angle=90, text=element_text(size=10)) 


ggsave('MaxLogRatio-controls-test.png', plot = last_plot(), scale = 1, dpi = 300, width = 5, height = 5, path='fig/')








## An idea of the bad classification ####

load('negation_data_processed.RData')
negation.true <- subset(negation_data, Response=='true')

p1 <- ggplot(subset(controls.true, Rank<n), aes(x=lda_measure, y=MaxLogRatio, color=Condition)) +
  geom_jitter(alpha=.4) +    # Use hollow circles
  ggtitle(paste0('Old data - RT mean: ', as.character(median(subset(controls.true, Rank<n)$RT)), sep = ' ')) +  
  scale_colour_jco() + theme_bw() + geom_vline(xintercept=0, linetype="dashed", color = "black") + scale_x_continuous(limits = c(-3, 3)) + scale_y_continuous(limits = c(-0.5, 8)) 
table(subset(controls.true, Rank<n)$Condition)


p2 <- ggplot(negation.true, aes(x=lda_measure, y=MaxLogRatio, color=Polarity)) +
  geom_jitter(alpha=.4) +    # Use hollow circles
  ggtitle(paste0('Dale and Duran Rep - RT mean: ', as.character(median(negation.true$RT)), sep = ' ')) +  
  scale_colour_jco() + theme_bw() + geom_vline(xintercept=0, linetype="dashed", color = "black") +
  scale_x_continuous(limits = c(-3, 3)) + scale_y_continuous(limits = c(-0.5, 8))

table(negation.true$Polarity)
p3 <- grid.arrange(p1,p2, ncol=2)
ggsave('bad_classification1.png', p3,  scale = 1, dpi = 300, width = 6, height = 5, path='fig/')



try = controls %>%
  dplyr::select(Subject, Item.number, Condition, lda_measure)

controls.positions <- dplyr::full_join(try, controls.positions, by=c("Subject", "Item.number",  "Condition"))
controls.positions.subset <- subset(controls.positions, Rank<25)
bad_classification_controls.positions  <- rbind(subset(controls.positions.subset, lda_measure<0 & MaxLogRatio>6))

ggplot(bad_classification_controls.positions, aes(x= X.Position, y=Y.Position, group=Item.number, color=Condition)) +
  geom_point(alpha=.5)+
  scale_colour_jco() +
  ggtitle('bad classification old data') +
  theme_bw() +
  theme(legend.position = "none", strip.background = element_blank()) +
  facet_wrap(~grp, ncol=5) + 
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )
ggsave('bad_classification2.png', scale = 1, dpi = 300, width = 6, height = 5, path='fig/')

