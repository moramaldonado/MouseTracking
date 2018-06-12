
load('negation_data_processed.RData')
table(negation_data$Subject, negation_data$Polarity)
table(controls$Subject, controls$Condition)

try = controls %>%
  dplyr::select(Subject, Item.number, Condition, lda_measure)
controls.positions <- subset(controls.positions, Item.number<50)
controls.positions <- dplyr::full_join(try, controls.positions, by=c("Subject", "Item.number",  "Condition"))
controls.positions$lda_cut_full <- cut(controls.positions$lda_measure,5)
controls.positions$ratio_cut <- cut(controls.positions$MaxLogRatio,5)

try = negation_data %>%
  dplyr::select(Subject, Item.number, MaxLogRatio)
negation_data_positions <- dplyr::full_join(try, negation_data_positions, by=c("Subject", "Item.number"))
negation_data_positions$lda_cut_full <- cut(negation_data_positions$lda_measure,5)
negation_data_positions$ratio_cut <- cut(negation_data_positions$MaxLogRatio,5)





bad_classification_controls <- rbind(subset(controls,  (lda_measure<=0 & MaxLogRatio>4)), subset(controls, (lda_measure>1 & MaxLogRatio<1))) 
bad_classification_negation <- rbind(subset(negation_data, (lda_measure<=0 & MaxLogRatio>4)), subset(negation_data, (lda_measure>1 & MaxLogRatio<1))) 
good_classification_negation <- rbind(subset(negation_data, lda_measure>2 & MaxLogRatio>4), subset(negation_data,lda_measure<0 & MaxLogRatio<1) )

nrow(bad_classification_negation)
nrow(bad_classification_controls)
nrow(good_classification_negation)

bad_classification_controls.positions  <- rbind(subset(controls.positions, lda_measure<=0 & MaxLogRatio>4) , subset(controls.positions,(lda_measure>1 & MaxLogRatio<1)))
bad_classification_negation.positions  <- rbind(subset(negation_data_positions, lda_measure<=0 & MaxLogRatio>4) , subset(negation_data_positions,(lda_measure>1 & MaxLogRatio<1)))

good_classification_negation.positions <- rbind(subset(negation_data_positions, lda_measure>1 & MaxLogRatio>4), subset(negation_data_positions, lda_measure<0 & MaxLogRatio<1))

## bad classification
p1 <- ggplot(bad_classification_controls.positions, aes(x= X.Position, y=Y.Position, group=Item.number, color=Condition)) +
  geom_point(alpha=.5)+
  scale_colour_jco() +
  ggtitle('bad classification old data') +
  theme_bw() +
  theme(legend.position = "none", strip.background = element_blank()) + 
  facet_wrap(round(MaxLogRatio,2)~round(lda_measure,2)) 
  #facet_wrap(Subject~Item.number)

p1bis <- ggplot(bad_classification_negation.positions, aes(x= X.Position, y=Y.Position, group=Item.number, color=Polarity)) +
  geom_point(alpha=.5)+
  scale_colour_jco() +
  ggtitle('bad classification DD data') +
  theme_bw() +
  theme(legend.position = "none", strip.background = element_blank()) + 
  facet_wrap(round(MaxLogRatio,2)~round(lda_measure,2)) 
  #facet_wrap(Subject~Item.number)


grid.arrange(p1,p1bis, ncol=2)




m <- subset(good_classification_negation.positions, MaxLogRatio>6)
select <- sample(m$grp, 15)

p2 <- ggplot(subset(good_classification_negation.positions, grp %in% select), aes(x= X.Position, y=Y.Position, group=Item.number, color=Polarity)) +
  geom_point(alpha=.5)+
  scale_colour_jco() +
  theme_bw() +
  ggtitle('good classification, deviated') +
  theme(legend.position = "none", strip.background = element_blank()) +
  #facet_wrap(~grp)
  facet_wrap(round(MaxLogRatio,2)~round(lda_measure,2)) 

grid.arrange(p1,p2, ncol=2)

p3 <- hist(bad_classification_controls$lda_measure)

a <- bad_classification_controls.positions$X.Position[bad_classification_controls.positions$Subject==7 & bad_classification_controls.positions$Item.number==28]
b <- good_classification_negation.positions$X.Position[good_classification_negation.positions$grp=='28 11']


dist(rbind(a,b))



## Pasar la misma LDA a todo

good_classification_negation$lda_cut <- cut(good_classification_negation$lda_measure,5)
good_classification_negation$Condition <- good_classification_negation$Polarity 
levels(good_classification_negation$Condition) <- c('Neg','Pos')
good_classification_negation$X.Position <- good_classification_negation$Normalized.positions.X 
good_classification_negation$Y.Position <- good_classification_negation$Normalized.positions.Y

load('LDA-Full.RData')
LDA_test.coord.dist(good_classification_negation,v_lda,b_lda,m_pca,all_data_columns,n_pca)
lda_measure_te.df$lda_measure_new <- lda_measure_te.df$lda_measure
lda_measure_te.df$lda_measure <- NULL
good_classification_negation <- dplyr::full_join(lda_measure_te.df, good_classification_negation, by=c("Subject", "Item.number", "Response"))

ggplot(good_classification_negation, aes(x=lda_measure.y, y=lda_measure_new)) +
  geom_point(alpha=.2) +
  facet_grid(Condition~Response) + theme_bw() + ggtitle('good classification trials, LDA applied again')




