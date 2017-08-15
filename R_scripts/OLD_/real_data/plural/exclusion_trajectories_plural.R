## Trajectories exclusion for PLURAL data

### TAKE TRAJECTORIES (x,y coordenates per line)
normalized_positions.plot.X = experimental_items %>%
  dplyr::select(Subject, Condition, Response, Picture, Predicate, RT.Log,Item.number, X.Position) %>%
  separate(X.Position, into= as.character(c(1:101)), sep = ",") %>%
  gather(Time.Step, X.Position, 8:108) %>%
  mutate(Time.Step = as.numeric(Time.Step))%>%
  mutate(X.Position = as.numeric(X.Position))
normalized_positions.plot.Y = experimental_items %>%
  dplyr::select(Subject,Condition, Response, Picture, Predicate, RT.Log, Item.number, Y.Position) %>%
  separate(Y.Position, into= as.character(c(1:101)), sep = ",") %>%
  gather(Time.Step, Y.Position, 8:108) %>%
  mutate(Time.Step = as.numeric(Time.Step))%>%
  mutate(Y.Position = as.numeric(Y.Position))
normalized_positions.plot <- merge(normalized_positions.plot.X, normalized_positions.plot.Y)
rm(normalized_positions.plot.Y, normalized_positions.plot.X)
summary(normalized_positions.plot)

pdf('histograms_positions(before exclusion)_plurals.pdf')
par(mfrow=c(2,1))
hist(normalized_positions.plot$Y.Position)
hist(normalized_positions.plot$X.Position)
dev.off()

#Calculating maximal and minimal x.values for exclusion
dat <- ddply(normalized_positions.plot, c("Subject", "Item.number"),
             function(normalized_positions.plot)c(min=min(normalized_positions.plot$X.Position, na.rm=T),
                                                  max=max(normalized_positions.plot$X.Position, na.rm=T))) 

lower.bound <- -1.7
upper.bound <- 1.7

#doubt: there are in particular 3 subjects that have this pattern of results, maybe it's better just to take them out (instead of )
#Excluding trials with extreme values.
experimental_items  <- merge(experimental_items, dat, by=c("Subject", "Item.number"))
normalized_positions.plot <- merge(normalized_positions.plot, dat, by=c("Subject", "Item.number"))
bad.expermental_items <- subset(experimental_items, min < lower.bound | max > upper.bound) 
percentage.excluded.traj <- nrow(bad.expermental_items)*100/nrow(experimental_items)
experimental_items <- subset(experimental_items, min > lower.bound & max < upper.bound )
normalized_positions.plot <- subset(normalized_positions.plot, min > lower.bound & max < upper.bound )

