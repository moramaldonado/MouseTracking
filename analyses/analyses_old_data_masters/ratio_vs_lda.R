

controls.positions$lda_cut_full <- cut(controls.positions$lda_measure_te,5)
controls.positions$lda_cut_full_2 <- cut(controls.positions$lda_measure_full,5)

controls.positions$ratio_full <- cut(controls.positions$MaxLogRatio,5)


means.controls <- ddply(controls.positions, c("Condition", "Response", "Time.Step", "lda_cut_full"),
                              function(controls.positions)c(X.Position.mean=mean(controls.positions$X.Position, na.rm=T), 
                                                                  Y.Position.mean=mean(controls.positions$Y.Position, na.rm=T)))

ggplot(means.controls, aes(x= X.Position.mean, y=Y.Position.mean, group=Condition, color=Condition)) +
  geom_point(alpha=.5)+
  scale_colour_jco() +
  theme_bw() +theme(legend.position = "none") + facet_grid(Response~lda_cut_full) 



means.controls_ratio <- ddply(controls.positions, c("Condition", "Response", "Time.Step", "ratio_full"),
                        function(controls.positions)c(X.Position.mean=mean(controls.positions$X.Position, na.rm=T), 
                                                      Y.Position.mean=mean(controls.positions$Y.Position, na.rm=T)))

ggplot(means.controls_ratio, aes(x= X.Position.mean, y=Y.Position.mean, group=Condition, color=Condition)) +
  geom_point(alpha=.5)+
  scale_colour_jco() +
  theme_bw() +theme(legend.position = "none") + facet_grid(Response~ratio_full) 



means.controls_ratio <- ddply(subset(controls.positions, Response=='true'), c("Condition", "Time.Step", "ratio_full", "lda_cut_full"),
                              function(controls.positions)c(X.Position.mean=mean(controls.positions$X.Position, na.rm=T), 
                                                            Y.Position.mean=mean(controls.positions$Y.Position, na.rm=T)))


ggplot(subset(controls.positions, Subject==1), aes(x= X.Position, y=Y.Position, group=Condition, color=Condition)) +
  geom_point(alpha=.5)+
  scale_colour_jco() +
  theme_bw() +theme(legend.position = "none") + facet_grid(lda_cut_full~ratio_full) 
