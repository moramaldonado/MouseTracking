
##PLOTTING different measures
#Input: data= data, measure = 'measure', division = 'division'
#Output: multiplot with plots for histogram, density and bar graph taken from means + print of means + SE
plot_measure_target <- function(data, measure, division){
  density <-ggplot(data, aes_string(x=measure, fill=division, color=division), environment = environment()) +
    geom_density(alpha=.5)+
    scale_colour_aaas() +  scale_fill_aaas() +
    theme(legend.position = "none")
  
  density <- density + theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
                        axis.title.x=element_blank(),
                        axis.text.x=element_blank(),
                         legend.text = element_text(size = 14), 
                         legend.title = element_text(size = 14), 
                         axis.title = element_text(size = 13), 
                         axis.text = element_text(size = 13)) + border() + theme(plot.margin = unit(c(0.3, 0, 0.3, 0.3), "cm")) + theme(legend.position = "none")

  mydata.agreggated <- ddply(data, c(division, "Subject"),
                             function(x,ind){mean(x[,ind])},measure)
  
  mydata.agreggated.overall <- ddply(mydata.agreggated, c(division),
                                     function(mydata.agreggated)c(mean=mean(mydata.agreggated$V1, na.rm=T), se=se(mydata.agreggated$V1, na.rm=T) ))
  
  max1 <- function(x, column){
    max(x[,column])
  }
  
  min1 <- function(x, column){
    min(x[,column])
  }
  
  max_v <- ceiling(max1(data, measure))
  min_v <- floor(min1(data, measure))
  
  
  mean.plot <-  ggbarplot(mydata.agreggated.overall, y = "mean", x = division, ylab= FALSE, 
                  color = division, fill= division,
                  alpha = 0.6) +   geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) + ylim(c(min_v, max_v))+
                  theme(axis.title.y=element_blank(), axis.text.y=element_blank()) + border()  + rotate() +
    scale_colour_aaas() +  scale_fill_aaas()+
                  theme(legend.position='none', legend.text = element_text(size = 14), legend.title =
                  element_text(size = 14), axis.title = element_text(size = 13), axis.text =
                  element_text(size = 13)) + theme(plot.margin = unit(c(0, 0.3, 0.3, 0.3), "cm"))
  
  plot <- ggarrange(density, mean.plot, 
            ncol = 1, nrow = 2,  align = "v", 
            widths = c(1, 1), heights = c(1,0.5),
            common.legend = FALSE)
  
  print (plot)
   return(plot)  }


