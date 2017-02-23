###########################################
# Lineup functions from TAS paper         #
###########################################

## ISSUES;
#  1. How we draw the line (not always "right" based on visual intuition)

std_lineup <- function(dframe) {
  require(ggplot2)
  require(grid)
  print(ggplot(aes(x=naive1.qq.x, y=naive1.qq.y), data = dframe) + 
          #  geom_smooth(aes(naive1.qq.x, naive1.env.fit.value), colour="grey50", se=FALSE, method="loess") +
          geom_abline(colour="grey50") +
          facet_wrap(~.sample, ncol=5) +
          geom_point() + 
          theme_bw() + xlab("") + ylab("") +
          geom_ribbon(aes(x = naive1.env.fit.value, ymin = naive1.env.lower, ymax = naive1.env.upper),alpha = .2)+
          theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
                axis.ticks = element_blank(), plot.margin=unit(c(0,0,0,0), "cm"))
  )  
}

std_ts_lineup <- function(dframe) {
  require(ggplot2)
  require(grid)
  print(ggplot(aes(x=ts.qq.x, y=ts.qq.y), data = dframe) + 
          #  geom_smooth(aes(naive1.qq.x, naive1.env.fit.value), colour="grey50", se=FALSE, method="loess") +
          geom_abline(colour="grey50") +
          facet_wrap(~.sample, ncol=5) +
          geom_point() + 
          theme_bw() + xlab("") + ylab("") +
          geom_ribbon(aes(x = ts.qq.x, ymin = ts.qq.lower, ymax = ts.qq.upper),alpha = .2)+
          theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
                axis.ticks = element_blank(), plot.margin=unit(c(0,0,0,0), "cm"))
  )  
}

rot2_lineup <- function(dframe) {
  require(ggplot2)
  require(grid)
  print(  ggplot(aes(x=naive1.qq.x, y=naive1.qq.y-naive1.env.fit.value), data = dframe) + 
            facet_wrap(~.sample, ncol=5) + 
            geom_hline(yintercept=0, colour="grey30")+
            geom_point() + 
            geom_ribbon(aes(x = naive1.qq.x, 
                            ymin = naive1.env.lower-naive1.env.fit.value, ymax = naive1.env.upper-naive1.env.fit.value),alpha = .2)+
            theme_bw() + xlab("") + ylab("") +
            ylim(range(dframe$naive1.qq.y)) +
            theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
                  axis.ticks = element_blank(), plot.margin=unit(c(0,0,0,0), "cm")))
}

rot2_ts_lineup <- function(dframe) {
  require(ggplot2)
  require(grid)
  print(  ggplot(aes(x=ts.qq.x, y=ts.qq.y-ts.qq.x), data = dframe) + 
            facet_wrap(~.sample, ncol=5) + 
            geom_hline(yintercept=0, colour="grey30")+
            geom_point() + 
            geom_ribbon(aes(x = ts.qq.x, 
                            ymin = ts.qq.lower-ts.qq.x, ymax = ts.qq.upper-ts.qq.x),alpha = .2)+
            theme_bw() + xlab("") + ylab("") +
            ylim(range(dframe$ts.qq.y)) +
            theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
                  axis.ticks = element_blank(), plot.margin=unit(c(0,0,0,0), "cm")))
}

rot_lineup <- function(dframe) {
  require(ggplot2)
  require(grid)
  print(  ggplot(aes(x=naive1.qq.x, y=naive1.qq.y-naive1.env.fit.value), data = dframe) + 
            facet_wrap(~.sample, ncol=5) + 
            geom_hline(yintercept=0, colour="grey30")+
            geom_point() + 
            geom_ribbon(aes(x = naive1.qq.x, 
                            ymin = naive1.env.lower-naive1.env.fit.value, ymax = naive1.env.upper-naive1.env.fit.value),alpha = .2)+
            theme_bw() + xlab("") + ylab("") +
            #            xlab("Normal Quantiles") + 
            #            ylab("Sample Quantiles") +
            theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
                  axis.ticks = element_blank(), plot.margin=unit(c(0,0,0,0), "cm")))
}

rot_ts_lineup <- function(dframe) {
  require(ggplot2)
  require(grid)
  print(  ggplot(aes(x=ts.qq.x, y=ts.qq.y-ts.qq.x), data = dframe) + 
            facet_wrap(~.sample, ncol=5) + 
            geom_hline(yintercept=0, colour="grey30")+
            geom_point() + 
            geom_ribbon(aes(x = ts.qq.x, 
                            ymin = ts.qq.lower-ts.qq.x, ymax = ts.qq.upper-ts.qq.x),alpha = .2)+
            theme_bw() + xlab("") + ylab("") +
            theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
                  axis.ticks = element_blank(), plot.margin=unit(c(0,0,0,0), "cm")))
}

ctrl_lineup <- function(dframe) {
  require(ggplot2)
  require(grid)
  print(ggplot(aes(x=naive1.qq.x, y=naive1.qq.y), data = dframe) + 
          geom_smooth(aes(naive1.qq.x, naive1.env.fit.value), colour="grey50", se=FALSE, method="loess") +
          geom_point() + 
          facet_wrap(~.sample, ncol=5) +
          theme_bw() + xlab("") + ylab("") +
          theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
                axis.ticks = element_blank(), plot.margin=unit(c(0,0,0,0), "cm")) 
  )
}