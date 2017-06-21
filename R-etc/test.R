require(devtools)

# including new packages into DESCRIPTION
devtools::use_package("ggplot2")

# testing exports .Rds
stat_qq_rot
stat_qq_line
stat_qq_band

# examples
require(ggplot2)

gg <- ggplot(data = mtcars, mapping = aes(sample = mpg)) +
	stat_qq_band(detrend = F, mapping = aes(x = mpg), fill = rgb(.7, .7, .7, .5), geom = "errorbar") +
  stat_qq_line(detrend = F, size = .8, color = rgb(.3, .3, .3)) +
  stat_qq_det(detrend = F)
  gg + labs(x = "theoretical", y = "sample")

gg <- ggplot(data = mtcars, mapping = aes(sample = mpg)) +
	stat_qq_band(detrend = F, mapping = aes(x = mpg), fill = rgb(.7, .7, .7, .5), distribution = "pois", dparam = list(lambda = 3)) +
	stat_qq_line(detrend = F, size = .8, color = rgb(.3, .3, .3), distribution = "pois", dparam = list(lambda = 3)) +
	stat_qq_det(detrend = F, distribution = "pois", dparam = list(lambda = 3))
	gg + labs(x = "theoretical", y = "sample")
