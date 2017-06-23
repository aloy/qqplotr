require(devtools)

# including new packages into DESCRIPTION
devtools::use_package("ggplot2")

# testing the .Rds
stat_qq_points
stat_qq_line
stat_qq_band

# examples
require(qqplotr)
require(ggplot2)

detrend <- F
d <- "t"
dp <- list(rate = 5)

gg <- ggplot(data = mtcars, mapping = aes(sample = mpg)) +
	stat_qq_band(mapping = aes(x = mpg), distribution = d, dparams = dp, detrend = detrend) +
	stat_qq_line(distribution = d, dparams = dp, detrend = detrend) +
	stat_qq_points(distribution = d, dparams = dp, detrend = detrend)
  gg + labs(x = "theoretical", y = "sample")

ggplot(data = mtcars, mapping = aes(sample = mpg)) +
	stat_qq_band(mapping = aes(x = mpg)) +
	stat_qq_points(distribution = "exp", dparams = list(rate = 1))

# debugging
ggplot_build(gg)$data[[1]] # stat_qq_points
ggplot_build(gg)$data[[2]] # stat_qq_line
ggplot_build(gg)$data[[3]] # stat_qq_band
