# --------------------------------------------------------------------- DEV ----

require(devtools)

# including new packages into DESCRIPTION
devtools::use_package("ggplot2", type = "Depends")
devtools::use_package("MASS", type = "Imports")
devtools::use_package("dplyr", type = "Imports")
devtools::use_package("robustbase", type = "Imports")

# if NAMESPACE is not yet created (or updated for some reason), run:
roxygen2::roxygenize()

# testing the .Rds
stat_qq_point
stat_qq_line
stat_qq_band

# ---------------------------------------------------------------- EXAMPLES ----

require(qqplotr)

detrend <- T
d <- "norm"
dp <- list()
bandType <- "ts"

gg <- ggplot(data = mtcars, mapping = aes(sample = mpg)) +
	stat_qq_band(mapping = aes(x = mpg), distribution = d, dparams = dp, detrend = detrend, bandType = bandType) +
	stat_qq_line(distribution = d, dparams = dp, detrend = detrend) +
	stat_qq_point(distribution = d, dparams = dp, detrend = detrend)
  gg + labs(x = "Theoretical Quantiles", y = "Sample Quantiles")

# adjusting y-axis limits of the detrended plot version
gg + scale_y_continuous(limits = c(-1,1) * diff(range(mtcars$mpg)))

# debugging
ggplot_build(gg)$data[[1]] # stat_qq_band
ggplot_build(gg)$data[[2]] # stat_qq_line
ggplot_build(gg)$data[[3]] # stat_qq_point
