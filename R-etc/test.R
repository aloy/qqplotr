# --------------------------------------------------------------------- DEV ----

require(devtools)

# including new packages into DESCRIPTION
devtools::use_package("dplyr", type = "Imports")
devtools::use_package("MASS", type = "Imports")
devtools::use_package("ggplot2", type = "Depends")

# if NAMESPACE is not yet created (or updated for some reason), run:
roxygen2::roxygenize()

# testing the .Rds
stat_qq_point
stat_qq_line
stat_qq_band

# ---------------------------------------------------------------- EXAMPLES ----

require(qqplotr)

detrend <- F
d <- "norm"
dp <- list()

gg <- ggplot(data = mtcars, mapping = aes(sample = mpg)) +
	stat_qq_band(mapping = aes(x = mpg), distribution = d, dparams = dp, detrend = detrend, bandType = "bootstrap") +
	stat_qq_line(distribution = d, dparams = dp, detrend = detrend) +
	stat_qq_point(distribution = d, dparams = dp, detrend = detrend)
  gg + labs(x = "theoretical", y = "sample")

# debugging
ggplot_build(gg)$data[[1]] # stat_qq_band
ggplot_build(gg)$data[[2]] # stat_qq_line
ggplot_build(gg)$data[[3]] # stat_qq_point




















