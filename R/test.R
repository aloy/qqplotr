library(ggplot2)

source("R/stat_qq_line.R")
source("R/stat_qq_band.R")

## Comments ----

# - stat_qq_band calculates the confidence envelopes as in car::qqPlot
#
# - stat_qq* functions may be called in different orders than presented below, and the
# result will be almost the same, with the exception of they layer ordering. However, the
# order presented below (stat_qq_band -> stat_qq_line -> stat_qq) is the one that, IMO,
# should be used.
#
# - Each individual stat should produce/plot their results independently from calling the
# other stats. That was achieved by class inheritance.
#
# - Keep in mind that the stat_qq is still the original verson, and not our own. That
# being said, the distribution parameter is slighty different: stat_qq expects you to
# provide the function name (e.g., qnorm, qt), while stat_qq_line and stat_qq_band will
# only need the distribution name (e.g., "norm", "t").

## Testing ----

# norm
gg <- ggplot(data = mtcars, mapping = aes(sample = mpg)) +
	stat_qq_band(mapping = aes(x = mpg), fill = rgb(.7, .7, .7, .5)) +
	stat_qq_line(size = .8, color = rgb(.3, .3, .3)) +
	stat_qq()
gg + labs(x = "theoretical", y = "sample") + theme_light()

# t
df <- 5 # change the df here
gg <- ggplot(data = mtcars, mapping = aes(sample = mpg)) +
	stat_qq_band(mapping = aes(x = mpg), fill = rgb(.7, .7, .7, .5), distribution = "t", dparams = list(df = df)) +
	stat_qq_line(size = .8, color = rgb(.3, .3, .3), distribution = "t", dparams = list(df = df)) +
	stat_qq(distribution = stats::qt, dparams = list(df = df))
gg + labs(x = "theoretical", y = "sample") + theme_light()

# chisq
df <- 2 # change the df here
gg <- ggplot(data = mtcars, mapping = aes(sample = mpg)) +
	stat_qq_band(mapping = aes(x = mpg), fill = rgb(.7, .7, .7, .5), distribution = "chisq", dparams = list(df = df)) +
	stat_qq_line(size = .8, color = rgb(.3, .3, .3), distribution = "chisq", dparams = list(df = df)) +
	stat_qq(distribution = stats::qchisq, dparams = list(df = df))
gg + labs(x = "theoretical", y = "sample") + theme_light()

# pois
lambda <- 3 # change the lambda here
gg <- ggplot(data = mtcars, mapping = aes(sample = mpg)) +
	stat_qq_band(mapping = aes(x = mpg), fill = rgb(.7, .7, .7, .5), distribution = "pois", dparams = list(lambda = lambda)) +
	stat_qq_line(size = .8, color = rgb(.3, .3, .3), distribution = "pois", dparams = list(lambda = lambda)) +
	stat_qq(distribution = stats::qpois, dparams = list(lambda = lambda))
gg + labs(x = "theoretical", y = "sample") + theme_light()
