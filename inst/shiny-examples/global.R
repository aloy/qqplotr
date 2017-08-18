require(ggplot2)
require(plotly)

dist <- c(
	"qbeta",
	"qbinom",
	"qchisq",
	"qexp",
	"qf",
	"qgamma",
	"qnorm",
	"qpois"
)

names(dist) <- c(
	"Beta",
	"Binomial",
	"Chi-Squared",
	"Exponential",
	"F",
	"Gamma",
	"Normal",
	"Poisson"
)
