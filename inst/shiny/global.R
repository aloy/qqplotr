if (!requireNamespace("plotly", quietly = TRUE)) {
	stop("'plotly' package is needed for this Shiny app to work. Please install it.",
			 call. = FALSE)
} else {
	require(plotly)
}

# 'ggplot2' is always loaded if 'qqplotr' is loaded
require(qqplotr)

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
