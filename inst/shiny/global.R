if (!requireNamespace("shinyBS", quietly = TRUE)) {
	stop("'shinyBS' package is needed for the Shiny example app to work properly. Please install it.",
			 call. = FALSE)
} else {
	require(shinyBS)
}

if (!requireNamespace("lattice", quietly = TRUE)) {
	stop("'lattice' package is needed for the Shiny example app to work properly. Please install it.",
			 call. = FALSE)
} else {
	require(lattice)
}

dist <- c(
	"beta",
	"cauchy",
	"chisq",
	"exp",
	"f",
	"gamma",
	"lnorm",
	"norm",
	"t",
	"unif",
	"weibull"
)

names(dist) <- c(
	"Beta",
	"Cauchy",
	"Chi-Squared",
	"Exponential",
	"F",
	"Gamma",
	"Log-Normal",
	"Normal",
	"Student's-t",
	"Uniform",
	"Weibull"
)

datasets <- c(
	"simulated",
	"airquality",
	"CO2",
	"barley",
	"ethanol",
	"melanoma"
)

names(datasets) <- c(
	"simulated-dists (qqplotr)",
	"airquality (datasets)",
	"CO2 (datasets)",
	"barley (lattice)",
	"ethanol (lattice)",
	"melanoma (lattice)"
)

# simulated Normal and Exponential data
simulated <- data.frame(
	beta = rbeta(100, 0, 1),
	cauchy = rcauchy(100),
	chisq = rchisq(100, 1),
	exp = rexp(100),
	f = rf(100, 1, 1),
	gamma = rgamma(100, 0, 1),
	lnorm = rlnorm(100, 0, 1),
	norm = rnorm(100),
	t = rt(100, 1),
	unif = runif(100),
	weibull = rweibull(100, 1, 1)
)

simulatedNames <- c(
	"Beta(0,1)" = "beta",
	"Cauchy(0,1)" = "cauchy",
	"Chi-Squared(1)" = "chisq",
	"Exponential(1)" = "exp",
	"F(1,1)" = "f",
	"Gamma(0,1)" = "gamma",
	"Log-Normal(0,1)" = "lnorm",
	"Normal(0,1)" = "norm",
	"Student's-t(0,1)" = "t",
	"Uniform(0,1)" = "unif",
	"Weibull(1,1)" = "weibull"
)
