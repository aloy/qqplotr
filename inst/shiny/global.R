if (!requireNamespace("shiny", quietly = TRUE)) {
	stop("'shinyBS' package is needed for the Shiny example app to work properly. Please install it.",
			 call. = FALSE)
} else {
	require(shinyBS)
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
	"Chi-Sqared",
	"Exponential",
	"F",
	"Gamma",
	"Log-Normal",
	"Normal",
	"Student's t",
	"Unif",
	"Weibull"
)

smp <- data.frame(norm = rnorm(100), exp = rexp(100))
