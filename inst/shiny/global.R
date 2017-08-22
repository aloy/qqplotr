dist <- c(
	"beta",
	"binom",
	"chisq",
	"exp",
	"f",
	"gamma",
	"norm",
	"pois"
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

smp <- data.frame(norm = rnorm(100), exp = rexp(100))
