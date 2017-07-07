library(robustbase)

# auxiliary functions to build the tail-sensitive confidence bands with unknown paramters
QnScale <- function(x) {
	Qn(x, finite.corr = FALSE)
}

QnLocation <- function(x) {
	s_Qn(x, mu.too = TRUE)[[1]]
}

# tail-sensitive confidence bands
# adapted from Aldor-Noiman et al. (2013)
tsCb <- function(x,
								 mu = 0,
								 sigma = 1,
								 M = 1000,
								 alpha = .05,
								 plot = TRUE,
								 unknown = FALSE,
								 centerFunc = QnLocation,
								 scaleFunc = QnScale) {
	n <- length(x)
	upperCi <- rep(NA, n)
	lowerCi <- rep(NA, n)
	pValue <- matrix(NA, nrow = n, ncol = M)

	# simulate data
	sim <- NULL
	if (unknown) {
		for (i in 1:M) sim <- cbind(sim, sort(rnorm(n)))

		# scale the simulated data
		center <- apply(sim, 2, centerFunc)
		scale <- apply(sim, 2, scaleFunc)
		sim <- sweep(sweep(sim, 2, center, FUN = "-"), 2, scale, FUN = "/")

		# convert from norm to beta
		sim <- t(apply(sim, 1, pnorm))
	} else {
		for (i in 1:M) sim <- cbind(sim, sort(runif(n)))
	}

	# widen the CI to get a simultanoues 1-alpha CI
	for (i in 1:n) {
		tmp <- pbeta(sim[i, ], shape1 = i, shape2 = n + 1 - i)
		pValue[i, ] <- apply(cbind(tmp, 1 - tmp), 1, min)
	}

	critical <- apply(pValue, 2, min)
	criticalC <- quantile(critical, prob = alpha)

	upperCi <- qbeta(1 - criticalC, shape1 = 1:n, shape2 = n + 1 - (1:n))
	lowerCi <- qbeta(criticalC, shape1 = 1:n, shape2 = n + 1 - (1:n))

	# now translate back to sample quantiles
	# upperNorm <- qnorm(upperCi) # OLD
	# lowerNorm <- qnorm(lowerCi) # OLD
	upperNorm <- qnorm(upperCi) * scaleFunc(x) + centerFunc(x) # MODIFIED
	lowerNorm <- qnorm(lowerCi) * scaleFunc(x) + centerFunc(x) # MODIFIED

	if (plot == TRUE) {
		q.prob <- qnorm((1:n) / (n + 1))
		plot(
			q.prob,
			upperNorm,
			type = "l",
			col = "red",
			# ylim = c(-3, 3), # OLD
			ylim = c(min(x), max(x)), # MODIFIED
			xlim = c(-3, 3),
			ylab = "Sample Quantile",
			xlab = "Theoretical Quantile"
		)
		lines(q.prob, lowerNorm, col = "red")
		if (unknown) {
			# sampleZ <- (x - centerFunc(x)) / scaleFunc(x) # OLD
			sampleZ <- x # MODIFIED
		} else {
			sampleZ <- (x - mu) / sigma
		}
		print(sampleZ)
		points(q.prob, sort(sampleZ), pch = 19, cex = 0.6)
	}

	return(list(lower = lowerNorm, upper = upperNorm))
}

tsCb(mtcars$mpg, unknown = T)
