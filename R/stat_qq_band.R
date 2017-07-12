#' Quantile-quantile confidence bands
#'
#' Draws quantile-quantile confidence bands, with an additional detrend option.
#'
#' @import ggplot2
#' @importFrom MASS fitdistr
#' @importFrom dplyr summarize
#' @importFrom robustbase s_Qn
#' @importFrom robustbase Qn
#'
#' @include stat_qq_point.R stat_qq_line.R
#'
#' @inheritParams stat_qq_point
#'
#' @param bandType Character. Either \code{"normal"}, \code{"bs"} or
#'   \code{"ts"}. \code{"normal"} constructs simultaneous confidence
#'   bands based on Normal confidence intervals. \code{"bs"} creates
#'   pointwise confidence bands based on a parametric bootstrap. Finally,
#'   \code{"ts"} constructs tail-sensitive confidence bands, as
#'   described in Aldor-Noiman et al. (2013).
#' @param B Integer. If \code{bandType = "bs"}, then \code{B} is the number of
#'   bootstrap replicates. If \code{bandType = "ts"}, then \code{B} is the
#'   number of simulated samples.
#' @param conf Numerical. Confidence level of the bands.
#' @param mu Numerical. Only used if \code{bandType = "ts"}. Center
#'   distributional parameter used to construct the simulated tail-sensitive
#'   confidence bands. If either \code{mu} or \code{sigma} are \code{NULL}, then
#'   those parameters are estimated using \code{\link[robustbase]{Qn}} and
#'   \code{\link[robustbase]{s_Qn}}, respectively.
#' @param sigma Numerical. Only used if \code{bandType = "ts"}. Scale
#'   distributional parameter used to construct the simulated tail-sensitive
#'   confidence bands. If either \code{mu} or \code{sigma} are \code{NULL}, then
#'   those parameters are estimated using \code{\link[robustbase]{Qn}} and
#'   \code{\link[robustbase]{s_Qn}}, respectively.
#'
#' @references
#' \itemize{
#' \item{\href{http://www.tandfonline.com/doi/abs/10.1080/00031305.2013.847865}{Aldor-Noiman,
#' S. et al. 2013. The Power to See: A New Graphical Test of Normality. The
#' American Statistician.}}
#' }
#'
#' @examples
#' # generate random Normal data
#' set.seed(0)
#' df <- data.frame(norm = rnorm(100))
#'
#' # Normal Q-Q plot of Normal data
#' gg <- ggplot(data = df, mapping = aes(sample = norm)) +
#'  stat_qq_band() +
#'  stat_qq_line() +
#'  stat_qq_point()
#' gg + labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
#'
#' # Exponential Q-Q plot of Normal data
#' di <- "exp"
#' dp <- list(rate = 1)
#' gg <- ggplot(data = df, mapping = aes(sample = norm)) +
#'  stat_qq_band(distribution = di, dparams = dp) +
#'  stat_qq_line(distribution = di, dparams = dp) +
#'  stat_qq_point(distribution = di, dparams = dp)
#' gg + labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
#'
#' # Detrended Normal Q-Q plot of Normal data
#' de <- TRUE
#' gg <- ggplot(data = df, mapping = aes(sample = norm)) +
#'  stat_qq_band(detrend = de) +
#'  stat_qq_line(detrend = de) +
#'  stat_qq_point(detrend = de)
#' gg + labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
#'
#' # Normal Q-Q plot of Normal data with boostrap confidence bands
#' bt <- "bs"
#' gg <- ggplot(data = df, mapping = aes(sample = norm)) +
#'  stat_qq_band(bandType = bt) +
#'  stat_qq_line() +
#'  stat_qq_point()
#' gg + labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
#'
#' # Normal Q-Q plot of Normal data with tail-sensitive confidence bands
#' bt <- "ts"
#' gg <- ggplot(data = df, mapping = aes(sample = norm)) +
#'  stat_qq_band(bandType = bt) +
#'  stat_qq_line() +
#'  stat_qq_point()
#' gg + labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
#'
#' @export
stat_qq_band <- function(data = NULL,
												 mapping = NULL,
												 geom = "ribbon",
												 position = "identity",
												 show.legend = NA,
												 inherit.aes = TRUE,
												 distribution = "norm",
												 dparams = list(),
												 bandType = "normal",
												 B = 1000,
												 conf = .95,
												 mu = NULL,
												 sigma = NULL,
												 detrend = FALSE,
												 ...) {
	# vector with common discrete distributions
	discreteDist <- c("binom", "geom", "hyper", "multinom", "nbinom", "pois")

	if (distribution %in% discreteDist) geom <- "errorbar"

	ggplot2::layer(
		data = data,
		mapping = mapping,
		stat = StatQqBand,
		geom = geom,
		position = position,
		show.legend = show.legend,
		inherit.aes = inherit.aes,
		params = list(
			distribution = distribution,
			dparams = dparams,
			bandType = bandType,
			B = B,
			conf = conf,
			mu = mu,
			sigma = sigma,
			detrend = detrend,
			discrete = distribution %in% discreteDist,
			...
		)
	)
}

#' StatQqBand
#'
#' @keywords internal
#' @usage NULL
#' @export
StatQqBand <- ggplot2::ggproto(
	`_class` = "StatQqBand",
	`_inherit` = StatQqLine,

	default_aes = ggplot2::aes(
		x = ..x..,
		ymin = ..lower..,
		ymax = ..upper..
	),

	required_aes = c("sample"),

	compute_group = {
		function(data,
						 self,
						 scales,
						 distribution = "norm",
						 dparams = list(),
						 bandType = "normal",
						 B = 1000,
						 conf = .95,
						 mu = NULL,
						 sigma = NULL,
						 detrend = FALSE,
						 discrete) {
			# distributional functions
			qFunc <- eval(parse(text = paste0("q", distribution)))
			dFunc <- eval(parse(text = paste0("d", distribution)))
			rFunc <- eval(parse(text = paste0("r", distribution)))

			# inherit from StatQqPoint
			smp <- self$super()$super()$compute_group(data = data,
																												distribution = distribution,
																												dparams = dparams)$sample
			theoretical <- self$super()$super()$compute_group(data = data,
																												distribution = distribution,
																												dparams = dparams)$theoretical

			n <- length(smp)

			# inherit from StatQqLine
			xline <- self$super()$compute_group(data = data,
																					distribution = distribution,
																					dparams = dparams)$xline
			yline <- self$super()$compute_group(data = data,
																					distribution = distribution,
																					dparams = dparams)$yline

			slope <- diff(yline) / diff(xline)
			intercept <- yline[1L] - slope * xline[1L]

			fittedValues <- (slope * theoretical) + intercept

			# confidence bands based on normal confidence intervals
			if (bandType == "normal") {
				quantiles <- do.call(dFunc, c(list(x = theoretical), dparams))

				zCrit <- qnorm(p = (1 - (1 - conf) / 2))
				stdErr <- (slope / do.call(dFunc, c(list(x = theoretical), dparams))) * sqrt(quantiles * (1 - quantiles) / n)

				upper <- fittedValues + (zCrit * stdErr)
				lower <- fittedValues - (zCrit * stdErr)
			}

			# parametric bootstrap pointwise confidence intervals
			if (bandType == "bs") {
				# correspondence between stats and MASS distributions names
				getDist <- function(distName) {
					switch (
						distName,
						beta = "beta",
						cauchy = "cauchy",
						chisq = "chi-squared",
						exp = "exponential",
						f = "f",
						gamma = "gamma",
						geom = "geometric",
						lnorm = "lognormal",
						logis = "logistic",
						nbinom = "negative binomial",
						norm = "normal",
						pois = "poisson",
						t = dt,
						weibull = "weibull"
					)
				}

				# TODO supplying pars to the following dists in MASS::fitdistr is not supported
				if (distribution %in% c("exp", "geom", "lnorm", "norm", "pois")) {
					mle <- MASS::fitdistr(x = smp, densfun = getDist(distribution))
				} else {
					mle <- MASS::fitdistr(x = smp, densfun = getDist(distribution), start = dparams)
				}

				bs <- apply(
					X = matrix(do.call(rFunc, c(list(n = n * B), as.list(mle$estimate))), n, B),
					MARGIN = 2,
					FUN = sort
				)

				upper <- apply(X = bs, MARGIN = 1, FUN = quantile, prob = (1 + conf) / 2)
				lower <- apply(X = bs, MARGIN = 1, FUN = quantile, prob = (1 - conf) / 2)
			}

			# tail-sensitive confidence bands
			if (bandType == "ts") {
				centerFunc <- function(x) robustbase::s_Qn(x, mu.too = TRUE)[[1]]
				scaleFunc <- function(x) robustbase::Qn(x, finite.corr = FALSE)

				upperCi <- rep(NA, n)
				lowerCi <- rep(NA, n)
				pValue <- matrix(NA, nrow = n, ncol = B)

				# simulate data
				sim <- NULL
				if (is.null(mu) | is.null(sigma)) {
					for (i in 1:B) sim <- cbind(sim, sort(rnorm(n)))

					# center and scale simulated data
					center <- apply(sim, 2, centerFunc)
					scale <- apply(sim, 2, scaleFunc)
					sim <- sweep(sweep(sim, 2, center, FUN = "-"), 2, scale, FUN = "/")

					# convert simulated values to probabilities
					sim <- t(apply(sim, 1, pnorm))
				} else {
					for (i in 1:B) sim <- cbind(sim, sort(runif(n)))
				}

				# widen the CIs to get simultanoues (100 * conf)% CIs
				for (i in 1:n) {
					tmp <- pbeta(sim[i, ], shape1 = i, shape2 = n + 1 - i)
					pValue[i, ] <- apply(cbind(tmp, 1 - tmp), 1, min)
				}

				critical <- apply(pValue, 2, min)
				criticalC <- quantile(critical, prob = 1 - conf)

				upperCi <- qbeta(1 - criticalC, shape1 = 1:n, shape2 = n + 1 - (1:n))
				lowerCi <- qbeta(criticalC, shape1 = 1:n, shape2 = n + 1 - (1:n))

				# translate back to sample quantiles
				if (is.null(mu) | is.null(sigma)) {
					upper <- qnorm(upperCi) * scaleFunc(smp) + centerFunc(smp)
					lower <- qnorm(lowerCi) * scaleFunc(smp) + centerFunc(smp)
				} else {
					upper <- qnorm(upperCi) * sigma + mu
					lower <- qnorm(lowerCi) * sigma + mu
				}
			}

			out <- data.frame(
				x = theoretical,
				upper = upper,
				lower = lower,
				fill = rgb(.6, .6, .6, .5)
			)

			if (discrete) {
				out$colour <- rgb(.5, .5, .5)
				# create a data.frame with unique rows
				out <- dplyr::summarize(
								group_by(out, x, fill, colour),
								upper = max(upper),
								lower = min(lower)
							 )
				out <- as.data.frame(out)
			}

			# detrend the confidence bands by keeping the same distance from the
			# stat_qq_line, which now should be a line centered on y = 0
			if (detrend) {
				aux <- c(max(out$upper), min(out$lower))
				out$upper <- out$upper - fittedValues
				out$lower <- out$lower - fittedValues
			}

			out
		}
	}
)
