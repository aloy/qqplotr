#' Quantile-quantile confidence bands
#'
#' Draws quantile-quantile confidence bands.
#'
#' @import ggplot2
#' @importFrom MASS fitdistr
#'
#' @include stat_qq_point.R stat_qq_line.R
#'
#' @inheritParams stat_qq_point
#'
#' @param bandType Character. Either \code{"normal"}, \code{"bootstrap"} or
#'   \code{"tail-sensitive"}. \code{"normal"} constructs simultaneous confidence
#'   bands based on Normal confidence intervals. \code{"bootstrap"} creates
#'   pointwise confidence bands based on a parametric bootstrap. Finally,
#'   \code{"tail-sensitive"} constructs tail-sensitive confidence bands, as
#'   described in Aldor-Noiman et al. (2013).
#'
#' @param conf Numerical. Confidence level when constructing the confidence
#'   bands.
#'
#' @param B Integer. Number of bootstrap replicates. Only useful when
#'   \code{bandType = "bootstrap"}
#'
#' @references
#' \itemize{
#' \item{\href{http://www.tandfonline.com/doi/abs/10.1080/00031305.2013.847865}{Aldor-Noiman,
#' S. et al. 2013. The Power to See: A New Graphical Test of Normality. The
#' American Statistician.}}
#' }
#'
#' @examples
#' # defaults to standard normal distribution, not detrended
#' gg <- ggplot(data = mtcars, mapping = aes(sample = mpg)) +
#'  stat_qq_band(mapping = aes(x = mpg)) +
#'	stat_qq_line() +
#' 	stat_qq_point()
#' gg + labs(x = "theoretical", y = "sample")
#'
#' # detrending the line and points
#' detrend <- TRUE
#' gg <- ggplot(data = mtcars, mapping = aes(sample = mpg)) +
#'  stat_qq_band(mapping = aes(x = mpg), detrend = detrend) +
#'	stat_qq_line(detrend = detrend) +
#' 	stat_qq_point(detrend = detrend)
#' gg + labs(x = "theoretical", y = "sample")
#'
#' # deterended exponential distribution with rate = 1
#' detrend <- TRUE
#' distribution <- "exp"
#' gg <- ggplot(data = mtcars, mapping = aes(sample = mpg)) +
#'  stat_qq_band(mapping = aes(x = mpg), detrend = detrend, distribution = distribution) +
#'	stat_qq_line(detrend = detrend, distribution = distribution) +
#' 	stat_qq_point(detrend = detrend, distribution = distribution)
#' gg + labs(x = "theoretical", y = "sample")
#'
#' # deterended poisson distribution with lambda = 7
#' detrend <- TRUE
#' distribution <- "pois"
#' dparams <- list(lambda = 7)
#' gg <- ggplot(data = mtcars, mapping = aes(sample = mpg)) +
#'  stat_qq_band(mapping = aes(x = mpg), detrend = detrend, distribution = distribution, dparams = dparams) +
#'	stat_qq_line(detrend = detrend, distribution = distribution, dparams = dparams) +
#' 	stat_qq_point(detrend = detrend, distribution = distribution, dparams = dparams)
#' gg + labs(x = "theoretical", y = "sample")
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
			detrend = detrend,
			discrete = distribution %in% discreteDist,
			...
		)
	)
}

#' @format NULL
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

	required_aes = c("sample", "x"),

	compute_group = {
		function(data,
						 self,
						 scales,
						 distribution = "norm",
						 dparams = list(),
						 bandType = "normal",
						 B = 1000,
						 conf = .95,
						 detrend = FALSE,
						 discrete) {
			# distributional functions
			qFunc <- eval(parse(text = paste0("q", distribution)))
			dFunc <- eval(parse(text = paste0("d", distribution)))
			rFunc <- eval(parse(text = paste0("r", distribution)))

			# inherit from StatQq
			theoretical <- self$super()$super()$compute_group(data = data,
																												distribution = distribution,
																												dparams = dparams)$theoretical
			quantiles <- do.call(dFunc, c(list(x = theoretical), dparams))
			n <- length(quantiles)

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
				zCrit <- stats::qnorm(p = (1 - (1 - conf) / 2))
				stdErr <- (slope / do.call(dFunc, c(list(x = theoretical), dparams))) * sqrt(quantiles * (1 - quantiles) / n)

				upper <- fittedValues + (zCrit * stdErr)
				lower <- fittedValues - (zCrit * stdErr)
			}

			# parametric bootstrap pointwise confidence intervals
			if (bandType == "bootstrap") {
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
					mle <- MASS::fitdistr(x = data$x, densfun = getDist(distribution))
				} else {
					mle <- MASS::fitdistr(x = data$x, densfun = getDist(distribution), start = dparams)
				}

				bs <- apply(
					X = matrix(do.call(rFunc, c(list(n = n * B), as.list(mle$estimate))), n, B),
					MARGIN = 2,
					FUN = sort
				)

				upper <- apply(X = bs, MARGIN = 1, FUN = quantile, prob = (1 + conf) / 2)
				lower <- apply(X = bs, MARGIN = 1, FUN = quantile, prob = (1 - conf) / 2)
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
