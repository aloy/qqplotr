#' Quantile-quantile confidence bands
#'
#' Draws quantile-quantile confidence bands.
#'
#' @import ggplot2
#'
#' @include stat_qq_points.R stat_qq_line.R
#'
#' @inheritParams stat_qq_points
#'
#' @param conf Numerical. Confidence level for the point-wise confidence envelope.
#'
#' @examples
#' require(ggplot2)
#'
#' # defaults to standard normal distribution, not detrended
#' gg <- ggplot(data = mtcars, mapping = aes(sample = mpg)) +
#'  stat_qq_band(mapping = aes(x = mpg)) +
#'	stat_qq_line() +
#' 	stat_qq_points()
#' gg + labs(x = "theoretical", y = "sample")
#'
#' # detrending the line and points
#' detrend <- TRUE
#' gg <- ggplot(data = mtcars, mapping = aes(sample = mpg)) +
#'  stat_qq_band(mapping = aes(x = mpg), detrend = detrend) +
#'	stat_qq_line(detrend = detrend) +
#' 	stat_qq_points(detrend = detrend)
#' gg + labs(x = "theoretical", y = "sample")
#'
#' # deterended exponential distribution with rate = 1
#' detrend <- TRUE
#' distribution <- "exp"
#' gg <- ggplot(data = mtcars, mapping = aes(sample = mpg)) +
#'  stat_qq_band(mapping = aes(x = mpg), detrend = detrend, distribution = distribution) +
#'	stat_qq_line(detrend = detrend, distribution = distribution) +
#' 	stat_qq_points(detrend = detrend, distribution = distribution)
#' gg + labs(x = "theoretical", y = "sample")
#'
#' # deterended poisson distribution with lambda = 7
#' detrend <- TRUE
#' distribution <- "pois"
#' dparams <- list(lambda = 7)
#' gg <- ggplot(data = mtcars, mapping = aes(sample = mpg)) +
#'  stat_qq_band(mapping = aes(x = mpg), detrend = detrend, distribution = distribution, dparams = dparams) +
#'	stat_qq_line(detrend = detrend, distribution = distribution, dparams = dparams) +
#' 	stat_qq_points(detrend = detrend, distribution = distribution, dparams = dparams)
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
	discreteDist <- c("binom", "geom", "hyper", "multinom", "nbinom", "pois")

	if (distribution %in% discreteDist) geom <- "errorbar"

	layer(
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
StatQqBand <- ggproto(
	`_class` = "StatQqBand",
	`_inherit` = StatQqLine,

	default_aes = aes(
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

			# confidende bands based on normal confidence intervals
			if(bandType == "normal") {
				zCrit <- stats::qnorm(p = (1 - (1 - conf) / 2))
				stdErr <- (slope / do.call(dFunc, c(list(x = theoretical), dparams))) * sqrt(quantiles * (1 - quantiles) / n)

				if (detrend) fittedValues <- rep(0, length(fittedValues))

				upper <- fittedValues + (zCrit * stdErr)
				lower <- fittedValues - (zCrit * stdErr)
			}

			# parametric bootstrap pointwise confidence intervals
			if(bandType == "bootstrap") {
				bs <- apply(
					X = matrix(do.call(rFunc, c(list(n = n * B), dparams)), n, B),
					MARGIN = 2,
					FUN = function(x) {
						sort(fittedValues + x)
					}
				)

				upper <- apply(X = bs, MARGIN = 1, FUN = quantile, prob = (1 + conf) / 2)
				lower <- apply(X = bs, MARGIN = 1, FUN = quantile, prob = (1 - conf) / 2)
			}

			out <- data.frame(
				x = theoretical,
				upper = upper,
				lower = lower,
				fill = fill <- rgb(.6, .6, .6, .5)
			)

			if (discrete) out$colour <- rgb(.5, .5, .5)

			out
		}
	}
)
