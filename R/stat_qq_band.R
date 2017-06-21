#' Quantile-quantile confidence bands
#'
#' Draws quantile-quantile confidence bands.
#'
#' @import ggplot2
#'
#' @include stat_qq_det.R stat_qq_line.R
#'
#' @inheritParams stat_qq_det
#'
#' @param conf Numerical. Confidence level for the point-wise confidence envelope.
#' @param detrend Logical. Should the confidence bands be detrended?
#'
#' @examples
#' require(ggplot2)
#'
#' gg <- ggplot(data = mtcars, mapping = aes(sample = mpg)) +
#'   stat_qq_band(mapping = aes(x = mpg), fill = rgb(.7, .7, .7, .5)) +
#'   stat_qq()
#'   gg + labs(x = "theoretical", y = "sample")
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
												 conf = .95,
												 detrend = FALSE,
												 ...) {
	discreteDist <- c(
		"binom", "geom", "hyper", "multinom", "nbinom", "pois"
	)

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
			conf = conf,
			detrend = detrend,
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

	default_aes = aes(x = ..x.., ymin = ..lower.., ymax = ..upper..),

	required_aes = c("sample", "x"),

	compute_group = {
		function(data,
						 self,
						 scales,
						 distribution = "norm",
						 dparams = list(),
						 conf = .95,
						 detrend = FALSE) {
			# distributional functions
			qFunc <- eval(parse(text = paste0("q", distribution)))
			dFunc <- eval(parse(text = paste0("d", distribution)))

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

			zCrit <- stats::qnorm(p = (1 - (1 - conf) / 2))
			stdErr <- (slope / do.call(dFunc, c(list(x = theoretical), dparams))) * sqrt(quantiles * (1 - quantiles) / n)
			fittedValues <- (slope * theoretical) + intercept

			if (detrend) {
				fittedValues <- rep(0, length(fittedValues))
			}

			out <- data.frame(
				x = theoretical,
				upper = fittedValues + (zCrit * stdErr),
				lower = fittedValues - (zCrit * stdErr)
			)

			out
		}
	}
)
