#' Quantile-quantile plots with detrend option
#'
#' Draws a regular quantile-quantile plot with an additional detrend option.
#'
#' @import ggplot2
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#'
#' @param distribution Character. Distribution function to use, if x not specified.
#' @param dparams List of additional parameters passed on to `distribution` function.
#' @param detrend Logical. Should the points be detrended?
#'
#' @examples
#' require(ggplot2)
#'
#' gg <- ggplot(data = mtcars, mapping = aes(sample = mpg)) +
#'   stat_qq_line(size = .8, color = rgb(.3, .3, .3)) +
#'   stat_qq()
#'   gg + labs(x = "theoretical", y = "sample")
#'
#' @export
stat_qq_det <- function(data = NULL,
												mapping = NULL,
												geom = "point",
												position = "identity",
												show.legend = NA,
												inherit.aes = T,
												distribution = "norm",
												dparams = list(),
												qtype = 7,
												probs = c(.25, .75),
												quantiles = NULL,
												detrend = FALSE,
												...) {
	layer(
		mapping = mapping,
		stat = StatQqDet,
		geom = geom,
		position = position,
		show.legend = show.legend,
		inherit.aes = inherit.aes,
		params = list(
			distribution = distribution,
			dparams = dparams,
			qtype = qtype,
			probs = probs,
			quantiles = quantiles,
			detrend = detrend,
			...
		)
	)
}

#' @format NULL
#' @usage NULL
#' @export
StatQqDet <- ggproto(
	`_class` = "StatQqDet",
	`_inherit` = Stat,

	default_aes = aes(x = ..theoretical.., y = ..sample..),

	required_aes = c("sample"),

	compute_group = function(data,
													 self,
													 scales,
													 distribution = "norm",
													 dparams = list(),
													 qtype = 7,
													 probs = c(.25, .75),
													 quantiles = NULL,
													 detrend = FALSE) {
		# distributional function
		qFunc <- eval(parse(text = paste0("q", distribution)))

		smp <- sort(data$sample)
		n <- length(smp)

		# compute theoretical quantiles
		if (is.null(quantiles)) {
			quantiles <- stats::ppoints(n)
		} else {
			stopifnot(length(quantiles) == n)
		}

		theoretical <- do.call(qFunc, c(list(p = quantiles), dparams))

		if (detrend) {
			xCoords <- do.call(qFunc, c(list(p = probs), dparams))
			yCoords <- do.call(quantile, list(x = smp, probs = probs, type = qtype))

			slope <- diff(yCoords) / diff(xCoords)
			intercept <- yCoords[1L] - slope * xCoords[1L]

			# calculate new ys for the detrended sample
			dSmp <- NULL
			for (i in 1:n) {
				lSmp <- slope * theoretical[i] + intercept
				dSmp[i] <- smp[i] - lSmp
			}

			out <- data.frame(sample = dSmp, theoretical)
		} else {
			out <- data.frame(sample = smp, theoretical)
		}

		out
	}
)
