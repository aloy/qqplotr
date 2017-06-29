#' Quantile-quantile plots with detrend option
#'
#' Draws a regular quantile-quantile plot with an additional detrend option.
#'
#' @import ggplot2
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#'
#' @param distribution Character. Distribution function to use, if \code{x} not
#'   specified. Do not provide the full distribution function name (e.g.,
#'   \code{"dnorm"}), instead, just use the shortened distribution name (e.g.,
#'   \code{"norm"}). If you wish to provide a custom distribution, you may do so
#'   by first creating the density and quantile functions following the standard
#'   nomenclature from the \code{stats} package (e.g., for \code{"custom"},
#'   create the \code{"dcustom"} and \code{"qcustom"} functions).
#' @param dparams List of additional parameters passed on to \code{distribution} function.
#' @param detrend Logical. Should the plot of the points be detrended?
#'
#' @examples
#' # defaults to standard normal distribution, not detrended
#' gg <- ggplot(data = mtcars, mapping = aes(sample = mpg)) +
#' 	stat_qq_point()
#' gg + labs(x = "theoretical", y = "sample")
#'
#' # detrending the line and points
#' detrend <- TRUE
#' gg <- ggplot(data = mtcars, mapping = aes(sample = mpg)) +
#' 	stat_qq_point(detrend = detrend)
#' gg + labs(x = "theoretical", y = "sample")
#'
#' # deterended exponential distribution with rate = 1
#' detrend <- TRUE
#' distribution <- "exp"
#' gg <- ggplot(data = mtcars, mapping = aes(sample = mpg)) +
#' 	stat_qq_point(detrend = detrend, distribution = distribution)
#' gg + labs(x = "theoretical", y = "sample")
#'
#' # deterended poisson distribution with lambda = 7
#' detrend <- TRUE
#' distribution <- "pois"
#' dparams <- list(lambda = 7)
#' gg <- ggplot(data = mtcars, mapping = aes(sample = mpg)) +
#' 	stat_qq_point(detrend = detrend, distribution = distribution, dparams = dparams)
#' gg + labs(x = "theoretical", y = "sample")
#'
#' @export
stat_qq_point <- function(data = NULL,
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
	ggplot2::layer(
		mapping = mapping,
		stat = StatQqPoint,
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
StatQqPoint <- ggplot2::ggproto(
	`_class` = "StatQqPoint",
	`_inherit` = Stat,

	default_aes = ggplot2::aes(x = ..theoretical.., y = ..sample..),

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
			quantiles <- ppoints(n)
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
