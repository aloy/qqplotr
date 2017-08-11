#' Probability-probability points
#'
#' Draws probability-probability points.
#'
#' @import ggplot2
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#'
#' @param distribution Character. Theoretical probability distribution function
#'   to use. Do not provide the full distribution function name (e.g.,
#'   \code{"dnorm"}). Instead, just provide its shortened name (e.g.,
#'   \code{"norm"}). If you wish to provide a custom distribution, you may do so
#'   by first creating the density, quantile, and random functions following the
#'   standard nomenclature from the \code{stats} package (i.e., for
#'   \code{"custom"}, create the \code{"dcustom"}, \code{"qcustom"}, and
#'   \code{"rcustom"} functions).
#' @param dparams List of additional parameters passed on to the previously
#'   chosen \code{distribution} function.
#' @param detrend Logical. Should the plot objects be detrended? If \code{TRUE},
#'   the objects will be detrended according to the default identity P-P line.
#'   This procedure was described by Thode (2002), and may help reducing visual
#'   bias caused by the orthogonal distances from P-P points to the reference
#'   line.
#'
#' @references
#' \itemize{
#' \item{\href{https://www.crcpress.com/Testing-For-Normality/Thode/p/book/9780824796136}{Thode,
#' H. (2002), Testing for Normality. CRC Press, 1st Ed.}}
#' }
#'
#' @examples
#' # generate random Normal data
#' set.seed(0)
#' smp <- data.frame(norm = rnorm(100))
#'
#' # Normal P-P plot of Normal data
#' gg <- ggplot(data = smp, mapping = aes(sample = norm)) +
#'  stat_pp_point() +
#'  labs(x = "Probability Points", y = "Cumulative Probability")
#' gg
#'
#' # Shifted Normal P-P plot of Normal data
#' dp <- list(mean = 1.5)
#' gg <- ggplot(data = smp, mapping = aes(sample = norm)) +
#'  stat_pp_point(dparams = dp) +
#'  labs(x = "Probability Points", y = "Cumulative Probability")
#' gg
#'
#' # Normal P-P plot of mean ozone levels (airquality dataset)
#' dp <- list(mean = 38, sd = 27)
#' gg <- ggplot(data = airquality, mapping = aes(sample = Ozone)) +
#' 	stat_pp_point(dparams = dp) +
#'  labs(x = "Probability Points", y = "Cumulative Probability")
#' gg
#'
#' @export
stat_pp_point <- function(data = NULL,
													mapping = NULL,
													geom = "point",
													position = "identity",
													na.rm = TRUE,
													show.legend = NA,
													inherit.aes = TRUE,
													distribution = "norm",
													dparams = list(),
													detrend = FALSE,
													...) {
	ggplot2::layer(
		mapping = mapping,
		stat = StatPpPoint,
		geom = geom,
		position = position,
		show.legend = show.legend,
		inherit.aes = inherit.aes,
		params = list(
			na.rm = na.rm,
			distribution = distribution,
			dparams = dparams,
			detrend = detrend,
			...
		)
	)
}

#' StatPpPoint
#'
#' @keywords internal
#' @usage NULL
#' @export
StatPpPoint <- ggplot2::ggproto(
	`_class` = "StatPpPoint",
	`_inherit` = ggplot2::Stat,

	default_aes = ggplot2::aes(x = ..theoretical.., y = ..sample..),

	required_aes = c("sample"),

	compute_group = function(data,
													 self,
													 scales,
													 distribution,
													 dparams,
													 detrend) {
		# distributional function
		qFunc <- eval(parse(text = paste0("q", distribution)))

		smp <- sort(data$sample)
		n <- length(smp)
		probs <- ppoints(n)

		# create an empirical cdf with the sample data
		empCdf <- ecdf(smp)

		# evaluate the empirical cdf on theoretical quantiles
		y <- empCdf(do.call(qFunc, c(list(p = probs), dparams)))

		if (detrend) {
			# calculate new ys for the detrended sample using the identity line
			dY <- y - probs

			out <- data.frame(sample = dY, theoretical = probs)
		} else {
			out <- data.frame(sample = y, theoretical = probs)
		}

		out
	}
)
