#' Quantile-quantile points
#'
#' Draws quantile-quantile points, with an additional detrend option.
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
#'   the objects will be detrended according to the reference Q-Q line. This
#'   procedure was described by Thode (2002), and may help reducing visual bias
#'   caused by the orthogonal distances from Q-Q points to the reference line.
#' @param quantiles Numeric vector. Specify the quantiles to be used for the Q-Q
#'   plot. If \code{quantiles = NULL}, quantiles are automatically created using
#'   \code{\link[stats]{ppoints}}.
#' @param qtype Integer between 1 and 9. Only used if \code{detrend = TRUE}.
#'   Type of the quantile algorithm to be used by the
#'   \code{\link[stats]{quantile}} function to construct the Q-Q line.
#' @param qprobs Numeric vector of length two. Only used if \code{detrend =
#'   TRUE}. Represents the quantiles used by the \code{\link[stats]{quantile}}
#'   function to construct the Q-Q line.
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
#' # Normal Q-Q plot of simulated Normal data
#' gg <- ggplot(data = smp, mapping = aes(sample = norm)) +
#'  stat_qq_point() +
#'  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
#' gg
#'
#' # Exponential Q-Q plot of mean ozone levels (airquality dataset)
#' di <- "exp"
#' dp <- list(rate = 1)
#' gg <- ggplot(data = airquality, mapping = aes(sample = Ozone)) +
#'  stat_qq_point(distribution = di, dparams = dp) +
#'  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
#' gg
#'
#' @export
stat_qq_point <- function(data = NULL,
													mapping = NULL,
													geom = "point",
													position = "identity",
													na.rm = TRUE,
													show.legend = NA,
													inherit.aes = TRUE,
													distribution = "norm",
													dparams = list(),
													quantiles = NULL,
													qtype = 7,
													qprobs = c(.25, .75),
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
			na.rm = na.rm,
			distribution = distribution,
			dparams = dparams,
			quantiles = quantiles,
			qtype = qtype,
			qprobs = qprobs,
			detrend = detrend,
			...
		)
	)
}

#' StatQqPoint
#'
#' @keywords internal
#' @usage NULL
#' @export
StatQqPoint <- ggplot2::ggproto(
	`_class` = "StatQqPoint",
	`_inherit` = ggplot2::Stat,

	default_aes = ggplot2::aes(x = ..theoretical.., y = ..sample..),

	required_aes = c("sample"),

	compute_group = function(data,
													 self,
													 scales,
													 distribution = "norm",
													 dparams = list(),
													 quantiles = NULL,
													 qtype = 7,
													 qprobs = c(.25, .75),
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
			xCoords <- do.call(qFunc, c(list(p = qprobs), dparams))
			yCoords <- do.call(quantile, list(x = smp, probs = qprobs, type = qtype))

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

