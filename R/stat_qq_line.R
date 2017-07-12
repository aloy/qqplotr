#' Quantile-quantile lines
#'
#' Draws a quantile-quantile line, with an additional detrend option.
#'
#' @import ggplot2
#'
#' @include stat_qq_point.R
#'
#' @inheritParams stat_qq_point
#'
#' @param qtype Integer between 1 and 9. Type of the quantile algorithm to be
#'   used by the \code{\link[stats]{quantile}} function to construct the Q-Q
#'   line.
#' @param qprobs Numeric vector of length two. Represents the quantiles used by
#'   the \code{\link[stats]{quantile}} function to construct the Q-Q line.
#'
#' @examples
#' # generate random Normal data
#' set.seed(0)
#' df <- data.frame(norm = rnorm(100))
#'
#' # Normal Q-Q plot of Normal data
#' gg <- ggplot(data = df, mapping = aes(sample = norm)) +
#'  stat_qq_line() +
#'  stat_qq_point()
#' gg + labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
#'
#' # Exponential Q-Q plot of Normal data
#' di <- "exp"
#' dp <- list(rate = 1)
#' gg <- ggplot(data = df, mapping = aes(sample = norm)) +
#'  stat_qq_line(distribution = di, dparams = dp) +
#'  stat_qq_point(distribution = di, dparams = dp)
#' gg + labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
#'
#' # Detrended Normal Q-Q plot of Normal data
#' de <- TRUE
#' gg <- ggplot(data = df, mapping = aes(sample = norm)) +
#'  stat_qq_line(detrend = de) +
#'  stat_qq_point(detrend = de)
#' gg + labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
#'
#' @export
stat_qq_line <- function(data = NULL,
												 mapping = NULL,
												 geom = "path",
												 position = "identity",
												 show.legend = NA,
												 inherit.aes = TRUE,
												 distribution = "norm",
												 dparams = list(),
												 qtype = 7,
												 qprobs = c(.25, .75),
												 detrend = FALSE,
												 ...) {
	ggplot2::layer(
		data = data,
		mapping = mapping,
		stat = StatQqLine,
		geom = geom,
		position = position,
		show.legend = show.legend,
		inherit.aes = inherit.aes,
		params = list(
			distribution = distribution,
			dparams = dparams,
			qtype = qtype,
			qprobs = qprobs,
			detrend = detrend,
			...
		)
	)
}

#' StatQqLine
#'
#' @keywords internal
#' @usage NULL
#' @export
StatQqLine <- ggplot2::ggproto(
	`_class` = "StatQqLine",
	`_inherit` = StatQqPoint,

	default_aes = ggplot2::aes(x = ..xline.., y = ..yline..),

	required_aes = c("sample"),

	compute_group = {
		function(data,
						 self,
						 scales,
						 distribution = "norm",
						 dparams = list(),
						 qtype = 7,
						 qprobs = c(.25, .75),
						 detrend = FALSE) {
			if (length(qprobs) != 2) {
				stop("Cannot fit line quantiles (",
					  paste0(qprobs, collapse = ", "),
					  "). 'qprobs' must have length two.",
					  call = FALSE)
			}

			# distributional function
			qFunc <- eval(parse(text = paste0("q", distribution)))

			# inherit j and theoretical from StatQqRot
			smp <- self$super()$compute_group(data = data,
																				distribution = distribution,
																				dparams = dparams)$sample
			theoretical <- self$super()$compute_group(data = data,
																								distribution = distribution,
																								dparams = dparams)$theoretical

			if (detrend) {
				out <- data.frame(xline = c(min(theoretical), max(theoretical)))
				out$yline <- 0
			} else {
				xCoords <- do.call(qFunc, c(list(p = qprobs), dparams))
				yCoords <- do.call(quantile, list(x = smp, probs = qprobs, type = qtype))

				do.call(quantile, list(x = smp, qprobs = qprobs, type = qtype))
				slope <- diff(yCoords) / diff(xCoords)
				intercept <- yCoords[1L] - slope * xCoords[1L]

				out <- data.frame(xline = c(min(theoretical), max(theoretical)))
				out$yline <- slope * out$xline + intercept
			}

			out$size <- .8
			out$colour <- rgb(.3, .3, .3)

			out
		}
	}
)
