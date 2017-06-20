#' Quantile-quantile line
#'
#' Draws a quantile-quantile line.
#'
#' @import ggplot2
#'
#' @include stat_qq_det.R
#'
#' @inheritParams stat_qq_det
#'
#' @param qtype Type of quantile computation used in `quantile`.
#' @param probs Numeric vector of length two, representing the quantiles used
#'   to compute the Q-Q line.
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
stat_qq_line <- function(data = NULL,
												 mapping = NULL,
												 geom = "path",
												 position = "identity",
												 show.legend = NA,
												 inherit.aes = TRUE,
												 distribution = "norm",
												 dparams = list(),
												 qtype = 7,
												 probs = c(.25, .75),
												 detrend = FALSE,
												 ...) {
	layer(
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
			probs = probs,
			detrend = detrend,
			...
		)
	)
}

#' @format NULL
#' @usage NULL
#' @export
StatQqLine <- ggproto(
	`_class` = "StatQqLine",
	`_inherit` = StatQqDet,

	default_aes = aes(x = ..xline.., y = ..yline..),

	required_aes = c("sample"),

	compute_group = {
		function(data,
						 self,
						 scales,
						 distribution = "norm",
						 dparams = list(),
						 qtype = 7,
						 probs = c(.25, .75),
						 detrend = FALSE) {
			if (length(probs) != 2) {
				stop("Cannot fit line quantiles (",
					  paste0(probs, collapse = ", "),
					  "). 'probs' must have length two.",
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
				xCoords <- do.call(qFunc, c(list(p = probs), dparams))
				yCoords <- do.call(quantile, list(x = smp, probs = probs, type = qtype))

				slope <- diff(yCoords) / diff(xCoords)
				intercept <- yCoords[1L] - slope * xCoords[1L]

				out <- data.frame(xline = c(min(theoretical), max(theoretical)))
				out$yline <- slope * out$xline + intercept
			}

			out
		}
	}
)
