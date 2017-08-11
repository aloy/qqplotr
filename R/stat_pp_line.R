#' Probability-probability lines
#'
#' Draws a probability-probability line.
#'
#' @import ggplot2
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_path
#'
#' @param ab Numeric vector of length two. The intercept (\code{a}) and slope
#'   (\code{b}) of the P-P line. Defaults to the identity line (\code{a = 0, b =
#'   1}).
#' @param detrend Logical. Should the plot objects be detrended? If \code{TRUE},
#'   the objects will be detrended according to the default identity P-P line.
#'   This procedure was described by Thode (2002), and may help reducing visual
#'   bias caused by the orthogonal distances from P-P points to the reference
#'   line.
#'
#' @examples
#' # generate random Normal data
#' set.seed(0)
#' smp <- data.frame(norm = rnorm(100))
#'
#' # Normal P-P plot of Normal data
#' gg <- ggplot(data = smp, mapping = aes(sample = norm)) +
#'  stat_pp_line() +
#'  stat_pp_point() +
#'  labs(x = "Probability Points", y = "Cumulative Probability")
#' gg
#'
#' # Shifted Normal P-P plot of Normal data
#' dp <- list(mean = 1.5)
#' gg <- ggplot(data = smp, mapping = aes(sample = norm)) +
#'  stat_pp_line() +
#'  stat_pp_point(dparams = dp) +
#'  labs(x = "Probability Points", y = "Cumulative Probability")
#' gg
#'
#' # Normal P-P plot of mean ozone levels (airquality dataset)
#' dp <- list(mean = 38, sd = 27)
#' gg <- ggplot(data = airquality, mapping = aes(sample = Ozone)) +
#'  stat_pp_line() +
#' 	stat_pp_point(dparams = dp) +
#'  labs(x = "Probability Points", y = "Cumulative Probability")
#' gg
#'
#' @export
stat_pp_line <- function(data = NULL,
												 mapping = NULL,
												 geom = "path",
												 position = "identity",
												 na.rm = TRUE,
												 show.legend = NA,
												 inherit.aes = TRUE,
												 ab = c(0, 1),
												 detrend = FALSE,
												 ...) {
	ggplot2::layer(
		data = data,
		mapping = mapping,
		stat = StatPpLine,
		geom = geom,
		position = position,
		show.legend = show.legend,
		inherit.aes = inherit.aes,
		params = list(
			na.rm = na.rm,
			ab = ab,
			detrend = detrend,
			...
		)
	)
}

#' StatPpLine
#'
#' @keywords internal
#' @usage NULL
#' @export
StatPpLine <- ggplot2::ggproto(
	`_class` = "StatPpLine",
	`_inherit` = ggplot2::Stat,

	default_aes = ggplot2::aes(x = ..xline.., y = ..yline..),

	required_aes = c("sample"),

	compute_group = {
		function(data,
						 self,
						 scales,
						 ab,
						 detrend) {
			if (detrend) {
				intercept <- 0
				slope <- 0
			} else {
				intercept <- ab[1]
				slope <- ab[2]
			}

			out <- data.frame(xline = c(0, 1))
			out$yline <- slope * out$xline + intercept
			out$size <- .8
			out$colour <- rgb(.3, .3, .3)

			# fix the line if it's drawn outside the unit square
			out$xline[which(out$xline < 0)] <- 0
			out$yline[which(out$yline < 0)] <- 0
			out$xline[which(out$xline > 1)] <- 1
			out$yline[which(out$yline > 1)] <- 1

			out
		}
	}
)
