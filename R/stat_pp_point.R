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
#'
#' @examples
#' # generate random Normal data
#' set.seed(0)
#' smp <- data.frame(norm = rnorm(100))
#'
#' # Normal P-P plot of Normal data
#' gg <- ggplot(data = smp, mapping = aes(sample = norm)) +
#'  stat_pp_point() +
#'  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
#' gg
#'
#' # Shifted Normal P-P plot of Normal data
#' dp <- list(mean = 1.5)
#' gg <- ggplot(data = smp, mapping = aes(sample = norm)) +
#'  stat_pp_point(dparams = dp) +
#'  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
#' gg
#'
#' # Normal P-P plot of mean ozone levels (airquality dataset)
#' dp <- list(mean = 38, sd = 27)
#' gg <- ggplot(data = airquality, mapping = aes(sample = Ozone)) +
#' 	stat_pp_point(dparams = dp) +
#' 	labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
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
													 distribution = "norm",
													 dparams = list()) {
		# distributional function
		qFunc <- eval(parse(text = paste0("q", distribution)))

		smp <- sort(data$sample)
		n <- length(smp)
		probs <- ppoints(n)

		# create an empirical cdf with the sample data
		empCdf <- ecdf(smp)

		# evaluate the empirical cdf on theoretical quantiles
		y <- empCdf(do.call(qFunc, c(list(p = probs), dparams)))

		out <- data.frame(sample = y, theoretical = probs)

		# TODO Here the plot limits are forced as a unit square. This is just a
		# placeholder until we figure out how to do this in a more elegant way
		# out$alpha <- NA
		# out <- rbind(out, tail(out, 2))
		# out$theoretical[length(out)] <- 0
		# out$sample[length(out)] <- 0
		# out$alpha[length(out)] <- 0
		# out$theoretical[length(out) - 1] <- 1
		# out$sample[length(out) - 1] <- 1
		# out$alpha[length(out) - 1] <- 0

		out
	}
)
