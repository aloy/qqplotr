#' Probability-probability confidence bands
#'
#' Draws probability-probability confidence bands.
#'
#' @import ggplot2
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_ribbon
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
#' @param bandType Character. Only \code{"bs"} is available for now. \code{"bs"}
#'   creates pointwise confidence bands based on a bootstrap.
#' @param B Integer. If \code{bandType = "bs"}, then \code{B} is the number of
#'   bootstrap replicates.
#' @param conf Numerical. Confidence level of the bands.
#'
#' @examples
#' # generate random Normal data
#' set.seed(0)
#' smp <- data.frame(norm = rnorm(100), exp = rexp(100))
#'
#' # Normal P-P plot of Normal data
#' gg <- ggplot(data = smp, mapping = aes(sample = norm)) +
#'  stat_pp_band() +
#'  stat_pp_line() +
#'  stat_pp_point() +
#'  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
#' gg
#'
#' # Shifted Normal P-P plot of Normal data
#' dp <- list(mean = 1.5)
#' gg <- ggplot(data = smp, mapping = aes(sample = norm)) +
#'  stat_pp_band(dparams = dp) +
#'  stat_pp_line() +
#'  stat_pp_point(dparams = dp) +
#'  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
#' gg
#'
#' # Exponential P-P plot of Exponential data
#' di <- "exp"
#' gg <- ggplot(data = smp, mapping = aes(sample = exp)) +
#'  stat_pp_band(distribution = di) +
#'  stat_pp_line() +
#'  stat_pp_point(distribution = di) +
#'  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
#' gg
#'
#' # Normal P-P plot of mean ozone levels (airquality dataset)
#' dp <- list(mean = 38, sd = 27)
#' gg <- ggplot(data = airquality, mapping = aes(sample = Ozone)) +
#'  stat_pp_band(dparams = dp) +
#'  stat_pp_line() +
#' 	stat_pp_point(dparams = dp) +
#' 	labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
#' gg
#'
#' @export
stat_pp_band <- function(data = NULL,
													mapping = NULL,
													geom = "ribbon",
													position = "identity",
													na.rm = TRUE,
													show.legend = NA,
													inherit.aes = TRUE,
													distribution = "norm",
													dparams = list(),
													bandType = "bs",
													B = 1000,
													conf = .95,
													...) {
	# vector with common discrete distributions
	discreteDist <- c("binom", "geom", "hyper", "multinom", "nbinom", "pois")

	if (distribution %in% discreteDist) geom <- "errorbar"

	ggplot2::layer(
		mapping = mapping,
		stat = StatPpBand,
		geom = geom,
		position = position,
		show.legend = show.legend,
		inherit.aes = inherit.aes,
		params = list(
			na.rm = na.rm,
			distribution = distribution,
			dparams = dparams,
			bandType = bandType,
			B = B,
			conf = conf,
			discrete = distribution %in% discreteDist,
			...
		)
	)
}

#' StatPpBand
#'
#' @keywords internal
#' @usage NULL
#' @export
StatPpBand <- ggplot2::ggproto(
	`_class` = "StatPpBand",
	`_inherit` = ggplot2::Stat,

	default_aes = ggplot2::aes(
		x = ..x..,
		ymin = ..lower..,
		ymax = ..upper..
	),

	required_aes = c("sample"),

	compute_group = {
		function(data,
						 self,
						 scales,
						 distribution = "norm",
						 dparams = list(),
						 bandType = "bs",
						 B = 1000,
						 conf = .95,
						 discrete) {
			# distributional functions
			dFunc <- eval(parse(text = paste0("d", distribution)))
			pFunc <- eval(parse(text = paste0("p", distribution)))
			qFunc <- eval(parse(text = paste0("q", distribution)))
			rFunc <- eval(parse(text = paste0("r", distribution)))

			smp <- data$sample
			n <- length(smp)
			probs <- ppoints(n)

			# bootstrap pointwise confidence intervals
			if (bandType == "bs") {
				bs <- matrix(do.call(rFunc, c(list(n = n * B), dparams)), n, B)

				sim <- apply(bs, MARGIN = 2, FUN = function(x) {
					# evaluate the cdf on the observed quantiles
					do.call(pFunc, c(list(q = sort(x)), dparams))
				})

				upper <- apply(X = sim, MARGIN = 1, FUN = quantile, prob = (1 + conf) / 2)
				lower <- apply(X = sim, MARGIN = 1, FUN = quantile, prob = (1 - conf) / 2)
			}

			out <- data.frame(
				x = probs,
				upper = upper,
				lower = lower,
				fill = rgb(.6, .6, .6, .5)
			)

			if (discrete) {
				out$colour <- rgb(.5, .5, .5)
				# create a data.frame with unique rows
				out <- dplyr::summarize(
					dplyr::group_by(out, x, fill, colour),
					upper = max(upper),
					lower = min(lower)
				)
				out <- as.data.frame(out)
			}

			out
		}
	}
)
