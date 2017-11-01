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
#'   \code{"custom"}, create the \code{dcustom}, \code{pcustom},
#'   \code{qcustom}, and \code{rcustom} functions).
#' @param dparams List of additional parameters passed on to the previously
#'   chosen \code{distribution} function. If an empty list is provided (default)
#'   then the distributional parameters are estimated via MLE. MLE for custom
#'   distributions is currently not supported, so you must provide the
#'   appropriate \code{dparams} in that case.
#' @param bandType Character. Only \code{"boot"} is available for now. \code{"boot"}
#'   creates pointwise confidence bands based on a bootstrap.
#' @param B Integer. If \code{bandType = "boot"}, then \code{B} is the number of
#'   bootstrap replicates.
#' @param conf Numerical. Confidence level of the bands.
#' @param detrend Logical. Should the plot objects be detrended? If \code{TRUE},
#'   the objects will be detrended according to the default identity P-P line.
#'   This procedure was described by Thode (2002), and may help reducing visual
#'   bias caused by the orthogonal distances from P-P points to the reference
#'   line.
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
#'  labs(x = "Probability Points", y = "Cumulative Probability")
#' gg
#'
#' # Shifted Normal P-P plot of Normal data
#' dp <- list(mean = 1.5)
#' gg <- ggplot(data = smp, mapping = aes(sample = norm)) +
#'  stat_pp_band(dparams = dp) +
#'  stat_pp_line() +
#'  stat_pp_point(dparams = dp) +
#'  labs(x = "Probability Points", y = "Cumulative Probability")
#' gg
#'
#' # Exponential P-P plot of Exponential data
#' di <- "exp"
#' gg <- ggplot(data = smp, mapping = aes(sample = exp)) +
#'  stat_pp_band(distribution = di) +
#'  stat_pp_line() +
#'  stat_pp_point(distribution = di) +
#'  labs(x = "Probability Points", y = "Cumulative Probability")
#' gg
#'
#' # Normal P-P plot of mean ozone levels (airquality dataset)
#' dp <- list(mean = 38, sd = 27)
#' gg <- ggplot(data = airquality, mapping = aes(sample = Ozone)) +
#'  stat_pp_band(dparams = dp) +
#'  stat_pp_line() +
#' 	stat_pp_point(dparams = dp) +
#'  labs(x = "Probability Points", y = "Cumulative Probability")
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
													bandType = "boot",
													B = 1000,
													conf = .95,
													detrend = FALSE,
													...) {
	# error handling
	if (!(distribution %in% c(
		"beta",
		"cauchy",
		"chisq",
		"exp",
		"f",
		"gamma",
		"geom",
		"lnorm",
		"logis",
		"norm",
		"nbinom",
		"pois",
		"t",
		"weibull")) &
		length(dparams) == 0 &
		table(sapply(formals(eval(parse(text = paste0("q", distribution)))), typeof))["symbol"] > 1) {
		stop(
			"MLE is currently not supported for custom distributions.\n",
			"Please provide all the custom distribution parameters to 'dparams'.",
			call. = FALSE
		)
	}
	if (conf < 0 | conf > 1) {
		stop("Please provide a valid confidence level for the bands: ",
				 "'conf' must be between 0 and 1.",
				 call. = FALSE)
	}
	if (B < 0) {
		stop("Please provide a positive value for B.",
				 call. = FALSE)
	}

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
			bandType = match.arg(bandType, c("boot")),
			B = round(B),
			conf = conf,
			discrete = distribution %in% discreteDist,
			detrend = detrend,
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
						 distribution,
						 dparams,
						 bandType,
						 B,
						 conf,
						 discrete,
						 detrend) {
			# distributional functions
			pFunc <- eval(parse(text = paste0("p", distribution)))
			rFunc <- eval(parse(text = paste0("r", distribution)))

			smp <- data$sample
			n <- length(smp)
			probs <- ppoints(n)

			# automatically estimate parameters with MLE, only if no parameters are
			# provided with dparams and there are at least one distributional parameter
			# without a default value
			if(length(dparams) == 0) {
				# equivalence between base R and MASS::fitdistr distribution names
				corresp <- function(distName) {
					switch(
						distName,
						beta = "beta",
						cauchy = "cauchy",
						chisq = "chi-squared",
						exp = "exponential",
						f = "f",
						gamma = "gamma",
						geom = "geometric",
						lnorm = "log-normal",
						logis = "logistic",
						norm = "normal",
						nbinom = "negative binomial",
						pois = "poisson",
						t = dt,
						weibull = "weibull",
						NULL
					)
				}

				# initial value for some distributions
				initVal <- function(distName) {
					switch(
						distName,
						beta = list(shape1 = 1, shape2 = 1),
						chisq = list(df = 1),
						f = list(df1 = 1, df2 = 2),
						t = list(df = 1),
						NULL
					)
				}

				suppressWarnings({
					if(!is.null(corresp(distribution))) {
						if(is.null(initVal(distribution))) {
							dparams <- MASS::fitdistr(x = smp, densfun = corresp(distribution))$estimate
						} else {
							dparams <- MASS::fitdistr(x = smp, densfun = corresp(distribution), start = initVal(distribution))$estimate
						}
					}
				})
			}

			# bootstrap pointwise confidence intervals
			if (bandType == "boot") {
				bs <- matrix(do.call(rFunc, c(list(n = n * B), dparams)), n, B)

				sim <- apply(bs, MARGIN = 2, FUN = function(x, dparams) {
					# evaluate the cdf on the observed quantiles
					do.call(pFunc, c(list(q = sort(x)), dparams))
				}, dparams = dparams)

				upper <- apply(X = sim, MARGIN = 1, FUN = quantile, prob = (1 + conf) / 2)
				lower <- apply(X = sim, MARGIN = 1, FUN = quantile, prob = (1 - conf) / 2)
			}

			out <- data.frame(
				x = probs,
				upper = upper,
				lower = lower,
				fill = if (is.null(data$fill)) rgb(.6, .6, .6, .5) else data$fill
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

			# detrend the confidence bands by keeping the same distance from the
			# identity line, which now is a line centered on y = 0
			if (detrend) {
				out$upper <- out$upper - probs
				out$lower <- out$lower - probs
			}

			out
		}
	}
)
