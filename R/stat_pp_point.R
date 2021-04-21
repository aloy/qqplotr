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
#'   \code{"custom"}, create the \code{dcustom}, \code{pcustom},
#'   \code{qcustom}, and \code{rcustom} functions).
#' @param dparams List of additional parameters passed on to the previously
#'   chosen \code{distribution} function. If an empty list is provided (default)
#'   then the distributional parameters are estimated via MLE. MLE for custom
#'   distributions is currently not supported, so you must provide the
#'   appropriate \code{dparams} in that case.
#' @param detrend Logical. Should the plot objects be detrended? If \code{TRUE},
#'   the objects will be detrended according to the default identity P-P line.
#'   This procedure was described by Thode (2002), and may help reducing visual
#'   bias caused by the orthogonal distances from P-P points to the reference
#'   line.
#'
#' @references
#' \itemize{
#' \item{\href{https://www.routledge.com/Testing-For-Normality/Thode/p/book/9780824796136}{Thode,
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
stat_pp_point <- function(
	mapping = NULL,
	data = NULL,
	geom = "point",
	position = "identity",
	na.rm = TRUE,
	show.legend = NA,
	inherit.aes = TRUE,
	distribution = "norm",
	dparams = list(),
	detrend = FALSE,
	...
) {
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

	optional_aes = c("label"),

	compute_group = function(data,
													 self,
													 scales,
													 distribution,
													 dparams,
													 detrend) {
		# cumulative distributional function
		pFunc <- eval(parse(text = paste0("p", distribution)))

		oidx <- order(data$sample)
		smp <- data$sample[oidx]
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

		# evaluate the cdf on the observed quantiles
		y <- do.call(pFunc, c(list(q = smp), dparams))

		if (detrend) {
			# calculate new ys for the detrended sample using the identity line
			dY <- y - probs

			out <- data.frame(sample = dY, theoretical = probs)
		} else {
			out <- data.frame(sample = y, theoretical = probs)
		}

		if (!is.null(data$label)) out$label <- data$label[oidx]
		out
	}
)
