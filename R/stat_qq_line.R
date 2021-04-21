#' Quantile-quantile lines
#'
#' Draws a quantile-quantile line, with an additional detrend option.
#'
#' @import ggplot2
#' @importFrom MASS fitdistr
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_path
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
#'   the objects will be detrended according to the reference Q-Q line. This
#'   procedure was described by Thode (2002), and may help reducing visual bias
#'   caused by the orthogonal distances from Q-Q points to the reference line.
#' @param identity Logical. Should an identity line be used as the reference
#'   line? If \code{TRUE}, the identity line is used. If \code{FALSE} (default),
#'   the commonly-used Q-Q line that intercepts two data quantiles specified in
#'   \code{qprobs} is used. Please notice that the chosen reference line will
#'   also be used for the detrending procedure, if \code{detrend = TRUE}.
#' @param qtype Integer between 1 and 9. Only used if \code{detrend = TRUE} and
#'   \code{identity =  FALSE}. Type of the quantile algorithm to be used by the
#'   \code{\link[stats]{quantile}} function to construct the Q-Q line.
#' @param qprobs Numeric vector of length two. Only used if \code{detrend =
#'   TRUE} and \code{identity =  FALSE}. Represents the quantiles used by the
#'   \code{\link[stats]{quantile}} function to construct the Q-Q line.
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
#' # Normal Q-Q plot of Normal data
#' gg <- ggplot(data = smp, mapping = aes(sample = norm)) +
#'  stat_qq_line() +
#'  stat_qq_point() +
#'  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
#' gg
#'
#' # Exponential Q-Q plot of mean ozone levels (airquality dataset)
#' di <- "exp"
#' dp <- list(rate = 1)
#' gg <- ggplot(data = airquality, mapping = aes(sample = Ozone)) +
#'  stat_qq_line(distribution = di, dparams = dp) +
#'  stat_qq_point(distribution = di, dparams = dp) +
#'  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
#' gg
#'
#' # Detrended Exponential Q-Q plot of mean ozone levels
#' di <- "exp"
#' dp <- list(rate = 1)
#' de <- TRUE
#' gg <- ggplot(data = airquality, mapping = aes(sample = Ozone)) +
#'  stat_qq_line(distribution = di, detrend = de) +
#'  stat_qq_point(distribution = di, detrend = de) +
#'  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
#' gg
#'
#' @export
stat_qq_line <- function(
	mapping = NULL,
	data = NULL,
	geom = "path",
	position = "identity",
	na.rm = TRUE,
	show.legend = NA,
	inherit.aes = TRUE,
	distribution = "norm",
	dparams = list(),
	detrend = FALSE,
	identity = FALSE,
	qtype = 7,
	qprobs = c(.25, .75),
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
	if (qtype < 1 | qtype > 9) {
		stop("Please provide a valid quantile type: ",
				 "'qtype' must be between 1 and 9.",
				 call. = FALSE)
	}
	if (length(qprobs) != 2) {
		stop("'qprobs' must have length two.",
				 call = FALSE)
	}
	if (sum(qprobs > 1) + sum(qprobs < 0)) {
		stop("'qprobs' cannot have any elements outside the probability domain [0,1].",
				 call = FALSE)
	}

	ggplot2::layer(
		data = data,
		mapping = mapping,
		stat = StatQqLine,
		geom = geom,
		position = position,
		show.legend = show.legend,
		inherit.aes = inherit.aes,
		params = list(
			na.rm = na.rm,
			distribution = distribution,
			dparams = dparams,
			detrend = detrend,
			identity = identity,
			qtype = qtype,
			qprobs = qprobs,
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
	`_inherit` = ggplot2::Stat,

	required_aes = c("sample"),

	default_aes = ggplot2::aes(
		x = ..xline..,
		y = ..yline..
	),

	compute_group = {
		function(data,
						 self,
						 scales,
						 distribution,
						 dparams,
						 detrend,
						 identity,
						 qtype,
						 qprobs) {
			# distributional function
			qFunc <- eval(parse(text = paste0("q", distribution)))

			smp <- sort(data$sample)
			n <- length(smp)
			quantiles <- ppoints(n)

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

			theoretical <- do.call(qFunc, c(list(p = quantiles), dparams))

			if (detrend) {
				out <- data.frame(xline = c(min(theoretical), max(theoretical)))
				out$yline <- 0
			} else {

				if (identity) {
					slope <- 1
					intercept <- 0
				} else {
					xCoords <- do.call(qFunc, c(list(p = qprobs), dparams))
					yCoords <- do.call(quantile, list(x = smp, probs = qprobs, type = qtype))
					slope <- diff(yCoords) / diff(xCoords)
					intercept <- yCoords[1] - slope * xCoords[1]
				}

				out <- data.frame(xline = c(min(theoretical), max(theoretical)))
				out$yline <- slope * out$xline + intercept
			}

			out$size <- .8
			# out$colour <- if (is.null(data$colour)) rgb(.3, .3, .3) else rep(data$colour[1], 2)

			out
		}
	}
)
