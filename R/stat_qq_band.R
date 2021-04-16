#' Quantile-quantile confidence bands
#'
#' Draws quantile-quantile confidence bands, with an additional detrend option.
#'
#' @import ggplot2
#' @importFrom dplyr summarize
#' @importFrom dplyr group_by
#' @importFrom MASS fitdistr
#' @importFrom robustbase s_Qn
#' @importFrom robustbase Qn
#'
#' @include stat_qq_line.R
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
#' @param detrend Logical. Should the plot objects be detrended? If \code{TRUE},
#'   the objects will be detrended according to the reference Q-Q line. This
#'   procedure was described by Thode (2002), and may help reducing visual bias
#'   caused by the orthogonal distances from Q-Q points to the reference line.
#' @param identity Logical. Should an identity line be used as the reference
#'   line used to construct the confidence bands? If \code{TRUE}, the identity
#'   line is used. If \code{FALSE} (default), the commonly-used Q-Q line that
#'   intercepts two data quantiles specified in \code{qprobs} is used. Please
#'   notice that the chosen reference line will also be used for the detrending
#'   procedure, if \code{detrend = TRUE}.
#' @param qtype Integer between 1 and 9. Type of the quantile algorithm to be
#'   used by the \code{\link[stats]{quantile}} function to construct the Q-Q
#'   line.
#' @param qprobs Numeric vector of length two. Represents the quantiles used by
#'   the \code{\link[stats]{quantile}} function to construct the Q-Q line.
#' @param bandType Character. Either \code{"pointwise"}, \code{"boot"}, \code{"ks"} or
#'   \code{"ts"}. \code{"pointwise"} constructs pointwise confidence bands based
#'   on Normal confidence intervals. \code{"boot"} creates pointwise confidence
#'   bands based on a parametric bootstrap; parameters are estimated with MLEs.
#'   \code{"ks"} constructs simultaneous confidence bands based on the Kolmogorov-Smirnov
#'   test. Finally, \code{"ts"} constructs tail-sensitive confidence bands, as
#'   described by Aldor-Noiman et al. (2013) (also, see 'Note' for
#'   limitations).
#' @param B Integer. If \code{bandType = "boot"}, then \code{B} is the number of
#'   bootstrap replicates. If \code{bandType = "ts"}, then \code{B} is the
#'   number of simulated samples.
#' @param conf Numerical. Confidence level of the bands.
#' @param mu Numerical. Only used if \code{bandType = "ts"}. Center
#'   distributional parameter used to construct the simulated tail-sensitive
#'   confidence bands. If either \code{mu} or \code{sigma} are \code{NULL}, then
#'   those parameters are estimated using \code{\link[robustbase]{Qn}} and
#'   \code{\link[robustbase]{s_Qn}}, respectively.
#' @param sigma Numerical. Only used if \code{bandType = "ts"}. Scale
#'   distributional parameter used to construct the simulated tail-sensitive
#'   confidence bands. If either \code{mu} or \code{sigma} are \code{NULL}, then
#'   those parameters are estimated using robust estimates from the \pkg{stats}
#'   package.
#'
#' @note
#' \itemize{
#' \item{Tail-sensitive confidence bands are only implemented for Normal Q-Q
#' plots. As a future update, we intend to generalize to other distributions.}
#' \item{Bootstrap bands are constructed based on a MLE parametric bootstrap.
#' Hence, it is not possible to construct such bands if the sample and
#' theoretical distributions present mismatching supports.}
#' }
#'
#' @references
#' \itemize{
#' \item{\href{https://www.routledge.com/Testing-For-Normality/Thode/p/book/9780824796136}{Thode,
#' H. (2002), Testing for Normality. CRC Press, 1st Ed.}}
#' \item{\href{https://www.tandfonline.com/doi/abs/10.1080/00031305.2013.847865}{Aldor-Noiman,
#' S. et al. (2013). The Power to See: A New Graphical Test of Normality. The
#' American Statistician. 67:4.}}
#' }
#'
#' @examples
#' # generate random Normal data
#' set.seed(0)
#' smp <- data.frame(norm = rnorm(100))
#'
#' # Normal Q-Q plot of Normal data
#' gg <- ggplot(data = smp, mapping = aes(sample = norm)) +
#'  stat_qq_band() +
#'  stat_qq_line() +
#'  stat_qq_point()
#' gg + labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
#'
#' # Exponential Q-Q plot of mean ozone levels (airquality dataset)
#' di <- "exp"
#' dp <- list(rate = 1)
#' gg <- ggplot(data = airquality, mapping = aes(sample = Ozone)) +
#'  stat_qq_band(distribution = di, dparams = dp) +
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
#'  stat_qq_band(distribution = di, detrend = de) +
#'  stat_qq_line(distribution = di, detrend = de) +
#'  stat_qq_point(distribution = di, detrend = de) +
#'  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
#' gg
#'
#' # Normal Q-Q plot of Normal data with boostrap confidence bands
#' bt <- "boot"
#' gg <- ggplot(data = smp, mapping = aes(sample = norm)) +
#'  stat_qq_band(bandType = bt) +
#'  stat_qq_line() +
#'  stat_qq_point() +
#'  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
#' gg
#'
#' # Normal Q-Q plot of Normal data with tail-sensitive confidence bands
#' bt <- "ts"
#' gg <- ggplot(data = smp, mapping = aes(sample = norm)) +
#'  stat_qq_band(bandType = bt) +
#'  stat_qq_line() +
#'  stat_qq_point() +
#'  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
#' gg
#'
#' @export
stat_qq_band <- function(
	mapping = NULL,
	data = NULL,
	geom = "qq_band",
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
	bandType = "pointwise",
	B = 1000,
	conf = .95,
	mu = NULL,
	sigma = NULL,
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
	if (conf < 0 | conf > 1) {
		stop("Please provide a valid confidence level for the bands: ",
				 "'conf' must be between 0 and 1.",
				 call. = FALSE)
	}
	if (B < 0) {
		stop("Please provide a positive value for B.",
				 call. = FALSE)
	}
	bandType <- match.arg(bandType, c("pointwise", "boot", "ts", "ks"))

	# vector with common discrete distributions
	discreteDist <- c("binom", "geom", "nbinom", "pois")

	if (distribution %in% discreteDist) geom <- "errorbar"

	ggplot2::layer(
		data = data,
		mapping = mapping,
		stat = StatQqBand,
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
			bandType = bandType,
			B = round(B),
			conf = conf,
			mu = mu,
			sigma = sigma,
			discrete = distribution %in% discreteDist,
			...
		)
	)
}

#' StatQqBand
#'
#' @keywords internal
#' @usage NULL
#' @export
StatQqBand <- ggplot2::ggproto(
	`_class` = "StatQqBand",
	`_inherit` = StatQqLine,

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
						 detrend,
						 identity,
						 qtype,
						 qprobs,
						 bandType,
						 B,
						 conf,
						 mu,
						 sigma,
						 discrete) {
			# distributional functions
			dFunc <- eval(parse(text = paste0("d", distribution)))
			qFunc <- eval(parse(text = paste0("q", distribution)))
			rFunc <- eval(parse(text = paste0("r", distribution)))

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
						t = "t",
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

			# inherit from StatQqLine
			xline <- self$super()$compute_group(data = data,
																					distribution = distribution,
																					dparams = dparams,
																					qtype = qtype,
																					qprobs = qprobs,
																					identity = identity,
																					detrend = FALSE)$xline
			yline <- self$super()$compute_group(data = data,
																					distribution = distribution,
																					dparams = dparams,
																					qtype = qtype,
																					qprobs = qprobs,
																					identity = identity,
																					detrend = FALSE)$yline

			slope <- diff(yline) / diff(xline)
			intercept <- yline[1L] - slope * xline[1L]

			fittedValues <- (slope * theoretical) + intercept

			# pointwise confidence bands based on normal confidence intervals
			if (bandType == "pointwise") {
				probs <- ppoints(n)
				stdErr <- (slope / do.call(dFunc, c(list(x = theoretical), dparams))) * sqrt(probs * (1 - probs) / n)
				zCrit <- qnorm(p = (1 - (1 - conf) / 2))

				upper <- fittedValues + (stdErr * zCrit)
				lower <- fittedValues - (stdErr * zCrit)
			}

			# parametric bootstrap pointwise confidence intervals
			if (bandType == "boot") {
				bs <- apply(
					X = matrix(do.call(rFunc, c(list(n = n * B), as.list(dparams))), n, B),
					MARGIN = 2,
					FUN = sort
				)

				upper <- apply(X = bs, MARGIN = 1, FUN = quantile, probs = (1 + conf) / 2)
				lower <- apply(X = bs, MARGIN = 1, FUN = quantile, probs = (1 - conf) / 2)
			}

			# using the DKW inequality for simultaneous bands
			if (bandType == "ks") {
				probs <- ppoints(n)
				epsilon <- sqrt((1 / (2 * n)) * log(2/(1-conf)))
				lp <- pmax(probs - epsilon, rep(0, n))
				up <- pmin(probs + epsilon, rep(1, n))
				lower <- intercept + slope * do.call(qFunc, c(list(p = lp), dparams))
				upper <- intercept + slope * do.call(qFunc, c(list(p = up), dparams))
			}

			# tail-sensitive confidence bands
			if (bandType == "ts") {
				if (distribution != "norm") {
					warning("Be aware that tail-sensitive confidence bands are _only_ implemented for Normal Q-Q plots.",
									call. = F)
				}

				centerFunc <- function(x) robustbase::s_Qn(x, mu.too = TRUE)[[1]]
				scaleFunc <- function(x) robustbase::Qn(x, finite.corr = FALSE)

				upperCi <- rep(NA, n)
				lowerCi <- rep(NA, n)
				pValue <- matrix(NA, nrow = n, ncol = B)

				# simulate data
				sim <- NULL
				if (is.null(mu) | is.null(sigma)) {
					for (i in 1:B) sim <- cbind(sim, sort(rnorm(n)))

					# center and scale simulated data
					center <- apply(sim, 2, centerFunc)
					scale <- apply(sim, 2, scaleFunc)
					sim <- sweep(sweep(sim, 2, center, FUN = "-"), 2, scale, FUN = "/")

					# convert simulated values to probabilities
					sim <- t(apply(sim, 1, pnorm))
				} else {
					for (i in 1:B) sim <- cbind(sim, sort(runif(n)))
				}

				# widen the CIs to get simultanoues (100 * conf)% CIs
				for (i in 1:n) {
					tmp <- pbeta(sim[i, ], shape1 = i, shape2 = n + 1 - i)
					pValue[i, ] <- apply(cbind(tmp, 1 - tmp), 1, min)
				}

				critical <- apply(pValue, 2, min)
				criticalC <- quantile(critical, prob = 1 - conf)

				upperCi <- qbeta(1 - criticalC, shape1 = 1:n, shape2 = n + 1 - (1:n))
				lowerCi <- qbeta(criticalC, shape1 = 1:n, shape2 = n + 1 - (1:n))

				# translate back to sample quantiles
				if (is.null(mu) | is.null(sigma)) {
					upper <- qnorm(upperCi) * scaleFunc(smp) + centerFunc(smp)
					lower <- qnorm(lowerCi) * scaleFunc(smp) + centerFunc(smp)
				} else {
					upper <- qnorm(upperCi) * sigma + mu
					lower <- qnorm(lowerCi) * sigma + mu
				}
			}

			out <- data.frame(
				x = theoretical,
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
			# stat_qq_line, which now is a line centered on y = 0
			if (detrend) {
				out$upper <- out$upper - fittedValues
				out$lower <- out$lower - fittedValues
			}

			out
		}
	}
)
