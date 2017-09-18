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
													auto = FALSE,
													qtype = 7,
													qprobs = c(.25, .75),
													detrend = FALSE,
													...) {
	# error handling
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
		stat = StatQqPoint,
		geom = geom,
		position = position,
		show.legend = show.legend,
		inherit.aes = inherit.aes,
		params = list(
			na.rm = na.rm,
			distribution = distribution,
			dparams = dparams,
			auto = auto,
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

	default_aes = ggplot2::aes(
		x = ..theoretical..,
		y = ..sample..
	),

	required_aes = c("sample"),

	compute_group = function(data,
													 self,
													 scales,
													 distribution,
													 dparams,
													 auto,
													 qtype,
													 qprobs,
													 detrend) {
		# distributional function
		dFunc <- eval(parse(text = paste0("d", distribution)))
		qFunc <- eval(parse(text = paste0("q", distribution)))

		oidx <- order(data$sample)
		smp <- data$sample[oidx]
		n <- length(smp)
		quantiles <- ppoints(n)

		if(auto) {
			# define here distributions with default parameters
			startList <- {function(distName) {
				switch (
					distName,
					cauchy = list(location = 0, scale = 1),
					exp = list(rate = 1),
					lnorm = list(meanlog = 0, sdlog = 1),
					logis = list(location = 0, scale = 1),
					norm = list(mean = 0, sd = 1),
					NULL
				)
			}}

			# log-likelihood function to maximize with stats4::mle
			logLik <- {function() {
				argList <- as.list(match.call())
				argList[[1]] <- NULL
				R <- do.call(dFunc, c(list(x = smp), argList))
				-sum(log(R))
			}}

			# for distributions with default values, there's no need to provide dparams
			if (!is.null(startList(distribution)) & length(dparams) == 0) {
				s <- startList(distribution)
				parList <- rep(list(bquote()), length(s))
				names(parList) <- names(s)
				formals(logLik) <- parList

				mleEst <- suppressWarnings(
					stats4::mle(minuslogl = logLik, start = s)
				)
			} else {
				parList <- rep(list(bquote()), length(dparams))
				names(parList) <- names(dparams)
				formals(logLik) <- parList

				mleEst <- suppressWarnings(
					stats4::mle(minuslogl = logLik, start = dparams)
				)
			}

			dparams <- as.list(mleEst@coef)
			print(dparams)
		}

		theoretical <- do.call(qFunc, c(list(p = quantiles), dparams))

		if (detrend) {
			xCoords <- do.call(qFunc, c(list(p = qprobs), dparams))
			yCoords <- do.call(quantile, list(x = smp, probs = qprobs, type = qtype))

			slope <- diff(yCoords) / diff(xCoords)
			intercept <- yCoords[1] - slope * xCoords[1]

			# calculate new ys for the detrended sample
			dSmp <- NULL
			for (i in 1:n) {
				lSmp <- slope * theoretical[i] + intercept
				dSmp[i] <- smp[i] - lSmp
			}

			out <- data.frame(sample = dSmp, theoretical = theoretical)
		} else {
			out <- data.frame(sample = smp, theoretical = theoretical)
		}

		out
	}
)

