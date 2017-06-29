require(devtools)

# including new packages into DESCRIPTION
devtools::use_package("MASS", type = "Imports")
devtools::use_package("ggplot2", type = "Depends")

# if NAMESPACE is not yet created (or updated for some reason), run:
roxygen2::roxygenize()

# testing the .Rds
stat_qq_point
stat_qq_line
stat_qq_band

# examples
require(qqplotr)

detrend <- F
d <- "norm"
dp <- list()

gg <- ggplot(data = mtcars, mapping = aes(sample = mpg)) +
	stat_qq_band(mapping = aes(x = mpg), distribution = d, dparams = dp, detrend = detrend, bandType = "bootstrap", B = 10000) +
	stat_qq_line(distribution = d, dparams = dp, detrend = detrend) +
	stat_qq_point(distribution = d, dparams = dp, detrend = detrend)
  gg + labs(x = "theoretical", y = "sample")

# debugging
ggplot_build(gg)$data[[1]] # stat_qq_point
ggplot_build(gg)$data[[2]] # stat_qq_line
ggplot_build(gg)$data[[3]] # stat_qq_band

# manual test below ----

# stat_qq_band2 <- function(data = NULL,
# 												 mapping = NULL,
# 												 geom = "ribbon",
# 												 position = "identity",
# 												 show.legend = NA,
# 												 inherit.aes = TRUE,
# 												 distribution = "norm",
# 												 dparams = list(),
# 												 bandType = "normal",
# 												 B = 1000,
# 												 conf = .95,
# 												 detrend = FALSE,
# 												 ...) {
# 	discreteDist <- c("binom", "geom", "hyper", "multinom", "nbinom", "pois")
#
# 	if (distribution %in% discreteDist) geom <- "errorbar"
#
# 	layer(
# 		data = data,
# 		mapping = mapping,
# 		stat = StatQqBand2,
# 		geom = geom,
# 		position = position,
# 		show.legend = show.legend,
# 		inherit.aes = inherit.aes,
# 		params = list(
# 			distribution = distribution,
# 			dparams = dparams,
# 			bandType = bandType,
# 			B = B,
# 			conf = conf,
# 			detrend = detrend,
# 			discrete = distribution %in% discreteDist,
# 			...
# 		)
# 	)
# }
#
# StatQqBand2 <- ggproto(
# 	`_class` = "StatQqBand",
# 	`_inherit` = StatQqLine,
#
# 	default_aes = aes(
# 		x = ..x..,
# 		ymin = ..lower..,
# 		ymax = ..upper..
# 	),
#
# 	required_aes = c("sample", "x"),
#
# 	compute_group = {
# 		function(data,
# 						 self,
# 						 scales,
# 						 distribution = "norm",
# 						 dparams = list(),
# 						 bandType = "normal",
# 						 B = 1000,
# 						 conf = .95,
# 						 detrend = FALSE,
# 						 discrete) {
# 			# distributional functions
# 			qFunc <- eval(parse(text = paste0("q", distribution)))
# 			dFunc <- eval(parse(text = paste0("d", distribution)))
# 			rFunc <- eval(parse(text = paste0("r", distribution)))
#
# 			# inherit from StatQq
# 			theoretical <- self$super()$super()$compute_group(data = data,
# 																												distribution = distribution,
# 																												dparams = dparams)$theoretical
# 			quantiles <- do.call(dFunc, c(list(x = theoretical), dparams))
# 			n <- length(quantiles)
#
# 			# confidende bands based on normal confidence intervals
# 			if(bandType == "normal") {
# 				# inherit from StatQqLine
# 				xline <- self$super()$compute_group(data = data,
# 																						distribution = distribution,
# 																						dparams = dparams)$xline
# 				yline <- self$super()$compute_group(data = data,
# 																						distribution = distribution,
# 																						dparams = dparams)$yline
#
# 				slope <- diff(yline) / diff(xline)
# 				intercept <- yline[1L] - slope * xline[1L]
#
# 				fittedValues <- (slope * theoretical) + intercept
#
# 				zCrit <- stats::qnorm(p = (1 - (1 - conf) / 2))
# 				stdErr <- (slope / do.call(dFunc, c(list(x = theoretical), dparams))) * sqrt(quantiles * (1 - quantiles) / n)
#
# 				if (detrend) fittedValues <- rep(0, length(fittedValues))
#
# 				upper <- fittedValues + (zCrit * stdErr)
# 				lower <- fittedValues - (zCrit * stdErr)
# 			}
#
# 			# parametric bootstrap pointwise confidence intervals
# 			if(bandType == "bootstrap") {
# 				mle <- fitdistr(x = data$x, densfun = "normal")
#
# 				bs <- apply(
# 					X = matrix(do.call(rFunc, c(list(n = n * B), as.list(mle$estimate))), n, B),
# 					MARGIN = 2,
# 					FUN = sort
# 				)
#
# 				upper <- apply(X = bs, MARGIN = 1, FUN = quantile, prob = (1 + conf) / 2)
# 				lower <- apply(X = bs, MARGIN = 1, FUN = quantile, prob = (1 - conf) / 2)
# 			}
#
# 			out <- data.frame(
# 				x = theoretical,
# 				upper = upper,
# 				lower = lower,
# 				fill = fill <- rgb(.6, .6, .6, .5)
# 			)
#
# 			if (discrete) out$colour <- rgb(.5, .5, .5)
#
# 			out
# 		}
# 	}
# )
#
# detrend <- F
# d <- "norm"
# dp <- list()
#
# gg <- ggplot(data = mtcars, mapping = aes(sample = mpg)) +
# 	stat_qq_band2(mapping = aes(x = mpg), distribution = d, dparams = dp, detrend = detrend, bandType = "normal") +
# 	stat_qq_line(distribution = d, dparams = dp, detrend = detrend) +
# 	stat_qq_point(distribution = d, dparams = dp, detrend = detrend)
# 	gg + labs(x = "theoretical", y = "sample")
