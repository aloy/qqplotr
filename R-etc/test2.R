stat_qq_band <- function(data = NULL,
												 mapping = NULL,
												 geom = "ribbon",
												 position = "identity",
												 show.legend = NA,
												 inherit.aes = TRUE,
												 distribution = "norm",
												 dparams = list(),
												 conf = .95,
												 ...) {
	layer(
		data = data,
		mapping = mapping,
		stat = StatQqBand,
		geom = geom,
		position = position,
		show.legend = show.legend,
		inherit.aes = inherit.aes,
		params = list(
			distribution = distribution,
			dparams = dparams,
			conf = conf,
			...
		)
	)
}

StatQqBand <- ggproto(
	`_class` = "StatQqBand",
	`_inherit` = StatQqLine,

	default_aes = aes(x = ..x.., ymin = ..lower.., ymax = ..upper..),

	required_aes = c("sample", "x"),

	compute_group = {
		function(data,
						 self,
						 scales,
						 distribution = "norm",
						 dparams = list(),
						 conf = .95) {

			# distributional functions
			qFunc <- eval(parse(text = paste0("q", distribution)))
			dFunc <- eval(parse(text = paste0("d", distribution)))

			# inherit from StatQq
			theoretical <- self$super()$super()$compute_group(data = data,
																												distribution = distribution,
																												dparams = dparams)$theoretical
			quantiles <- do.call(dFunc, c(list(x = theoretical), dparams))
			n <- length(quantiles)

			print(self$super()$compute_group(data = data))

			# inherit from StatQqLine
			xline <- self$super()$compute_group(data = data,
																					distribution = distribution,
																					dparams = dparams)$xline
			yline <- self$super()$compute_group(data = data,
																					distribution = distribution,
																					dparams = dparams)$yline

			slope <- diff(yline) / diff(xline)
			intercept <- yline[1L] - slope * xline[1L]

			zCrit <- stats::qnorm(p = (1 - (1 - conf) / 2))
			stdErr <- (slope / do.call(dFunc, c(list(x = theoretical), dparams))) * sqrt(quantiles * (1 - quantiles) / n)
			fittedValues <- (slope * theoretical) + intercept

			out <- data.frame(
				x = theoretical,
				upper = fittedValues + (zCrit * stdErr),
				lower = fittedValues - (zCrit * stdErr)
			)

			out
		}
	}
)

gg <- ggplot(data = mtcars, mapping = aes(sample = mpg)) +
	stat_qq_band(mapping = aes(x = mpg), fill = rgb(.7, .7, .7, .5))
	# stat_qq_line(detrend = F, size = .8, color = rgb(.3, .3, .3)) +
	# stat_qq_rot(detrend = F)
	gg + labs(x = "theoretical", y = "sample")

# ggplot_build(gg)
