StatQqBand <- ggproto(
	`_class` = "StatQqBand",
	`_inherit` = StatQqLine,

	default_aes = aes(x = ..x.., ymin = ..lower.., ymax = ..upper..),

	required_aes = c("sample", "x"),

	compute_group = {function(data,
									  self,
									  scales,
									  distribution = "norm",
									  dparams = list(),
									  conf = .95,
									  quantiles = NULL) {
		sample <- sort(data$sample)
		n <- length(sample)

		# compute theoretical quantiles
		if (is.null(quantiles)) {
			quantiles <- stats::ppoints(n)
		} else {
			stopifnot(length(quantiles) == n)
		}

		# distributional functions
		qFunc <- eval(parse(text = paste0("q", distribution)))
		dFunc <- eval(parse(text = paste0("d", distribution)))

		# inherit from StatQq
		theoretical <- self$super()$super()$compute_group(data = data, distribution = qFunc, dparams = dparams)$theoretical

		# inherit from StatQqLine
		xline <- self$super()$compute_group(data = data, distribution = distribution, dparams = dparams)$xline
		yline <- self$super()$compute_group(data = data, distribution = distribution, dparams = dparams)$yline

		slope <- diff(yline) / diff(xline)
		intercept <- yline[1L] - slope * xline[1L]

		zCrit <- stats::qnorm(p = (1 - (1 - conf) / 2))
		stdErr <- (slope / do.call(dFunc, c(list(x = theoretical), dparams))) * sqrt(quantiles * (1 - quantiles) / n)
		fittedValues <- (slope * theoretical) + intercept

		out <- data.frame(
			x = theoretical,
			upper = fittedValues + zCrit * stdErr,
			lower = fittedValues - zCrit * stdErr
		)

		out
	}}
)

stat_qq_band <- {function(mapping = NULL,
								  data = NULL,
								  geom = "ribbon",
								  position = "identity",
								  distribution = "norm",
								  dparams = list(),
								  conf = .95,
								  quantiles = NULL,
								  show.legend = NA,
								  inherit.aes = T,
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
			conf = conf,
			quantiles = quantiles,
			distribution = distribution,
			dparams = dparams,
			...
		)
	)
}}
