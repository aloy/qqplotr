StatQqLine <- ggproto(
	`_class` = "StatQqLine",
	`_inherit` = StatQq,

	default_aes = aes(x = ..xline.., y = ..yline..),

	required_aes = c("sample"),

	compute_group = function(data,
									 self,
									 scales,
									 distribution = "norm",
									 dparams = list(),
									 probs = c(.25, .75),
									 qparams = list()) {
		if(length(probs) != 2) {
			stop(
				"Cannot fit line quantiles ",
				probs,
				". Parameter probs must have length 2.",
				call = F
			)
		}

		# distributional functions
		qFunc <- eval(parse(text = paste0("q", distribution)))

		# inherit sample and theoretical from StatQq
		sample <- self$super()$compute_group(data = data, distribution = qFunc, dparams = dparams)$sample
		theoretical <- self$super()$compute_group(data = data, distribution = qFunc, dparams = dparams)$theoretical

		xCoords <- do.call(qFunc, c(list(p = probs), dparams))
		yCoords <- do.call(quantile, c(list(x = sample, probs = probs), qparams))

		slope <- diff(yCoords) / diff(xCoords)
		intercept <- yCoords[1L] - slope * xCoords[1L]

		out <- data.frame(xline = c(min(theoretical), max(theoretical)))
		out$yline <- slope * out$xline + intercept

		out
	}
)

stat_qq_line <- function(mapping = NULL,
								 data = NULL,
								 geom = "path",
								 position = "identity",
								 distribution = "norm",
								 dparams = list(),
								 qparams = list(),
								 probs = c(.25, .75),
								 show.legend = NA,
								 inherit.aes = T,
								 ...) {
	layer(
		data = data,
		mapping = mapping,
		stat = StatQqLine,
		geom = geom,
		position = position,
		show.legend = show.legend,
		inherit.aes = inherit.aes,
		params = list(
			distribution = distribution,
			dparams = dparams,
			probs = probs,
			...
		)
	)
}
