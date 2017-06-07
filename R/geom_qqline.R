GeomQqLine <- ggproto(
	"GeomQqLine",
	Geom,
	default_aes = aes(y = ..sample.., x = ..theoretical..),
	required_aes = c("sample"),

	draw_panel = function(data, panel_params, coord, distribution, dparams) {
		ranges <- coord$range(panel_params)
		sample <- sort(data$sample)

		# Compute theoretical quantiles
		y <- quantile(sample[!is.na(sample)], c(0.25, 0.75))
		x <- do.call(distribution, c(list(p = c(0.25, 0.75)), dparams))
		slope <- diff(y)/diff(x)
		intercept <- y[1] - slope * x[1]

		data$x    <- ranges$x[1]
		data$xend <- ranges$x[2]
		data$y    <- ranges$x[1] * slope + intercept
		data$yend <- ranges$x[2] * slope + intercept

		GeomSegment$draw_panel(unique(data), panel_params, coord)
	},

	default_aes = aes(
		colour = "black",
		size = 0.5,
		linetype = 2,
		alpha = 1.0
	),

	draw_key = draw_key_abline
)

#' Quantile-quantile line
#'
#' A quantile-quantile line geom to be used with ggplot.
#'
#' @import ggplot2
#'
#' @inheritParams ggplot2::stat_qq
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(data = mtcars, mapping = aes(sample = mpg)) +
#'    geom_qq(distribution = qnorm) +
#'    geom_qqline(distribution = qnorm)
#'
#' ggplot(data = mtcars, mapping = aes(sample = mpg)) +
#'    geom_qq(distribution = qt, dparams = list(df = 2)) +
#'    geom_qqline(distribution = qt, dparams = list(df = 2))
#'
#' @export
geom_qqline <- function(mapping = NULL,
						data = NULL,
						...,
						position = "identity",
						distribution = stats::qnorm,
						dparams = list(),
						na.rm = F,
						show.legend = NA,
						inherit.aes = T) {
	layer(
		data = data,
		mapping = mapping,
		stat = StatIdentity,
		geom = GeomQqLine,
		position = position,
		show.legend = show.legend,
		inherit.aes = inherit.aes,
		params = list(
			distribution = distribution,
			dparams = dparams,
			na.rm = na.rm,
			...
		)
	)
}
