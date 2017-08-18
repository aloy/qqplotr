#' @rdname stat_qq_band
#' @param stat statistic to use to calculate confidence bands. Should be `qq_band`.
#' @export
geom_qq_band <- function(mapping = NULL,
												 data = NULL,
												 stat = "qq_band",
												 position = "identity",
												 na.rm = FALSE,
												 distribution = "norm",
												 dparams = list(),
												 qtype = 7,
												 qprobs = c(.25, .75),
												 bandType = "normal",
												 B = 1000,
												 conf = .95,
												 mu = NULL,
												 sigma = NULL,
												 detrend = FALSE,
												 show.legend = NA,
												 inherit.aes = TRUE,
												 ...) {
	# error handling
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

	discreteDist <- c("binom", "geom", "hyper", "multinom", "nbinom", "pois")

	ggplot2::layer(
		data = data,
		mapping = mapping,
		stat = stat,
		geom = GeomQqBand,
		position = position,
		show.legend = show.legend,
		inherit.aes = inherit.aes,
		params = list(
			na.rm = na.rm,
			distribution = distribution,
			dparams = dparams,
			qtype = qtype,
			qprobs = qprobs,
			bandType = match.arg(bandType, c("normal", "ts", "bs")),
			B = round(B),
			conf = conf,
			mu = mu,
			sigma = sigma,
			detrend = detrend,
			discrete = distribution %in% discreteDist,
			...
		)
	)
}

#' @importFrom grid grobTree
GeomQqBand <- ggplot2::ggproto(
	`_class` = "GeomQqBand",
	`_inherit` = ggplot2::Geom,

	default_aes = ggplot2::aes(
		width = 0.75,
		linetype = "solid",
		fontsize = 5,
		shape = 19,
		colour = NA,
		size = .1,
		fill = "blue",
		alpha = .8,
		stroke = 0.1,
		linewidth = .1,
		weight = 1,
		x = NULL,
		y = NULL,
		conds = NULL
	),

	required_aes = c("x", "ymin", "ymax"),

	setup_data = function(data, params) {
		data
	},

  draw_group = ggplot2::GeomRibbon$draw_group,

	draw_key = ggplot2::draw_key_polygon
)
