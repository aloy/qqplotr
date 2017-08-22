if (!requireNamespace("shiny", quietly = TRUE)) {
	stop("'shiny' package is needed for this Shiny app to work. Please install it.",
			 call. = FALSE)
} else {
	require(shiny)
}

source("global.R")

shinyServer(
	function(input, output, session) {
		# update qprobs sliders
		observeEvent(input$optQprobsMin, {
			updateSliderInput(
				session = session,
				inputId = "optQprobsMax",
				min = input$optQprobsMin
			)
		})
		observeEvent(input$optQprobsMax, {
			updateSliderInput(
				session = session,
				inputId = "optQprobsMin",
				max = input$optQprobsMax
			)
		})

		# v <- reactiveValues(
		# 	paramList = list(
		# 		"beta" = list("shape1" = 0, "shape2" = 1),
		# 		"binom" = list("prob" = 0.5, "size" = length(mtcars$mpg)),
		# 		"chisq" = list("df" = 1), ###
		# 		"exp" = list("rate" = 1), ###
		# 		"f" = list("df1" = 1, "df2" = 1),
		# 		"gamma" = list("shape" = 0, "rate" = 1),
		# 		"norm" = list("mean" = 0, "sd" = 1),
		# 		"pois" = list("lambda" = 1) ###
		# 	)
		# )
		#
		# observeEvent(input$dist, {
		# 	if(input$dist %in% c("qbeta", "qchisq", "qexp", "qf", "qgamma", "qpois"))
		# 		updateSliderInput(
		# 			session,
		# 			inputId = "sliderPos1",
		# 			label = names(v$paramList[[input$dist]])[1],
		# 			value = 1
		# 		)
		# 	if(input$dist %in% c("qbeta", "qf", "qgamma", "qnorm"))
		# 		updateSliderInput(
		# 			session,
		# 			inputId = "sliderPos2",
		# 			label = names(v$paramList[[input$dist]])[2],
		# 			value = 1
		# 		)
		# 	if(input$dist %in% c("qnorm"))
		# 		updateSliderInput(
		# 			session,
		# 			inputId = "sliderReal",
		# 			label = names(v$paramList[[input$dist]])[1],
		# 			value = 0
		# 		)
		# 	if(input$dist %in% c("qbinom"))
		# 		updateSliderInput(
		# 			session,
		# 			inputId = "slider01",
		# 			label = names(v$paramList[[input$dist]])[1],
		# 			value = 0.5
		# 		)
		# })
		#
		# observeEvent(input$sliderPos1, {
		# 	v$paramList[["qbeta"]][[1]] <- input$sliderPos1
		# 	v$paramList[["qchisq"]][[1]] <- input$sliderPos1
		# 	v$paramList[["qexp"]][[1]] <- input$sliderPos1
		# 	v$paramList[["qf"]][[1]] <- input$sliderPos1
		# 	v$paramList[["qgamma"]][[1]] <- input$sliderPos1
		# 	v$paramList[["qpois"]][[1]] <- input$sliderPos1
		# })
		#
		# observeEvent(input$sliderPos2, {
		# 	v$paramList[["qbeta"]][[2]] <- input$sliderPos2
		# 	v$paramList[["qf"]][[2]] <- input$sliderPos2
		# 	v$paramList[["qgamma"]][[2]] <- input$sliderPos2
		# 	v$paramList[["qnorm"]][[2]] <- input$sliderPos2
		# })
		#
		# observeEvent(input$sliderReal, {
		# 	v$paramList[["qnorm"]][[1]] <- input$sliderReal
		# })
		#
		# observeEvent(input$slider01, {
		# 	v$paramList[["qbinom"]][[1]] <- input$slider01
		# })

		output$ggPlot <- renderPlot({
			gg <- ggplot(data = smp, mapping = aes(sample = norm))
			if(input$plotType == "qq") {
				if("band" %in% input$plotFunc) {
					gg <- gg +
						stat_qq_band(
							detrend = input$optDetrend,
							qprobs = c(input$optQprobsMin, input$optQprobsMax),
							bandType = input$optBandTypeQq,
							conf = input$optConf,
							B = input$optB,
							distribution = input$optDistribution
						)
				}
				if("line" %in% input$plotFunc) {
					gg <- gg +
						stat_qq_line(
							detrend = input$optDetrend,
							qprobs = c(input$optQprobsMin, input$optQprobsMax),
							distribution = input$optDistribution
						)
				}
				if("point" %in% input$plotFunc) {
					gg <- gg +
						stat_qq_point(
							detrend = input$optDetrend,
							qprobs = c(input$optQprobsMin, input$optQprobsMax),
							distribution = input$optDistribution
						)
				}
				gg + labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
			}
			else {
				if("band" %in% input$plotFunc) {
					gg <- gg + stat_pp_band(detrend = input$optDetrend,
																	bandType = input$optBandTypePp,
																	conf = input$optConf,
																	B = input$optB,
																	distribution = input$optDistribution)
				}
				if("line" %in% input$plotFunc) {
					gg <- gg + stat_pp_line(detrend = input$optDetrend,
																	ab = c(input$optABIntercept, input$optABSlope))
				}
				if("point" %in% input$plotFunc) {
					gg <- gg + stat_pp_point(detrend = input$optDetrend,
																	 distribution = input$optDistribution)
				}
				gg + labs(x = "Probability Points", y = "Cumulative Probability")
			}
		})
	}
)
