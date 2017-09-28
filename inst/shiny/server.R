if (!requireNamespace("shiny", quietly = TRUE)) {
	stop("'shiny' package is needed for the Shiny example app to work properly. Please install it.",
			 call. = FALSE)
} else {
	require(shiny)
}

source("global.R")

shinyServer(
	function(input, output, session) {
		v <- reactiveValues(
			paramList = list(
				"beta" = list("shape1" = 0, "shape2" = 1),
				"cauchy" = list("scale" = 1, "location" = 0),
				"chisq" = list("df" = 1),
				"exp" = list("rate" = 1),
				"f" = list("df1" = 1, "df2" = 1),
				"gamma" = list("shape" = 0, "scale" = 1),
				"lnorm" = list("sdlog" = 1, "meanlog" = 0),
				"norm" = list("sd" = 1, "mean" = 0),
				"t" = list("df" = 1),
				"unif" = list("min" = 0, "max" = 1),
				"weibull" = list("shape" = 0, "scale" = 1)
			)
		)

		# update input variable selectInput
		observeEvent(input$inputData, {
			if(input$inputData == "simulated") {
				updateSelectInput(
					session = session,
					inputId = "inputVar",
					choices = simulatedNames
				)
			} else {
				updateSelectInput(
					session = session,
					inputId = "inputVar",
					choices = names(get(input$inputData))
				)
			}
		})

		# update qprobs sliders limits
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

		# change sliders' names based on input$optDistribution
		observeEvent(input$optDistribution, {
			if(input$optDistribution %in% c("beta", "gamma", "weibull")) {
				updateSliderInput(
					session,
					inputId = "sliderPos1",
					label = names(v$paramList[[input$optDistribution]])[1],
					value = 0
				)
			}
			if(input$optDistribution %in% c("cauchy", "chisq", "exp", "f", "lnorm", "norm", "t")) {
				updateSliderInput(
					session,
					inputId = "sliderPos1",
					label = names(v$paramList[[input$optDistribution]])[1],
					value = 1
				)
			}
			if(input$optDistribution %in% c("beta", "f", "gamma", "weibull")) {
				updateSliderInput(
					session,
					inputId = "sliderPos2",
					label = names(v$paramList[[input$optDistribution]])[2],
					value = 1
				)
			}
			if(input$optDistribution %in% c("cauchy", "lnorm", "norm")) {
				updateSliderInput(
					session,
					inputId = "sliderReal1",
					label = names(v$paramList[[input$optDistribution]])[2],
					value = 0
				)
			}
			if(input$optDistribution %in% c("unif")) {
				updateSliderInput(
					session,
					inputId = "sliderReal1",
					label = names(v$paramList[[input$optDistribution]])[1],
					value = 0
				)
				updateSliderInput(
					session,
					inputId = "sliderReal2",
					label = names(v$paramList[[input$optDistribution]])[2],
					value = 1
				)
			}
		})

		# change distribution parameter values based on the sliders' input
		observeEvent(input$sliderPos1, {
			v$paramList[["beta"]][[1]] <- input$sliderPos1
			v$paramList[["cauchy"]][[1]] <- input$sliderPos1
			v$paramList[["chisq"]][[1]] <- input$sliderPos1
			v$paramList[["exp"]][[1]] <- input$sliderPos1
			v$paramList[["f"]][[1]] <- input$sliderPos1
			v$paramList[["gamma"]][[1]] <- input$sliderPos1
			v$paramList[["lnorm"]][[1]] <- input$sliderPos1
			v$paramList[["norm"]][[1]] <- input$sliderPos1
			v$paramList[["t"]][[1]] <- input$sliderPos1
			v$paramList[["weibull"]][[1]] <- input$sliderPos1
		})
		observeEvent(input$sliderPos2, {
			v$paramList[["beta"]][[2]] <- input$sliderPos2
			v$paramList[["f"]][[2]] <- input$sliderPos2
			v$paramList[["gamma"]][[2]] <- input$sliderPos2
			v$paramList[["weibull"]][[2]] <- input$sliderPos2
		})
		observeEvent(input$sliderReal1, {
			v$paramList[["cauchy"]][[2]] <- input$sliderReal1
			v$paramList[["lnorm"]][[2]] <- input$sliderReal1
			v$paramList[["norm"]][[2]] <- input$sliderReal1
			v$paramList[["unif"]][[1]] <- input$sliderReal1
		})
		observeEvent(input$sliderReal2, {
			v$paramList[["unif"]][[2]] <- input$sliderReal2
		})

		# renders ggplot
		output$ggPlot <- renderPlot({
			gg <- ggplot(data = get(input$inputData), mapping = aes_string(sample = input$inputVar))
			if(input$plotType == "qq") {
				if("band" %in% input$plotFunc) {
					gg <- gg +
						stat_qq_band(
							detrend = input$optDetrend,
							qprobs = c(input$optQprobsMin, input$optQprobsMax),
							bandType = input$optBandTypeQq,
							conf = input$optConf,
							B = input$optB,
							distribution = input$optDistribution,
							dparams = v$paramList[[input$optDistribution]]
						)
				}
				if("line" %in% input$plotFunc) {
					gg <- gg +
						stat_qq_line(
							detrend = input$optDetrend,
							qprobs = c(input$optQprobsMin, input$optQprobsMax),
							distribution = input$optDistribution,
							dparams = v$paramList[[input$optDistribution]]
						)
				}
				if("point" %in% input$plotFunc) {
					gg <- gg +
						stat_qq_point(
							detrend = input$optDetrend,
							qprobs = c(input$optQprobsMin, input$optQprobsMax),
							distribution = input$optDistribution,
							dparams = v$paramList[[input$optDistribution]]
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
																	distribution = input$optDistribution,
																	dparams = v$paramList[[input$optDistribution]])
				}
				if("line" %in% input$plotFunc) {
					gg <- gg + stat_pp_line(detrend = input$optDetrend,
																	ab = c(input$optABIntercept, input$optABSlope))
				}
				if("point" %in% input$plotFunc) {
					gg <- gg + stat_pp_point(detrend = input$optDetrend,
																	 distribution = input$optDistribution,
																	 dparams = v$paramList[[input$optDistribution]])
				}
				gg + labs(x = "Probability Points", y = "Cumulative Probability")
			}
		})
	}
)
