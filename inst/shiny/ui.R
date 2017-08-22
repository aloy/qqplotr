if (!requireNamespace("shiny", quietly = TRUE)) {
	stop("'shiny' package is needed for this Shiny app to work. Please install it.",
			 call. = FALSE)
} else {
	require(shiny)
}

shinyUI(
	fluidPage(
		sidebarLayout(
			sidebarPanel(
				h2("Plot"),

				fluidRow(
					column(
						width = 6,
						radioButtons(inputId = "plotType",
												 label = "Type",
												 choiceValues = c("qq", "pp"),
												 choiceNames = c("Q-Q", "P-P"),
												 inline = TRUE)
					),
					column(
						width = 6,
						checkboxGroupInput(inputId = "plotFunc",
															 label = "Functions",
															 choiceValues = c("point", "line", "band"),
															 choiceNames = c("Point", "Line", "Band"),
															 inline = TRUE)
					)
				),

				conditionalPanel(
					condition = "input.plotFunc.indexOf('point') != -1 | input.plotFunc.indexOf('line') != -1 | input.plotFunc.indexOf('band') != -1",

					h2("Options"),

					# detrend
					h5(strong("Detrend")),
					checkboxInput(inputId = "optDetrend",
												label = "Yes",
												value = FALSE),

					# qprobs
					conditionalPanel(
						condition = "input.plotType.indexOf('qq') != -1",
						h5(strong("Quantile Probabilities")),
						fluidRow(
							column(
								width = 6,
								h5("Lower"),
								sliderInput("optQprobsMin",
														label = NULL,
														min = 0,
														max = 1,
														value = 0.25,
														step = 0.05,
														ticks = FALSE)
							),
							column(
								width = 6,
								h5("Upper"),
								sliderInput("optQprobsMax",
														label = NULL,
														min = 0,
														max = 1,
														value = 0.75,
														step = 0.05,
														ticks = FALSE)
							)
						)
					),

					fluidRow(
						# bandType
						column(
							width = 6,
							conditionalPanel(
								condition = "input.plotFunc.indexOf('band') != -1 & input.plotType.indexOf('qq') != -1",
								selectInput(inputId = "optBandTypeQq",
														label = "Confidence Bands Method",
														choices = c("Normal" = "normal", "Parametric Bootstrap" = "bs", "Tail-sensitive" = "ts"),
														selected = "normal")
							),
							conditionalPanel(
								condition = "input.plotFunc.indexOf('band') != -1 & input.plotType.indexOf('pp') != -1",
								selectInput(inputId = "optBandTypePp",
														label = "Confidence Bands Method",
														choices = c("Parametric Bootstrap" = "bs"),
														selected = "bs")
							)
						),
						# conf
						column(
							width = 6,
							conditionalPanel(
								condition = "input.plotFunc.indexOf('band') != -1",
								sliderInput("optConf",
														label = "Bands Confidence Level",
														min = 0,
														max = 1,
														value = 0.95,
														step = 0.05,
														ticks = FALSE)
							)
						)
					),

					fluidRow(
						# B (number of simulations)
						column(
							width = 6,
							conditionalPanel(
								condition = "input.plotFunc.indexOf('band') != -1 & (input.optBandTypeQq.indexOf('bs') != -1 | input.optBandTypeQq.indexOf('ts') != -1 | input.optBandTypePp.indexOf('bs') != -1)",
								sliderInput("optB",
														label = "B (# of Simulations)",
														min = 500,
														max = 5000,
														value = 1000,
														step = 500,
														ticks = FALSE)
							)
						)
					),

					# ab (intercept / slope of P-P reference line)
					conditionalPanel(
						condition = "input.plotFunc.indexOf('line') != -1 & input.plotType.indexOf('pp') != -1 & !input.optDetrend",
						h5(strong("Reference Line Parameters")),
						fluidRow(
							column(
								width = 6,
								h5("Intercept"),
								sliderInput("optABIntercept",
														label = NULL,
														min = 0,
														max = 1,
														value = 0,
														step = 0.05,
														ticks = FALSE)
							),
							column(
								width = 6,
								h5("Slope"),
								sliderInput("optABSlope",
														label = NULL,
														min = -1,
														max = 1,
														value = 1,
														step = 0.05,
														ticks = FALSE)
							)
						)
					),

					# distribution
					conditionalPanel(
						condition = "input.plotType.indexOf('qq') != -1 | (input.plotType.indexOf('pp') != -1 & (input.plotFunc.indexOf('point') != -1 | input.plotFunc.indexOf('band') != -1))",

						# distribution
						selectInput(inputId = "optDistribution",
												label = "Distribution",
												choices = dist,
												selected = "norm"),
						helpText(HTML('<div align="justify">For more information about the distribution parameters,
													please refer to their documentation.</div>'))

						# dparams
						# conditionalPanel(
						# 	condition = "input.dist == 'beta' | input.dist == 'chisq' | input.dist == 'exp' | input.dist == 'f' | input.dist == 'gamma' | input.dist == 'pois'",
						# 	sliderInput("sliderPos1",
						# 							label = "",
						# 							min = 10e-20,
						# 							max = 100,
						# 							value = 1)
						# ),
						# conditionalPanel(
						# 	condition = "input.dist == 'norm'",
						# 	sliderInput("sliderReal",
						# 							label = "",
						# 							min = -100,
						# 							max = 100,
						# 							value = 0)
						# ),
						# conditionalPanel(
						# 	condition = "input.dist == 'beta' | input.dist == 'f' | input.dist == 'gamma' | input.dist == 'norm'",
						# 	sliderInput("sliderPos2",
						# 							label = "",
						# 							min = 10e-20,
						# 							max = 100,
						# 							value = 1)
						# ),
						# conditionalPanel(
						# 	condition = "input.dist == 'binom'",
						# 	sliderInput("slider01",
						# 							label = "",
						# 							min = 0,
						# 							max = 1,
						# 							value = 0.5)
						# )
					)
				)
			),

			# Show a plot of the generated distribution
			mainPanel(
				plotOutput("ggPlot", height = "500px")
			)
		)
	)
)
