if (!requireNamespace("shiny", quietly = TRUE)) {
	stop("'shiny' package is needed for the Shiny example app to work properly. Please install it.",
			 call. = FALSE)
} else {
	require(shiny)
}

shinyUI(
	fluidPage(
		titlePanel(strong("qqplotr Example App")),
		sidebarLayout(
			sidebarPanel(
				h2("Input"),

				fluidRow(
					column(
						width = 6,
						selectInput(inputId = "inputData",
												label = "Dataset",
												choices = datasets,
												selected = "simulated")
					),
					column(
						width = 6,
						selectInput(inputId = "inputVar",
												label = "Variable",
												choices = NULL,
												selected = NULL)
					)
				),

				bsTooltip(
					id = "inputData",
					title = '<div align="justify">Format: <br> dataset (package)</div>',
					placement = "top",
					trigger = "focus",
					options = list(container = "body")
				),

				h2("Plot"),

				fluidRow(
					column(
						width = 4,
						radioButtons(inputId = "plotType",
												 label = "Type",
												 choiceValues = c("qq", "pp"),
												 choiceNames = c("Q-Q", "P-P"),
												 inline = TRUE)
					),
					column(
						width = 8,
						checkboxGroupInput(inputId = "plotFunc",
															 label = "Functions",
															 choiceValues = c("point", "line", "band"),
															 choiceNames = c("Point", "Line", "Band"),
															 inline = TRUE)
					)
				),

				conditionalPanel(
					condition = "input.plotFunc.indexOf('point') != -1 | input.plotFunc.indexOf('line') != -1 | input.plotFunc.indexOf('band') != -1",

					hr(),

					h2("Options"),

					fluidRow(
						# detrend
						column(
							width = 4,
							h5(strong("Detrend")),
							checkboxInput(inputId = "optDetrend",
														label = "Yes",
														value = FALSE)
						),
						# qprobs
						column(
							width = 8,
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
							)
						)
					),

					conditionalPanel(
						condition = "input.plotFunc.indexOf('band') != -1",

						hr(),

						fluidRow(
							# bandType
							column(
								width = 4,
								conditionalPanel(
									condition = "input.plotType.indexOf('qq') != -1",
									selectInput(inputId = "optBandTypeQq",
															label = "Confidence Bands Method",
															choices = c("Normal" = "pointwise", "Bootstrap" = "boot", "Tail-sensitive" = "ts", "Kolmogorov-Smirnov" = "ks"),
															selected = "pointwise")
								),
								conditionalPanel(
									condition = "input.plotType.indexOf('pp') != -1",
									selectInput(inputId = "optBandTypePp",
															label = "Confidence Bands Method",
															choices = c("Bootstrap" = "boot"),
															selected = "bs")
								)
							),
							# conf
							column(
								width = 4,
								sliderInput("optConf",
														label = "Confidence Level",
														min = 0,
														max = 0.99,
														value = 0.95,
														step = 0.05,
														ticks = FALSE)
							),
							# B (number of simulations)
							column(
								width = 4,
								conditionalPanel(
									condition = "(input.plotType.indexOf('qq') != -1 & (input.optBandTypeQq.indexOf('bs') != -1 | input.optBandTypeQq.indexOf('ts') != -1)) | (input.plotType.indexOf('pp') != -1 & input.optBandTypePp.indexOf('bs') != -1)",
									sliderInput("optB",
															label = "Number of Simulations",
															min = 500,
															max = 5000,
															value = 1000,
															step = 500,
															ticks = FALSE)
								)
							)
						)
					),

					# ab (intercept / slope of P-P reference line)
					conditionalPanel(
						condition = "input.plotFunc.indexOf('line') != -1 & input.plotType.indexOf('pp') != -1 & !input.optDetrend",

						hr(),

						h5(strong("Reference Line Parameters")),

						fluidRow(
							column(
								width = 4,
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
								width = 4,
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

						hr(),

						fluidRow(
							# distribution
							column(
								width = 4,
								selectInput(inputId = "optDistribution",
														label = "Distribution",
														choices = dist,
														selected = "norm"),
								bsTooltip(
									id = "optDistribution",
									title = '<div align="justify">For more info on the distributional parameters, please refer to the <strong>stats</strong> package documentation.</div>',
									placement = "top",
									trigger = "focus",
									options = list(container = "body")
								)
							),
							# dparams
							column(
								width = 4,
								conditionalPanel(
									condition = "input.optDistribution == 'beta' | input.optDistribution == 'cauchy' | input.optDistribution == 'chisq' | input.optDistribution == 'exp' | input.optDistribution == 'f' | input.optDistribution == 'gamma' | input.optDistribution == 'lnorm' | input.optDistribution == 'norm' | input.optDistribution == 't' | input.optDistribution == 'weibull'",
									sliderInput("sliderPos1",
															label = "",
															min = 10e-20,
															max = 100,
															value = 1)
								)
							),
							column(
								width = 4,
								conditionalPanel(
									condition = "input.optDistribution == 'beta' | input.optDistribution == 'f' | input.optDistribution == 'gamma' | input.optDistribution == 'weibull'",
									sliderInput("sliderPos2",
															label = "",
															min = 10e-20,
															max = 100,
															value = 1)
								)
							),
							column(
								width = 4,
								conditionalPanel(
									condition = "input.optDistribution == 'cauchy' | input.optDistribution == 'lnorm' | input.optDistribution == 'norm' | input.optDistribution == 'unif'",
									sliderInput("sliderReal1",
															label = "",
															min = -100,
															max = 100,
															value = 0)
								)
							),
							column(
								width = 4,
								conditionalPanel(
									condition = "input.optDistribution == 'unif'",
									sliderInput("sliderReal2",
															label = "",
															min = -100,
															max = 100,
															value = 0)
								)
							)
						)
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
