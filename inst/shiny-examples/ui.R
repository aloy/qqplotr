require(shiny)

shinyUI(
	fluidPage(
		sidebarLayout(
			sidebarPanel(
				selectInput("dist",
										label = "Choose the distribution:",
										choices = dist,
										selected = "qnorm"),
				helpText(HTML('<div align="justify">For more information about the distribution parameters,
											please refer to their documentation.</div>')),
				conditionalPanel(
					condition = "input.dist == 'qbeta' | input.dist == 'qchisq' | input.dist == 'qexp' | input.dist == 'qf' | input.dist == 'qgamma' | input.dist == 'qpois'",
					sliderInput("sliderPos1",
											label = "",
											min = 10e-20,
											max = 100,
											value = 1)
				),
				conditionalPanel(
					condition = "input.dist == 'qnorm'",
					sliderInput("sliderReal",
											label = "",
											min = -100,
											max = 100,
											value = 0)
				),
				conditionalPanel(
					condition = "input.dist == 'qbeta' | input.dist == 'qf' | input.dist == 'qgamma' | input.dist == 'qnorm'",
					sliderInput("sliderPos2",
											label = "",
											min = 10e-20,
											max = 100,
											value = 1)
				),
				conditionalPanel(
					condition = "input.dist == 'qbinom'",
					sliderInput("slider01",
											label = "",
											min = 0,
											max = 1,
											value = 0.5)
				)
				),

			# Show a plot of the generated distribution
			mainPanel(
				plotlyOutput("distPlot")
			)
		)
	)
)
