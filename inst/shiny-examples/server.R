require(shiny)

source("global.R")

shinyServer(
	function(input, output, session) {
		v <- reactiveValues(
			paramList = list(
				"qbeta" = list("shape1" = 0, "shape2" = 1),
				"qbinom" = list("prob" = 0.5, "size" = length(mtcars$mpg)),
				"qchisq" = list("df" = 1), ###
				"qexp" = list("rate" = 1), ###
				"qf" = list("df1" = 1, "df2" = 1),
				"qgamma" = list("shape" = 0, "rate" = 1),
				"qnorm" = list("mean" = 0, "sd" = 1),
				"qpois" = list("lambda" = 1) ###
			)
		)

		observeEvent(input$dist, {
			if(input$dist %in% c("qbeta", "qchisq", "qexp", "qf", "qgamma", "qpois"))
				updateSliderInput(
					session,
					inputId = "sliderPos1",
					label = names(v$paramList[[input$dist]])[1],
					value = 1
				)
			if(input$dist %in% c("qbeta", "qf", "qgamma", "qnorm"))
				updateSliderInput(
					session,
					inputId = "sliderPos2",
					label = names(v$paramList[[input$dist]])[2],
					value = 1
				)
			if(input$dist %in% c("qnorm"))
				updateSliderInput(
					session,
					inputId = "sliderReal",
					label = names(v$paramList[[input$dist]])[1],
					value = 0
				)
			if(input$dist %in% c("qbinom"))
				updateSliderInput(
					session,
					inputId = "slider01",
					label = names(v$paramList[[input$dist]])[1],
					value = 0.5
				)
		})

		observeEvent(input$sliderPos1, {
			v$paramList[["qbeta"]][[1]] <- input$sliderPos1
			v$paramList[["qchisq"]][[1]] <- input$sliderPos1
			v$paramList[["qexp"]][[1]] <- input$sliderPos1
			v$paramList[["qf"]][[1]] <- input$sliderPos1
			v$paramList[["qgamma"]][[1]] <- input$sliderPos1
			v$paramList[["qpois"]][[1]] <- input$sliderPos1
		})

		observeEvent(input$sliderPos2, {
			v$paramList[["qbeta"]][[2]] <- input$sliderPos2
			v$paramList[["qf"]][[2]] <- input$sliderPos2
			v$paramList[["qgamma"]][[2]] <- input$sliderPos2
			v$paramList[["qnorm"]][[2]] <- input$sliderPos2
		})

		observeEvent(input$sliderReal, {
			v$paramList[["qnorm"]][[1]] <- input$sliderReal
		})

		observeEvent(input$slider01, {
			v$paramList[["qbinom"]][[1]] <- input$slider01
		})

		output$distPlot <- renderPlotly({
			gg <- ggplot(mtcars, aes(sample = mpg)) +
				geom_qq(distribution = input$dist,
								dparams = v$paramList[[input$dist]])

			plotly::ggplotly(gg)
		})
	}
)
