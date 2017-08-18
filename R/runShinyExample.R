#' 'qqplotr' Example Shiny App
#' @keywords internal
#' @usage NULL
#' @export
runShinyExample <- function() {
	appDir <- system.file("shiny", package = "qqplotr")
	if (appDir == "") {
		stop("Could not find example directory. Try re-installing `qqplotr`.",
				 call. = FALSE)
	}

	shiny::runApp(appDir, display.mode = "normal")
}
