
#' run_viewer
#'
#' Launch a local shiny app for viewing fires
#'
#' @export
run_viewer <- function() {

  appDir <- system.file("shiny", "fire_viewer", package = "goesfire")
  if (appDir == "") {
    stop("Could not find shiny directory. Try reinstalling `goesfire`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")

}
