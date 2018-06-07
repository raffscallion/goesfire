
#' run_viewer
#'
#' Launch a local shiny app for viewing fires
#'
#' @export
run_viewer <- function() {

  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' required to use this function. Please install it.\n
         Also need 'leaflet', 'leaflet.extras', 'cowplot', 'DT'", call. = FALSE)
  }
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("Package 'leaflet' required to use this function. Please install it.\n
         Also need 'shiny', 'leaflet.extras', 'cowplot', 'DT'", call. = FALSE)
  }
  if (!requireNamespace("leaflet.extras", quietly = TRUE)) {
    stop("Package 'leaflet.extras' required to use this function. Please install it.\n
         Also need 'leaflet', 'shiny', 'cowplot', 'DT'", call. = FALSE)
  }
  if (!requireNamespace("cowplot", quietly = TRUE)) {
    stop("Package 'cowplot' required to use this function. Please install it.\n
         Also need 'leaflet', 'leaflet.extras', 'shiny', 'DT'", call. = FALSE)
  }
  if (!requireNamespace("DT", quietly = TRUE)) {
    stop("Package 'DT' required to use this function. Please install it.\n
         Also need 'leaflet', 'leaflet.extras', 'cowplot', 'shiny'", call. = FALSE)
  }

  appDir <- system.file("shiny", "fire_viewer", package = "goesfire")
  if (appDir == "") {
    stop("Could not find shiny directory. Try reinstalling `goesfire`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")

}
