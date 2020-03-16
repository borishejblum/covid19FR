#' Launch Covid19FRNA Shiny App
#'
#'@param ... additional arguments to be passed to the \link[shiny]{runApp} function.
#'
#'@examples
#'if(interactive()){
#' vici::run_app()
#'}
#'
#' @export
#' @importFrom shiny runApp
run_app <- function(...) {
  shiny::runApp(system.file("app", package = "Covid19FRNA"), port=8080 ,...)
}


