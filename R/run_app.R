#' Launch covid19FR Shiny App
#'
#'@param ... additional arguments to be passed to the \link[shiny]{runApp} function.
#'
#'@examples
#'if(interactive()){
#' covid19FR::run_app()
#'}
#'
#' @export
#' @importFrom shiny runApp
run_app <- function(...) {
  shiny::runApp(list(ui = app_ui, 
                     server = app_server), 
                port = 8080,
                ...)
}


