#' Run the rapadm Shiny Application
#'
#' @return An object representing the rapadm app
#' @export

run_app <- function() {

  rapbase::loggerSetup()
  shiny::shinyApp(ui = app_ui, server = app_server)
}
