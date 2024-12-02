#' Run the rapadm Shiny Application
#'
#' @return An object representing the rapadm app
#' @export

run_app <- function(browser = FALSE) {

  rapbase::loggerSetup()
  shiny::shinyApp(
    ui = app_ui,
    server = app_server,
    options = list(launch.browser = browser)
  )
}
