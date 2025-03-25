#' Run the rapadm Shiny Application
#'
#' @param browser Open app in browser window
#' @param logAsJson Log in json-format
#'
#' @return An object representing the rapadm app
#' @export

run_app <- function(browser = FALSE, logAsJson = FALSE) {

  if (logAsJson) {
    rapbase::loggerSetup()
  }
  shiny::shinyApp(
    ui = app_ui,
    server = app_server,
    options = list(launch.browser = browser)
  )
}
