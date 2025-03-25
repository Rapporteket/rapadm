#' Client (ui) for the rapadm app
#'
#' @return An shiny app ui object
#' @export

app_ui <- function() {

  app_title <- "RapAdm"

  shiny::tagList(
    shiny::navbarPage(
      theme = bslib::bs_theme(
        bootswatch = "flatly"
      ),
      title = app_title,
      windowTitle = app_title,
      id = "tabs",

      shiny::tabPanel(
        "Environment",
        rapbase::navbarWidgetInput("rapadm-widget", selectOrganization = TRUE),
        shiny::h4("Test 'rapbase' functions using the session object:"),
        shiny::textOutput("user"),
        shiny::textOutput("group"),
        shiny::textOutput("resh_id"),
        shiny::textOutput("role"),
        shiny::textOutput("email"),
        shiny::textOutput("full_name"),
        shiny::textOutput("phone"),
        shiny::h4("Environment var R_RAP_INSTANCE:"),
        shiny::textOutput("instance"),
        shiny::h4("Environment var R_RAP_CONFIG_PATH:"),
        shiny::textOutput("config_path"),
        shiny::h4("Environment var(s) provided by SHINYPROXY (if any):"),
        shiny::textOutput("sp_usergroups"),
        shiny::h4("Locale settings:"),
        shiny::textOutput("locale")
      ),

      shiny::tabPanel(
        "Configuration",

        shiny::tabsetPanel(
          shiny::tabPanel(
            "rapbaseConfig",
            shiny::verbatimTextOutput("rapbase_config")
          )
        )
      ),

      shiny::tabPanel(
        "Logs",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::uiOutput("container_log_ui")
          ),
          shiny::mainPanel(
            shiny::verbatimTextOutput("container_log")
          )
        )
      ),

      shiny::tabPanel(
        "Usestats",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::radioButtons(
              "type",
              label = shiny::tags$div(
                shiny::HTML(as.character(shiny::icon("shapes")), "Type:")
              ),
              choices = list(Application = "app", Report = "report")
            ),
            shiny::radioButtons(
              "downloadFormat",
              label = shiny::tags$div(
                shiny::HTML(
                  as.character(shiny::icon("file-csv")), "File format:"
                )
              ),
              choices = c("csv", "xlsx-csv")
            ),
            shiny::downloadButton("download", "Download!")
          ),
          shiny::mainPanel(
            rpivotTable::rpivotTableOutput("pivot")
          )
        )
      ),

      shiny::tabPanel(
        "Autoreports",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::p("Filter"),
            shiny::uiOutput("fpackage"),
            shiny::uiOutput("ftype"),
            shiny::uiOutput("fowner"),
            shiny::uiOutput("forganization")
          ),
          shiny::mainPanel(
            shiny::h2("Auto reports running one year from now"),
            shiny::plotOutput("calendar"),
            shiny::h2("Auto report raw data"),
            DT::DTOutput("autoreport_data")
          )
        )
      )
    )
  )
}
