#' Server logic for the rapadm app
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#'
#' @return A shiny app server object
#' @export

app_server <- function(input, output, session) {
  rapbase::logShinyInputChanges(input)

  user <- rapbase::navbarWidgetServer2(
    "rapadm-widget",
    orgName = "RapAdm",
    caller = packageName()
  )

  # Environment
  output$user <- shiny::renderText({
    paste("rapbase::getUserName(session):",
          user$name())
  })
  output$group <- shiny::renderText({
    paste("rapbase::getUserGroups(session):",
          user$group())
  })
  output$resh_id <- shiny::renderText({
    paste("rapbase::getUserReshId(session):",
          user$org())
  })
  output$role <- shiny::renderText({
    paste("rapbase::getUserRole(session):",
          user$role())
  })
  output$email <- shiny::renderText({
    paste("rapbase::getUserEmail(session):",
          user$name())
  })
  output$full_name <- shiny::renderText({
    paste("rapbase::getUserFullName(session):",
          user$name())
  })
  output$phone <- shiny::renderText({
    paste("rapbase::getUserPhone(session):",
          user$phone())
  })
  output$instance <- shiny::renderText({
    Sys.getenv("R_RAP_INSTANCE")
  })
  output$config_path <- shiny::renderText({
    Sys.getenv("R_RAP_CONFIG_PATH")
  })
  output$sp_usergroups <- shiny::renderText({
    paste("Sys.getenv('SHINYPROXY_USERGROUPS'):",
          Sys.getenv("SHINYPROXY_USERGROUPS"))
  })
  output$locale <- shiny::renderText({
    Sys.getlocale()
  })


  # Configuration"
  output$rapbase_config <- shiny::renderText({
    f <- file.path(Sys.getenv("R_RAP_CONFIG_PATH"), "rapbaseConfig.yml")
    if (file.exists(f)) {
      yaml::as.yaml(yaml::read_yaml(f))
    } else {
      "Not found!"
    }
  })


  # Logs
  output$container_log_ui <- shiny::renderUI({
    f <- file.info(
      list.files("/container_logs", full.names = TRUE, pattern = "\\.log$")
    )
    f <- f %>%
      dplyr::arrange(dplyr::desc(.data$mtime)) %>%
      dplyr::slice_head(n = 50)
    log_file <- rownames(f)
    names(log_file) <- basename(rownames(f))
    shiny::selectInput(
      inputId = "container_log",
      label = "Select a log file:",
      choices = as.list(log_file)
    )
  })

  output$container_log <- shiny::renderText({
    shiny::req(input$container_log)
    raw_text <- readLines(input$container_log)
    paste0(raw_text, collapse = "\n")
  })


  # Usestats
  log <- shiny::reactive({
    rapbase:::readLog(type = input$type, name = "") %>%
      rapbase::logFormat()
    # NULL
  })

  output$download <- shiny::downloadHandler(
    filename = function() {
      basename(
        tempfile(
          pattern = paste0(input$type, "_usestats_"),
          fileext = ".csv"
        )
      )
    },
    content = function(file) {
      if (input$downloadFormat == "xlsx-csv") {
        readr::write_excel_csv2(log(), file)
      } else {
        readr::write_csv2(log(), file)
      }
    }
  )

  output$pivot <- rpivotTable::renderRpivotTable(
    rpivotTable::rpivotTable(
      log(),
      rows = c("group"),
      cols = c("year", "month"),
      rendererName = "Heatmap"
    )
  )

  # Autoreport
  ar <- rapbase::readAutoReportData(target = "db")

  far <- shiny::reactive({
    shiny::req(input$fpackage, input$ftype, input$fowner, input$forganization)
    far <- ar
    if (input$fpackage != "no filter") {
      far <- rapbase::filterAutoRep(
        far,
        "package",
        input$fpackage,
        target = "db"
      )
    }
    if (input$ftype != "no filter") {
      far <- rapbase::filterAutoRep(far, "type", input$ftype, target = "db")
    }
    if (input$fowner != "no filter") {
      far <- rapbase::filterAutoRep(far, "owner", input$fowner, target = "db")
    }
    if (input$forganization != "no filter") {
      far <- rapbase::filterAutoRep(
        far,
        "organization",
        input$forganization,
        target = "db"
      )
    }
    far
  })

  output$fpackage <- shiny::renderUI({
    shiny::selectInput(
      "fpackage",
      "- registry:",
      choices = c("no filter", unique_autoreport(ar, "package"))
    )
  })
  output$ftype <- shiny::renderUI({
    shiny::selectInput(
      "ftype",
      "- type:",
      choices = c("no filter", unique_autoreport(ar, "type"))
    )
  })
  output$fowner <- shiny::renderUI({
    shiny::selectInput(
      "fowner",
      "- owner:",
      choices = c("no filter", unique_autoreport(ar, "owner"))
    )
  })
  output$forganization <- shiny::renderUI({
    shiny::selectInput(
      "forganization",
      "- organization:",
      choices = c("no filter", unique_autoreport(ar, "organization"))
    )
  })

  output$calendar <- shiny::renderPlot({
    plot(calendar_autoreport(far()))
  })

  output$autoreport_data <- DT::renderDT({
    far()
  })

  # Staging
  shiny::observeEvent(input$do_remove, {
    rapbase::cleanStagingData(0, dryRun = FALSE)
  })
  output$remove_staging <- shiny::renderUI({
    if (input$delete_staging) {
      shiny::actionButton(
        "do_remove", "Delete staging data", icon = shiny::icon("trash")
      )
    } else {
      NULL
    }
  })
  output$staging_data <- shiny::renderPrint({
    input$do_remove
    res <- rapbase::cleanStagingData(0)
    if (length(res) < 1) {
      "There is no staging data."
    } else {
      res
    }
  })

}
